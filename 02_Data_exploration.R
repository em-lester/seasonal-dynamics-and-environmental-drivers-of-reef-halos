###################################################
## Some exploratory plots for halo size over time
##################################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----
library(tidyverse)
library(MetBrewer)

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

dat <- read.csv("2017_2021_mean_lengths.csv")%>%
  dplyr::select(id, Location, Status, date, MeanLength)%>%
  unique()%>%
  glimpse()



dat$date <-  as.Date(as.character(dat$date),format = "%Y%m%d") 
str(dat$date)

# Create separate columns for day, month, and year
dat$day <- format(dat$date, "%d")
dat$month <- format(dat$date, "%m")
dat$year <- format(dat$date, "%Y")

dat$month <- as.factor(dat$month)
dat$year <- as.factor(dat$year)
glimpse(dat)
str(dat$year)

# Find relative difference between length and mean length per halo ----

dat <- dat %>%
  group_by(id)%>%
  mutate(FiveYearMean =mean(MeanLength))%>%
  ungroup()%>%
  group_by(id, month, year)%>%
  mutate(Relative_Length =  FiveYearMean - MeanLength)%>%
  glimpse()

# Summary data for plot 
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

glimpse(dat)
summary_dat <- summarySE(dat, measurevar="Relative_Length", groupvars=c("year","month"))
summary_dat
summary(summary_dat)

summary_month_dat <- summarySE(dat, measurevar="Relative_Length", groupvars=c("month"))
summary_month_dat
summary(summary_dat)


summary_mean_dat <- summarySE(dat, measurevar="MeanLength", groupvars=c("year", "month"))
summary_mean_dat

summary(summary_mean_dat)

mean_halo_length <- mean(dat$MeanLength)
mean_halo_length
summary(dat)

test1 <- lm(MeanLength ~ year, dat=dat)

summary(test1)
anova(test1)
plot(test1)

# plot monthly mean length?

plot <-  ggplot(aes(x=month,y=MeanLength,  color=year), data=dat)+ facet_wrap(~id, scales='free')+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()

plot

# relative difference

relative_plot <-  ggplot(aes(x=month,y=Relative_Length,  color=year), data=dat)+ facet_wrap(~id, scales='free')+
  geom_point()+
  ylab("Length anomaly (m)")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()

relative_plot

plot_combined <-  ggplot(aes(x=month,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()

plot_combined

plot_combined_year <-  ggplot(aes(x=year,y=MeanLength,  color=year), data=dat)+
  geom_boxplot()+
  ylab("Mean halo length (m)")+
  xlab('Year')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()

plot_combined_year

glimpse(dat)
glimpse(summary_dat)

relative_plot_combined <-  ggplot(aes(x=month,y=Relative_Length, colour=year), data=dat)+
  geom_point(alpha=0.6)+
  ylab("Length anomaly (m)")+
  xlab('Month')+
 # geom_line(data = summary_dat, aes(x = month, y = Relative_Length, group = year, color = year), linewidth = 1) +
 # geom_ribbon(data = summary_dat, aes(x = month, ymin = Relative_Length-ci, ymax = Relative_Length+ci, group = year, fill=year), alpha = 0.1) +
  scale_color_manual(values=met.brewer("Renoir", 5))+
  scale_fill_manual(values=met.brewer("Renoir", 5))+
  theme_classic()

relative_plot_combined

setwd(plots.dir)
ggsave("Relative_Halo_Length_noline_exploratory_plot.tiff", relative_plot_combined, dpi=300)
ggsave("Mean_Halo_length_exploratory_plot.tiff", plot_combined, dpi=300)

# Look at halo per year
glimpse(dat)

summary_year_dat <- summarySE(dat, measurevar="Relative_Length", groupvars=c("year"))
summary_year_dat
summary(summary_dat)


year_plot_combined <-  ggplot(aes(x=year,y=Relative_Length*-1, colour=year, fill=year), data=summary_year_dat)+
  geom_point(size=3)+
  ylab("Relative Halo Length (m)")+
  xlab('Year')+
  geom_errorbar(aes(ymin=(Relative_Length*-1)-ci, ymax=(Relative_Length*-1)+ci,colour=year,), width=.3,  linewidth=2) +
  # geom_line(data = summary_dat, aes(x = month, y = Relative_Length, group = year, color = year), linewidth = 1) +
  # geom_ribbon(data = summary_dat, aes(x = month, ymin = Relative_Length-ci, ymax = Relative_Length+ci, group = year, fill=year), alpha = 0.1) +
  scale_color_manual(values=met.brewer("Renoir", 5))+
  scale_fill_manual(values=met.brewer("Renoir", 5))+
  theme_classic()+ theme(legend.position = "none")
year_plot_combined 

# Fin