###################################
## Downloading environmental data
##################################

# Clear memory ----
rm(list=ls())

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

# Load the necessary packages
library(rerddap)
library(ncdf4)
library(mapdata)
library(tidyverse)
library(lubridate)
library(MetBrewer)

# Define the ERDDAP server URL and dataset ID ----

server_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "NOAA_DHW"  

#browse('NOAA_DHW')
info('NOAA_DHW')

# Query parameters 

latitude = c(-22.37, -22.75)
longitude = c(113.78, 113.62)
time = c("2017-01-01", "2021-12-31")

# create gridapp query ----

sstInfo <- info('NOAA_DHW')
SST <- griddap(sstInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c("2017-01-01", "2021-12-31"), fields = 'CRW_SST')
head(SST)

# play with daily sst to check spatial extent of data

sstInfo <- info('NOAA_DHW')
# get latest daily sst
murSST <- griddap(sstInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c('last','last'), fields = 'CRW_SST')
mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(-22.37, -22.75), xlim = c(113.78, 113.62))

ggplot(data = murSST$data, aes(x = longitude, y = latitude, fill = CRW_SST)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(113, 114.5),  ylim = c(-22, -23)) + ggtitle("Latest SST")


# Check the structure of the data
str(SST)

# Download the data to a dataframe
sst_df <- SST$data

# View the first few rows of the data
head(sst_df)
glimpse(sst_df)

# alrighty, now I guess we want to format date and time into something nice
# then find mean monthly temp 

# Convert the "time" column to a POSIXct datetime object ----
mean_sst_df <- sst_df %>%
  mutate(
    time = ymd_hms(time),  # Assumes the date string format "YYYY-MM-DDTHH:MM:SSZ"
    date = format(time, "%y-%m-%d"),
    year = factor(year(time)),
    month = factor(month(time, label = FALSE))
  )%>%
  
  group_by(year,month)%>%
  na.omit()%>%
  mutate(mean_sst= mean(CRW_SST))%>%
  dplyr::select(month, year, mean_sst)%>%
  distinct()%>%
  ungroup()%>%
  unite(yearmonth, c(year, month), sep = "-", remove = FALSE)%>%
  glimpse()

# quick plot

sst_plot <- ggplot(data =mean_sst_df, aes(x = month, y = mean_sst, colour = mean_sst)) +
  geom_point( size=3)+
  scale_colour_gradientn(colors = met.brewer("Hokusai3"))+
  theme_classic()
sst_plot

# sick

# Extract chla using dataset id erdMH1chla1day_R2022NRT ----

server_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "erdMH1chla1day"
 
#browse('erdMH1chla1day')
info('erdMH1chla1day')
 
# Query parameters 
 
latitude = c(-22.37, -22.75)
longitude = c(113.78, 113.62)
time = c("2017-01-01", "2021-12-31")

 # create gridapp query ----
 
chlaInfo <- info('erdMH1chla1day')
chla <- griddap(chlaInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c("2017-01-01", "2021-12-31"), fields = 'chlorophyll')
 head(chla)
 
# play with daily chla to check spatial extent of data
 
chlaInfo <- info('erdMH1chla1day')
chlaInfo

# get latest daily chla
Cloates_chla <- griddap(chlaInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c('2017-01-03','2017-01-03' ), fields = 'chlorophyll')
head(Cloates_chla)
mycolor <- colors$chlorophyll

w <- map_data("worldHires", ylim = c(-22.37, -22.75), xlim = c(113.78, 113.62))
 
ggplot(data = Cloates_chla$data, aes(x = longitude, y = latitude, fill = chlorophyll)) +
   geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
   geom_raster(interpolate = FALSE) +
   scale_fill_gradientn(colours = mycolor, na.value = NA) +
   theme_bw() + ylab("latitude") + xlab("longitude") +
   coord_fixed(1.3, xlim = c(113, 114.5),  ylim = c(-22, -23)) + ggtitle("Latest Chl a")
 
# Looks alright :) 
 
# reformat date, plot spatial extent and calculate monthly values ----
 
# Check the structure of the data
str(chla)

# Download the data to a dataframe
chla_df <- chla$data

# View the first few rows of the data
head(chla_df)
glimpse(chla_df)

# format date and time into something nice then find mean monthly temp 

mean_chla_df <- chla_df %>%
  mutate(
    time = ymd_hms(time),  # Assumes the date string format "YYYY-MM-DDTHH:MM:SSZ"
    date = format(time, "%y-%m-%d"),
    year = factor(year(time)),
    month = factor(month(time, label = FALSE))
  )%>%
  
  group_by(year, month)%>%
  na.omit()%>%
  mutate(mean_chla= mean(chlorophyll))%>%
  ungroup()%>%
  dplyr::select(month, year, mean_chla)%>%
  distinct()%>%
  unite(yearmonth, c(year, month), sep = "-", remove = FALSE)%>%
  glimpse()

head(mean_chla_df)
head(mean_sst_df)

# quick plot

chla_plot <- ggplot(data =mean_chla_df, aes(x = month, y = mean_chla, colour = mean_chla)) +
  geom_point( size=3)+
  scale_colour_gradientn(colors = met.brewer("Hokusai3"))+
  theme_classic()
chla_plot

# combine with sst and write nice csv file ----

sst_chla <- left_join(mean_sst_df, mean_chla_df, by =c("yearmonth", "month", "year"))%>%
            dplyr::select(month:mean_chla)%>%
            glimpse()

setwd(tidy.dir)

write.csv(sst_chla, file="SST_Chla_2017_2021.csv")


## PAR data ----

setwd(raw.dir)
dir()

PAR_dat <- read.csv("PAR.csv") %>%
  dplyr::select(location_name, time, qc_value, qc_flag)%>%
  unique()%>%
  mutate_at(vars(location_name, qc_flag), list(as.factor)) %>%
  filter(qc_flag == "Good")%>%
  mutate(
    time = ymd_hms(time),  # Assumes the date string format "YYYY-MM-DDTHH:MM:SSZ"
    date = format(time, "%y-%m-%d"),
    year = factor(year(time)),
    month = factor(month(time, label = FALSE)),
    day= factor(day(time)))%>%
  group_by(year, month,day)%>%
  mutate(daily_PAR=mean(qc_value))%>%
  glimpse()

group_by(year, month)%>%
mutate(monthly_PAR = mean(qc_value))%>%
  ungroup()%>%
  dplyr::select(location_name, year, month, monthly_PAR)%>%
  unique()%>%
  glimpse()

gg_PAR <- ggplot(aes(x=month, y=monthly_PAR, colour=year), data=PAR_dat)+
         geom_point()+
         scale_colour_manual(values=met.brewer("Hokusai3", 5))+
         theme_classic()
gg_PAR

setwd(plots.dir)
ggsave("monthlyPAR.tiff", gg_PAR, dpi=300 )
dir()

# Rainfall data ----
setwd(raw.dir)
rain_dat <- read.csv("Learmonth rainfall.csv")%>%
  dplyr::select(Year, Month, Day, Rainfall.amount..millimetres.)%>%
  na.omit()%>%
  filter(Year >2016)%>%
  filter(Year <2022)%>%
  mutate_at(vars(Year, Month), list(as.factor)) %>%
  group_by(Year, Month)%>%
  mutate(total_monthly_rain = sum( Rainfall.amount..millimetres.))%>%
  dplyr::select(Year, Month,total_monthly_rain )%>%
  rename(year = Year) %>%
  rename(month = Month) %>%
  unique()%>%
  glimpse()

gg_rain <- ggplot(aes(x=Month, y=total_monthly_rain, colour=Year), data=rain_dat)+
  geom_point()+
  scale_colour_manual(values=met.brewer("Hokusai3", 5))+
  theme_classic()
gg_rain

setwd(plots.dir)
ggsave("monthly_total_rain.tiff", gg_rain, dpi=300 )

# join rain and PAR ----

rain_PAR_dat <- left_join(rain_dat, PAR_dat, by = c("year", "month"))

head(rain_PAR_dat)

setwd(tidy.dir)
write.csv(file="Rainfall_PAR.csv", rain_PAR_dat)

## Significant wave height & wind data ----
## Wind = proxy for upwelling

setwd(raw.dir)
dir()

wave_wind_dat <- read.csv("wavesNorth_monthly.csv") %>%
  dplyr::select(Time, hs, vwnd) %>%
  mutate(date = dmy(Time)) %>%
  mutate(date = format(date, "%y-%m-%d"),
         year = factor(year(date)),
         month = factor(month(date, label = FALSE)),
         day = factor(day(date))) %>%
  group_by(month, year)%>%
  mutate(mean_hs =mean(hs))%>%
  ungroup()%>%
  filter(!year ==16)%>%
  glimpse()


gg_wave <- ggplot(aes(x=month, y=mean_hs, colour=year), data=wave_wind_dat)+
  geom_point()+
  scale_colour_manual(values=met.brewer("Hokusai3", 7))+
  theme_classic()
gg_wave

gg_wind <- ggplot(aes(x=month, y=vwnd, colour=year), data=wave_wind_dat )+
  geom_point()+
  scale_colour_manual(values=met.brewer("Hokusai3", 7))+
  theme_classic()
gg_wind 

setwd(plots.dir)
ggsave("wind_strength.tiff", gg_wave, dpi=300 )

setwd(tidy.dir)
write.csv(wave_wind_dat, file="wind_wave_2017_2021.csv")

# Fin