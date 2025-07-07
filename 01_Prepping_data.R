#####################################################################
## Data prep for analysis of halo size per month (2021 as test run)
#####################################################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----

#install.packages("raster")
#install.packages("rgdal")
#install.packages("sp")

library(raster)
library(rgdal)
library(sp)
library(sf)
library(rgeos)
library(tidyverse)
#install.packages("Rmisc")
#library(geosphere)
library(maptools)
library(Rmisc)

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
setwd(shapefile.dir)
dir()

# Define a vector with the filenames of the shapefiles (removed 20170726 as corrupt?)
shapefile_filenames <- c("Measurements_20210128_Cloates.shp", "Measurements_20210321_Cloates.shp",
                         "Measurements_20210421_Cloates.shp", "Measurements_20210521_Cloates.shp",
                         "Measurements_20210628_Cloates.shp", "Measurements_20210723_Cloates.shp",
                         "Measurements_20210819_Cloates.shp", "Measurements_20210927_Cloates.shp",
                         "Measurements_20211030_Cloates.shp", "Measurements_20211220_Cloates.shp",
                         "Measurements_20200224_Cloates.shp", "Measurements_20200306_Cloates.shp",
                         "Measurements_20200502_Cloates.shp", "Measurements_20201008_Cloates.shp",
                         "Measurements_20200828_Cloates.shp", "Measurements_20200610_Cloates.shp",
                         "Measurements_20191203_Cloates.shp", "Measurements_20191107_Cloates.shp",
                         "Measurements_20190620_Cloates.shp", "Measurements_20190516_Cloates.shp",
                         "Measurements_20190426_Cloates.shp", "Measurements_20190307_Cloates.shp",
                         "Measurements_20190220_Cloates.shp", "Measurements_20190110_Cloates.shp",
                         "Measurements_20181229_Cloates.shp", "Measurements_20181126_Cloates.shp",
                         "Measurements_20180922_Cloates.shp", "Measurements_20180823_Cloates.shp",
                         "Measurements_20180727_Cloates.shp", "Measurements_20180629_Cloates.shp",
                         "Measurements_20180521_Cloates.shp", "Measurements_20180403_Cloates.shp",
                         "Measurements_20180201_Cloates.shp", "Measurements_20180106_Cloates.shp", 
                         "Measurements_20171027_Cloates.shp", 
                         "Measurements_20170629_Cloates.shp", "Measurements_20170412_Cloates.shp",
                         "Measurements_20170331_Cloates.shp", "Measurements_20170113_Cloates.shp",
                         "Measurement_20190920_Cloates.shp", "Measurement_20190811_Cloates.shp",
                         "Measurement_20190731_Cloates.shp", "Measurement_20170905_Cloates.shp")

# Create an empty data frame to store mean lengths from all shapefiles
all_mean_lengths <- data.frame()

# Loop through the shapefile filenames
for (filename in shapefile_filenames) {
  cat("Processing:", filename, "\n")
  
  # Read the shapefile
  shapefile <- st_read(filename)
  
  # Extract the date from the filename using regular expressions
  date <- gsub(".*_(\\d{8})_.*", "\\1", filename)
  
  # Convert the shapefile to a dataframe
  shapefile_df <- as.data.frame(shapefile) %>%
    dplyr::select(id, Location, Status, Measure) %>%
    glimpse()
  
  # Calculate the length of each line
  shapefile_df$LineLength <- st_length(shapefile)
  
  # Add a "date" column to the dataframe
  shapefile_df$date <- date
  
  # Group by 'date', 'id', and calculate the mean length for each group
  mean_lengths <- shapefile_df %>%
    na.omit() %>%
    filter(Location == "Cloates") %>%
    group_by(date, id) %>%
    dplyr::mutate(MeanLength = mean(LineLength))
  
  # Append the mean lengths to the 'all_mean_lengths' data frame
  all_mean_lengths <- bind_rows(all_mean_lengths, mean_lengths)
  
  cat("Processed:", filename, "\n")
}

glimpse(all_mean_lengths)

setwd(tidy.dir)

# Save all mean lengths as a single CSV file
write.csv(all_mean_lengths, file = "2017_2021_mean_lengths.csv", row.names = FALSE)

# Fin
