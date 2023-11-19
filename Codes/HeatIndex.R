## Heat Index ------------------------------------------------------------------
# Script description: 
#
#

# Initial Set-up ---------------------------------------------------------------
require(pacman)
p_load('tidyverse', 'sf', 'leaflet', 'stars', 'raster', 'weathermetrics', 
       'lubridate', 'plm')

#You need to set your working directory.
setwd("~/Library/CloudStorage/Dropbox/GitHub/Colombia-Climate/Codes") #Folder where this script is saved
dir<-getwd()
codes<-nchar("/codes")
dir<-substring(dir, 1, nchar(dir)-codes) 
setwd(dir)


dir.create('Data/Created/Daily/', showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create('Data/Created/Monthly/', showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create('Data/Created/Year/', showWarnings = FALSE, recursive = FALSE, mode = "0777")

## Shape of Colombian municipalities -------------------------------------------
municipios_trabajar_geo <-st_read('Data/Shapes/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')

## Coopernicus Rasters ----------------------------------------------------------
# August 2023 as example

year=2023
for (month in 1:8){
date_inicial <- paste0(year,"-", month, "-01 00:00:00") 
date_final <- paste0(year,"-", month, "-31 23:00:00") 

temp_path   <-file.path(paste0("Data/Copernicus/Temperature - 2m/temp-",month,"-",year,".grib"))
dp_path     <-file.path(paste0("Data/Copernicus/DewPoint - 2m/dp-",month,"-",year,".grib"))
precip_path <-file.path(paste0("Data/Copernicus/Total - precipitation/prep-",month,"-",year,".grib"))

#Temperature
dataframe <- st_as_sf(read_stars(temp_path), as_points=F , na.rm=T) 
size_temp <- ncol(dataframe)-1
dataframe_name<-paste0("temperature_",seq(1, size_temp))
dataframe <- st_transform(dataframe, st_crs(municipios_trabajar_geo))
dataframe <- dataframe[municipios_trabajar_geo,]
colnames(dataframe) <- c(dataframe_name, "geometry")
dataframe    <- dataframe %>% gather(., date, temperature, starts_with("temperature_") , factor_key=TRUE) 
dataframe$temperature<-kelvin.to.celsius(dataframe$temperature)
dataframe$date<-as.numeric(gsub("\\D", "", dataframe$date))

fechita_dataframe<-cbind.data.frame(seq(1, size_temp),seq(as.POSIXct(date_inicial), as.POSIXct(date_final), by="hour"))
colnames(fechita_dataframe)<-c("date", "dateformat")
temperature_final <- merge(dataframe, fechita_dataframe, by='date')

temperature_final <-
  st_join(temperature_final, municipios_trabajar_geo, largest = FALSE)
   temperature_final$dateformat<-format(temperature_final$dateformat, "%d-%m-%Y, %H:%M:%S")
   temperature_final<-temperature_final[,c("dateformat", "date", "DPTO_CCDGO", "MPIO_CCNCT", "temperature")]
   
#DewPoint
dataframe <- st_as_sf(read_stars(dp_path), as_points=F , na.rm=T) 
size_temp <- ncol(dataframe)-1
dataframe_name<-paste0("dewpoint_",seq(1, size_temp))
dataframe <- st_transform(dataframe, st_crs(municipios_trabajar_geo))
dataframe <- dataframe[municipios_trabajar_geo,]
colnames(dataframe) <- c(dataframe_name, "geometry")
dataframe    <- dataframe %>% gather(., date, dewpoint, starts_with("dewpoint_") , factor_key=TRUE) 
dataframe$dewpoint<-kelvin.to.celsius(dataframe$dewpoint)
dataframe$date<-as.numeric(gsub("\\D", "", dataframe$date))

fechita_dataframe<-cbind.data.frame(seq(1, size_temp),seq(as.POSIXct(date_inicial), as.POSIXct(date_final), by="hour"))
colnames(fechita_dataframe)<-c("date", "dateformat")
dewpoint_final <- merge(dataframe, fechita_dataframe, by='date')
dewpoint_final$dateformat<-format(dewpoint_final$dateformat, "%d-%m-%Y, %H:%M:%S")
dewpoint_final <-
  st_join(dewpoint_final, municipios_trabajar_geo, largest = FALSE)
dewpoint_final<-dewpoint_final[,c("dateformat", "date", "DPTO_CCDGO", "MPIO_CCNCT", "dewpoint")]

# Heat Index
#Confirmar - lo mejor es el merge de los raster
heat<-heat.index(t = temperature_final$temperature, dp = dewpoint_final$dewpoint,
                 temperature.metric = "celsius", output.metric = "celsius", round = 3)

temperature_final$heat<-heat

temperature_mpio<- temperature_final
st_geometry(temperature_mpio) <- NULL
temperature_mpio$day<-substring(temperature_mpio$dateformat, 1, 2)
temperature_mpio$month<-substring(temperature_mpio$dateformat, 4, 5)
temperature_mpio$year<-substring(temperature_mpio$dateformat, 7, 10)

temperature_daily <- temperature_mpio %>% group_by(day, month, year, MPIO_CCNCT) %>%
  summarize(temp_max=max(temperature, na.rm=TRUE),
            temp_min=min(temperature, na.rm=TRUE),
            temp_mean=mean(temperature, na.rm=TRUE),
            temp_median=median(temperature, na.rm=TRUE),
            temp_sd=sd(temperature, na.rm=TRUE),
            temp_q05=quantile(temperature, probs=.05, na.rm=TRUE),
            temp_q10=quantile(temperature, probs=.10, na.rm=TRUE),
            temp_q25=quantile(temperature, probs=.25, na.rm=TRUE),
            temp_q05=quantile(temperature, probs=.75, na.rm=TRUE),
            temp_q10=quantile(temperature, probs=.90, na.rm=TRUE),
            temp_q25=quantile(temperature, probs=.95, na.rm=TRUE),
            heat_max=max(heat, na.rm=TRUE),
            heat_min=min(heat, na.rm=TRUE),
            heat_mean=mean(heat, na.rm=TRUE),
            heat_median=median(heat, na.rm=TRUE),
            heat_sd=sd(heat, na.rm=TRUE),
            heat_q05=quantile(heat, probs=.05, na.rm=TRUE),
            heat_q10=quantile(heat, probs=.10, na.rm=TRUE),
            heat_q25=quantile(heat, probs=.25, na.rm=TRUE),
            heat_q05=quantile(heat, probs=.75, na.rm=TRUE),
            heat_q10=quantile(heat, probs=.90, na.rm=TRUE),
            heat_q25=quantile(heat, probs=.95, na.rm=TRUE))

temperature_monthly<- temperature_mpio %>% group_by(month, MPIO_CCNCT) %>%
  summarize(temp_max=max(temperature, na.rm=TRUE),
            temp_min=min(temperature, na.rm=TRUE),
            temp_mean=mean(temperature, na.rm=TRUE),
            temp_median=median(temperature, na.rm=TRUE),
            temp_sd=sd(temperature, na.rm=TRUE),
            temp_q05=quantile(temperature, probs=.05, na.rm=TRUE),
            temp_q10=quantile(temperature, probs=.10, na.rm=TRUE),
            temp_q25=quantile(temperature, probs=.25, na.rm=TRUE),
            temp_q05=quantile(temperature, probs=.75, na.rm=TRUE),
            temp_q10=quantile(temperature, probs=.90, na.rm=TRUE),
            temp_q25=quantile(temperature, probs=.95, na.rm=TRUE),
            heat_max=max(heat, na.rm=TRUE),
            heat_min=min(heat, na.rm=TRUE),
            heat_mean=mean(heat, na.rm=TRUE),
            heat_median=median(heat, na.rm=TRUE),
            heat_sd=sd(heat, na.rm=TRUE),
            heat_q05=quantile(heat, probs=.05, na.rm=TRUE),
            heat_q10=quantile(heat, probs=.10, na.rm=TRUE),
            heat_q25=quantile(heat, probs=.25, na.rm=TRUE),
            heat_q75=quantile(heat, probs=.75, na.rm=TRUE),
            heat_q90=quantile(heat, probs=.90, na.rm=TRUE),
            heat_q95=quantile(heat, probs=.95, na.rm=TRUE))

temperature_monthly$year <- year

temperature_mpio$hora<-as.numeric(substring(temperature_mpio$dateformat, 13, 14))
temperature_mpio$part_pf_day<-ifelse(temperature_mpio$hora<=5 | 19<=temperature_mpio$hora, "Night","Day" )

# Precipitaciones
dataframe <- st_as_sf(read_stars(precip_path), as_points=F , na.rm=T) 
size_temp <- ncol(dataframe)-1
dataframe_name<-paste0("precipitation_",seq(1, size_temp))
dataframe <- st_transform(dataframe, st_crs(municipios_trabajar_geo))
dataframe <- dataframe[municipios_trabajar_geo,]
colnames(dataframe) <- c(dataframe_name, "geometry")
dataframe    <- dataframe %>% gather(., date, precipitation, starts_with("precipitation_") , factor_key=TRUE) 
dataframe$date<-as.numeric(gsub("\\D", "", dataframe$date))

fechita_dataframe<-cbind.data.frame(seq(1, size_temp),seq(as.POSIXct(date_inicial), as.POSIXct(date_final), by="hour"))
colnames(fechita_dataframe)<-c("date", "dateformat")
precipitacion_final <- merge(dataframe, fechita_dataframe, by='date')
precipitacion_final$dateformat<-format(precipitacion_final$dateformat, "%d-%m-%Y, %H:%M:%S")
precipitacion_final <-
  st_join(precipitacion_final, municipios_trabajar_geo, largest = FALSE)
precipitacion_final<-precipitacion_final[,c("dateformat", "date", "DPTO_CCDGO", "MPIO_CCNCT", "precipitation")]

precipitacion_mpio<- precipitacion_final
st_geometry(precipitacion_mpio) <- NULL
precipitacion_mpio$day<-substring(precipitacion_mpio$dateformat, 1, 2)
precipitacion_mpio$month<-substring(precipitacion_mpio$dateformat, 4, 5)
precipitacion_mpio$year<-substring(precipitacion_mpio$dateformat, 7, 10)

precipitacion_daily <- precipitacion_mpio %>% group_by(day, month, year, MPIO_CCNCT) %>%
  summarize(prec_total=sum(precipitation, na.rm=TRUE))

precipitacion_monthly <- precipitacion_daily %>% group_by(month, year, MPIO_CCNCT) %>%
  summarize(prec_max=max(prec_total, na.rm=TRUE),
            prec_min=min(prec_total, na.rm=TRUE),
            prec_mean=mean(prec_total, na.rm=TRUE),
            prec_median=median(prec_total, na.rm=TRUE),
            prec_sd=sd(prec_total, na.rm=TRUE),
            prec_q05=quantile(prec_total, probs=.05, na.rm=TRUE),
            prec_q10=quantile(prec_total, probs=.10, na.rm=TRUE),
            prec_q25=quantile(prec_total, probs=.25, na.rm=TRUE),
            prec_q75=quantile(prec_total, probs=.75, na.rm=TRUE),
            prec_q90=quantile(prec_total, probs=.90, na.rm=TRUE),
            prec_q95=quantile(prec_total, probs=.95, na.rm=TRUE))

climate_daily<-merge(temperature_daily, precipitacion_daily,  by=c("day", "month", "year","MPIO_CCNCT"))
climate_monthly<-merge(temperature_monthly, precipitacion_monthly,  by=c("month", "year","MPIO_CCNCT"))

assign(paste0("climate_daily_",year,"_",month), climate_daily) 

assign(paste0("climate_month_",year,"_",month), climate_monthly) 
  }


write.csv(climate_daily_2023_8, paste0("Data/Created/Daily/daily_",year,"_",month,".csv"), row.names=FALSE)
write.csv(climate_month_2023_8, paste0("Data/Created/Monthly/month_",year,"_",month,".csv"), row.names=FALSE)


# Monthly land data
ini_year=1950
fin_year=2022

temp_path   <-file.path(paste0("Data/Copernicus/Month/Temperature/temp-",ini_year,"-",fin_year,".grib"))
dp_path     <-file.path(paste0("Data/Copernicus/Month/DewPoint/dp-",month,"-",year,".grib"))
precip_path <-file.path(paste0("Data/Copernicus/Month/Total/prep-",month,"-",year,".grib"))

date_inicial <- paste0(ini_year,"-01-01") 
date_final <- paste0(fin_year,"-12-01") 


#Temperature
dataframe <- st_as_sf(read_stars(temp_path), as_points=F , na.rm=T) 
size_temp <- ncol(dataframe)-1
dataframe_name<-paste0("temperature_",seq(1, size_temp))
dataframe <- st_transform(dataframe, st_crs(municipios_trabajar_geo))
dataframe <- dataframe[municipios_trabajar_geo,]
colnames(dataframe) <- c(dataframe_name, "geometry")
dataframe    <- dataframe %>% gather(., date, temperature, starts_with("temperature_") , factor_key=TRUE) 
dataframe$temperature<-kelvin.to.celsius(dataframe$temperature)
dataframe$date<-as.numeric(gsub("\\D", "", dataframe$date))

fechita_dataframe<-cbind.data.frame(seq(1, size_temp),seq(as.Date(date_inicial), as.Date(date_final), by="months"))
colnames(fechita_dataframe)<-c("date", "dateformat")
temperature_final <- merge(dataframe, fechita_dataframe, by='date')

temperature_final <-
  st_join(temperature_final, municipios_trabajar_geo, largest = FALSE)
temperature_final$dateformat<-format(temperature_final$dateformat, "%d-%m-%Y, %H:%M:%S")
temperature_final<-temperature_final[,c("dateformat", "date", "DPTO_CCDGO", "MPIO_CCNCT", "temperature")]

temperature_mpio<- temperature_final
st_geometry(temperature_mpio) <- NULL
  temperature_mpio$day<-substring(temperature_mpio$dateformat, 1, 2)
  temperature_mpio$month<-substring(temperature_mpio$dateformat, 4, 5)
  temperature_mpio$year<-substring(temperature_mpio$dateformat, 7, 10)
  
  temperature_month <- temperature_mpio %>% group_by(month, year, MPIO_CCNCT) %>%
    summarize(temp_max=max(temperature, na.rm=TRUE),
              temp_min=min(temperature, na.rm=TRUE),
              temp_mean=mean(temperature, na.rm=TRUE),
              temp_median=median(temperature, na.rm=TRUE),
              temp_sd=sd(temperature, na.rm=TRUE),
              temp_q05=quantile(temperature, probs=.05, na.rm=TRUE),
              temp_q10=quantile(temperature, probs=.10, na.rm=TRUE),
              temp_q25=quantile(temperature, probs=.25, na.rm=TRUE),
              temp_q75=quantile(temperature, probs=.75, na.rm=TRUE),
              temp_q90=quantile(temperature, probs=.90, na.rm=TRUE),
              temp_q95=quantile(temperature, probs=.95, na.rm=TRUE))
  
  write.csv(temperature_month, "Data/Created/COL-temp-1950-2022.csv", row.names=FALSE)
  
