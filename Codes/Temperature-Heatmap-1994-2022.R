## From Raster to Municipalities -----------------------------------------------
# Script description: 
#
#
#


# Initial Set-up ---------------------------------------------------------------
require(pacman)
p_load('tidyverse', 'sf', 'leaflet', 'stars', 'raster', 'weathermetrics', 
       'lubridate', 'plm')

#You need to set your working directory.
dir<-getwd() #Folder where this script is saved
dir<-substring(dir, 1, nchar(dir)-6) 
setwd(dir)

## Shape of Colombian municipalities -------------------------------------------
municipios_trabajar_geo <-st_read('Data/Shapes/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')


## Coopernicus Rasters ----------------------------------------------------------

for (year in 1994:2022){
  date_inicial <- paste0(year,"-01-01 00:00:00") 
  date_final <- paste0(year,"-12-31 23:00:00") 
  
  temp_path <-file.path(paste0("Year by Year/temperature_",year,".grib"))
  
  #Temperature
  pruebita <- st_as_sf(read_stars(temp_path), as_points=F , na.rm=T) 
  size_temp <- ncol(pruebita)-1
  pruebita_name<-paste0("temperature_",seq(1, size_temp))
  pruebita <- st_transform(pruebita, st_crs(municipios_trabajar_geo))
  pruebita <- pruebita[municipios_trabajar_geo,]
  colnames(pruebita) <- c(pruebita_name, "geometry")
  pruebita    <- pruebita %>% gather(., date, temperature, starts_with("temperature_") , factor_key=TRUE) 
  pruebita$temperature<-kelvin.to.celsius(pruebita$temperature)
  pruebita$date<-as.numeric(gsub("\\D", "", pruebita$date))
  
  seq_date<-seq(as.POSIXct(date_inicial), as.POSIXct(date_final), by="hour")
  fechita_pruebita<-cbind.data.frame(seq(1, size_temp-1),seq_date)
  colnames(fechita_pruebita)<-c("date", "dateformat")
  fechita_pruebita$dateformat<-format(fechita_pruebita$dateformat, "%d-%m-%Y")
  temperature_final <- merge(pruebita, fechita_pruebita, by='date')
  
  temperature_final <-
    st_join(temperature_final, municipios_trabajar_geo, largest = FALSE)
  st_geometry(temperature_final) <- NULL
  
  temperature_daily <- temperature_final %>% group_by(dateformat, DOMINIO) %>%
    summarize(max_temp=max(temperature))
  temperature_daily$mayor27<-ifelse(temperature_daily$max_temp>=27,1,0)
  temperature_daily$mayor30<-ifelse(temperature_daily$max_temp>=30,1,0)
  temperature_daily$mayor32<-ifelse(temperature_daily$max_temp>=32,1,0)
  
  temperature_daily$month<-month(temperature_daily$dateformat)
  temperature_month <- temperature_daily %>% group_by(month, DOMINIO) %>%
    summarize(mean_temp=mean(max_temp),
              median_temp=median(max_temp),
              max_temp=max(max_temp))
  temperature_month$year <- year
  
  cooling_days<-temperature_daily%>%group_by(DOMINIO)%>%summarise(day_over_27=sum(mayor27),
                                                                  day_over_30=sum(mayor30),
                                                                  day_over_32=sum(mayor32))
  
  temperature_daily$year <- year
  
  assign(paste0("temperature_daily_",year), temperature_daily) 
  assign(paste0("temperature_month_",year), temperature_month) 
}

year=1991

temp_daily_list <- lapply(ls(pattern = "temperature_daily_*"), get)
merged.daily = Reduce(function(...) merge(..., all=T, 
                                          by=c("DOMINIO", "year", "day_over_27", "day_over_30", "day_over_32")), 
                      temp_daily_list)
save(merged.daily, file="daily_temp_over.Rdata")
temp_monthly_list <- lapply(ls(pattern = "temperature_month_*"), get)
merged.monthly = Reduce(function(...) merge(..., all=T, 
                                            by=c("DOMINIO", "month", "mean_temp", "year")), 
                        temp_monthly_list)
save(merged.monthly, file="monthly_temp_over.Rdata")

merged.monthly$date<-paste0(merged.monthly$year,"/",merged.monthly$month,"/15")
merged.monthly$date<-as.Date(merged.monthly$date, "%Y/%m/%d") 
patterns <- unique(substr(names(merged.monthly),1,9))  # store patterns in a vector
merged.monthly$max_temp_total <- rowSums(merged.monthly[,grep("max_temp", names(merged.monthly)), drop=TRUE], na.rm=TRUE)  # loop through
merged.monthly$median_temp_total <- rowSums(merged.monthly[,grep("median_temp", names(merged.monthly)), drop=TRUE], na.rm=TRUE)  # loop through
merged.monthly<-merged.monthly[,c("DOMINIO", "month", "mean_temp", "year", "max_temp_total", "median_temp_total", "date")]

## HeatMap ---------------------------------------------------------------------
heatmap<-merge(merged.monthly, municipios_trabajar_geo, by="DOMINIO")
heatmap<-heatmap[heatmap$year>=1994,]
heatmap$DOMINIO<-str_to_title(heatmap$DOMINIO)

heatmap$date <- paste0(heatmap$year,"/",heatmap$month,"/15")
heatmap$date <- as.Date(heatmap$date, "%Y/%m/%d")

heatmap_cities<-heatmap %>%
  mutate(name = fct_reorder(DOMINIO, desc(days_over_30))) %>%
  ggplot(aes(x=date, y=name, fill=median_temp))+
  geom_tile(width=100)+
  scale_fill_gradient2(low = "#55ddff",
                       high = "#ed217c",
                       mid ="#FBF1CB",
                       name = "Celsius",
                       midpoint = 30,
                       breaks = c(15,20,25,30,35,40,45),
                       na.value = "#D9D9D6",
                       guide= "colourbar")+
  theme_classic()+xlab("")+ylab("")+
  theme(legend.position="bottom", text=element_text(family="Verdana"), 
        legend.title = element_text(size=8))

ggsave(plot = heatmap_cities, filename = 'Graphs/Rstudio/Ch2/Heatmap_cities.jpeg')
