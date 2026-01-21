#### i. LIBRARY IMPORTS ####
## Tables
library(data.table)
install.packages("readxl")
library(readxl)
install.packages("rgdal")
library(rgdal)
install.packages("lubridate")
library(lubridate)
install.packages("tidyr")
library(tidyr)
install.packages("broom")
library(broom)

## Plots
library(ggplot2)
install.packages("maps")
library(maps)
install.packages("scale")
library(scales)
install.packages("ggthemes")
library(ggthemes)
install.packages("ggpubr")
library(ggpubr)
install.packages("gstat")
library(gstat)
install.packages("markdown")
library(markdown)
install.packages("ggtext")
library(ggtext)
install.packages("patchwork")
library(patchwork)
install.packages("egg")
library(egg)
install.packages("zoo")
library(zoo)
install.packages("stringr")
library(stringr)

## Data download
install.packages("dataRetrieval")
library(dataRetrieval)
install.packages("hydat")
download_hydat()
install.packages("tidyhydat")
library(tidyhydat)

## Analysis
library(glmnet)
library(Hmisc)
library(changepoint)
library(tictoc) # For timing each step
library(rgdal) # Not used right now in the script, but probably necessary to save the shapefile
library(sf)
library(sp)


#### ii. THEMES ####
theme_evan <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey70'),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    text = element_text(size=8),
    axis.text = element_text(size = 8), 
    plot.title = element_text(size = 9)
  )

theme_evan_facet <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    # legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size=12),
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 13)
  )
season_facet <- theme_evan_facet + theme(
  legend.position = 'none', 
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)

theme_dark_mode <- season_facet +
  theme(
    axis.title.x = element_markdown(color = 'white'),
    axis.title.y = element_markdown(color = 'white'),
    strip.text.x.top = element_markdown(color = 'white', size=12),
    panel.grid.major.x = element_line(color = 'grey90', linewidth = 0.25),
    panel.grid.major.y = element_line(color = 'grey90', linewidth = 0.25),
    panel.grid.minor.x = element_line(color = 'grey90', linewidth = 0.1),
    panel.grid.minor.y = element_line(color = 'grey90', linewidth = 0.1),
    text = element_text(color = 'white'),
    panel.background = element_rect(fill = 'black'),
    plot.background = element_rect(fill = 'black'),
    # axis.text = element_markdown(color = 'white'),
    axis.text = element_text(color = 'white'),
    panel.border = element_rect(color = 'white'),
    legend.background = element_rect(color = 'white', fill = 'black'),
    legend.key = element_rect(fill = 'black')
  ) 

fancy_scientific_modified <- function(l) { 
  # turn in to character string in scientific notation 
  if(abs(max(log10(l), na.rm = T) - min(log10(l), na.rm = T)) > 2 | 
     # min(l, na.rm = T) < 0.01 | 
     max(l, na.rm = T) > 1e5){ 
    l <- log10(l)
    label <- parse(text = paste("10^",as.character(l),sep = ""))
  }else{
    label <- parse(text = paste(as.character(l), sep = ""))
  }
  # print(label)
  # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
  return(label)
}

lat_dd_lab <- function(l){
  label <- c()
  for(i in 1:length(l)){
    label_sel <- ifelse(l[i] < 0, paste0(abs(l[i]), '°S'), 
                        paste0(abs(l[i]), '°N'))
    label <- c(label, label_sel)
  }
  return(label)}

long_dd_lab <- function(l){
  label <- c()
  for(i in 1:length(l)){
    label_sel <- ifelse(l[i] < 0, paste0(abs(l[i]), '°W'), 
                        paste0(abs(l[i]), '°E'))
    label <- c(label, label_sel)
  }
  return(label)}

abbrev_year <- function(l){
  label <- c() 
  for(i in 1:length(l)){
    label_sel <- paste0("'",substr(as.character(l[i]),3,4))
    label <- c(label, label_sel)  
  }
  return(label)}

mean_na <- function(x){
  return(mean(x, na.rm=T))
}
#### iii. SET DIRECTORIES ####
# Set root directory
wd_root <- getwd()

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/imports/")

wd_figures <- paste0(wd_root, "/figures/")

wd_station_metadata <- paste0(wd_imports, 'station_metadata/')
wd_metadata_misc <- paste0(wd_imports, 'training_data_miscellaneous/')
wd_training <- paste0(wd_imports, 'training/')

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_figures,wd_station_metadata, wd_training, wd_metadata_misc)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}

#### 1. IMPORT SITE METADATA FOR DIFFERENT REGIONS ####
list.files(wd_station_metadata)
list.files(paste0(wd_training,'Danube/'))

canada_stns <- fread(paste0(wd_station_metadata, 'Canada_Water-Qual-Eau-Sites-National.csv'))[,.(
  source = 'Canada', site_no = as.character(SITE_NO), station_nm = as.character(SITE_NAME), 
  country = 'Canada', territory = as.character(PROV_TERR),
  latitude = as.numeric(LATITUDE), longitude = as.numeric(LONGITUDE), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(NA),
  river_name = as.character(PEARSEDA), river_id = as.character(NA), 
  start_date = ymd(NA), end_date = ymd(NA), n_samples = as.numeric(NA), 
  notes = as.character(SITE_DESC))]

danube_stns <- fread(paste0(wd_training, 'Danube/danube_ssc.csv'))[,.(
  start_date = min(date(date_of_sampling), na.rm = T),
  end_date = max(date(date_of_sampling), na.rm = T),
  n_samples = .N
  ),
  by = .(station_code, location, country, y_coord, x_coord,
         river, river_km)][,.(
  source = 'Danube', site_no = as.character(station_code), station_nm = as.character(location), 
  country = country, territory = as.character(NA),
  latitude = as.numeric(y_coord), longitude = as.numeric(x_coord), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(NA),
  river_name = as.character(river), river_id = as.character(river_km), 
  start_date = ymd(start_date), end_date = ymd(end_date), n_samples = as.numeric(n_samples), 
  notes = as.character(NA))]

morepoc_stns <- fread(paste0(wd_training, 'MOREPOC/MOREPOC_v1.1.csv'))[,.(
  start_date = min(ymd(paste0(time_y, '-',time_m, '-', time_d)), na.rm = T),
  end_date = max(ymd(paste0(time_y, '-',time_m, '-', time_d)), na.rm = T),
  n_samples = .N
  ),
  by = .(code, country, lat, lon,
         riv_id, ref)][,.(
  source = 'MOREPOC', site_no = as.character(paste0(riv_id, '_', code, '_', lat, '_', lon)), station_nm = as.character(paste0(riv_id, '_', code, '_', lat, '_', lon)), 
  country = country, territory = as.character(NA),
  latitude = as.numeric(lat), longitude = as.numeric(lon), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(NA),
  river_name = as.character(riv_id), river_id = as.character(riv_id), 
  start_date = ymd(start_date), end_date = ymd(end_date), n_samples = as.numeric(n_samples), 
  notes = as.character(ref))][
    # n_samples >= 1
    ]

france_stns <- fread(paste0(wd_training, 'France/france_moose_insitu.csv'))[TSM >= 0][,.(
  start_date = min(mdy(sample_dt), na.rm = T),
  end_date = max(mdy(sample_dt), na.rm = T),
  n_samples = .N
  ),
  by = .(site_no, station_nm, Latitude, Longitude)][,.(
  site_no = as.character(site_no), station_nm = as.character(station_nm), 
  source = 'France', country = 'France', territory = as.character(NA),
  latitude = as.numeric(Latitude), longitude = as.numeric(Longitude), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(NA),
  river_name = as.character(NA), river_id = as.character(NA), 
  start_date = ymd(start_date), end_date = ymd(end_date), n_samples = as.numeric(n_samples), 
  notes = as.character(NA))]

korea_stns <- fread(paste0(wd_station_metadata, 'korea_stations_metadata_clean.csv'))[,.(
  site_no = as.character(site_no), station_nm = as.character(station_nm), 
  source = 'Korea', country = 'Korea', territory = as.character(NA),
  latitude = as.numeric(latitude), longitude = as.numeric(longitude), 
  drainage_area_km2 = as.numeric(drainage_area_km2), width_m = as.numeric(width_m), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(bed_elevation_m),
  river_name = as.character(river_name), river_id = as.character(river_name), 
  start_date = ymd(start_date), end_date = ymd(NA), n_samples = as.numeric(NA), 
  notes = as.character(NA))]

switzerland_stns <- fread(paste0(wd_station_metadata, 'switzerland_station_metadata.csv'))[,.(
  source = 'Switzerland', site_no = as.character(station_abbreviation), station_nm = as.character(station), 
  country = 'Switzerland', territory = as.character(NA),
  latitude = as.numeric(latitude_WGS84), longitude = as.numeric(longitude_WGS84), 
  drainage_area_km2 = as.numeric(catchment_area), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(station_elevation),
  river_name = as.character(river), river_id = as.character(NA), 
  start_date = ymd(ifelse(!is.na(as.numeric(substr(`start and end`, 1,4))), 
                             paste0(substr(`start and end`, 1,4), '-01-01'),
                                 NA)),
  end_date = ymd(ifelse(!is.na(as.numeric(substr(`start and end`, 1,4))), 
                        paste0(substr(`start and end`, 8,12), '-01-01'),
                        NA)), 
  n_samples = as.numeric(NA), 
  notes = as.character(NA))]


finland_stns <- fread(paste0(wd_station_metadata, 'finland_station_metadata.csv'))[,.(
  site_no = as.character(Paikka_Id), station_nm = as.character(Nimi), 
  source = 'Finland', country = 'Finland', territory = as.character(H_Kunta_Id),
  latitude = as.numeric(KoordErLat), longitude = as.numeric(KoordErLong), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(Syvyys), elevation_m = as.numeric(NA),
  river_name = as.character(NA), river_id = as.character(V_Vesimuodostuma_Id), 
  start_date = ymd(NA), end_date = ymd(NA), n_samples = as.numeric(NA), 
  notes = as.character(Lisatieto))]

mexico_stns <- fread(paste0(wd_station_metadata, 'mexico_station_metadata.csv'))[,.(
  site_no = as.character(`CLAVE SITIO`), station_nm = as.character(`NOMBRE DEL SITIO`), 
  source = 'Mexico', country = 'Mexico', territory = as.character(ESTADO),
  latitude = as.numeric(Latitude), longitude = as.numeric(Longitude), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(NA),
  river_name = as.character(CUENCA), river_id = as.character(`CLAVE ACUÍFERO`), 
  start_date = ymd(NA), end_date = ymd(NA), n_samples = as.numeric(NA), 
  notes = as.character(`TIPO DE CUERPO DE AGUA`))]

usa_stns <- fread(paste0(wd_station_metadata, 'United_States_USGS_p80154_station_info.csv'))[,.(
  site_no = as.character(site_no), station_nm = as.character(station_nm), 
  source = 'United States of America', country = 'United States of America', territory = as.character(NA),
  latitude = as.numeric(dec_lat_va), longitude = as.numeric(dec_long_va), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(alt_va)/3.28,
  river_name = as.character(NA), river_id = as.character(NA), 
  start_date = ymd(begin_date), end_date = ymd(end_date), n_samples = as.numeric(count_nu), 
  notes = as.character(NA))][
    n_samples > 1
  ]

mekong_stns <- fread(paste0(wd_metadata_misc, 'Mekong_metadata/mekong_station_metadata_basic.csv'),
                     colClasses = c('Code' = 'character'))[,.(
  site_no = as.character(Code), station_nm = as.character(Name), 
  source = 'Mekong', country = 'Mekong', territory = as.character(NA),
  latitude = as.numeric(NA), longitude = as.numeric(NA), 
  drainage_area_km2 = as.numeric(NA), width_m = as.numeric(NA), 
  depth_m = as.numeric(NA), elevation_m = as.numeric(NA),
  river_name = as.character('Mekong'), river_id = as.character(NA), 
  start_date = ymd(NA), end_date = ymd(NA), n_samples = as.numeric(NA), 
  notes = as.character(NA))][
  ]


#### 2. IMPORT SSC DATA FOR METADATA SUPPLEMENTARY STATISTICS ####
#### 2A. CANADA SSC DATA ####
# Import SSC data for Canada -- Adding start/end date, n samples
# Missing width, drainage area, elevation

## TO DO: IMPORT FROM WATER SURVEY OF CANADA; SO FAR JUST WATER QUALITY

# Files with SSC data
canada_ssc_files <- list.files(paste0(wd_training, 'Canada'))

# Import and combine files from different regions
canada_ssc <- rbindlist(
  lapply(
    paste0(wd_training, 'Canada/', canada_ssc_files),
    fread
  ),
  fill = T, use.names = T
)
library(tidyhydat)
ca_Q_stations <- data.table(hy_stations())
white_river_yk_daily <- data.table(hy_daily_flows("09CB001"))
white_river_yk_daily
ggplot(white_river_yk_daily[year(Date) > 2020], aes(x = Date, y = Value)) + 
  geom_line(color = 'white') + 
  theme_dark_mode

ggplot(white_river_yk_daily[], aes(x = yday(Date), y = Value)) + 
  stat_summary(geom = 'line', 
               aes(group = ifelse(year(Date) < 2000, 'pre-2000', 'post-2000'),
                   color = ifelse(year(Date) < 2000, 'pre-2000', 'post-2000'))) + 
  theme_dark_mode +
  theme(legend.position = 'top')

# Isolate just SSC data and calculate station metadata
canada_metadata_supplement <- canada_ssc[
  grepl('TOTAL SUSPENDED SOLIDS', VARIABLE) | grepl('TURBIDITY', VARIABLE) ][,.(
  start_date = date(min(ymd_hm(DATE_TIME_HEURE), na.rm = T)),
  end_date = date(max(ymd_hm(DATE_TIME_HEURE), na.rm = T)),
  n_samples = .N
),
by = .(site_no = as.character(SITE_NO))]

# Merge with Canada station metadata
merge_cols <- colnames(canada_metadata_supplement)[which(colnames(canada_metadata_supplement) != 'site_no')]

canada_stns <- merge(canada_stns[ , .SD, .SDcols = !merge_cols],
                     canada_metadata_supplement, 
                     on = 'site_no')

#Beginning of my code

canada_ssc[, site_prefix := substr(SAMPLE_ID_ÉCHANTILLON, 1, 2)]

# Aggregate daily per station, keeping site_prefix
Canada_daily_SSC <- usa_ssc[
  SSC_mgL > 0,   # remove zero / negative
  .(SSC_mgL = mean(SSC_mgL, na.rm = TRUE)),
  by = .(site_no, site_prefix, date)   # <- include site_prefix
]

ggplot(Canada_daily_SSC, aes(x = date, y = SSC_mgL, color = site_no)) +
  geom_point(size = 0.5, alpha = 0.3) +
  facet_wrap(. ~ site_prefix) +
  scale_y_log10(labels = fancy_scientific_modified) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration over time (USA stations)"
  ) +
  theme_dark_mode +
  facet_wrap(.~substr(site_no, 1,2)) +
  theme(
    axis.text.x = element_text(size = 6)  # smaller text
  )

# End of my code.

#### 2B. DANUBE SSC DATA ####
# Import SSC data for Danube -- Adding start/end date, n samples
# Missing width, drainage area, elevation
# Supplementary data: POC (limited), 

# TO DO: make date more flexible to these formats: 2006-01-18 (ymd) and 1996-01-17 09:00:00
danube_ssc <- fread(paste0(wd_training, 'Danube/danube_ssc.csv'))[
  determinand == 'Suspended solids'][
  ,':='(site_no = as.character(station_code), 
        Source = 'Danube',
        station_nm = as.character(location), 
        SSC_mgL = as.numeric(result_value))
][,':='(
  date = date(date_of_sampling)
)]

danube_poc <- fread(paste0(wd_training, 'Danube/danube_poc.csv'))[
  determinand == 'Suspended solids - organic fraction'][
    ,':='(site_no = as.character(station_code), 
          Source = 'Danube',
          station_nm = as.character(location), 
          POC_mgL = as.numeric(result_value))
  ][,':='(
    date = date(date_of_sampling)
  )]

danube_ssc <- merge(
  danube_ssc,
  danube_poc[,.(site_no, date, POC_mgL)],
  on = c('site_no', 'date'),
  all.x = TRUE)

# My code for the Danube stations SSC
danube_ssc

library(lubridate)

danube_ssc[, date := dmy_hm(date_of_sampling_display)]
danube_ssc_plot <- danube_ssc[SSC_mgL > 0]

library(ggplot2)
library(scales)

ggplot(danube_ssc_plot, aes(x = date, y = SSC_mgL)) +
  geom_point(color = "#5F8C9E", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration (Danube Stations)"
  ) +
  theme_dark_mode

# My plots of POC (log and nonlog scale)
# Keep only rows where POC_mgL is not NA and greater than 0
danube_poc_plot <- danube_ssc_plot[!is.na(POC_mgL) & POC_mgL > 0]

ggplot(danube_poc_plot, aes(x = date, y = POC_mgL)) +
  geom_point(color = "#5F8C9E", alpha = 0.9, size = 2.5) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date (2020)",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Danube Stations)"
  ) +
  theme_dark_mode

library(ggplot2)

ggplot(danube_poc_plot, aes(x = date, y = POC_mgL)) +
  geom_point(color = "#5F8C9E", alpha = .9, size = 2.5) +
  labs(
    x = "Date (2020)",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Danube Stations)"
  ) +
  theme_dark_mode

ggplot(danube_ssc, aes(x = SSC_mgL, y = POC_mgL/SSC_mgL)) +
  geom_point(aes(color = station_code)) +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  labs(
    x = 'SSC (mg/L)',
    y = 'POC (mg/L)'
  ) +
  theme_dark_mode

ggplot(danube_ssc, aes(x))
danube_SSC_methods <- danube_ssc[,.(n_samples = .N), by = sampling_method]
danube_POC_methods <- danube_poc[,.(n_samples = .N), by = sampling_method]

#### 2C. MEXICO SSC DATA ####
# Import SSC data for Mexico -- Adding start/end date, n samples
# Missing width, drainage area, elevation
# Supplementary data: Turbidity, TOC, DOC, POC (calculated), 
# nutrients, metals, organic compounds, pH, dissolved oxygen, temperature
mexico_ssc <- fread(paste0(wd_training, 'Mexico/mexico_ssc_insitu.csv'))[
  grepl('LÓTICO', `TIPO CUERPO DE AGUA`)
  ][
    ,':='(site_no = as.character(`CLAVE SITIO`), 
          Source = 'Mexico',
       date = mdy(`FECHA REALIZACIÓN`), 
       SSC_mgL = as.numeric(SST),
       POC_mgL = as.numeric(COT) - as.numeric(COT_SOL),
       Turbidity_NTU = as.numeric(TURBIEDAD))
  ]

str(mexico_ssc$SSC_mgL)

#my plot of SSC (Mexico)
ggplot(mexico_ssc, aes(x = date, y = SSC_mgL)) +
  geom_point(color = "#D689BF", alpha = .2, size = 1) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Content (Mexico Station)"
  ) +
  theme_dark_mode

#my plot of POC (Mexico)

# Calculate Q1, Q3, and IQR
Q1 <- quantile(mexico_ssc$POC_mgL, 0.25, na.rm = TRUE)
Q3 <- quantile(mexico_ssc$POC_mgL, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower <- Q1 - (1.5 * IQR)
upper <- Q3 + (1.5 * IQR)

lower
upper

#Plot program that I used for the saved image
ggplot(mexico_ssc, aes(x = date, y = POC_mgL)) +
  geom_point(color = "#D1AD3F", alpha = .3, size = 1) +
  scale_y_log10(
    breaks = c(.001, .01, .1, 1, 10, 100, 1000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Mexico Station)"
  ) +
  theme_dark_mode

ggplot(mexico_ssc, aes(x = date, y = POC_mgL)) +
  geom_point(color = "#D1AD3F", alpha = .5, size = 1) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Mexico Station)"
  ) +
  theme_dark_mode

mexico_ssc_histogram <- ggplot(mexico_ssc) +
  geom_histogram(aes(x = SSC_mgL), fill = 'purple', color = 'white',
                 bins = 30) + 
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    legend.position = 'top'
  ) + 
  labs(
    x = 'SSC (mg/L)',
    y = 'N samples'
  )

#The version that works on my computer
mexico_ssc_plot[, log_SSC := log10(SSC_mgL)]

ggplot(mexico_ssc_plot) +
  geom_histogram(aes(x = log_SSC), fill = "purple", color = "white", bins = 30) +
  scale_x_continuous(
    breaks = 0:5,   # adjust to your data
    labels = function(x) 10^x
  ) +
  labs(
    x = "SSC (mg/L)",
    y = "N samples"
  ) +
  theme_dark_mode

mexico_ssc$SSC_mgL
summary(mexico_ssc$SSC_mgL)
mexico_metadata_supplement <- mexico_ssc[,.(site_no, date, SSC_mgL)][
  SSC_mgL >= 0
  ][,.(
    start_date = min(date, na.rm = T),
    end_date = max(date, na.rm = T),
    n_samples = .N
  ),
  by = .(site_no)]

# Merge with Mexico station metadata
merge_cols <- colnames(mexico_metadata_supplement)[which(colnames(mexico_metadata_supplement) != 'site_no')]

mexico_stns <- merge(mexico_stns[ , .SD, .SDcols = !merge_cols],
                     mexico_metadata_supplement, 
                     on = 'site_no')

#### 2D. MOREPOC SSC DATA ####
# Import SSC data for MOREPOC
# Supplementary data: POC, fraction POC, carbon isotopes and age, cn ration (selected)
# Missing summary stats for width, drainage area, elevation
morepoc_ssc <- fread(paste0(wd_training, 'MOREPOC/MOREPOC_v1.1.csv'))[,':='(
         Source = 'MOREPOC', site_no = as.character(paste0(riv_id, '_', code, '_', lat, '_', lon)), station_nm = as.character(code), 
         country = country, territory = as.character(NA),
         size_fraction = gsub('>|<', "", gsub("<.*?>", "", gsub('μm', ' microns', fra_spm))),
       SSC_mgL = as.numeric(conc_spm),
       POC_mgL = as.numeric(conc_poc))
  ][,':='(date = ymd(paste0(time_y, '-', str_pad(time_m, 2, pad = "0"), '-', str_pad(time_d, 2, pad = "0"))))]

morepoc_ssc$date

# My plots
ggplot(morepoc_ssc, aes(x = date, y = POC_mgL)) +
  geom_point(color = "#D1503F", alpha = .5, size = 1.5) +
  scale_y_log10(
    breaks = c(.1, 1, 10, 100, 1000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Morepoc Station)"
  ) +
  theme_dark_mode

ggplot(morepoc_ssc, aes(x = date, y = SSC_mgL)) +
  geom_point(color = "#D1503F", alpha = .5, size = 1.5) +
  scale_y_log10(
    breaks = c(.1, 1, 10, 100, 1000, 10000, 100000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Content (Morepoc Station)"
  ) +
  theme_dark_mode
unique(morepoc_ssc$fra_spm)
unique(morepoc_ssc$size_fraction)


ggplot(morepoc_ssc[
  # riv_id == 'Congo'
  ]) +
  geom_histogram(aes(x = SSC_mgL), fill = 'purple', color = 'white',
                 bins = 30) + 
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  facet_wrap(.~size_fraction) +
  theme(
    legend.position = 'top'
  ) + 
  labs(
    x = 'SSC (mg/L)',
    y = 'N samples'
  )

  
morepoc_ssc[is.na(date)]
morepoc_metadata_supplement <- morepoc_ssc[,.(site_no, date, SSC_mgL)][
  SSC_mgL >= 0
][,.(
  start_date = min(date, na.rm = T),
  end_date = max(date, na.rm = T),
  n_samples = .N
),
by = .(site_no)]

# Merge with Mexico station metadata
merge_cols <- colnames(morepoc_metadata_supplement)[which(colnames(morepoc_metadata_supplement) != 'site_no')]

morepoc_stns <- merge(morepoc_stns[ , .SD, .SDcols = !merge_cols],
                      morepoc_metadata_supplement, 
                    on = 'site_no')

#### 2E. FINLAND SSC DATA ####
# Import SSC data for Finland
# Supplementary data: DOC, TOC, POC (calculated for limited samples),
# Missing summary stats for width, drainage area, elevation
list.files(paste0(wd_training, 'Finland/'))
finland_ssc <- fread(paste0(wd_training, 'Finland/finland_ssc_data.csv'))[,':='(
      site_no = as.character(Site_Id), 
      Source = 'Finland',
      station_nm = as.character(Site)
      # SSC_mgL = as.numeric(conc_spm),
      # POC_mgL = as.numeric(conc_poc)
  )][,':='(date = date(Time))]


finland_ssc_sample_categories_long <- finland_ssc[,.(N_samples = .N),
            by = .(Determination_Id, DeterminationName)
]

fwrite(finland_ssc_sample_categories_long, paste0(wd_station_metadata, '/variable_descriptions/', 'finland_ssc_variables.csv'))
finland_ssc_sample_categories_simple <- fread(paste0(wd_station_metadata, '/variable_descriptions/', 'finland_ssc_variables_simple.csv'))

finland_ssc <- merge(finland_ssc,
                     finland_ssc_sample_categories_simple[,.(Determination_Id, DeterminationName_simple)],
                     on = c('Determination_Id'))
# Convert from long to wide table with DOC SSC and TOC
# TO DO: investigate samples with multiple values on a day
finland_ssc_wide <- dcast.data.table(
  site_no + station_nm + Source + date + SampleDepth_m ~ DeterminationName_simple,
  value.var = c('Value'),
  data = finland_ssc,
  fun.aggregate = mean_na
)[,':='(
  POC_mgL = TOC_mgL - DOC_mgL
)]

summary(finland_ssc_wide)

columns <- c("date", "SSC_mgL")

Date_vs_SSC_data.table_finland <- finland_ssc_wide[, ..columns]

Date_vs_SSC_data.table_finland

library(ggplot2)
library(ggtext)  # only if using element_markdown in theme

# Olivia's code of the time series plot
ggplot(Date_vs_SSC_data.table_finland,
       aes(x = date, y = SSC_mgL)) +
  geom_point(color = "#99C2AD", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000),          # choose ticks you want
    labels = scales::trans_format(
      "log10", scales::math_format(10^.x)
    )
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration (Finland)"
  ) +
  theme_dark_mode

library(dplyr)
library(ggplot2)

finland_ssc_wide_clean <- finland_ssc_wide %>%
  filter(date >= as.Date("2021-01-01"))

ggplot(finland_ssc_wide_clean, aes(x = date, y = POC_mgL)) +
  geom_point(color = "#99C2AD", alpha = 0.7, size = 3) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Finland)"
  ) +
  theme_dark_mode


ggplot(finland_ssc_wide[TOC_mgL > 0 & DOC_mgL > 0], aes(x = TOC_mgL, y = DOC_mgL)) + 
  geom_point()

ggplot(finland_ssc_wide[POC_mgL > 0 & SSC_mgL > 0], aes(x = SSC_mgL, y = POC_mgL/SSC_mgL)) + 
  geom_point()

finland_doc_vs_ssc <- ggplot(finland_ssc_wide[DOC_mgL > 0 & SSC_mgL > 0], aes(x = SSC_mgL, y = DOC_mgL/SSC_mgL)) + 
  geom_point(aes(color = site_no)) +
  geom_smooth(method = 'lm', color = 'white', lty = 'dashed') +
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified)

finland_ssc_histogram <- ggplot(finland_ssc_wide[SSC_mgL > 0]) +
  geom_histogram(aes(x = SSC_mgL), fill = 'purple', color = 'white',
                 bins = 30) + 
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    legend.position = 'top'
  ) + 
  labs(
    x = 'SSC (mg/L)',
    y = 'N samples'
  )

finland_sample_count_by_site <- finland_ssc_wide[,.(N_samples = .N),
                 by = .(site_no, station_nm)][
                   order(-N_samples)
                 ]
ggplot(finland_ssc_wide[SSC_mgL > 0][
  site_no %in% finland_sample_count_by_site$site_no[1:9]
]) +
  stat_summary(geom = 'line', aes(x = yday(date) - yday(date)%%10, y = SSC_mgL), color = 'white') +
  stat_summary(aes(x = yday(date) - yday(date)%%10, y = SSC_mgL), fill = 'purple', color = 'white',
                 pch = 21) + 
  theme_dark_mode +
  scale_y_log10(labels = fancy_scientific_modified) +
  theme(
    legend.position = 'top'
  ) + 
  # facet_wrap(.~site_no) +
  labs(
    x = 'Day of Year',
    y = 'SSC (mg/L)',
  )

finland_metadata_supplement <- finland_ssc_wide[,.(site_no, date, SSC_mgL)][
  SSC_mgL >= 0
][,.(
  start_date = min(date, na.rm = T),
  end_date = max(date, na.rm = T),
  n_samples = .N
),
by = .(site_no)]

# Merge with Mexico station metadata
merge_cols <- colnames(finland_metadata_supplement)[which(colnames(finland_metadata_supplement) != 'site_no')]

finland_stns <- merge(finland_stns[ , .SD, .SDcols = !merge_cols],
                      finland_metadata_supplement, 
                     on = 'site_no')

#### 2F. KOREA SSC DATA ####
# Import SSC data for Finland
# Supplementary data: SSC, Q
# Missing summary stats for start_date, end_date, N_samples
list.files(paste0(wd_training, 'Korea/'))
korea_ssc <- fread(file = paste0(wd_training, 'Korea/', 'korea_ssc.csv'),
                   colClasses = c('site_no' = 'character'))[,':='(
                     date = date(sample_dt),
                     Source = 'Korea')]

print(korea_ssc)

korea_metadata_supplement <- korea_ssc[,.(site_no, date, SSC_mgL)][
  SSC_mgL >= 0
][,.(
  start_date = min(date, na.rm = T),
  end_date = max(date, na.rm = T),
  n_samples = .N
),
by = .(site_no)]

#Olivia's code start

library(lubridate)

korea_ssc[, date := as.Date(parse_date_time(
  sample_dt,
  orders = c("ymd", "ymd HMS", "mdy", "mdy HMS")
))]
str(korea_ssc$date)
korea_ssc <- korea_ssc[!is.na(date)]

korea_ssc[, .(
  n_total = .N,
  n_na    = sum(is.na(date))
)]

install.packages("tsibble")
library(tsibble)

korea_daily <- korea_ssc[, .(
  SSC_mgL = mean(SSC_mgL, na.rm = TRUE)
), by = .(site_no, date)]

korea_ts <- as_tsibble(
  korea_daily,
  key   = site_no,
  index = date
)

library(ggplot2)

ggplot(korea_daily, aes(x = date, y = SSC_mgL)) +
  geom_point(color = "#E65517", alpha = 0.5, size = 2) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration (Korea Stations)"
  ) +
  theme_dark_mode

#Olivia's Code End
korea_ssc[, date := dmy_hm(start_date)]
korea_ssc_plot <- korea_ssc[SSC_mgL > 0]

korea_metadata_supplement

# Merge with Mexico station metadata
merge_cols <- colnames(korea_metadata_supplement)[which(colnames(korea_metadata_supplement) != 'site_no')]

korea_stns <- merge(korea_stns[ , .SD, .SDcols = !merge_cols],
                      korea_metadata_supplement, 
                     on = 'site_no')

#### 2G. SWITZERLAND SSC DATA ####
# Import SSC data for Finland
# Supplementary data: SSC, Q, oxygen, temperature, alkalinity, salts, metals, nutrients
# Missing summary stats for start_date, end_date, N_samples
list.files(paste0(wd_training, 'Switzerland/'))

switzerland_ssc <- fread(file = paste0(wd_training, 'Switzerland/', 'naduf_data_original.csv'))[,':='(
  Source = 'Switzerland', site_no = as.character(station), station_nm = as.character(station)
)][,':='(
  date = date(dmy_hm(date_end)),
  SSC_mgL = suspended_material,
  DOC_mgL = DOC,
  POC_mgL = TOC - DOC
)]

str(switzerland_ssc$date)
  
switzerland_metadata_supplement <- switzerland_ssc[,.(site_no, date, SSC_mgL)][
  SSC_mgL >= 0
][,.(
  start_date = min(date, na.rm = T),
  end_date = max(date, na.rm = T),
  n_samples = .N
),
by = .(site_no)]

#Start of my code

#SSC 

library(lubridate)

switzerland_ssc_plot <- switzerland_ssc[!is.na(date)]
switzerland_ssc_plot[, .(
  n_total = .N,
  n_na    = sum(is.na(date))
)]

install.packages("tsibble")
library(tsibble)

switzerland_daily <- switzerland_ssc[, .(
  SSC_mgL = mean(SSC_mgL, na.rm = TRUE)
), by = .(site_no, date)]

switzerland_daily <- switzerland_ssc[
  , .(SSC_mgL = mean(SSC_mgL, na.rm = TRUE)),
  by = .(site_no, date)
]

setkey(switzerland_daily, site_no, date)
stopifnot(anyDuplicated(switzerland_daily[, .(site_no, date)]) == 0)

switzerland_ts <- as_tsibble(
  switzerland_daily,
  key   = site_no,
  index = date
)

library(ggplot2)

ggplot(
  switzerland_daily[SSC_mgL > 0],
  aes(x = date, y = SSC_mgL)
) +
  geom_point(color = "#D8C4F3", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration (Switzerland Stations)"
  ) +
  theme_dark_mode

unique(switzerland_daily$Source)
unique(switzerland_ssc$Source)

#POC plot

switzerland_poc_plot <- switzerland_ssc[!is.na(date)]
switzerland_poc_plot[, .(
  n_total = .N,
  n_na    = sum(is.na(date))
)]

install.packages("tsibble")
library(tsibble)

switzerland_daily_POC <- switzerland_ssc[, .(
  POC_mgL = mean(POC_mgL, na.rm = TRUE)
), by = .(site_no, date)]

switzerland_daily_POC <- switzerland_ssc[
  , .(POC_mgL = mean(POC_mgL, na.rm = TRUE)),
  by = .(site_no, date)
]

setkey(switzerland_daily_POC, site_no, date)
stopifnot(anyDuplicated(switzerland_daily_POC[, .(site_no, date)]) == 0)

switzerland_ts_2 <- as_tsibble(
  switzerland_daily_POC,
  key   = site_no,
  index = date
)

ggplot(
  switzerland_daily_POC[POC_mgL > 0],
  aes(x = date, y = POC_mgL)
) +
  geom_point(color = "#D8C4F3", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Switzerland Stations)"
  ) +
  theme_dark_mode

#end of my code

unique(switzerland_daily$Source)
unique(switzerland_ssc$Source)
# Merge with Mexico station metadata
merge_cols <- colnames(switzerland_metadata_supplement)[which(colnames(switzerland_metadata_supplement) != 'site_no')]

switzerland_stns <- merge(switzerland_stns[ , .SD, .SDcols = !merge_cols],
                    switzerland_metadata_supplement, 
                     on = 'site_no')

#### 2H. FRANCE SSC DATA ####
# Import SSC data for Finland
# Supplementary data: SSC, Q, oxygen, temperature, alkalinity, salts, metals, nutrients
# Missing summary stats for start_date, end_date, N_samples
list.files(paste0(wd_training, 'France/'))

france_ssc <- fread(file = paste0(wd_training, 'France/', 'France_moose_insitu.csv'))[,':='(
  Source = 'France', site_no = as.character(site_no), station_nm = as.character(station_nm)
)][,':='(
  date = date(mdy(sample_dt)),
  Q_cms = Q,
  SSC_mgL = TSM,
  DOC_mgL = DOC * 0.01201, # micromoles per liter converted to mgL
  POC_mgL = POC * 0.01201 # micromoles per liter converted to mgL
)]
  
print(france_ssc)

ggplot(france_ssc, aes(x = SSC_mgL, y = DIP)) + 
  geom_point(color = 'white') + 
  geom_smooth(method = 'lm', color = 'white', lty = 'dashed') +
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) 

#my code 

#SSC

ggplot(
  france_ssc[SSC_mgL > 0],
  aes(x = date, y = SSC_mgL)
) +
  geom_point(color = "#7478E3", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration (French Stations)"
  ) +
  theme_dark_mode

#POC

ggplot(
  france_ssc[POC_mgL > 0],
  aes(x = date, y = POC_mgL)
) +
  geom_point(color = "#7478E3", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (French Stations)"
  ) +
  theme_dark_mode

#end of my code

france_metadata_supplement <- france_ssc[,.(site_no, date, SSC_mgL)][
  SSC_mgL >= 0
][,.(
  start_date = min(date, na.rm = T),
  end_date = max(date, na.rm = T),
  n_samples = .N
),
by = .(site_no)]

# Merge with Mexico station metadata
merge_cols <- colnames(france_metadata_supplement)[which(colnames(france_metadata_supplement) != 'site_no')]

france_stns <- merge(france_stns[ , .SD, .SDcols = !merge_cols],
                          france_metadata_supplement, 
                     on = 'site_no')

#### 2I. UNITED STATES SSC DATA ####
# Import SSC data for the United States
# Supplementary data: SSC, Q, oxygen, temperature, alkalinity, salts, metals, nutrients
# Missing summary stats for start_date, end_date, N_samples
usa_ssc_files <- list.files(paste0(wd_training, 'United_states/'))

usa_ssc <- rbindlist(
  lapply(
    paste0(wd_training, 'United_states/', usa_ssc_files),
    fread, colClasses = c('site_no' = 'character')),
          use.names = T, fill = T)[
            ,':='(
              Source = 'USA',
              SSC_mgL = ifelse(!is.na(p80154), p80154, p00530),
              POC_mgL = p00689,
              p63 = ifelse(!is.na(p70331),p70331,p70342),
              width_m = p00004/3.28,
              sample_depth_m = ifelse(!is.na(p00003), p00003/3.28, p00098),
              sample_method = p82398
            )
          ][
            ,':='(date = date(sample_dt))
          ]

#Start of my code

#SSC (it runs slow; don't get scared)

library(ggplot2)
ggplot(usa_ssc[SSC_mgL > 0], aes(x = date, y = SSC_mgL)) +
  geom_point(color = "#2C7FB8", alpha = 0.3, size = .75) +
  scale_y_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration (USA Stations)"
  ) +
  theme_dark_mode

#SSC for different stations

# Add a column for site prefix (first 2 characters of site_no)
usa_ssc[, site_prefix := substr(site_no, 1, 2)]

usa_daily <- usa_ssc[
  SSC_mgL > 0,   # remove zero / negative
  .(SSC_mgL = mean(SSC_mgL, na.rm = TRUE)),
  by = .(site_no, date)
]

# Add site_prefix before aggregation
usa_ssc[, site_prefix := substr(site_no, 1, 2)]

# Aggregate daily per station, keeping site_prefix
usa_daily <- usa_ssc[
  SSC_mgL > 0,   # remove zero / negative
  .(SSC_mgL = mean(SSC_mgL, na.rm = TRUE)),
  by = .(site_no, site_prefix, date)   # <- include site_prefix
]

ggplot(usa_daily, aes(x = date, y = SSC_mgL, color = site_no)) +
  geom_point(size = 0.5, alpha = 0.3) +
  facet_wrap(. ~ site_prefix) +
  scale_y_log10(labels = fancy_scientific_modified) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration over time (USA stations)"
  ) +
  theme_dark_mode +
  facet_wrap(.~substr(site_no, 1,2)) +
  theme(
    axis.text.x = element_text(size = 6)  # smaller text
  )

#POC 

#Single graph

library(ggplot2)
ggplot(usa_ssc[POC_mgL > 0], aes(x = date, y = POC_mgL)) +
  geom_point(color = "#FEF6B9", alpha = 0.3, size = .75) +
  scale_y_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (USA Stations)"
  ) +
  theme_dark_mode

#Multiple Graphs

# Add a column for site prefix (first 2 characters of site_no)
usa_ssc[, site_prefix := substr(site_no, 1, 2)]

usa_daily_POC <- usa_ssc[
  POC_mgL > 0,   # remove zero / negative
  .(POC_mgL = mean(POC_mgL, na.rm = TRUE)),
  by = .(site_no, date)
]

# Add site_prefix before aggregation
usa_ssc[, site_prefix := substr(site_no, 1, 2)]

# Aggregate daily per station, keeping site_prefix
usa_daily_POC <- usa_ssc[
  POC_mgL > 0,   # remove zero / negative
  .(POC_mgL = mean(POC_mgL, na.rm = TRUE)),
  by = .(site_no, site_prefix, date)   # <- include site_prefix
]

ggplot(usa_daily_POC, aes(x = date, y = POC_mgL, color = site_no)) +
  geom_point(size = 0.5, alpha = 0.3) +
  facet_wrap(. ~ site_prefix) +
  scale_y_log10(labels = fancy_scientific_modified) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration over time (USA stations)"
  ) +
  theme_dark_mode +
  facet_wrap(.~substr(site_no, 1,2)) +
  theme(
    axis.text.x = element_text(size = 6)  # smaller text
  )

#end of my code

ggplot(usa_ssc[POC_mgL > 0 & SSC_mgL > 0], aes(x = SSC_mgL, y = POC_mgL/SSC_mgL)) + 
  # geom_point(color = 'white') + 
  geom_point(aes(color = substr(site_no, 1,2))) + 
  geom_smooth(method = 'lm', color = 'white', lty = 'dashed') +
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  facet_wrap(.~substr(site_no, 1,2)) +
  #I added this
  theme(
    axis.text.x = element_text(size = 8)  # smaller text
  )

# Grain size
usa_sites_with_grain_size <- usa_ssc[p63 >= 0 & p63 < 100 & SSC_mgL > 0][
  ,.(N_p63_samples = .N), by = .(site_no)][
    order(-N_p63_samples) & N_p63_samples > 100]

ggplot(usa_ssc[p63 >= 0 & p63 < 100 & SSC_mgL > 0][site_no %chin% usa_sites_with_grain_size[, site_no][1:9]], 
       aes(x = SSC_mgL, y = p63)) + 
  # geom_point(color = 'white') + 
  # geom_point(aes(color = substr(site_no, 1,2))) + 
  geom_point(aes(color = site_no)) + 
  # geom_smooth(method = 'lm', color = 'white', lty = 'dashed') +
  geom_smooth(method = 'lm', color = 'black', lty = 'dashed') +
  season_facet +
  scale_x_log10(labels = fancy_scientific_modified) +
  # scale_y_log10(labels = fancy_scientific_modified) +
  # facet_wrap(.~substr(site_no, 1,2))
  facet_wrap(.~site_no) +
  labs(
    x = '***SSC (mg/L)***',
    y = '***% finer than sand***'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

ggplot(usa_ssc[substr(site_no, 1,2) == '01' & POC_mgL > 0 & SSC_mgL > 0], aes(x = SSC_mgL, y = POC_mgL/SSC_mgL)) + 
  # geom_point(color = 'white') + 
  stat_summary(geom = 'line', aes(color = substr(site_no, 1,2))) + 
  stat_summary(geom = 'point', aes(color = substr(site_no, 1,2))) + 
  # geom_smooth(method = 'lm', color = 'white', lty = 'dashed') +
  theme_dark_mode +
  # scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  facet_wrap(.~substr(site_no, 1,2))

usa_metadata_supplement <- usa_ssc[,.(site_no, date, SSC_mgL, width_m)][
  SSC_mgL >= 0
][,.(
  start_date = min(date, na.rm = T),
  end_date = max(date, na.rm = T),
  n_samples = .N,
  width_m = mean(width_m, na.rm = T)
),
by = .(site_no)]


# Merge with Mexico station metadata
merge_cols <- colnames(usa_metadata_supplement)[which(colnames(usa_metadata_supplement) != 'site_no')]

usa_stns <- merge(usa_stns[ , .SD, .SDcols = !merge_cols],
                          usa_metadata_supplement, 
                     on = 'site_no')

#### 2I. MEKONG SSC DATA ####
# Import SSC data for the Mekong River
# Supplementary data: SSC
# Missing summary stats for start_date, end_date, N_samples
mekong_ssc_files <- list.files(paste0(wd_training, 'Mekong/'))
mekong_metadata_files <- list.files(paste0(wd_metadata_misc, 'Mekong_metadata/'), '.txt')

mekong_ssc <- rbindlist(
  lapply(
    paste0(wd_training, 'Mekong/', mekong_ssc_files),
    fread, colClasses = c('Station Code' = 'character')),
          use.names = T, fill = T)[
            ,':='(
              Source = 'Mekong',
              SSC_mgL = Value,
              sample_method = Label,
              site_no = as.character(`Station Code`), 
              country = as.character(`Country Code`)
            )
          ][
            ,':='(date = date(`Timestamp (UTC+07:00)`))
          ]

#Start of my code 

#SSC

library(ggplot2)
ggplot(mekong_ssc[SSC_mgL > 0], aes(x = date, y = SSC_mgL)) +
  geom_point(color = "#FEB9FC", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "SSC (mg/L)",
    title = "Suspended Sediment Concentration (Mekong Stations)"
  ) +
  theme_dark_mode

#POC

library(ggplot2)
ggplot(mekong_ssc[POC_mgL > 0], aes(x = date, y = POC_mgL)) +
  geom_point(color = "#FEB9FC", alpha = 0.5, size = 1) +
  scale_y_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    x = "Date",
    y = "POC (mg/L)",
    title = "Particulate Organic Carbon (Mekong Stations)"
  ) +
  theme_dark_mode

#End of my code
get_mekong_station_meta <- function(filename){
  mekong_meta_file <- fread(paste0(wd_metadata_misc, 'Mekong_metadata/', filename),
                skip = 9, sep = ':', nrows = 4, header = FALSE)
  
  site_no_sel <- substr(filename, 41, 46)
  
  mekong_dt <- dcast.data.table(.~V1, value.var = 'V2', data = mekong_meta_file)
  colnames(mekong_dt) <- gsub('# ', '', casefold(colnames(mekong_dt)))
  
  mekong_dt <- mekong_dt[,':='(river_name = ifelse(river == 'N/A', 'Mekong', river))]
  
  return(mekong_dt[,.(site_no = site_no_sel, river_name, 
                      latitude = as.numeric(latitude), longitude = as.numeric(longitude))])
}

mekong_station_file <- rbindlist(
  lapply(mekong_metadata_files, get_mekong_station_meta),
  fill = T, use.names = T)
  
  

mekong_metadata_supplement <- merge(
  mekong_ssc[,.(site_no, country, date, SSC_mgL)][
    SSC_mgL >= 0
  ][,.(
    start_date = min(date, na.rm = T),
    end_date = max(date, na.rm = T),
    n_samples = .N
  ),
  by = .(site_no, country)],
  mekong_station_file,
  on = c('site_no')
)

# Merge with Mexico station metadata
merge_cols <- colnames(mekong_metadata_supplement)[which(colnames(mekong_metadata_supplement) != 'site_no')]

mekong_stns <- merge(mekong_stns[ , .SD, .SDcols = !merge_cols],
                          mekong_metadata_supplement, 
                     on = 'site_no')

fwrite(mekong_stns, file = paste0(wd_station_metadata, 'mekong_mrc_station_metadata.csv'))

#### COMBINE STATION DATA FROM DIFFERENT SOURCES ####
combined_stns <-rbindlist(
  list(
    finland_stns, usa_stns, canada_stns, 
    mexico_stns,switzerland_stns, korea_stns,
    danube_stns, france_stns, 
    morepoc_stns, mekong_stns
  ), use.names = T, fill = T)


combined_stns_metadata_summary <- combined_stns[,.(
  width_m = mean(width_m, na.rm = T),
  drainage_area_km2 = mean(drainage_area_km2, na.rm = T),
  elevation_m = mean(elevation_m, na.rm = T),
  start_date = min(start_date, na.rm = T),
  end_date = max(end_date, na.rm = T),
  n_samples = sum(n_samples, na.rm = T),
  n_sites = uniqueN(site_no)
),
by = .(source)]

print(paste0('N samples: ', sum(combined_stns_metadata_summary$n_samples, na.rm = T)))
print(paste0('N sites: ', sum(combined_stns_metadata_summary$n_sites, na.rm = T)))
print(paste0('N countries: ', length(unique(combined_stns$country))))

#### MAP STATIONS ####
world_map <- data.table(map_data(map = 'world'))
## Fig. 1a
global_ssc_sample_sites_map <- ggplot() +
  geom_polygon(data = world_map[long > -183 & long < 195 & lat > -50 & lat < 80], 
               aes(x = long, y = lat, group = group),
            fill = 'grey30', color = 'grey90', lwd = 0.2) +
  geom_point(data = combined_stns[
    # order(n_samples)
    # source == 'Canada'
    ][,':='(n_samples = ifelse(is.na(n_samples), 1, n_samples))], 
    aes(x = longitude, y = latitude, fill = n_samples), 
             pch = 21,
             stroke = 0.2, size = 2.75
             ) +
  season_facet +
  scale_fill_gradientn(colors = c('#22A2F2', '#F2BE22'), limits = c(1, 1000), 
                       trans = 'log10', oob = squish) +
  # scale_shape_manual(values = c('Gold' = 21, 'Diamonds' = 23, 'Nickel' = 22, 'Other' = 24)) +
  scale_x_continuous(limits = c(-183, 195), expand = expansion(mult = c(0, 0)),
                     breaks = seq(-180,180,60),
                     labels = long_dd_lab,
                     sec.axis = dup_axis()) +
  scale_y_continuous(limits = c(-50, 80), expand = expansion(mult = c(0, 0)),
                     breaks = seq(-60,80,20),
                     labels = lat_dd_lab,
                     sec.axis = dup_axis()) + 
  labs(
    x = '',
    y = '',
    fill = '**N samples**'
  ) +
  theme_dark_mode +
  theme(legend.position = c(0.02, 0.4),
        legend.justification = 'left',
        legend.key.width = unit(0.4,"cm"),
        legend.title = element_markdown())
  

ggsave(global_ssc_sample_sites_map, filename = paste0(wd_figures, 'global_ssc_sample_sites_map.pdf'),
       width = 11, height = 5, useDingbats = F)
ggsave(global_ssc_sample_sites_map, filename = paste0(wd_figures, 'global_ssc_sample_sites_map.png'),
       width = 11, height = 5)


#### TURBIDITY VS SSC RELATIONSHIPS, MULITPLE SOURCES ####
# Canada
# Table of turbidity and SSC
canada_turbidity_tss_table_all <- dcast.data.table(SITE_NO + date(ymd_hm(DATE_TIME_HEURE)) ~ VARIABLE, 
                                               value.var = 'VALUE_VALEUR',
                                               data = canada_ssc[,':='(VALUE_VALEUR = as.numeric(VALUE_VALEUR))],
                                               fun.aggregate = mean_na
)[,':='(SSC_mgL = ifelse(!is.na(`TOTAL SUSPENDED SOLIDS (NON-FILTERABLE RESIDUE)`), 
                         `TOTAL SUSPENDED SOLIDS (NON-FILTERABLE RESIDUE)`,
                         `TOTAL SUSPENDED SOLIDS (NON-FILTERABLE RESIDUES OR TOTAL SUSPENDED MATERIALS)`),
        Turbidity_NTU = ifelse(!is.na(`TURBIDITY (FIELD)`),
                               `TURBIDITY (FIELD)`,
                               TURBIDITY),
        POC_mgL = `CARBON PARTICULATE ORGANIC`
)
][
  SSC_mgL > 0 & Turbidity_NTU > 0
]

canada_turbidity_tss_table <- canada_turbidity_tss_table_all[,.(
  Source = 'Canada',
  site_no = SITE_NO,
  date = DATE_TIME_HEURE,
  SSC_mgL,
  Turbidity_NTU,
  POC_mgL
)]

# Model, Canada
canada_turbidity_tss_model <- lm(log10(SSC_mgL) ~ log10(Turbidity_NTU), data = canada_turbidity_tss_table)

summary(canada_turbidity_tss_model)
SSC_Turb_labs <- labs(
  x = 'Turbidity (NTU)',
  y = 'SSC (mg/L)'
)
# Plot SSC vs. Turbidity, Canada
turbidity_vs_SSC_canada_plot <- 
  ggplot(canada_turbidity_tss_table, 
         aes(x = Turbidity_NTU, y = SSC_mgL)) +
  geom_point(aes(color = site_no)) +
  stat_summary(aes(
    x = (Turbidity_NTU = 10^(log10(Turbidity_NTU) - 
                               log10(Turbidity_NTU)%%mod_avg_bin + mod_avg_bin/2))),
    color = 'white') +
  geom_smooth(method = 'lm', color = 'white', lty = 'dashed') +
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  SSC_Turb_labs

mod_avg_bin <- 0.25

mexico_ssc_binned <- mexico_ssc[,.(
  SSC_mgL = 10^mean(log10(SSC_mgL), na.rm = T)),
  by = .(Turbidity_NTU = 10^(log10(Turbidity_NTU) - log10(Turbidity_NTU)%%mod_avg_bin + mod_avg_bin/2))][
    order(Turbidity_NTU)
  ]

# Mexico
# Plot SSC vs. Turbidity, Mexico
turbidity_vs_SSC_mexico_plot <- ggplot(mexico_ssc, aes(x = Turbidity_NTU, y = SSC_mgL)) +
  geom_point(aes(color = site_no), alpha = 0.1)  +
  stat_summary(aes(
    x = (Turbidity_NTU = 10^(log10(Turbidity_NTU) - 
                               log10(Turbidity_NTU)%%mod_avg_bin + mod_avg_bin/2))),
    color = 'white') +
  # geom_point(data = mexico_ssc_binned,
  #   color = 'white') +
  geom_smooth(data = mexico_ssc_binned,
    method = 'lm', color = 'white', lty = 'dashed') +
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  SSC_Turb_labs

n_mexico_tss_turbidity <- nrow(mexico_ssc[Turbidity_NTU > 0 & !is.na(SSC_mgL)])
n_canada_tss_turbidity <- nrow(canada_turbidity_tss_table[Turbidity_NTU > 0 &
    !is.na(SSC_mgL)])


#### SSC AND CONSTITUENT RELATIONSHIPS, MULTIPLE SOURCES ####
# Combined turbidity vs. SSC, multiple sources
turbidity_vs_SSC_combined_plot <- 
  ggplot() +
  geom_point(data = mexico_ssc, 
             aes(x = Turbidity_NTU, y = SSC_mgL, 
                 color = paste0('Mexico (',format(n_mexico_tss_turbidity, big.mark=",", trim = T), ')'))) +
  geom_point(data = canada_turbidity_tss_table, 
             aes(x = Turbidity_NTU, y = SSC_mgL, 
                 color = paste0('Canada (',format(n_canada_tss_turbidity, big.mark=",", trim = T), ')'))) +
  geom_smooth(method = 'lm') +
  # theme_dark_mode +
  season_facet +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  theme(
    legend.position = 'top'
  ) + 
  labs(
    x = 'Turbidity (NTU)',
    y = 'SSC (mg/L)',
    color = ''
  )

colnames(canada_turbidity_tss_table)

n_morepoc_SSC_POC <- nrow(morepoc_ssc[POC_mgL > 0 & !is.na(SSC_mgL)])
n_danube_SSC_POC <- nrow(danube_ssc[POC_mgL > 0 & !is.na(SSC_mgL)])
n_finland_SSC_POC <- nrow(finland_ssc_wide[POC_mgL > 0 & !is.na(SSC_mgL)])
n_switzerland_SSC_POC <- nrow(switzerland_ssc[POC_mgL > 0 & !is.na(SSC_mgL)])
n_france_SSC_POC <- nrow(france_ssc[POC_mgL > 0 & !is.na(SSC_mgL)])
n_usa_SSC_POC <- nrow(usa_ssc[POC_mgL > 0 & !is.na(SSC_mgL)])

# Combined SSC vs. POC, multiple sources
SSC_POC_dt_all <- rbindlist(
  list(
    mexico_ssc[,.(Source, date, site_no, SSC_mgL, POC_mgL)],
    switzerland_ssc[,.(Source, date, site_no, SSC_mgL, POC_mgL)],
    usa_ssc[,.(Source, site_no, date, SSC_mgL, POC_mgL)],
    france_ssc[,.(Source, site_no, date, SSC_mgL, POC_mgL)],
    korea_ssc[,.(Source, site_no, date, SSC_mgL)],
    canada_turbidity_tss_table[,.(Source, site_no, date, SSC_mgL, POC_mgL)],
    morepoc_ssc[,.(Source, site_no, date, SSC_mgL, POC_mgL)],
    danube_ssc[,.(Source, site_no, date, SSC_mgL, POC_mgL)],
    finland_ssc_wide[,.(Source, site_no, date, SSC_mgL, POC_mgL)],
    mekong_ssc[,.(Source, site_no, date, SSC_mgL)]
  ),
  use.names = T, fill = T
)

SSC_POC_dt_all <- SSC_POC_dt_all[,':='(month = month(date),
                                       year = year(date))]
# By-site summary
SSC_POC_dt_site_summary <- SSC_POC_dt_all[,.(
  N_samples = .N,
  SSC_mgL = mean(SSC_mgL, na.rm = T),
  POC_mgL = mean(POC_mgL, na.rm = T),
  `%POC` = mean(POC_mgL/SSC_mgL, na.rm = T) * 100
), by = .(Source, site_no)]

# Monthly summary
SSC_POC_dt_site_month_summary <- SSC_POC_dt_all[,.(
  N_samples = .N,
  SSC_mgL = mean(SSC_mgL, na.rm = T),
  POC_mgL = mean(POC_mgL, na.rm = T),
  `%POC` = mean(POC_mgL/SSC_mgL, na.rm = T) * 100
), by = .(Source, site_no, month)]

# Standardize month data
SSC_POC_dt_site_month_summary <- SSC_POC_dt_site_month_summary[,':='(
  SSC_mgL_mean = mean(SSC_mgL, na.rm = T),
  POC_mgL_mean = mean(POC_mgL, na.rm = T),
  `%POC_mean` = mean(`%POC`, na.rm = T),
  N_total_samples = sum(N_samples, na.rm = T)),
  by = .(Source, site_no)][,':='(
  SSC_star = SSC_mgL/SSC_mgL_mean,
  POC_star = POC_mgL/POC_mgL_mean,
  `%POC_star` = `%POC`/`%POC_mean`), 
  by = .(Source, site_no, month)]


unique_sources <- unique(SSC_POC_dt_site_month_summary[,Source])

site_no_examples <- unique(SSC_POC_dt_site_month_summary[Source == unique_sources[3] & N_total_samples > 40,site_no])

# ggplot(SSC_POC_dt_site_month_summary[site_no %chin% site_no_examples[1:3]],
ggplot(SSC_POC_dt_all[site_no %chin% site_no_examples[7:10]],
       aes(x = SSC_mgL, y = POC_mgL/SSC_mgL)) +
  # stat_summary(geom = 'point', aes(color = month)) +
  geom_point(aes(fill = site_no), color = 'white', size = 3, pch = 21) +
  geom_smooth(method = 'lm', se = F, aes(group = site_no, color = site_no)) +
  # geom_smooth(method = 'lm', se = F, aes(group = month, color = month)) +
  # facet_wrap(.~site_no, scales = 'free') + 
  scale_x_log10(labels = fancy_scientific_modified) + 
  scale_y_log10(labels = fancy_scientific_modified) + 
  theme_dark_mode + 
  theme(legend.position = 'top')

SSC_POC_dt_example <- SSC_POC_dt_site_month_summary[
  Source == 'Finland' & N_total_samples >= 50
]

SSC_POC_dt_site_month_summary
(ggplot(SSC_POC_dt_example, 
  aes(x = month, y = SSC_star)) + 
  stat_summary(color = 'white') + 
  stat_summary(geom = 'line', color = 'white') + 
  theme_dark_mode +
  labs(
    x = 'Month',
    y = 'SSC*'
  ) + ggplot(SSC_POC_dt_example, 
  aes(x = month, y = `%POC`)) + 
  stat_summary(color = 'red') + 
  stat_summary(geom = 'line', color = 'red') + 
  theme_dark_mode +
  labs(
    x = 'Month',
    y = '%POC*'
  )
) +
  plot_layout(ncol = 1)

ggplot(SSC_POC_dt_example, 
  aes(x = month, y = SSC_star)) + 
  geom_line(aes(group = site_no, color = site_no)) + 
  geom_line(aes(group = site_no,  y = `%POC_star`), color = 'white') +
  theme_dark_mode +
  facet_wrap(.~site_no) +
  labs(
    x = 'Month',
    y = 'SSC*'
  )

ggplot(SSC_POC_dt_example) + 
  geom_point(aes(x = SSC_mgL,  y = `%POC`, color = site_no)) +
  geom_smooth(aes(x = SSC_mgL,  y = `%POC`, color = site_no), se = F, method = 'lm') +
  theme_dark_mode +
  # facet_wrap(.~site_no) +
  scale_x_log10(labels = fancy_scientific_modified) + 
  scale_y_log10(labels = fancy_scientific_modified) + 
  labs(
    x = 'SSC*',
    y = 'POC*'
  )

ggplot(SSC_POC_dt_example) + 
  geom_boxplot(aes(x = site_no,  y = `%POC`, fill = site_no), color = 'white') +
  theme_dark_mode +
  # facet_wrap(.~site_no) +
  scale_y_log10(labels = fancy_scientific_modified) + 
  labs(
    x = 'Site',
    y = 'POC*'
  )

SSC_vs_POC_combined_plot <- 
  ggplot() +
  geom_point(data = mexico_ssc, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL, 
                 color = paste0('Mexico (',format(n_mexico_tss_turbidity, big.mark=",", trim = T), ')')),
             alpha = 0.05) +
  geom_point(data = usa_ssc, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL, 
                 color = paste0('USA (',format(n_usa_SSC_POC, big.mark=",", trim = T), ')')),
             alpha = 0.05) +
  geom_point(data = switzerland_ssc, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL,
                 color = paste0('Switzerland (',format(n_switzerland_SSC_POC, big.mark=",", trim = T), ')')),
             alpha = 0.05) +
  geom_point(data = france_ssc, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL,
                 color = paste0('France (',format(n_france_SSC_POC, big.mark=",", trim = T), ')')),
             alpha = 0.05) +
  geom_point(data = canada_turbidity_tss_table, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL,
                 color = paste0('Canada (',format(n_canada_tss_turbidity, big.mark=",", trim = T), ')')),
             alpha = 0.6) +
  geom_point(data = morepoc_ssc, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL,
                 color = paste0('MOREPOC (',format(n_morepoc_SSC_POC, big.mark=",", trim = T), ')')),
             alpha = 0.6) +
  geom_point(data = danube_ssc, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL,
                 color = paste0('Danube (',format(n_danube_SSC_POC, big.mark=",", trim = T), ')')),
             alpha = 0.6) +
  geom_point(data = finland_ssc_wide, 
             aes(x = SSC_mgL, y = POC_mgL/SSC_mgL,
                 color = paste0('Finland (',format(n_finland_SSC_POC, big.mark=",", trim = T), ')')),
             alpha = 0.6) +
  geom_smooth(method = 'lm') +
  facet_wrap(.~Source) +
  season_facet +
  # theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified, limits = c(1e-4, 1)) +
  theme(
    legend.position = 'top'
  ) + 
  labs(
    x = 'SSC (mg/L)',
    y = '%POC',
    color = ''
  )


SSC_vs_POC_summary_combined_plot <- 
  ggplot() +
  geom_point(data = SSC_POC_dt_site_summary[!is.na(`%POC`)][
    N_samples > 10
  ], 
             aes(x = SSC_mgL, y = `%POC`, 
                 color = Source), alpha = 0.3) +
  geom_smooth(method = 'lm') +
  facet_wrap(.~Source) +
  theme_dark_mode +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified, limits = c(NA, 100)) +
  theme(
    legend.position = 'top'
  ) + 
  labs(
    x = 'SSC (mg/L)',
    y = '%POC',
    color = ''
  )

