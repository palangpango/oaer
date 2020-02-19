<<<<<<< HEAD
pkgs = c("tidyverse", "readxl", "leaflet", "RColorBrewer",
         "plotly", "hrbrthemes", "gghighlight"
        )

inst = lapply(pkgs, suppressWarnings(library), character.only=TRUE)

path <- "../data/_meta/countries.xlsx"
=======
library(tidyverse)
library(readxl)
library(leaflet)
library(RColorBrewer)


#จัดการข้อมูล
path <- "../data/trade/export_1006_th.xls"
export_1006 <- read_excel(path, skip=1, na = c("-","",0)) %>%
    gather("type_year", "amount", -(1:7)) %>%
    drop_na("amount") %>%
    separate(type_year, sep="25", into=c("type","year")) %>%
    mutate(year_thai = as.numeric(paste0("25", year))) %>%
    mutate(year = year_thai-543)

path <- "../data/_meta/countries.xlsx"
tbl_countries <- read_excel(path, skip=0, na = c("-",""))
>>>>>>> master

# Grouping Table: HS Code
tbl_countries <- read_excel(path, skip=0, na = c("-",""))
path <- "../data/_meta/hs1006_th.xlsx"
tbl_hs1006 <- read_excel(path, sheet='hs_rice', skip=0)
tbl_hs1006_group <- read_excel(path, sheet='rice_group', skip=0)
<<<<<<< HEAD

# World Map: geojson format
world_map <- geojsonio::geojson_read("../data/map/world_lowres.geojson", what = "sp")
=======


# join countries&hscode 
# left join(ตารางที่เอาเข้ามา join, by=c(ตัวที่เหมือนกันให้ matchกัน))
export_1006_joined <- export_1006 %>% 
    left_join(tbl_countries, by=c("country_name_th" = "country_name_oae")) %>%
    left_join(tbl_hs1006, by=c("hscode" = "hscode"))


export_by_year_country_varities <- export_1006_joined %>%
    filter(type=="vol") %>%
    group_by(year, varities, iso3) %>%
    mutate(amount = amount/1e6) %>%
    summarize(export_vol = sum(amount))

ex_vol_hommali_2010 <- export_by_year_country_varities %>%
    filter(year == 2010 & varities == "hommali")


world_map <- geojsonio::geojson_read("../data/map/world_lowres.geojson", what = "sp")
world_map_joined <- world_map
world_map_joined@data <- world_map_joined@data %>%
    left_join(ex_vol_hommali_2010, by = c('adm0_a3' = 'iso3'))
head(world_map_joined@data)

world_map_joined@data %>%
    filter(grepl('United', sovereignt))



mybins <- c(0,10,20,50,100,200,Inf)
mypalette <- colorBin( palette="YlOrRd", domain=world_map_joined$export_vol, na.color="transparent", bins=mybins)
mytext <- paste(
    "Country: ",world_map_joined$sovereignt ,"<br/>", 
    "Volume: ", round(world_map_joined$export_vol, 2), 
    sep="") %>%
  lapply(htmltools::HTML)
>>>>>>> master

# Import Export Data
export_all <- readRDS("../data/export_1006.rds")

<<<<<<< HEAD
# Functions

query <- function(data, year=NULL, varities=NULL, data_type='vol') {
=======

query <- function(data, year=2018, varities='hommali', data_type='vol') {
>>>>>>> master
    # input = a data frame rice export from thailand 
    # output = a data frame by by hscode and countries and year-month
    
    if(is.null(year)) {
        year_list <- unique(data$year) 
    } else {
        year_list <- year
    }
    
    if(is.null(varities)) {
        varities_list <- unique(data$varities) 
    } else {
        varities_list <- varities
    }
    
    res <- data %>%
        filter(type == data_type) %>%
        group_by(year, varities, iso3, country_name_en, region) %>%
        filter(year %in% year_list & varities %in% varities_list) %>%
        mutate(amount = amount/1e6) %>%
        summarize(amount = sum(amount))
    return(res)
}
<<<<<<< HEAD
=======


plot_map <- function(map, data, year=2018, varities='hommali', data_type='vol') {
    # step 1 query data given by year varities and data_type
    # step 2 left join queied data into world_map
    # step 3 plot map using leaflet
    
    varities_name = str_to_title(varities)
    
    if (data_type == 'vol'){
        dtype_name <- "Volume"
    }
    else {
        dtype_name <- "Value"
    }
    
    # step 1: Query
    quried <- query(data, year=year, varities=varities, data_type=data_type)
    
    # step 2: Join
    map_joined <- map
    map_joined@data <- map_joined@data %>%
        left_join(quried, by = c('adm0_a3' = 'iso3'))
    
    # step 3: Plot 
    mypalette <- colorBin( palette="YlOrRd", domain=map_joined$amount, na.color="transparent", bins=mybins)
    mytext <- paste(
        "Country: ",map_joined$sovereignt ,"<br/>", 
        "Volume: ", round(map_joined$amount, 2), 
        sep="") %>%
      lapply(htmltools::HTML)
>>>>>>> master


# query <- function(data, year=2018, varities='hommali', data_type='vol') {
#     # input = a data frame rice export from thailand 
#     # output = a data frame by by hscode and countries and year-month
#     yr <- year
#     var <- varities
#     res <- data %>%
#         filter(type == data_type) %>%
#         group_by(year, varities, iso3, country_name_en, region) %>%
#         filter(year %in% yr & varities == var) %>%
#         mutate(amount = amount/1e6) %>%
#         summarize(amount = sum(amount))
#     return(res)
# }

source("plot_ts.R")
source("plot_map.R")
source("plot_top20.R")
source("plot_bar.R")
source("create_table.R")
