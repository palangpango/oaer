pkgs = c("tidyverse", "readxl", "leaflet", "RColorBrewer",
         "plotly", "hrbrthemes", "gghighlight"
        )
library(tidyverse)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(gghighlight)


path <- "../data/trade/export_1006_th.xls"

# Data
# Export Data
export_1006 <- read_excel(path, skip=1, na = c("-","",0), guess_max=5000) %>%
    gather("type_year", "amount", -(1:7)) %>%
    drop_na("amount") %>%
    separate(type_year, sep="25", into=c("type","year")) %>%
    mutate(year_thai = as.numeric(paste0("25", year))) %>%
    mutate(year = year_thai-543)

# Grouping Table: Countries
path <- "../data/_meta/countries.xlsx"
tbl_countries <- read_excel(path, skip=0, na = c("-",""))

# Grouping Table: HS Code
path <- "../data/_meta/hs1006_th.xlsx"
tbl_hs1006 <- read_excel(path, sheet='hs_rice', skip=0)
tbl_hs1006_group <- read_excel(path, sheet='rice_group', skip=0)

# World Map: geojson format
world_map <- geojsonio::geojson_read("../data/map/world_lowres.geojson", what = "sp")


# join countries&hscode
# left join(ตารางที่เอาเข้ามา join, by=c(ตัวที่เหมือนกันให้ matchกัน))
export_1006_joined <- export_1006 %>% 
    left_join(tbl_countries, by=c("country_name_th" = "country_name_oae")) %>%
    left_join(tbl_hs1006, by=c("hscode" = "hscode"))


# Functions
query <- function(data, year=2018, varities='hommali', data_type='vol') {
    # input = a data frame rice export from thailand 
    # output = a data frame by by hscode and countries and year-month
    yr <- year
    var <- varities
    res <- data %>%
        filter(type == data_type) %>%
        group_by(year, varities, iso3, region) %>%
        filter(year %in% yr & varities == var) %>%
        mutate(amount = amount/1e6) %>%
        summarize(amount = sum(amount))
    return(res)
}

plot_map <- function(data, map = world_map, year=2018, varities='hommali', data_type='vol') {
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
    mybins <- c(0,10,20,50,100,200,Inf)
    mypalette <- colorBin( palette="YlOrRd", domain=map_joined$amount, na.color="transparent", bins=mybins)
    mytext <- paste(
        "Country: ",map_joined$sovereignt ,"<br/>", 
        "Volume: ", round(map_joined$amount, 2), 
        sep="") %>% lapply(htmltools::HTML)

    plt <- leaflet(map_joined) %>%
        addTiles() %>%
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons(
            fillColor = ~mypalette(amount),
            label = mytext,
            stroke = FALSE, 
            fillOpacity = 0.5, 
            smoothFactor = 0.5,
            ) %>%
        addLegend(pal=mypalette, values=~amount, opacity=0.9, 
                  title = paste(varities_name, "Rice Export", dtype_name, year), position = "bottomleft")
    
    return(plt)
}

     plot_ts <- function(data, varities='hommali', data_type='vol'){
    data <- query(data, varities = varities, data_type = data_type) %>%
        drop_na(region) %>%
        group_by(year, region) %>%
        summarize(amount = sum(amount))
    
    g <- data %>%
        ggplot( aes(x=year, y=amount, color = region), size=4) +
        geom_line(size=1) +
        theme_ipsum()
    return(ggplotly(g))
    }

    plot_tstop5 <- function(data, year=NULL, varities='hommali', data_type='vol'){
    
    year_ <- ifelse(is.null(year), max(data$year), year)
    varities_ <- varities
    
    top5 <- data %>%
        filter(year==year_ & varities==varities_) %>%
        group_by(iso3) %>%
        summarize(amount = sum(amount)) %>%
        top_n(5, amount)
    
    data <- query(data, varities = varities, data_type = data_type) %>%
        drop_na(region) %>%
        filter(iso3 %in% top5$iso3) %>%
        group_by(year, iso3) %>%
        summarize(amount = sum(amount))

   
        
    # map
    years <- unique(export_1006_joined$year)
    export_by_type <- query(export_1006_joined, year=years)
    plot_map(export_1006_joined, year=2011, varities='white', data_type='vol')
    plot_map <- export_by_type %>%
        drop_na(region) %>%
    #   filter(region %in% c("Asia","Europe","Africa","Oceania","Americas")) %>%
        filter(varities =="hommali") %>%
        group_by(year, region) %>%
        summarize(amount = sum(amount))
    
    # table
    export_all <- readRDS("../data/export_1006.rds")
    data <- export_all %>%
        group_by(year, varities, iso3, type) %>%
        summarize(amount = sum(amount)) %>%
        spread(type, amount)

    datatable(data,class = 'cell-border stripe',rownames = TRUE, filter = 'top', 
         options = list( pageLength = 5, autoWidth = TRUE)
         DT:::DT2BSClass(c('compact', 'cell-border'))
    

