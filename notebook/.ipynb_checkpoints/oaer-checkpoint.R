pkgs = c("tidyverse", "readxl", "leaflet", "RColorBrewer",
         "plotly", "hrbrthemes", "gghighlight"
        )

inst = lapply(pkgs, suppressWarnings(library), character.only=TRUE)

path <- "../data/_meta/countries.xlsx"

# Grouping Table: HS Code
tbl_countries <- read_excel(path, skip=0, na = c("-",""))
path <- "../data/_meta/hs1006_th.xlsx"
tbl_hs1006 <- read_excel(path, sheet='hs_rice', skip=0)
tbl_hs1006_group <- read_excel(path, sheet='rice_group', skip=0)

# World Map: geojson format
world_map <- geojsonio::geojson_read("../data/map/world_lowres.geojson", what = "sp")

# Import Export Data
export_all <- readRDS("../data/export_1006.rds")

# Functions

query <- function(data, year=NULL, varities=NULL, data_type='vol') {
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
