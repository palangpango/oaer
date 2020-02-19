library(tidyverse)
library(readxl)

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

saveRDS(export_1006_joined, file = "../data/export_1006.rds")