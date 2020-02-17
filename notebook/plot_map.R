world_map <- geojsonio::geojson_read("../data/map/world_lowres.geojson", what = "sp")

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
        group_by(year, varities, iso3, region) %>%
        filter(year %in% year_list & varities %in% varities_list) %>%
        mutate(amount = amount/1e6) %>%
        summarize(amount = sum(amount))
    return(res)
}

plot_map <- function(data, map=world_map, year=2018, varities='hommali', data_type='vol') {
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