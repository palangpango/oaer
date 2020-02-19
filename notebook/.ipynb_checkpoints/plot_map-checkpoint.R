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
        setView( lat=10, lng=0 , zoom=1) %>%
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