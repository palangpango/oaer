plot_top20 <- function(data, year=NULL, varities='hommali', data_type='vol'){
    
    year_ <- ifelse(is.null(year), max(data$year), year)
    varities_ <- varities
    
    top20 <- data %>%
        filter(year==year_ & varities==varities_) %>%
        group_by(iso3) %>%
        summarize(amount = sum(amount)) %>%
        top_n(20, amount)
    
    data <- query(data, varities = varities, data_type = data_type) %>%
        drop_na(region) %>%
        filter(iso3 %in% top20$iso3) %>%
        group_by(year, iso3,region) %>%
        summarize(amount = sum(amount)) 
    
    plt <- data %>%
        ggplot( aes(x=year, y=amount, color = region), size=4 ) +
        geom_line(size=1) +
        theme_ipsum() +
        facet_wrap(~iso3) #+
#         theme(panel.spacing.x=unit(6, "mm")) 
    
    return(ggplotly(plt))}