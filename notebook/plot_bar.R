library(hrbrthemes)
library(plotly)
plot_bar <- function(data, year=2018 ,varities='hommali', data_type='vol'){
    year_ <- ifelse(is.null(year), max(data$year), year)
    varities_ <- varities
    

    data <- query(data,year = year, varities = varities) %>%
        drop_na(region) %>%
        filter(year==year_ & varities==varities_) %>%
        group_by(iso3, region) %>%
        summarize(amount = sum(amount)) %>%
        ungroup() %>%
        top_n(20, amount) 
    
    plot <- data %>%
        ggplot( aes(x = reorder(iso3, -amount), y=amount, fill = region), size=4 ) +
        geom_bar(stat = "identity") +
        ggtitle(paste(varities,"export volume in", year)) +
        theme_ipsum() 
    return(ggplotly(plot)) }