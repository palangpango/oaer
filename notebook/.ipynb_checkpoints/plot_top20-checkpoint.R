plot_top20 <- function(data, year=NULL, varities='hommali', data_type='vol'){
    
    year_ <- ifelse(is.null(year), max(data$year), year)
    varities_ <- varities
    
    top20<- data %>%
        filter(year==year_ & varities==varities_) %>%
        group_by(iso3) %>%
        summarize(amount = sum(amount)) %>%
        top_n(20, amount)
    
    data <- query(data, varities = varities, data_type = data_type) %>%
        drop_na(region) %>% 
        filter(iso3 %in% top20$iso3) %>%
        group_by(year, iso3, region,) %>%
        summarize(amount = sum(amount)) 
    
    plt <- data %>%
        ggplot( aes(x=year, y=amount, color = region), size=4 ) +
        geom_line(size=1) +
        theme_bw() +
        facet_wrap(~reorder(iso3,-amount),scales = "fixed",ncol=4,nrow=5) +
        theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.2, vjust = 0.2),
              axis.text.y = element_text(colour = "grey20", size = 8),
              text = element_text(size = 10))
        expand_limits(y=0)
       
    return((ggplotly(plt)))}