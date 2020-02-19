# plot_ts <- function(data, varities='hommali', data_type='vol'){
#     data <- query(data, varities = varities, data_type = data_type) %>%
#         drop_na(region) %>%
#         group_by(year, region) %>%
#         summarize(amount = sum(amount))
    
#     g <- data %>%
#         ggplot( aes(x=year, y=amount, color = region), size=4) +
#         geom_line(size=1) +
#         theme_ipsum()
#     return(ggplotly(g))
# }

# plot_tstop5 <- function(data, year=NULL, varities='hommali', data_type='vol'){
    
#     year_ <- ifelse(is.null(year), max(data$year), year)
#     varities_ <- varities
    
#     top5 <- data %>%
#         filter(year==year_ & varities==varities_) %>%
#         group_by(iso3) %>%
#         summarize(amount = sum(amount)) %>%
#         top_n(5, amount)
    
#     data <- query(data, varities = varities, data_type = data_type) %>%
#         drop_na(region) %>%
#         filter(iso3 %in% top5$iso3) %>%
#         group_by(year, iso3) %>%
#         summarize(amount = sum(amount))
    
#     plt <- data %>%
#         ggplot( aes(x=year, y=amount, color = iso3), size=4 ) +
#         geom_line(size=1) +
#         theme_ipsum()
    
#     return(ggplotly(plt))
# #     return(ggplotly(g))
# } 

library(streamgraph)

plot_tstop5 <- function(data, year=NULL, varities='hommali', data_type='vol'){
    
    year_ <- ifelse(is.null(year), max(data$year), year)
    varities_ <- varities
    
    top5 <- data %>%
        filter(year==year_ & varities==varities_) %>%
        group_by(iso3) %>%
        summarize(amount = sum(amount)) %>%
        top_n(5, amount)
    
    data <- query(data, varities = varities, data_type = data_type) %>%
        filter(iso3 %in% top5$iso3) %>%
        group_by(year, iso3) %>%
        summarize(amount = sum(amount))

     plt <- data %>%
        streamgraph(data, key="iso3", value="amount", date="year" , 
        offset="zero",interactive=TRUE , width="600px", height="300px")
   
    return(plt) 
} 
