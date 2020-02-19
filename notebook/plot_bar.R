library(hrbrthemes)
library(plotly)
# source("query.R")

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

plot_bar <- function(data, year=2018 ,varities='hommali', data_type='vol'){
    year_ <- ifelse(is.null(year), max(data$year), year)
    varities_ <- varities
    

    data <- query(data, year = year, varities = varities) %>%
        drop_na(region) %>%
        filter(year==year_ & varities==varities_) %>%
        group_by(iso3, country_name_en, region) %>%
        summarize(amount = sum(amount)) %>%
        ungroup() %>%
        top_n(20, amount)

    plot <- data %>%
        ggplot( aes(x=reorder(iso3, -amount), y=amount, fill=region, country=country_name_en), size=4 ) +
        geom_bar(stat = "identity") +
        theme_ipsum()+
        theme(axis.text.x = element_text(angle = 90),
              plot.margin = margin(0, 0, 0, 0, "cm"),
              panel.background = element_rect(fill = "white")
             )
    return(ggplotly(plot, tooltip = c("y", "country")))
}
    
