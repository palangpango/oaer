query <- function(data, year=NULL, varities='hommali', data_type='vol') {
    # input = a data frame rice export from thailand 
    # output = a data frame by by hscode and countries and year-month
    year_ <- if(is.null(year)) {
        data$year
    } else {
       year 
    }
    var <- varities
    res <- data %>%
        filter(type == data_type) %>%
        group_by(year, varities, iso3, country_name_en, region) %>%
        filter(year %in% year_ & varities == var) %>%
        mutate(amount = amount/1e6) %>%
        summarize(amount = sum(amount))
    return(res)
}