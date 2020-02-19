library(tidyverse)
library(readxl)
library(DT)
options(DT.options = list(scrollY="50vh"))
# DT:::DT2BSClass(c('compact', 'cell-border'))
create_table <- function(data, varities='hommali'){    
    varities_ <- varities
    data <- data %>%
        filter(varities==varities_) %>%
        group_by(year, country_name_short, type) %>%
        summarize(amount = sum(amount)) %>%
        spread(type, amount) %>%
        mutate(vol = vol/1e3) %>%
        mutate(val = val/1e6) %>%
        arrange(-year, -vol) %>%
        select(year, country_name_short, vol, val)
#     dt <- datatable(data, class = 'cell-border stripe', rownames = TRUE, filter = 'top',     
    dt <- datatable(data, class = 'compact', rownames = TRUE, filter = 'top', 
                    options = list(pageLength = 10, autoWidth = TRUE,
                                   dom = 'Blfrtip',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All")
                                                    )
                                  ),
                    colnames = c('Year', 'Country', 'Volume (Tonnes)', 'Value (Million Bahts)'),
                    extensions = 'Buttons',
                    ) %>%
        formatCurrency(c('val', 'vol'), currency = "", digit=0)
    
    return(dt)
}