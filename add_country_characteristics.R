#Add country metrics data 

metrics <- readRDS(paste0(output, 'test_metrics.rds'))

#Economic data
country_data <- 
  qDT(read_dta("C:/Users/matan/OneDrive/Documents/Masters/Thesis/Macro Data/GMD.dta"))
country_data <- country_data[between(year, 1990, 2024)]


country_data <- country_data[ISO3 %in% metrics$iso3]

    
metrics_full <- merge(metrics, country_data[, 
                                               .(real_gdp = rGDP, inflation =CPI,
                                                 unemployment= unemp,
                                                 population=pop, intrest =cbrate,
                                                 iso3=ISO3, year)], 
                      by = c('iso3', 'year'), all.x=T)
rm(country_data)

#market cap indicator from world bank api
market_cap<-wb_data('CM.MKT.LCAP.CD', start_date = 1990, end_date = 2024)

market_cap<-qDT(market_cap)

market_cap <- market_cap[, .(iso3=iso3c, year=date, 
                             market_cap =CM.MKT.LCAP.CD)]
metrics_full <- merge(metrics_full, market_cap, by=c('iso3', 'year'), all.x = T)
rm(market_cap)

#data from the vdem index https://v-dem.net/data/the-v-dem-dataset/ v-dem full
democracy <- qDT(read_dta("C:/Users/matan/OneDrive/Documents/Masters/Thesis/main_data/vdemocracy.dta"))[, 
                                   .(iso3=country_text_id, year,
                                     elec_dem = v2x_polyarchy,
                                     lib_dem = v2x_libdem)]

metrics_full <- merge(metrics_full,democracy, by=c('iso3', 'year'), all.x = T)
rm(democracy)

saveRDS(metrics_full, paste0(output, 'panel_w_metrics.rds'))
fwrite(metrics_full, paste0(output, 'panel_w_metrics.csv'))

rm(metrics_full, metrics)
