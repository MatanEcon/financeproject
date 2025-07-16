#Create rankings and graphs
count_index <-readRDS( paste0(output, 'panel_w_metrics.rds'))

#How we measure efficiency, distance to 1
count_index[, efficiency := 1-(cj_ratio-1)^2 ]

count_index[, gdp_pc := real_gdp/population]
count_index[, lgdp_pc := log(real_gdp/population)]
count_index[, inflation:= inflation/100]
count_index[, unemployment:= unemployment/100]
count_index[, intrest:= intrest/100]
count_index[, lmarket_cap:= log(market_cap)]


efficiency_model <- feols(data = count_index,
                          efficiency~lgdp_pc+inflation+unemployment+intrest+
                            lmarket_cap+elec_dem)
efficiency_model_cfe <- feols(data = count_index,
                          efficiency~lgdp_pc+inflation+unemployment+intrest+
                            lmarket_cap+elec_dem |iso3)
efficiency_model_yfe <- feols(data = count_index,
                              efficiency~lgdp_pc+inflation+unemployment+intrest+
                                lmarket_cap+elec_dem| year)
efficiency_model_fe<- feols(data = count_index,
                              efficiency~lgdp_pc+inflation+unemployment+intrest+
                                lmarket_cap+elec_dem| iso3 + year)
etable(efficiency_model, efficiency_model_cfe, efficiency_model_yfe, 
       efficiency_model_fe)



ggplot(data=count_index[efficiency<0.25], aes(x=efficiency, y=log(market_cap))) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
ggplot(data=count_index, aes(x=rank_closeness, y=log(pop))) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
ggplot(data=count_index, aes(x=rank_closeness, y=intrest)) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
ggplot(data=count_index, aes(x=rank_closeness, y=unemp, color=period)) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
ggplot(data=count_index, aes(x=rank_closeness, y=log(gdp_pc))) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()



ggplot(data=count_index, aes(x=rank_closeness, y=log(inflation))) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()+facet_wrap(~period)
ggplot(data=count_index, aes(x=rank_closeness, y=log(pop))) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
ggplot(data=count_index, aes(x=rank_closeness, y=intrest)) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
ggplot(data=count_index, aes(x=rank_closeness, y=unemp)) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
ggplot(data=count_index, aes(x=rank_closeness, y=log(rGDP))) +geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()
