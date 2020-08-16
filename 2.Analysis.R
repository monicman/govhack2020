library('tidyverse')
library('leaflet')
library('scales')


final <- read_csv('./output_data/combined_data.csv') %>% 
         mutate(classes = case_when(bus_stops_nearby > 4 | park_nearby > 0 | tourist_nearby > 0 ~ 2,
                                    bus_stops_nearby > 2 | park_nearby > 0 | tourist_nearby > 0 ~ 1,
                                    TRUE~0),
                treatment = case_when(classes == 0 ~ "Dim lights",
                                      classes == 1 ~ "Replace with LED + metered",
                                      classes == 2 ~ "Dim lights"),
                fixed_cost = case_when(treatment=="Replace with Solar" & wattage > 100 ~ -8100,
                                            treatment=="Replace with LED + metered" & wattage > 100 ~ -700,
                                            TRUE~0),
                variable_benefit = case_when(treatment=="Replace with Solar" & wattage > 100 ~ 0.4*(wattage/1000)*3650,
                                             treatment=="Replace with LED + metered" & wattage > 100 ~ ((3650-3090)*0.4*(wattage/1000) + (wattage-18+5)/1000*0.4),
                                             treatment == 'Dim lights' ~ 0.04*((wattage+0.00001)/1000)*3650,
                                             TRUE~0)) %>% 
                group_by(treatment) %>% 
                summarise(sum_fixed_cost = sum(fixed_cost),
                          sum_variable_benefit = sum(variable_benefit,na.rm=TRUE)) %>% 
                mutate(newcol = 1)



simulation <- as.data.frame(seq(from=2020,to=2080))%>% rename('Year' = 'seq(from = 2020, to = 2080)') %>% 
              mutate(newcol = 1) %>% left_join(final) %>% arrange(treatment,Year) %>% 
              mutate(year_total = ifelse(Year == 2020,sum_fixed_cost + sum_variable_benefit,sum_variable_benefit)) %>% 
              group_by(treatment) %>% mutate(cumulative=cumsum(year_total))

total_budget_impact <- simulation %>% group_by(Year) %>% summarise(total_cost = sum(year_total)) %>% 
  mutate(cumulative=cumsum(total_cost))


ggplot(total_budget_impact,aes(y=cumulative,x=Year))+ geom_line() 
  scale_y_continuous(name="Running budget total", labels = comma) + 
  ggtitle('Break even analysis by time,') + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", size=1.5)




ggplot(simulation,aes(y=cumulative,x=Year))+ geom_line() +  facet_wrap(vars(treatment), scales = "free") + 
  scale_y_continuous(name="Running budget total", labels = comma) + 
  ggtitle('Break even analysis by time and policy') + 
   theme_bw() + 
   geom_hline(yintercept = 0, color = "red", size=1.5)



  leaflet(final) %>% addTiles() %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, color = ~pal(classes),fillOpacity = 1
  )
  
