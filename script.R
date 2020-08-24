# libraries
library(tidyverse)
library(tabulizer)
library(janitor)
library(plotly)

# load data
bbva_tables <- extract_tables('BBVA.pdf',pages = 12)

# clean data
bbva_paidsearch <- bbva_tables[[1]] %>% 
  data.frame() %>% 
  set_names(paste(bbva_tables[[1]][1,], bbva_tables[[1]][2,],sep = ' ')) %>% 
  slice(3:7) %>%
  clean_names() %>% 
  mutate(site_media_spend = str_extract_all(str_remove(site_media_spend, ','), '\\d{1,}')) %>% 
  mutate_all(~ as.numeric(str_remove_all(., ',|\\$'))) %>% 
  mutate(applications_approved = round(.8* applications_completed),
         client_year_or_more = round(.55*applications_approved)) %>% 
  summarise_all(sum) %>% 
  select(impressions:client_year_or_more, -cost_per_application)

# plot data

fig <- plot_ly() 
fig <- fig %>%
  add_trace(
    type = "funnel",
    y = names(bbva_paidsearch),
    x = as.numeric(as.vector(bbva_paidsearch[1,])))

fig <- fig %>%
  layout(yaxis = list(categoryarray = names(bbva_paidsearch)))

fig


