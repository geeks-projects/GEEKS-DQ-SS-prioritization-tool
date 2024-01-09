
library(tidyverse)
library(tidyr)
percentage <- function(indicator){
  (sum(indicator, na.rm = T)/n())*100
}


district_data <- final_df |> 
  group_by(District) |> 
  summarise(across(missing_reports:neg_wastage_rates_dpt, percentage)) |> 
  
  
  
## Visualisation 

ggplot() +
  statebins:::geom_rtile(
    data = district_data,
    mapping = aes(x = week, y = weekday, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) +
  statebins:::geom_rtile(
    data = leg_data,
    mapping = aes(x = week, y = weekday, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) 