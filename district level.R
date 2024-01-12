
library(tidyverse)

percentage <- function(indicator){
  (sum(indicator, na.rm = T)/n())*100
}


district_data <- final_df |> 
  group_by(Region) |> 
  summarise(across(missing_reports:neg_wastage_rates_dpt, percentage)) |> 
  pivot_longer(cols = missing_reports:neg_wastage_rates_dpt,  names_to = "data_quality_issue")
  
  
  
## Visualisation 
district_data |> 
ggplot( mapping = aes(y = Region, x = data_quality_issue, fill = value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed()
