
data <- read.csv("./data/dhis2-data.csv")

library(dplyr)

data_clean  <- data |> rowwise() |> 
  mutate(missing_reports = Expected.reports - Actual.reports, 
         missing_reports = if_else(missing_reports == 0, FALSE, TRUE),
         neg_dropout_rate_dpt1_3 = if_else(dropout.rate >= 0, FALSE, TRUE),
         discrepancy_dpt3_pcv3 = DPT3 - PCV.3, 
         discrepancy_dpt3_pcv3 = if_else(discrepancy_dpt3_pcv3 == 0, FALSE, TRUE),
         neg_wastage_rates_dpt = if_else(wastage.rate >= 0, FALSE, TRUE),
         .keep = "unused")


## getting NAs 

data_clean [apply(is.na(data_clean), 1, any),]

## 
final_df <-data_clean  |>  
  mutate(priorty_name = sum(missing_reports, neg_dropout_rate_dpt1_3, 
                            discrepancy_dpt3_pcv3, neg_wastage_rates_dpt, na.rm = T))

##
write.csv(final_df, "processed_data.csv", row.names = F)

