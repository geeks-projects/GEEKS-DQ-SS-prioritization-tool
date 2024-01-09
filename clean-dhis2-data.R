
data <- read.csv("./data/dhis2-data2.csv")

library(dplyr)

data_clean  <- data |> rowwise() |> 
  mutate(missing_reports = Expected.reports - Actual.reports, 
         missing_reports = if_else(missing_reports == 0, FALSE, TRUE),
         neg_dropout_rate_dpt1_3 = if_else(dropout.rate >= 0, FALSE, TRUE),
         discrepancy_dpt3_pcv3 = DPT3 - PCV.3, 
         discrepancy_dpt3_pcv3 = if_else(discrepancy_dpt3_pcv3 == 0, FALSE, TRUE),
         neg_wastage_rates_dpt = if_else(wastage.rate >= 0, FALSE, TRUE),
         .keep = "unused") |> 
  filter( !(is.na(missing_reports)& is.na(neg_dropout_rate_dpt1_3)& is.na(discrepancy_dpt3_pcv3) & is.na(neg_wastage_rates_dpt)))



## 
final_df <-data_clean  |>  
  mutate(priorty_number = sum(missing_reports, neg_dropout_rate_dpt1_3, 
                            discrepancy_dpt3_pcv3, neg_wastage_rates_dpt, na.rm = T)) |> 
  arrange(desc(priorty_number))

##
#write.csv(final_df, "processed_data.csv", row.names = F)

# library(reactable)
# 
# 
# 
# reactable(final_df,  bordered = TRUE, highlight = TRUE,
#           paginationType = "jump", defaultPageSize = 30,
#           
#           defaultColDef = colDef(
#             cell = function(value) format(value, nsmall = 1),
#             align = "center",
#             minWidth = 70,
#             headerStyle = list(background = "#f9f9f9")
#           ),
#           columns = list(
#          HF = colDef(name = "Health Facility"),
#          missing_reports = colDef(name = "Missing reports"),
#          neg_dropout_rate_dpt1_3 = colDef(name = "Negative dropout rate DPT 1-3"),
#          discrepancy_dpt3_pcv3 = colDef(name = "Discrepancy DPT3 - PCV3"),
#          neg_wastage_rates_dpt = colDef(name = "Negative wastage rate DPT"),
#          priorty_number = colDef(name = "Priorty number")
#         # Species = colDef(align = "center")
#          ),
#           
#           rowStyle = function(index) {
#             if (final_df[index, "priorty_number"]  > 2) {
#               list(background = "#cf3045")
#             }else if(final_df[index, "priorty_number"]  == 2) {
#               list(background = "#e6ae22")
#             }else{
#               list(background = "#11ad5d")
#             }
#           }
#                     
# )


#####



