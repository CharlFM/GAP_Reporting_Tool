#############################################################################################
# Smoothing of Average Severity and appending additional claims imposed by IBNR calculation #
#############################################################################################

All_cl_Data$CLAIM_PAYOUT_OS[All_cl_Data$CLAIM_PAYOUT_OS == 0]   <-   NA
All_cl_Data$CLAIM_PAYOUT_OK[All_cl_Data$CLAIM_PAYOUT_OK == 0]   <-   NA

Ave_Claim_Sev <- All_cl_Data  %>% 
  group_by(IncYear_Month)     %>%
  summarise(Ave_Sev      =  mean(CLAIM_PAYOUT_OK, na.rm = TRUE),
            Claim_Count  =  n())

tot <- seq(1,(as.numeric(YearEnd) - 2012) * 12 + as.numeric(MonthEnd))
loSev <- loess(Ave_Claim_Sev$Ave_Sev ~ tot)

Ave_Claim_Sev$Ave_Sev <- loSev$fitted

rm(tot, loSev)















