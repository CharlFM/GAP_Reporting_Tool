#############################################################################################
# Smoothing of Average Severity and appending additional claims imposed by IBNR calculation #
#############################################################################################

All_cl_Data$CLAIM_PAYOUT_OS[All_cl_Data$CLAIM_PAYOUT_OS == 0]   <-   NA
All_cl_Data$CLAIM_PAYOUT_OK[All_cl_Data$CLAIM_PAYOUT_OK == 0]   <-   NA

Ave_Claim_Sev <- All_cl_Data    %>% 
  group_by(IncYear_Month)     %>%
  summarise(Ave_Sev      =  mean(CLAIM_PAYOUT_OK, na.rm = TRUE),
            Claim_Count  =  n())

IncidentYearMonth <- Ave_Claim_Sev$IncYear_Month

Claim_Count    <-  Ave_Claim_Sev$Claim_Count
Ave_Claim_Sev  <-  Ave_Claim_Sev$Ave_Sev

counter   <-  0
count2    <-  0
temp_ave  <-  0

for(i in 1:length(Ave_Claim_Sev)){
  
  if (Ave_Claim_Sev[i] > 0) {
    
    counter            <-  counter + 1
    temp_ave[counter]  <-  Ave_Claim_Sev[i]
    
    if (counter > 6) {
      
      count2   <-  count2 + 1
      meansev  <-  mean(temp_ave[-length(temp_ave)], na.rm = TRUE)
      stdsev   <-  var(temp_ave[-length(temp_ave)], na.rm = TRUE)
      temp_ave[count2] <- NA
      
      lbound <- meansev - 1.96 * sqrt(stdsev)
      ubound <- meansev + 1.96 * sqrt(stdsev)
      
      if ( Ave_Claim_Sev[i] > ubound ) {
        Ave_Claim_Sev[i]  <-  ubound
      } else if (Ave_Claim_Sev[i] < lbound ) {
        Ave_Claim_Sev[i]  <-  lbound
      }
      
    }
    
  }
  
}

Ave_Claim_Sev <- data.frame(Year_Mon        =  IncidentYearMonth, 
                            Ave_Claim_Sev   =  Ave_Claim_Sev, 
                            Claim_Count     =  Claim_Count)
















