## Claims Structure R code

# Create Key to find unique combo of policy number and incident date
All_cl_Data$Key <- paste(All_cl_Data$EXTERNAL_POLICY_REFERENCE, All_cl_Data$Overall_Incident_Month, sep = "_")

# Add counters for Outstanding claims and paid claims
All_cl_Data$OSCounter                                    <- 0
All_cl_Data$OSCounter[All_cl_Data$CLAIM_PAYOUT_OS != 0]  <- 1
All_cl_Data$Counter                                      <- 0
All_cl_Data$Counter[All_cl_Data$CLAIM_PAYOUT != 0]       <- 1

New_AD <- data.frame()
New_AD <- All_Dat
# Add Comment
# Filter data to only work with summarised claim data per individual policy (grouping them together)
Work <- data.frame()
Work <- All_cl_Data %>% 
           group_by(Key) %>% 
           summarise(no_claims      =  sum(Counter,                  na.rm = T),
                     sum_claims     =  sum(CLAIM_PAYOUT,             na.rm = T),
                     no_os_claims   =  sum(OSCounter,                na.rm = T),
                     sum_os_claims  =  sum(CLAIM_PAYOUT_OS,          na.rm = T),
                     Incident       =  mean(Overall_Incident_Month,  na.rm = T))

Work$Key <- substr(Work$Key, 1, unlist(gregexpr("_", Work$Key)) - 1) 

# Get a vector with all the month names
Max_Inc_Month  <-  (as.numeric(YearEnd) - 2012) * 12 + as.numeric(MonthEnd) + 12
months         <-  vector()
for (i in 2012:(2012 + floor((Max_Inc_Month - 13) / 12))){
  for (j in 01:12){
    if (nchar(j) == 1){
      j <- paste0("0", as.character(j))
    }
    months <- c(months, paste0(i, ".", j))
  }
}

months <- months[1:(Max_Inc_Month - 12)] 

# Appending all the incident month in one dataframe
for (i in 13:Max_Inc_Month){
  
  # Date at current iteration
  Date <- as.Date(paste(substr(months[i - 12], 1, 4), substr(months[i - 12],6,7), "01", sep="-"))
  
  Temp_DF <- data.frame()
  Temp_DF <- Work[Work$Incident == i,]
  Temp_DF <- subset(Temp_DF, select = -Incident)
  
  colnames(Temp_DF) <- c("Key",
                         paste("no.Claims",    months[i - 12], sep="_"),
                         paste("sum.Claims",   months[i - 12], sep="_"),
                         paste("no.OsClaims",  months[i - 12], sep="_"),
                         paste("sum.OsClaims", months[i - 12], sep="_"))
  
  if (i > 13){
    Temp_DF2 <- merge(Temp_DF2, Temp_DF, by.x = "Key", by.y = "Key", all = T)
    
  }else{
    Temp_DF2 <- Temp_DF
  }
  
  # Determine if individuals are exposed in current month (iteration)
  New_AD$tempexp                                                       <-  1 
  New_AD$tempexp[New_AD$DATEEND <= Date | New_AD$COMMENCEDATE > Date]  <-  NA
  
  # Find ages
  New_AD$tempage                        <- (as.numeric(Date - New_AD$DATEBORN)) / 365.25 
  New_AD$tempage[is.na(New_AD$tempexp)] <- NA
  
  colnames(New_AD)[colnames(New_AD) == "tempage"] <- paste0("age_at",      months[i - 12])
  colnames(New_AD)[colnames(New_AD) == "tempexp"] <- paste0("exposure_at", months[i - 12])


}

New_AD  <- merge(New_AD, Temp_DF2, by.x = "POLICYNO", by.y = "Key", all = T)
All_Dat <- New_AD

rm(i, j, Max_Inc_Month, months, Date, Work, Temp_DF, Temp_DF2, New_AD)






