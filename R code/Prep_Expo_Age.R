##############################
# Allocates Exposure and age #
##############################

# Set up Dataframe and initialize some core variales
Year_Exp_Age  <-  matrix(ncol = 4, nrow = (as.numeric(YearEnd) - 2012) * 12 + as.numeric(MonthEnd))
Year_Month    <-  rep(NA, nrow(Year_Exp_Age))

# Get exposure, by month
Tot_Exp <- colSums(All_Dat %>%  
                     select(starts_with("exposure_at")), na.rm = TRUE)

# Get total exposure where premiums were recieved
Tot_Exp_Rec <- colSums(All_Dat_Complete %>%  
                         select(starts_with("exposure_at")), na.rm = TRUE)

# Get average age by month
AveAge <- colMeans(All_Dat %>%  
                     select(starts_with("age_at")), na.rm = TRUE)

# Get year and month key
Year_Month  <-  as.character(substr(seq(as.Date("2012-01-01"), by = "month", length.out = nrow(Year_Exp_Age)),1,7))

# Complete  data frame
Year_Exp_Age                      <-  as.data.frame(Year_Exp_Age)
colnames(Year_Exp_Age)            <-  c("Year_Month", "Total_Exposure", "Total_Expo_Prem_Rec", "Average_Age")
Year_Exp_Age$Year_Month           <-  Year_Month
Year_Exp_Age$Total_Exposure       <-  as.numeric(as.character(Tot_Exp))
Year_Exp_Age$Total_Expo_Prem_Rec  <-  as.numeric(as.character(Tot_Exp_Rec))
Year_Exp_Age$Average_Age          <-  as.numeric(as.character(AveAge))

rm(Tot_Exp, AveAge, Year_Month, Tot_Exp_Rec)












