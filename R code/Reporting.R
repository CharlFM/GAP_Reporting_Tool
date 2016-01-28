##################
# Reporting Data #
##################

Data_Out <- merge(merge(Data_Out, Ave_Claim_Sev, by.x = "IncYear_Month", by.y = "IncYear_Month", all = TRUE), 
                  Year_Exp_Age, by.x = "IncYear_Month", by.y = "Year_Month", all = TRUE)

Data_Out$IncYear <- as.numeric(substr(Data_Out$IncYear_Month, 1, 4))
Data_Out$IncMont <- as.numeric(substr(Data_Out$IncYear_Month, 6, 7))
Data_Out <- Data_Out[with(Data_Out, order(IncYear,IncMont)), ]

Data_Out <- Data_Out[ , -which(names(Data_Out) %in% c("IncYear", "IncMont"))]

Data_Out$Risk_P_per_life           <-  Data_Out$Core_Risk_Pr  /  (Data_Out$Total_Expo_Prem_Rec)
Data_Out$Ult[is.na(Data_Out$Ult)]  <-  0
Data_Out$Profit                    <-  Data_Out$Core_Risk_Pr  -  Data_Out$Ult
Data_Out$True_Risk_P               <-  Data_Out$Ult           /  (Data_Out$Total_Exposure)
Data_Out$True_Risk_P_Rec           <-  Data_Out$Ult           /  (Data_Out$Total_Expo_Prem_Rec)
Data_Out$Claim_Count               <-  round(Data_Out$Ult     / Data_Out$Ave_Sev)
Data_Out$Year                      <-  substr(Data_Out$IncYear_Month, 1, 4)

Report_Data <- Data_Out %>%
  group_by(Year) %>%
  summarise(Total_Exposure               = sum(Total_Exposure, na.rm = TRUE)  / 12, 
            Average_Age                  = mean(Average_Age,   na.rm = TRUE), 
            Risk_Prem_Per_Life           = sum(Core_Risk_Pr,   na.rm = TRUE)  / (12 * sum(Total_Exposure,       na.rm = TRUE)),
            Total_Risk_Premiums          = sum(Core_Risk_Pr,   na.rm = TRUE),
            Total_Claims_Paid            = sum(claimtot,       na.rm = TRUE),
            Total_Outstanding_Claims     = sum(OS_Claim,       na.rm = TRUE),
            IBNR                         = sum(IBNR,           na.rm = TRUE),
            Profit                       = sum(Profit,         na.rm = TRUE),
            Loss_Ratio                   = (100 * sum(Ult,     na.rm = TRUE)) / sum(Core_Risk_Pr,               na.rm = TRUE),
            True_Risk_Premium            = sum(Ult,            na.rm = TRUE)  / (12 * sum(Total_Exposure,       na.rm = TRUE)),
            True_Risk_Premium_PR         = sum(Ult,            na.rm = TRUE)  / (sum(Total_Expo_Prem_Rec,       na.rm = TRUE)))

Report <- as.data.frame(t(Report_Data))
colnames(Report) <- Report_Data$Year











