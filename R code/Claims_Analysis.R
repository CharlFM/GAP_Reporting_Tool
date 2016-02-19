By_ICD10 <- All_cl_Data %>% 
  group_by(SERVICE_CODE) %>% 
  summarise(n         =  n(), 
            Max_Severity_ZL    =  max(SERV_PAID, na.rm = TRUE),
            Ave_Severity_ZL    =  mean(SERV_PAID, na.rm = TRUE),
            Min_Severity_ZL    =  min(SERV_PAID, na.rm = TRUE),
            Tot_Paid_ZL        =  sum(SERV_PAID, na.rm = TRUE),
            Volatility_ZL      =  var(SERV_PAID, na.rm = TRUE),
            Max_Severity_Prov  =  max(SERV_PROVIDER_CHARGE, na.rm = TRUE),
            Ave_Severity_Prov  =  mean(SERV_PROVIDER_CHARGE, na.rm = TRUE),
            Min_Severity_Prov  =  min(SERV_PROVIDER_CHARGE, na.rm = TRUE),
            Tot_Paid_Prov      =  sum(SERV_PROVIDER_CHARGE, na.rm = TRUE),
            Volatility_Prov    =  var(SERV_PROVIDER_CHARGE, na.rm = TRUE))

descr <- subset(All_cl_Data, select = c(SERVICE_CODE, SERVICE_DESCRIPTION))
descr <- subset(descr, !duplicated(SERVICE_DESCRIPTION))

By_ICD10 <- merge(By_ICD10, 
                  descr, 
                  by = "SERVICE_CODE")

By_ICD10 <- By_ICD10[By_ICD10$n > 10,]

By_ICD10  <-  By_ICD10[with(By_ICD10, order(Volatility_Prov)), ]
ncount <- ceiling(nrow(By_ICD10) * 0.1)

Top <- By_ICD10$SERVICE_CODE[((nrow(By_ICD10) - ncount) : nrow(By_ICD10))              ]
Top
Choice <- 501 #Top[length(Top)-9] 

par(mfrow=c(1,2))
hist(All_cl_Data$SERV_PAID[All_cl_Data$SERVICE_CODE == Choice], breaks = 20, main = "ZL Paid", xlab = Choice)
hist(All_cl_Data$SERV_PROVIDER_CHARGE[All_cl_Data$SERVICE_CODE == Choice], breaks = 20, main = "Service Charge", xlab = Choice)

By_ICD10$Gr5 <- ifelse(By_ICD10$n >= 5, 1, 0) 
sum(By_ICD10$Gr5)

By_ICD10$Gr10 <- ifelse(By_ICD10$n >= 10, 1, 0)
sum(By_ICD10$Gr10)

By_ICD10$Gr50 <- ifelse(By_ICD10$n >= 50, 1, 0)
sum(By_ICD10$Gr50)

DataCheck <- All_cl_Data[All_cl_Data$SERVICE_CODE == 501,]


