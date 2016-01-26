############################
# Applying Filters on Data #
############################

GAP_RP_Data <- filter(All_Dat_Complete, grepl("GAP", POLICYNO))

MPW_RP_Data <- filter(All_Dat_Complete, grepl("MPW", POLICYNO))

Risk_Prem_Core            <-  select(GAP_RP_Data, starts_with("RISKPREMIUMGAPGUARDRISK"))
Risk_Prem_Core_Cancer     <-  select(GAP_RP_Data, contains("STDCANCER"))
Risk_Prem_Core_Dentistry  <-  select(GAP_RP_Data, contains("EMBEDDEDDENTISTRY"))

yearstrtr             <-  2012
monthstrtr            <-  1

Risk_Prem_Core_Total <- as.data.frame(matrix(NA, nrow(Risk_Prem_Core), ncol(Risk_Prem_Core)))

for (i in 1:ncol(Risk_Prem_Core)){
  
  if (as.integer(monthstrtr) == 13) {
    monthstrtr  <-  1
    yearstrtr   <-  yearstrtr + 1
  }
  
  if (monthstrtr < 10){ monthstrtr <- paste(0,as.character(monthstrtr), sep = "") }
  
  V1 <- select(Risk_Prem_Core, contains(paste(yearstrtr, monthstrtr, sep = "")))
  if (ncol(V1) == 0) {V1$RiskPr <- 0}
  
  V2 <- select(Risk_Prem_Core_Cancer, contains(paste(yearstrtr, monthstrtr, sep = "")))
  if (ncol(V2) == 0) {V2$RiskPr <- 0}
  
  V3 <- select(Risk_Prem_Core_Dentistry, contains(paste(yearstrtr, monthstrtr, sep = "")))
  if (ncol(V3) == 0) {V3$RiskPr <- 0}
  
  Risk_Prem_Core_Total[i] <- V1 + V2 + V3
  colnames(Risk_Prem_Core_Total)[i] <- paste("CoreRiskPr", yearstrtr, monthstrtr, sep = "")
  monthstrtr <- as.integer(monthstrtr) + 1
  
}

# Dentistry extender
Risk_Prem_Denis_Ext      <-  select(GAP_RP_Data, contains("DENTISTRYEXTENDERTODENIS"))

# Cancer Extender
Risk_Prem_Cancer_Ext     <-  select(GAP_RP_Data, contains("CANCEREXTENDER"))
Risk_Prem_Cancer_Ext_ZL  <-  select(Risk_Prem_Cancer_Ext, contains("ZLCELLCAPTIVE"))
Risk_Prem_Cancer_Ext_GR  <-  select(Risk_Prem_Cancer_Ext, contains("GUARDRISK"))

# Totals
CoreRiskPrTots <-  colSums(Risk_Prem_Core_Total, na.rm = TRUE)


# Premium data starts from 2012-01
Year_Mon  <-  as.character(substr(seq(as.Date("2012-01-01"), by = "month", length.out = length(CoreRiskPrTots)),1,7))

PremsDat  <-  data.frame(Year_Mon = Year_Mon, Core_Risk_Pr = CoreRiskPrTots)

rm(V1, V2, V3, GAP_RP_Data, MPW_RP_Data, 
   Risk_Prem_Core, Risk_Prem_Core_Cancer, Risk_Prem_Core_Dentistry,
   yearstrtr, monthstrtr,
   i,
   Risk_Prem_Denis_Ext, Risk_Prem_Cancer_Ext, Risk_Prem_Cancer_Ext_ZL, Risk_Prem_Cancer_Ext_GR,
   CoreRiskPrTots)












