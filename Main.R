#########################################################################################################
############################################# Claims Project ############################################
#########################################################################################################

# Overview of tool
# Build Claims Experience Report for GAP.

# Report layout:

#                               2012        2013        2014        2015        2016 (update each quarter)
# (1)  Total Exposure           
# (2)  Average Age               
# (3)  Risk Premiums per life   
#
# (4)  Total Risk Premiums
# (5)   Total Claims Paid 
#     (On an Incurred basis)
# (6)  Total Outstanding Claims
# (7)  IBNR

# (8)  Profit
# (9)  Loss Ratio

# (10) True Risk Premium       

# Do report for each of the following:
# Split by benefit type for:
#     Individuals
#         Lib vs Non-lib
#     Groups
#         Voluntary vs Compulsory

# Benefit Type : Total | GAP | Embeded Cancer | Casualty | MPW | Embedded Dentistry | Cancer Extender | Dentistry Extender

# Claims From :          GR         KEY            KEY     US            US                 US                 US

#########################################################################################################
# Input Variables #
###################

# Notes - 1) Add input files to folders.
#         2) Make sure file names are consistent with current file names
#         3) Make sure that the input files have only 1 sheet each
#         4) With the Claims data from GR, first double click on the pivot table summary to access the data in the background
#            be sure to double click on the Total value of claims paid (this will access all data). Then copy the data into a
#            new sheet (copy paste values) and then remove all other sheets except the new sheet.

# Clears Memory
rm(list = ls())
gc()

DateEnd <- as.Date("2015/12/31")

##################
# Select Filters #
##################

Product <- "GAP" # Choices : "" (blank) <- All products i.e. NO FILTER
#                            "MPW"      <- MPW products ONLY
#                            "GAP"      <- GAP products ONLY 

MainAff <- c("LIBERTYHEALTH", "DIRECTAXISSAPTYLTD", "BOOKINNRESERVATIONSERVICESPTYLTDTALOGICALL", "ZESTLIFEINVESTMENTSPTYLTD",
              "ZWING", "LEADSOURCE", "GUARDRISKINSURANCECOMPANYLTD", "HIPPOINSURANCE", "THINKMONEY", "ZESTWEB",
              "VANBREDA", "GEMSNAB", "MEDICALERT")
Affinity <- MainAff[5]  # Choices : MainAff[1], MainAff[2] or MainAff[3]... (add others that the user would want in MainAff) 
#                                         "" (blank) <- NO FILTER
# NB -> to select multiple affinities : replace the choice with c(MainAff[1], MainAff[2], MainAff[4]...)
Everything_Else <- "NO" # Choices : "YES", "NO
# To select everything except the selected affinity (i.e. if you want everything except LIB (i.e NON LIB), switch Everything_Else to Yes)
Indiv_Group <- "IND" # Choices : "" (blank) <- NO FILTER
#                                IND        <- Individual ONLY
#                                GRP        <- Group ONLY
Vol_Comp <- "" # Choices : "" (blank) <- NO FILTER
#                          "V"        <- Voluntary ONLY
#                          "C"        <- Compulsory ONLY
Gender <- "" # Choices : "", "M", "F"

Rejoiners <- 0 # Choices : 0 -> NO FILTER or 1, 2, 3, 4...
# For multiple choices -> c(1, 2, 3...)

Status <- "" # Choices : "ACT" "CAN" "LAP" "NTU" "DEC" "PRE" (use c(...) to combine multiple statusses)

Age_Classes <- c(0, 60, 999) # Add more classes if needed - i.e change to c(0, 30, 60, 999), ignore if no filter needed - do not set to ""
Age_Fliter  <- ""      # Choice depends on number of classes. Set to "" for NO FILTER 

#########################################################################################################
# Set-up #
##########

# Get Project wd
Path <- getwd()
source(paste(Path, "/R code/Initialize.R", sep = ""))

#########################################################################################################
# (5 - 7) Claims  #
###################

# Call all data prep file
source(paste(Path, "/R code/Prep_All_Dat.R", sep = "")) 

# Combine Claim files and cleans data
source(paste(Path, "/R code/Prep_Claim_Data.R", sep = ""))

# Combine Claim and All Data
source(paste(Path, "/R code/Append_All_Dat_w_Claims.R", sep = ""))

#########################################################################################################     
# Calculate IBNR #
##################

source(paste(Path, "/R code/IBNR_Calc.R", sep = ""))

#########################################################################################################
# (1 + 2) Get Total Exposure and average age #
##############################################

source(paste(Path, "/R code/Claims_Structure.R", sep = ""))

#########################################################################################################
# Get Risk Premiums #
#####################

source(paste(Path, "/R code/Prep_Risk_Prem_Data.R", sep = ""))

#########################################################################################################
# Merge Risk Premiums with All Data #
#####################################

All_Dat_Complete <- merge(All_Dat, Premium_Data_Rp, by.x = "POLICYNO", by.y = "POLICYNUMBER")

missed <- merge(Premium_Data_Rp, All_Dat, by.x = "POLICYNUMBER", by.y = "POLICYNO", all.x = TRUE)
missed <- missed[is.na(missed$ID), ]

#########################################################################################################
# Reshuffel Exposure and age #
##############################

source(paste(Path, "/R code/Prep_Expo_Age.R", sep = ""))

#########################################################################################################
# Selection and Calculation of Risk Premium Categories #
########################################################

source(paste(Path, "/R code/Data_Filter.R", sep = ""))

#########################################################################################################
# Fill Ultimate and get loss ratio #
####################################

Data_Out     <-  merge(Claims_Dat, PremsDat, by.x = "IncYear_Month", by.y = "Year_Mon", all = TRUE)
Data_Out$LR  <-  (100 * Data_Out$Ult) / Data_Out$Core_Risk_Pr

Data_Out$IncYear <- as.numeric(substr(Data_Out$IncYear_Month, 1, 4))
Data_Out$IncMont <- as.numeric(substr(Data_Out$IncYear_Month, 6, 7))
Data_Out <- Data_Out[with(Data_Out, order(IncYear, IncMont)), ]

Data_Out <- Data_Out[ , -which(names(Data_Out) %in% c("IncYear", "IncMont"))]

#########################################################################################################
# Get average Claim size per month #
####################################

source(paste(Path, "/R code/Smoothing_Average_Severity.R", sep = ""))

#########################################################################################################
# Get Reporting Data #
######################

source(paste(Path, "/R code/Reporting.R", sep = ""))

#########################################################################################################
# Premium Projection #
######################

source(paste(Path, "/R code/Forecasting.R", sep = ""))

#########################################################################################################
# Saving The Reports #
######################

source(paste(Path, "/R code/Generating_Reports.R", sep = ""))

Year_Exp_Age$Ratio <- Year_Exp_Age$Total_Expo_Prem_Rec / Year_Exp_Age$Total_Exposure
Year_Exp_Age
mean(Year_Exp_Age$Ratio)

# Checks

print("Only head printed")
head(Missed_Claim_Dat[, 1:3], 2)       #  Claims not matching with policy Info
head(missed[, 1:3], 2)                 #  Premium Data that did not merge with All_Data

levels(as.factor(substr(missed$POLICYNUMBER, 1, 3))) # If levels are only MPW, then OK (or only GAP with MPW run)

# If needed - 
source(paste(Path, "/R code/Frequency_To_Claim.R", sep = ""))
ggplot(Combo, aes(Var1, Freq)) + geom_point()
as.numeric(Combo$Var1[Combo$Freq == max(Combo$Freq)])
