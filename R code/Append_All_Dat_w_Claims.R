# Append All Data and claims data
matched_dat <- merge(All_cl_Data, All_Dat, by.x = "EXTERNAL_POLICY_REFERENCE", by.y = "POLICYNO", all.x = TRUE)

# Get Missed Data ONLY
missed_dat   <-  matched_dat[is.na(matched_dat$AFFGRPNAME), ]

# Exclude duplicate column
matched_dat <- subset(matched_dat, select = -ID) 

# Get Matched Data ONLY
matched_dat  <-  matched_dat[!is.na(matched_dat$AFFGRPNAME), ]

# Select the original columns for the missed data
missed_dat <- missed_dat %>% select(CLAIM_IDENTIFIER, POLICY_HOLDER_ID, EXTERNAL_POLICY_REFERENCE,
                                    POLICY_HOLDER_NAME, INCIDENT_CAUSE_CODE, INCIDENT_CAUSE_DESCRIPTION,
                                    LOCATION_CODE, LOCATION_NAME, PRODUCT_NAME, MEDICAL_SCHEME, 
                                    CLAIM_INCIDENT_DATE, CLAIM_DATE_OF_PAYMENT, CLAIM_PAYOUT,
                                    POLICY_HOLDER_DOB, PATIENT_DOB)

# Next match the missed data by ID
testr <- merge(missed_dat, All_Dat, by.x = "POLICY_HOLDER_ID", by.y = "ID", all.x = TRUE)

# Initialize duplicate ID column
testr$dup  <-  0

# Indicates whether the row is a duplicate
testr$dup[duplicated(testr$CLAIM_IDENTIFIER) | duplicated(testr$CLAIM_IDENTIFIER, fromLast = TRUE)] <- 1

# Find the correct duplicate
testr$timeDiff  <-  testr$CLAIM_INCIDENT_DATE - testr$COMMENCEDATE
testr           <-  testr[testr$dup == 0 | (testr$dup == 1 & testr$timeDiff >= 0),]

# removes incorrect duplicates
temptestr  <-  testr[testr$dup == 1, ]
temptestr  <-  temptestr[with(temptestr, order(timeDiff)), ]
temptestr  <-  temptestr[!duplicated(temptestr$CLAIM_IDENTIFIER), ]
temptestr  <-  temptestr[!is.na(temptestr$AFFGRPNAME), ]
testr      <-  testr[testr$dup == 0, ]

# Get the data now to add back to the matched data
Add_Back                            <-   rbind(testr, temptestr)
Add_Back$EXTERNAL_POLICY_REFERENCE  <-   Add_Back$POLICYNO
Add_Back                            <-   subset(Add_Back, select = -c(dup, timeDiff, POLICYNO)) # Exclude duplicate and temp columns

# Missed Claim Data - To check
Missed_Claim_Dat <- Add_Back[is.na(Add_Back$AFFGRPNAME), ]
  
# Removes all claims that is not in filtered data
Add_Back <- Add_Back[!is.na(Add_Back$AFFGRPNAME), ]

# Add the additional "Add_Back" data back to the matched data
All_cl_Data <- rbind(matched_dat, Add_Back)

# Selects the claims data that maches the policies in the all data only.
All_cl_Data <- All_cl_Data[All_cl_Data$EXTERNAL_POLICY_REFERENCE %in% All_Dat$POLICYNO, ]

rm(testr, temptestr, matched_dat, Add_Back, missed_dat)

# Do some calculations
All_cl_Data$POLICY_HOLDER_NAME  <-  toupper(All_cl_Data$POLICY_HOLDER_NAME)
All_cl_Data$Title               <-  word(All_cl_Data$POLICY_HOLDER_NAME, -1)

All_cl_Data$Age_Join                <-  as.numeric((All_cl_Data$COMMENCEDATE        - All_cl_Data$POLICY_HOLDER_DOB) / 365.25)
All_cl_Data$Age_Claim               <-  as.numeric((All_cl_Data$CLAIM_INCIDENT_DATE - All_cl_Data$POLICY_HOLDER_DOB) / 365.25)
All_cl_Data$Age_Patient             <-  as.numeric((All_cl_Data$CLAIM_INCIDENT_DATE - All_cl_Data$PATIENT_DOB)       / 365.25)
All_cl_Data$Age_Now                 <-  as.numeric((Sys.Date()                      - All_cl_Data$POLICY_HOLDER_DOB) / 365.25)

All_cl_Data$Time_to_Claim           <-  All_cl_Data$Age_Claim - All_cl_Data$Age_Join

All_cl_Data$Incident_Year           <-  as.numeric(format(All_cl_Data$CLAIM_INCIDENT_DATE, '%Y'))
All_cl_Data$Incident_Month          <-  as.numeric(format(All_cl_Data$CLAIM_INCIDENT_DATE, '%m'))

All_cl_Data$Development             <-  round(as.numeric((All_cl_Data$CLAIM_DATE_OF_PAYMENT - All_cl_Data$CLAIM_INCIDENT_DATE) / 30.4375))

All_cl_Data$Negative_Payment        <-  ifelse(All_cl_Data$CLAIM_PAYOUT < 0, 1, 0)

All_cl_Data$Overall_Incident_Month  <-  All_cl_Data$Incident_Month + 12 * (All_cl_Data$Incident_Year - 2011)

All_cl_Data$IncYear_Month           <-  as.factor(substr(All_cl_Data$CLAIM_INCIDENT_DATE, 1, 7))

All_cl_Data$CLAIM_PAYOUT_OK <- 0
All_cl_Data$CLAIM_PAYOUT_OS <- 0

All_cl_Data$CLAIM_PAYOUT_OK[!is.na(All_cl_Data$CLAIM_DATE_OF_PAYMENT)]  <-  All_cl_Data$CLAIM_PAYOUT[!is.na(All_cl_Data$CLAIM_DATE_OF_PAYMENT)]
All_cl_Data$CLAIM_PAYOUT_OS[ is.na(All_cl_Data$CLAIM_DATE_OF_PAYMENT)]  <-  All_cl_Data$CLAIM_PAYOUT[is.na(All_cl_Data$CLAIM_DATE_OF_PAYMENT)]













