###################
# Prep Claim Data #
###################

# Start
cl_File_List      <-  list.files(paste(Path, "/Data/Claims Data", sep = ""))
num_cl_file       <-  length(cl_File_List) - 2    #  Number of files in folder - 2 due to folders

for(clfile in 1:num_cl_file){
  
  # Check to see if CSV should be created
  if (file.exists(paste(Path, "/Data/Claims Data/CSV/", paste0(substr(cl_File_List[clfile], 1, 4), ".csv"), sep = ""))) {
    
    fileCSVDate <- file.mtime(paste(Path, "/Data/Claims Data/CSV/", paste0(substr(cl_File_List[clfile], 1, 4), ".csv"), sep = ""))
    fileXLSDate <- file.mtime(paste(Path, "/Data/Claims Data/", cl_File_List[clfile], sep = ""))
    
    if (fileXLSDate > fileCSVDate){
      excelToCsv(paste(Path, "/Data/Claims Data/", cl_File_List[clfile], sep = ""))
    }
  } else {
    excelToCsv(paste(Path, "/Data/Claims Data/",   cl_File_List[clfile], sep = ""))
  }
  
  CSV_cl_File_List  <-  list.files(paste(Path, "/Data/Claims Data/CSV", sep = ""))
  
  cl_Data <- fread(paste(Path, "/Data/Claims Data/CSV/",paste0(substr(cl_File_List[clfile], 1, 4), ".csv"), sep = ""),
                   colClasses  =  "character",
                   header      =  TRUE)
  cl_Data <- as.data.frame(cl_Data)
  
  if(clfile == 1) {
    All_cl_Data <- cl_Data
  } else{
    
    common_cols <- intersect(colnames(All_cl_Data), colnames(cl_Data)) # Combine only the common columns (in case of missmatches)
    
    All_cl_Data <- rbind(
      subset(All_cl_Data,  select = common_cols), 
      subset(cl_Data,      select = common_cols)
    )
    
  }
  
  print(cl_File_List[clfile])
  
}

# Remove data from workspace (to save memory and time)
rm(cl_Data, clfile, num_cl_file, cl_File_List, common_cols, fileCSVDate, fileXLSDate, CSV_cl_File_List) 

# Clean up Claim data
All_cl_Data$PRODUCT_DESCRIPTION         <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_cl_Data$PRODUCT_DESCRIPTION)))
All_cl_Data$CLAIM_APPROVAL_STATUS_CODE  <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_cl_Data$CLAIM_APPROVAL_STATUS_CODE)))
All_cl_Data$EXTERNAL_POLICY_REFERENCE   <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_cl_Data$EXTERNAL_POLICY_REFERENCE)))

All_cl_Data$CLAIM_PAYOUT <- as.numeric(as.character(All_cl_Data$CLAIM_PAYOUT))
All_cl_Data <- All_cl_Data[with(All_cl_Data, order(-CLAIM_PAYOUT)), ]
All_cl_Data <- All_cl_Data[!duplicated(All_cl_Data$CLAIM_IDENTIFIER), ]
All_cl_Data <- All_cl_Data %>% 
  filter(grepl("ZESTLIFE", PRODUCT_DESCRIPTION)) %>%
  filter(CLAIM_APPROVAL_STATUS_CODE %in% c("A", "O", "U"))

All_cl_Data <- All_cl_Data[All_cl_Data$CLAIM_PAYOUT != 0, ]

# Select only neccesary columns
All_cl_Data <- All_cl_Data %>% select(CLAIM_IDENTIFIER, POLICY_HOLDER_ID, EXTERNAL_POLICY_REFERENCE,
                                      POLICY_HOLDER_NAME, INCIDENT_CAUSE_CODE, INCIDENT_CAUSE_DESCRIPTION,
                                      LOCATION_CODE, LOCATION_NAME, PRODUCT_NAME, MEDICAL_SCHEME, 
                                      CLAIM_INCIDENT_DATE, CLAIM_DATE_OF_PAYMENT, CLAIM_PAYOUT,
                                      POLICY_HOLDER_DOB, PATIENT_DOB, 
                                      SERVICE_CODE, SERVICE_DESCRIPTION,
                                      SERV_PAID, CMS_MED_AID_VALUE, SERV_PROVIDER_CHARGE)

# Fix data format that is imposed by the excelToCsv function.
All_cl_Data <- data.frame(gsub(".0000000", "", as.matrix(All_cl_Data)))
All_cl_Data$CLAIM_PAYOUT <- as.numeric(as.character(All_cl_Data$CLAIM_PAYOUT))

# Fix Dates
All_cl_Data$CLAIM_INCIDENT_DATE    <-  DateConv(All_cl_Data$CLAIM_INCIDENT_DATE)
All_cl_Data$CLAIM_DATE_OF_PAYMENT  <-  DateConv(All_cl_Data$CLAIM_DATE_OF_PAYMENT)
All_cl_Data$POLICY_HOLDER_DOB      <-  DateConv(All_cl_Data$POLICY_HOLDER_DOB)
All_cl_Data$PATIENT_DOB            <-  DateConv(All_cl_Data$PATIENT_DOB)

# Cleans ID
All_cl_Data$POLICY_HOLDER_ID  <-  gsub(" ", "", All_cl_Data$POLICY_HOLDER_ID)
All_cl_Data$POLICY_HOLDER_ID  <-  substr(All_cl_Data$POLICY_HOLDER_ID, 4, nchar(All_cl_Data$POLICY_HOLDER_ID))

# Fix Values
All_cl_Data$SERV_PAID             <-  as.numeric(as.character(All_cl_Data$SERV_PAID))
All_cl_Data$SERV_PROVIDER_CHARGE  <-  as.numeric(as.character(All_cl_Data$SERV_PROVIDER_CHARGE))












