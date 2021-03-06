#################
# Prep All Data #
#################

# Load All data and combine the files

# Check to see if CSV should be created
if (file.exists(paste(Path, "/Data/CSV/All_Data_Extract.csv", sep = ""))) {
  
  fileCSVDate <- file.mtime(paste(Path, "/Data/CSV/All_Data_Extract.csv", sep = ""))
  fileXLSDate <- file.mtime(paste(Path, "/Data/All_Data_Extract.xlsx",    sep = ""))
  
  if (fileXLSDate > fileCSVDate){
    excelToCsv(paste(Path, "/Data/All_Data_Extract.xlsx", sep = "")) 
  }
} else {
  excelToCsv(paste(Path, "/Data/All_Data_Extract.xlsx",   sep = "")) 
}

# Check to see if CSV should be created
if (file.exists(paste(Path, "/Data/CSV/All_Data_Extract Groups.csv", sep = ""))) {
  
  fileCSVDate <- file.mtime(paste(Path, "/Data/CSV/All_Data_Extract Groups.csv", sep = ""))
  fileXLSDate <- file.mtime(paste(Path, "/Data/All_Data_Extract Groups.xlsx",    sep = ""))
  
  if (fileXLSDate > fileCSVDate){
    excelToCsv(paste(Path, "/Data/All_Data_Extract Groups.xlsx", sep = ""))
  }
} else {
  excelToCsv(paste(Path, "/Data/All_Data_Extract Groups.xlsx",   sep = ""))
}

rm(fileCSVDate, fileXLSDate)

All_Dat_I <- fread(paste(Path, "/Data/CSV/All_Data_Extract.csv", sep = ""),
                   colClasses  =  "character",
                   header      =  TRUE)
All_Dat_I <- as.data.frame(All_Dat_I)

colnames(All_Dat_I) <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(colnames(All_Dat_I))))

# Sorting Individual Data
All_Dat_I <- All_Dat_I[, !duplicated(colnames(All_Dat_I))]
All_Dat_I <- All_Dat_I %>% select(IDNUMBER, NONSAIDIND, COMMENCEDATE, AFFGRPNAME, POLICYNO, PRODCODE, 
                                  MEDAID, MEDAIDOPTION, MEDAIDNO, OPTION, POLICYSTATUS, CLIENTGENDER, DATEEND, DATEBORN)

# Fix data format that is imposed by the excelToCsv function.
All_Dat_I <- data.frame(gsub(".0000000", "", as.matrix(All_Dat_I)))

# Creating new columns
All_Dat_I$VOLUNTARYCOMPULSORY     <-   "V"
All_Dat_I$POLICYHOLDERCLIENTCODE  <-   ""
All_Dat_I$COMPANYNAME             <-   ""
All_Dat_I$INDIVGRP                <-   "IND"

# Fix Dates
All_Dat_I$COMMENCEDATE   <-   DateConv(All_Dat_I$COMMENCEDATE)
All_Dat_I$DATEEND        <-   DateConv(All_Dat_I$DATEEND)
All_Dat_I$DATEBORN       <-   DateConv(All_Dat_I$DATEBORN)

All_Dat_G <- fread(paste(Path, "/Data/CSV/All_Data_Extract Groups.csv", sep = ""), 
                   colClasses = "character",
                   header = TRUE)
All_Dat_G <- as.data.frame(All_Dat_G)

colnames(All_Dat_G) <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(colnames(All_Dat_G))))

# Sorting group data
All_Dat_G <- All_Dat_G[, !duplicated(colnames(All_Dat_G))]
All_Dat_G <- All_Dat_G %>% select(IDNUMBER, NONSAIDIND, COMMENCEDATE, AFFGRPNAME, POLICYNO, PRODCODE, 
                                  VOLUNTARYCOMPULSORY, POLICYHOLDERCLIENTCODE, COMPANYNAME,
                                  MEDAID, MEDAIDOPTION, MEDAIDNO, OPTION, POLICYSTATUS, CLIENTGENDER, DATEEND, DATEBORN)

# Fix data format that is imposed by the excelToCsv function.
All_Dat_G <- data.frame(gsub(".0000000", "", as.matrix(All_Dat_G)))

All_Dat_G$INDIVGRP      <-  "GRP"

# Fix Dates
All_Dat_G$COMMENCEDATE  <-  DateConv(All_Dat_G$COMMENCEDATE)
All_Dat_G$DATEEND       <-  DateConv(All_Dat_G$DATEEND)
All_Dat_G$DATEBORN      <-  DateConv(All_Dat_G$DATEBORN)

# Combine Data
All_Dat <- rbind(All_Dat_I, All_Dat_G)

All_Dat$POLICYNO              <-  paste(gsub(" ", "", All_Dat$POLICYNO), gsub(" ", "", All_Dat$POLICYHOLDERCLIENTCODE), sep = "")
All_Dat$IDNUMBER              <-  gsub(" ", "", All_Dat$IDNUMBER)
All_Dat$VOLUNTARYCOMPULSORY   <-  gsub(" ", "", All_Dat$VOLUNTARYCOMPULSORY)
All_Dat$PRODCODE              <-  gsub(" ", "", All_Dat$PRODCODE)
All_Dat$NONSAIDIND            <-  gsub(" ", "", All_Dat$NONSAIDIND)

# Cleans Affinity Group Name
All_Dat$AFFGRPNAME <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_Dat$AFFGRPNAME)))

rm(All_Dat_G, All_Dat_I)

# Clear blank spaces from front and back of data
All_Dat$MEDAID          <-   str_trim(All_Dat$MEDAID)
All_Dat$MEDAIDOPTION    <-   str_trim(All_Dat$MEDAIDOPTION)
All_Dat$MEDAIDNO        <-   str_trim(All_Dat$MEDAIDNO)
All_Dat$OPTION          <-   str_trim(All_Dat$OPTION)
All_Dat$POLICYSTATUS    <-   str_trim(All_Dat$POLICYSTATUS)
All_Dat$CLIENTGENDER    <-   gsub(" ", "", All_Dat$CLIENTGENDER)
All_Dat$IDNUMBER        <-   gsub(" ", "", All_Dat$IDNUMBER)

# Determine Final ID
All_Dat$ID                     <- All_Dat$IDNUMBER
All_Dat$ID[is.na(All_Dat$ID)]  <- All_Dat$NONSAIDIND[is.na(All_Dat$ID)]
All_Dat$ID[All_Dat$ID == ""]   <- All_Dat$NONSAIDIND[All_Dat$ID == ""]

# Check SA ID Length
All_Dat$ID_LEN                                 <- 0
All_Dat$ID_LEN[nchar(All_Dat$IDNUMBER) != 13]  <- nchar(All_Dat$IDNUMBER)[nchar(All_Dat$IDNUMBER) != 13]

# Fix commencement date if NA
All_Dat$COMMENCEDATE[is.na(All_Dat$COMMENCEDATE)]     <-   as.Date(firstDayMonth(Sys.Date()))

# fix ID's which start with a zero (born between 2000 and 2009)
one_zero   <- paste("0",   All_Dat$IDNUMBER, sep = "")
two_zero   <- paste("00",  All_Dat$IDNUMBER, sep = "")
three_zero <- paste("000", All_Dat$IDNUMBER, sep = "")

All_Dat$ID[nchar(All_Dat$IDNUMBER) == 12 & substr(All_Dat$DATEBORN, 3, 3) == "0"]     <-  one_zero[nchar(All_Dat$IDNUMBER)   == 12 & 
                                                                                                     substr(All_Dat$DATEBORN, 3, 3)   == "0"]
All_Dat$ID[nchar(All_Dat$IDNUMBER) == 11 & substr(All_Dat$DATEBORN, 3, 4) == "00"]    <-  two_zero[nchar(All_Dat$IDNUMBER)   == 11 & 
                                                                                                     substr(All_Dat$DATEBORN, 3, 4)   == "00"]                  
All_Dat$ID[nchar(All_Dat$IDNUMBER) == 10 & substr(All_Dat$DATEBORN, 3, 6) == "00-0"]  <-  three_zero[nchar(All_Dat$IDNUMBER) == 10 & 
                                                                                                       substr(All_Dat$DATEBORN, 3, 6) == "00-0"]

# Tag people who left and joined again later (exlcuding people who moved from IND to GRP and vice versa)
All_Dat$TempKey <- paste(All_Dat$ID, All_Dat$PRODCODE,sep = "")

testr <- All_Dat[All_Dat$INDIVGRP == "IND",]
testr <- as.data.frame(table(testr$TempKey))

testr$Var1 <- as.character(testr$Var1)

All_Dat <- merge(All_Dat, testr, by.x = "TempKey", by.y = "Var1", all.x = TRUE)

All_Dat$Freq[All_Dat$INDIVGRP == "GRP" | All_Dat$ID == ""] <-  NA
All_Dat                                                    <-  subset(All_Dat, select = -TempKey)

rm(testr)


# Get age categories
All_Dat$Age_Comm <- as.numeric(All_Dat$COMMENCEDATE - All_Dat$DATEBORN)/365.25
All_Dat$Age_Comm <- as.character(cut(All_Dat$Age_Comm, Age_Classes))

##################################################################################################################################
############# APPLY FILTER HERE ##################################################################################################
##################################################################################################################################


if (Age_Fliter != "") {All_Dat <- All_Dat[All_Dat$Age_Comm == Age_Fliter, ] }
if (Product    != "") {All_Dat <- All_Dat[All_Dat$PRODCODE == Product, ]}

if (Affinity != "") {
  if (Everything_Else == "YES") {
    All_Dat <- All_Dat[!(All_Dat$AFFGRPNAME %in% Affinity), ]
  } else {
    All_Dat <- All_Dat[All_Dat$AFFGRPNAME %in% Affinity, ]
  }
}

if (Indiv_Group    != "") {All_Dat <- All_Dat[All_Dat$INDIVGRP             ==  Indiv_Group, ]}
if (Vol_Comp       != "") {All_Dat <- All_Dat[All_Dat$VOLUNTARYCOMPULSORY  ==  Vol_Comp, ]}
if (Gender         != "") {All_Dat <- All_Dat[All_Dat$CLIENTGENDER         ==  Gender, ]}
if (max(Rejoiners) >  0 ) {All_Dat <- All_Dat[All_Dat$Freq %in% Rejoiners, ]}

if (Status != "") {
  if (Everything_Else == "YES") {
    All_Dat <- All_Dat[All_Dat$AFFGRPNAME %in% Status, ]
  } else {
    All_Dat <- All_Dat[!(All_Dat$AFFGRPNAME %in% Status), ]
  }
}

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

rm(one_zero, two_zero, three_zero)

# Create Checks DF
len    <- length(All_Dat$POLICYNO)
Checks <- data.frame(Policy_ID  =  All_Dat$POLICYNO, 
                     Gender     =  All_Dat$CLIENTGENDER, 
                     ID         =  All_Dat$ID, 
                     DOB        =  All_Dat$DATEBORN,
                     ID_missing =  numeric(len), 
                     ID_Length  =  integer(len), 
                     ID_Gender  =  character(len), 
                     ID_DOB     =  numeric(len))

Checks$Policy_ID  <-  as.character(Checks$Policy_ID)
Checks$Gender     <-  as.character(Checks$Gender)
Checks$ID         <-  as.character(Checks$ID)

rm(len)

# Foreign ID indicator
Checks$FID <- 0
Checks$FID[All_Dat$NONSAIDIND != "" | !is.na(All_Dat$NONSAIDIND) ] <- 1

# Length of SA ID's (must be 13)
Checks$ID_Length <- 0
Checks$ID_Length[nchar(All_Dat$IDNUMBER) != 13] <- nchar(All_Dat$IDNUMBER)[nchar(All_Dat$IDNUMBER) != 13]

# Missing ID's
Checks$ID_missing <- 0
Checks$ID_missing[All_Dat$ID == ""] <- 1

# Missmatch in DOB's
Checks$ID_DOB <- 0

cent <- ifelse(substr(All_Dat$IDNUMBER, 1, 2) < as.numeric(substr(format(Sys.Date(), "%Y"), 3, 4)), 
               as.numeric(substr(format(Sys.Date(), "%Y"), 1, 2)),
               as.numeric(substr(format(Sys.Date(), "%Y"), 1, 2)) - 1)

Temp_DB <- All_Dat$IDNUMBER
Temp_DB[Temp_DB == ""]        <- "1111111111111"
Temp_DB[nchar(Temp_DB) != 13] <- "1111111111111"
Temp_DB <- as.Date(paste(cent, substr(Temp_DB, 1, 2), "-", 
                                              substr(Temp_DB, 3, 4), "-", 
                                              substr(Temp_DB, 5, 6), sep = ""))

Checks$ID_DOB[Temp_DB != All_Dat$DATEBORN] <- 1 
Checks$ID_DOB[All_Dat$IDNUMBER == ""]      <- 0

# Complete Missing DOB's Also
Checks$ID[Checks$ID == ""]        <-  NA
Checks$ID_DOB[is.na(Checks$DOB)]  <-   1

rm(cent, Temp_DB)

# Check gender in the same 
Checks$ID_Gender <- 0
temp_gen <- ifelse(Checks$ID_missing == 0 & Checks$FID == 0, 
                   ifelse(substr(All_Dat$IDNUMBER, 7, 7) <= 4, "F", 
                          ifelse(substr(All_Dat$IDNUMBER, 7, 7) >= 5, "M", 0)), 0)

Checks$ID_Gender[str_trim(All_Dat$CLIENTGENDER) != temp_gen & temp_gen != 0] <- 1

# Complete Missing Genders Also
Checks$Gender[Checks$Gender == ""]      <-  NA
Checks$ID_Gender[is.na(Checks$Gender)]  <-  1

# Remove useless data
All_Dat <- subset(All_Dat, select = c(-IDNUMBER, -NONSAIDIND, -POLICYHOLDERCLIENTCODE, -ID_LEN))
rm(temp_gen)

# Set missing Dates of birth as 45 years back
All_Dat$DATEBORN[is.na(All_Dat$DATEBORN)] <- as.Date(firstDayMonth(Sys.Date())) - 365.25 * 45 

# Clear Check df
Checks$temp  <-  Checks$ID_missing + Checks$ID_Length + Checks$ID_Gender + Checks$ID_DOB
Checks       <-  Checks[Checks$temp != 0, ]
Checks       <-  subset(Checks, select = c(-FID, -temp))











