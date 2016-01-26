#########################
# Get risk Premium Data #
#########################

# Number of directories in current folder
numfold  <-  length(list.dirs(paste(Path, "/Data/Premium Data/", sep = "")))             
dirs     <-  seq(12, 11 + numfold)

source(paste(Path, "/R code/OpenDB.R", sep = ""))

Premium_Data_Rp <- dbReadTable(mydb, "Prem_Data_Risk_Prem")

DatesInDb  <-  colnames(Premium_Data_Rp %>% select(contains("RISKPREM")))
DatesInDb  <-  substr(DatesInDb, nchar(DatesInDb) - 5, nchar(DatesInDb))
yearstrt   <-  max(as.integer(substr(DatesInDb, 1, 4)), na.rm = T)
monthstrt  <-  DatesInDb[grepl(yearstrt, DatesInDb)]
monthstrt  <-  max(as.integer(substr(monthstrt, 5, 7)),na.rm = T)

if(monthstrt == 12){
  monthstrt  <-  1
  yearstrt   <-  as.integer(substr(yearstrt,3,5)) + 1
}else{
  monthstrt  <-  monthstrt + 1
  yearstrt   <-  as.integer(substr(yearstrt,3,5))
}

foldstrt <- which(dirs == yearstrt)

if (length(foldstrt) != 0) {
  
  for(fold in foldstrt:numfold){
    
    File_List  <-  list.files(paste(Path, "/Data/Premium Data/F", dirs[fold], "/", sep = ""))
    numfile    <-  length(File_List) - 1    #  Number of files in folder - 1 due to folder CSV
    
    if (monthstrt <= numfile){
      
      for(file in monthstrt:numfile){
        
        # Check to see if CSV should be created
        if (file.exists(paste(Path, "/Data/Premium Data/F", dirs[fold], "/CSV/", paste0(substr(File_List[file], 1, 6), ".csv"), sep = ""))){
          
          fileCSVDate <- file.mtime(paste(Path, "/Data/Premium Data/F", dirs[fold], "/CSV/", paste0(substr(File_List[file], 1, 6), ".csv"), 
                                          sep = ""))
          fileXLSDate <- file.mtime(paste(Path, "/Data/Premium Data/F", dirs[fold], "/", File_List[file], sep = ""))
          
          if (fileXLSDate > fileCSVDate){
            excelToCsv(paste(Path, "/Data/Premium Data/F",dirs[fold], "/", File_List[file], sep = ""))
          }
        } else {
          excelToCsv(paste(Path, "/Data/Premium Data/F",dirs[fold], "/", File_List[file], sep = ""))
        }
      
        CSVFile_List <- list.files(paste(Path, "/Data/Premium Data/F", dirs[fold], "/CSV/", sep = ""))
        
        TempIn            <-  fread(paste(Path, "/Data/Premium Data/F",dirs[fold], "/CSV/", CSVFile_List[file], sep = ""),
                                    colClasses  =  "character",
                                    header      =  TRUE)
        TempIn            <-  as.data.frame(TempIn)
        colnames(TempIn)  <-  TempIn[1,]
        TempIn            <-  TempIn[2:nrow(TempIn),]
        
        colnames(TempIn)  <-  gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(colnames(TempIn))))
        TempIn            <-  TempIn[, !duplicated(colnames(TempIn), fromLast = TRUE)]
        TempIn            <-  as.data.table(TempIn)
        
        Temp_Premium_Data <- TempIn %>% 
          select(POLICYNUMBER, 
                 contains("RISKPREM"))
        
        Temp_Premium_Data$POLICYNUMBER  <-  gsub(" ", "", Temp_Premium_Data$POLICYNUMBER)
        Temp_Premium_Data               <-  Temp_Premium_Data[!is.na(Temp_Premium_Data$POLICYNUMBER), ]
        Temp_Premium_Data               <-  Temp_Premium_Data[Temp_Premium_Data$POLICYNUMBER != "", ]
        Temp_Premium_Data               <-  as.data.frame(Temp_Premium_Data)

        # Conver to numeric type
        NumCols                     <-  colnames(Temp_Premium_Data)[!(colnames(Temp_Premium_Data) == "POLICYNUMBER")]
        Temp_Premium_Data[NumCols]  <-  sapply(Temp_Premium_Data[NumCols], gsub, pattern = " ", replacement = "")
        Temp_Premium_Data[NumCols]  <-  sapply(Temp_Premium_Data[NumCols], gsub, pattern = ",", replacement = "")
        Temp_Premium_Data[NumCols]  <-  sapply(Temp_Premium_Data[NumCols], as.numeric)
        
        Temp_Premium_Data[is.na(Temp_Premium_Data)] <- 0

        # First sum duplicates (these duplicates was actually received or billed twice for some reason)
        Temp_Premium_Data <- as.data.table(Temp_Premium_Data) 
        Temp_Premium_Data <- Temp_Premium_Data[, lapply(.SD, sum), 
                                               by = POLICYNUMBER]
        
        Temp_Premium_Data <- as.data.frame(Temp_Premium_Data)
        
        New_Col_Names <- paste(colnames(Temp_Premium_Data)[!colnames(Temp_Premium_Data) == "POLICYNUMBER"], 
                               substr(File_List[file],1,6), sep = "_")
        
        colnames(Temp_Premium_Data)[!colnames(Temp_Premium_Data) == "POLICYNUMBER"] <- New_Col_Names
        
        Temp_Premium_Data_Rp <- Temp_Premium_Data %>% select(POLICYNUMBER, 
                                                             contains("RISKPREM"))
        
        Premium_Data_Rp <- merge(Premium_Data_Rp, Temp_Premium_Data_Rp, by.x = "POLICYNUMBER", by.y = "POLICYNUMBER", all = TRUE)
        
        # Here remove duplicates
        Premium_Data_Rp <- Premium_Data_Rp %>% distinct(POLICYNUMBER)
        
        print(File_List[file])
        
      }
      
      monthstrt <- 1
      
    }
    
  }
  
}

if (length(foldstrt) != 0 ) {
  if (monthstrt <= numfile) {
    Premium_Data_Rp[Premium_Data_Rp == 0]    <-  NA
    Premium_Data_Rp                          <-  Premium_Data_Rp[rowSums(is.na(Premium_Data_Rp)) != ncol(Premium_Data_Rp) - 1, ]
    Premium_Data_Rp[is.na(Premium_Data_Rp)]  <-  0
    
    dbWriteTable(mydb, "Prem_Data_Risk_Prem", Premium_Data_Rp, overwrite = TRUE)
  }
}

dbDisconnectAll()

rm(foldstrt, yearstrt, monthstrt, DatesInDb, mydb, dirs, numfold, File_List, numfile, fold, file, 
   CSVFile_List, Temp_Premium_Data, TempIn, NumCols, New_Col_Names,
   fileCSVDate, fileXLSDate)










