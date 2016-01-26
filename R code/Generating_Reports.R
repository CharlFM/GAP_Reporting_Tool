
# Check to see if today's folder is created
TodayDate <- as.character(Sys.Date())

DateFolders <- list.files(paste(Path, "/Results", sep = ""))

if (TodayDate %in% DateFolders){
  Time <- as.character(format(Sys.time(), "%H_%M_%S"))
  dir.create(paste(Path, "/Results/", TodayDate, "/", Time, sep = ""))
} else {
  Time <- as.character(format(Sys.time(), "%H_%M_%S"))
  dir.create(paste(Path, "/Results/", TodayDate, sep = ""))
  dir.create(paste(Path, "/Results/", TodayDate, "/", Time, sep = ""))
}

write.csv(Data_Out, paste(Path, "/Results/", TodayDate, "/", Time, "/", "Monthly_GAP_Results.csv",  sep = ""))
write.csv(Report,   paste(Path, "/Results/", TodayDate, "/", Time, "/", "Yearly_GAP_Results.csv",   sep = ""))
write.csv(fcast,    paste(Path, "/Results/", TodayDate, "/", Time, "/", "Forecast_GAP_Results.csv", sep = ""))

png(file = paste(Path, "/Results/", TodayDate, "/", Time, "/", "Forecast_GAP_Results.jpg",  sep = ""))

par(mfrow = c(1,1))

plot(fcast, type = "l", main = "Premium Forecast")

dev.off()

png(file = paste(Path, "/Results/", TodayDate, "/", Time, "/", "Observed_Severity_Frequency.jpg",  sep = ""))
par(mfrow = c(1, 2))

plot(ClaimSev,  type = "l", main = "Observed Average Severity")
plot(ClaimFreq, type = "l", main = "Observed (and imputed) Frequency")

par(mfrow = c(1, 1))

dev.off()

















