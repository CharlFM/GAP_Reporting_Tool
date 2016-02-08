

Time_to_Claim  <- All_cl_Data$Time_to_Claim[All_cl_Data$Time_to_Claim > 0]*12
Tot_Duration   <- as.numeric(Sys.Date() - All_Dat$COMMENCEDATE)/30.437

Tot_Duration[!is.na(All_Dat$DATEEND)] <- as.numeric(All_Dat$DATEEND[!is.na(All_Dat$DATEEND)] - All_Dat$COMMENCEDATE[!is.na(All_Dat$DATEEND)]) / 30.437

Tot_Duration <- Tot_Duration[Tot_Duration > 0]

Time_To_Claims_Bins <- data.frame(table(as.numeric(cut(Time_to_Claim, seq(1, ceiling(max(c(Tot_Duration, Time_to_Claim), na.rm = T)))))))
Duration_Bins       <- data.frame(table(as.numeric(cut(Tot_Duration,  seq(1, ceiling(max(c(Tot_Duration, Time_to_Claim), na.rm = T)))))))

Combo <- merge(Duration_Bins, Time_To_Claims_Bins, by = "Var1", all = T)

Combo$Freq.x[is.na(Combo$Freq.x)] <- 0
Combo$Freq.y[is.na(Combo$Freq.y)] <- 0

Combo$Freq.x <- rev(cumsum(rev(Combo$Freq.x)))

Combo$Freq <- Combo$Freq.y / Combo$Freq.x

tot <- seq(1:nrow(Combo))
loFreq <- loess(Combo$Freq ~ tot)
Combo$Freq <- loFreq$fitted

myPlot <- ggplot(Combo, aes(Var1, Freq)) + geom_point()

ggsave(filename = paste(Path, "/Results/", TodayDate, "/", Time, "/", "Claim_Freq_T2Claim.jpg",  sep = ""), plot = myPlot)





