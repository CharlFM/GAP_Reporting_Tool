Time_to_Claim  <- All_cl_Data$Time_to_Claim[All_cl_Data$Time_to_Claim > 0]*12
Tot_Duration   <- as.numeric(Sys.Date() - All_Dat$COMMENCEDATE)/30.437
Tot_Duration[!is.na(All_Dat$DATEEND)] <- as.numeric(All_Dat$DATEEND[!is.na(All_Dat$DATEEND)] - All_Dat$COMMENCEDATE[!is.na(All_Dat$DATEEND)])/30.437
Tot_Duration <- Tot_Duration[Tot_Duration>0]

Time_To_Claims_Bins <- data.frame(table(as.numeric(cut(Time_to_Claim, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)))))
Duration_Bins       <- data.frame(table(as.numeric(cut(Tot_Duration, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)))))

summary(Time_to_Claim)
summary(Tot_Duration)
hist(Time_to_Claim)
hist(Tot_Duration)

plot(Time_To_Claims_Bins$Freq~Time_To_Claims_Bins$Var1, type = "l")
plot(Duration_Bins$Freq~Duration_Bins$Var1, type = "l")

Combo <- merge(Duration_Bins, Time_To_Claims_Bins, by = "Var1", all = T)
Combo$Freq.x[is.na(Combo$Freq.x)] <- 0
Combo$Freq.y[is.na(Combo$Freq.y)] <- 0
Combo$Freq.x <- rev(cumsum(rev(Combo$Freq.x)))

Combo$Freq <- Combo$Freq.y / Combo$Freq.x

plot(Combo$Freq~Combo$Var1, type = "p")
plot(Combo$Freq, type = "l")