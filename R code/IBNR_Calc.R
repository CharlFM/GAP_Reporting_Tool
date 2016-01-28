##################
# Calculate IBNR #
##################

MaxDev <- (as.numeric(YearEnd) - 2012) * 12 + as.numeric(MonthEnd) + 12

# Determine upper triangle.
Xij <- matrix(NA, 12, 12)
for(i in 1:12){
  for(j in 1:12){
    Xij[i, j] <- (All_cl_Data %>% 
                    filter(Overall_Incident_Month == MaxDev - 12 + i) %>%
                    filter(Development == j - 1) %>% 
                    summarise(totl = sum(CLAIM_PAYOUT_OK)))$totl
  }
}

# Set as triangle class
Xij2 <- as.triangle(Xij)

temp            <-  rotate(t(upper.tri(Xij, T)))
Xijtri          <-  t(Xij)[temp]
temp[temp == 0] <-  NA
Xij             <-  Xij2 * temp
Xij[Xij == 0]   <-  0.5

# Determine Cumulative
Cij <- incr2cum(Xij)

############## Create a predicted matrix #################

predicted <- Cij

###############################################
# Include the following if you want averaging #
###############################################

if (mean(Cij[5:11, 2]) > predicted[12, 1]) {predicted[12, 2] <- mean(Cij[5:11, 2])}
if (mean(Cij[4:10, 3]) > predicted[11, 2]) {predicted[11, 3] <- mean(Cij[4:10, 3])}
if (mean(Cij[3:9 , 4]) > predicted[10, 3]) {predicted[10, 4] <- mean(Cij[3:9 , 4])}

predicted <- as.triangle(predicted)

###############################
# Bornhuetter-Ferguson Method #
###############################

# Predict ultimates using another method
# predict ultimates using Mack:
Comp <- MackChainLadder(Cij, est.sigma = "Mack", alpha = 2)
# predict using bootstrap
# Comp <-  BootChainLadder(Cij, R = 999, process.distr="gamma")
ultimate  <-  Cij

#########################################################################
#add if you want the ultimates to be predicted using the averaged values#
if (mean(Cij[5:11, 2]) > ultimate[12, 1]) {ultimate[12, 2] <- mean(Cij[5:11, 2])}
if (mean(Cij[4:10, 3]) > ultimate[11, 2]) {ultimate[11, 3] <- mean(Cij[4:10, 3])}
if (mean(Cij[3:9 , 4]) > ultimate[10, 3]) {ultimate[10, 4] <- mean(Cij[3:9 , 4])}

for (i in 1:12){
  for(j in 1:12){
    if (is.na(ultimate[i, j])){
      ultimate[i, j]  <-  ultimate[i, j - 1] * Comp$f[j - 1]
    }
  }
}

# Create vector of estimated ultimates
ultimates <- ultimate[,12]

# Vector of incremental percentage of the ultimate
# Using Boot
Comp2 <- BootChainLadder(Cij, R = 999, process.distr = "gamma")
# Using Mack
# Comp2 <- MackChainLadder(Cij,est.sigma = "Mack", alpha = 2)
factors <- Comp2$f

# Use factors to create incremental percentages
incremental     <-  array(NA)
incremental[1]  <-  1 / prod(factors)
for (i in 1:11){
  incremental[i + 1] <- (factors[i] - 1) / prod(factors[i:12])
}


# If cell ij is empty then Cij = Cij - 1 + incremental percentage * estimated ultimate.

for (i in 1:12){ 
  for (j in 1:12){ 
    if (is.na(predicted[i, j])){
      predicted[i, j] <- predicted[i, j - 1] + ultimates[i] * incremental[j]
    }
  }
}

inc_pred <- cum2incr(predicted)

temp[temp == 1]   <- 0
temp[is.na(temp)] <- 1

IBNR <- rowSums(temp*inc_pred) - rowSums(Xij2*temp) # Removes outstanding claims from IBNR (Otherwise included twice : 
                                                    #                                             once in IBNR and once in Claimtot)

rm(predicted, inc_pred, temp, Xijtri, Xij, Cij, i, j, incremental, factors, Comp2, ultimates, ultimate, Comp, Xij2)

#########################################################################################################    
# Get Incurred Claims + Ultimate #
##################################

detach(package:plyr)
Claims_Dat <- All_cl_Data %>% 
  group_by(IncYear_Month) %>%
  summarise(claimtot = sum(CLAIM_PAYOUT_OK, na.rm = TRUE),
            OS_Claim = sum(CLAIM_PAYOUT_OS, na.rm = TRUE))

Year_Month  <-  as.character(substr(seq(as.Date("2012-01-01"), by = "month", length.out = MaxDev - 12), 1, 7))
YearsMonths <- data.frame(IncYear_Month = Year_Month, TempVar = seq(1,length(Year_Month)))

Claims_Dat <- merge(Claims_Dat, YearsMonths, by = "IncYear_Month", all = TRUE)
Claims_Dat <- Claims_Dat[order(Claims_Dat$TempVar),]
Claims_Dat <- subset(Claims_Dat, select = -TempVar)

Claims_Dat$IBNR <- c(rep(0, nrow(Claims_Dat) - length(IBNR)), IBNR)

Claims_Dat$claimtot[is.na(Claims_Dat$claimtot)] <- 0
Claims_Dat$OS_Claim[is.na(Claims_Dat$OS_Claim)] <- 0

Claims_Dat$Ult <- Claims_Dat$claimtot + Claims_Dat$IBNR + Claims_Dat$OS_Claim

rm(IBNR, MaxDev, Year_Month, YearsMonths)







