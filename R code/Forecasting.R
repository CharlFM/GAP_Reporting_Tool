###############
# Forecasting #
###############

ClaimFreq <- Data_Out$Claim_Count  /  Data_Out$Total_Exposure
ClaimSev  <- Data_Out$Ult          /  Data_Out$Claim_Count

Prem <- ClaimFreq * ClaimSev

summary(ClaimSev)
plot(ClaimSev,    type = "l")

summary(ClaimFreq)
plot(ClaimFreq,   type = "l")

summary(Prem)
plot(Prem,        type = "l")

fit    <-  auto.arima(Prem)
fcast  <-  forecast(fit, h = 12)
plot(fcast)
grid()

