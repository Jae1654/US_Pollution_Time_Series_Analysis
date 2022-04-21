#to do: do double differencing first

library(tidyverse)
library(ggplot2)

O3 <- read_csv(file = 'O3.csv')
plot(x=O3$time, y=O3$o3, type='l')
ozone <- O3 %>%
  select('time','o3')
#strong seasonality, trend not obvious

###First Difference
diff_ozone <- diff(O3$o3)
plot(diff_ozone, type='l')
acf(diff_ozone)
pacf(diff_ozone)
#ACF shows that there still is strong seasonality (every lag 7 or so)

###Second Difference
diff2_ozone <- diff(diff_ozone)
plot(diff2_ozone, type='l')
acf(diff2_ozone)
pacf(diff2_ozone)

###Third Difference
diff3_ozone <- diff(diff2_ozone)
plot(diff3_ozone, type='l')
acf(diff3_ozone)
pacf(diff3_ozone)
#ACF shows high spike at lag 1 -> possibility of MA(1) model. Seasonality also 
#needs to be taken into account as many spikes shoots beyond the blue line and 
#are at consistent space in between.


###Sinusoid
time = 1:nrow(O3)
cosX = matrix(NA, ncol=6, nrow=nrow(O3))
sinX = matrix(NA, ncol=5, nrow=nrow(O3))
for (i in 1:6) {
  cosX[,i] = cos(2*pi*time*i/12)
  if (i<6){
    sinX[,i] = sin(2*pi*time*i/12)
  }
}

model1 = lm(diff3_ozone ~ time + cosX + sinX)
plot(time, O3$o3, type='l')
lines(time, model1$fitted.values, col='red')

plot(time, model1$residuals, type='l', main='Residuals of Sinusoid Trend')


periodogram = periodogram(diff3_ozone,plot=TRUE,ylab="Periodogram", xlab="Frequency")
#2 spikes with leakage
ordered_period = sort(periodogram$spec,decreasing = TRUE)
maxest = ordered_period[1]
maxier = ordered_period[2]

maxest_freq = periodogram$freq[periodogram$spec==maxest]
maxier_freq = periodogram$freq[periodogram$spec==maxier]

time = 1:length(diff3_ozone)
first_sin = sin(2*pi*maxest_freq*time)
second_sin = sin(2*pi*maxier_freq*time)
first_cos = cos(2*pi*maxest_freq*time)
second_cos = cos(2*pi*maxier_freq*time)

sinusoid_model = lm(diff3_ozone ~ time + first_sin + first_cos + second_sin + second_cos)

sinusoid_residual = sinusoid_model$residuals
plot(sinusoid_residual, type='l')
acf(sinusoid_residual)
pacf(sinusoid_residual)

###Fit MA(1) model to this
ma1 <- sarima(sinusoid_residual, p=0, d=0, q=1, P=0, D=0, Q=0, S=0)

#borrowing Jae's code
coeff_table <- as.data.frame(ma1$ttable)
coeff_table <-coeff_table %>% mutate(ci_lower = Estimate-1.96*SE,ci_upper =  Estimate+1.96*SE) 
coeff_table # show estimated coefficient and its ci

# AIC, AICc, BIC
eval<- function(model){
  return (c(model$AIC, model$AICc,model$BIC))
}
evaluation = eval(ma1)
