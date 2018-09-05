#Read in birth data
o_data <- read.csv("Yr1116Birth.csv", 
                   na.strings=c("9","99", "9999"))
#SEX=1 male, 2 female; male=1 male, 0 female
o_data$male=2-o_data$SEX #binary gender for interpretation

birth_data <- na.omit(o_data)

birth_data$GEST_C=birth_data$GEST
birth_data$BWTG_C=birth_data$BWTG
birth_data$GEST_C[birth_data$GEST_C>50]=NA
birth_data$GEST_C[birth_data$GEST_C<20]=NA
birth_data$BWTG_C[birth_data$BWTG_C<500]=NA
bdclean=na.omit(birth_data)

birth2016=subset(birth_data,birth_data$YOB=='2016')
#CORES code for Tyrrell Co is 89
tyrrell=subset(birth2016,birth2016$CORES=='89')

# Linear regression
linear_model <- lm(BWTG_C~GEST_C+male, data=tyrrell)
summary(linear_model)

plot(tyrrell$GEST_C, tyrrell$BWTG_C, type="n")
lines(tyrrell$GEST_C[tyrrell$male==1],linear_model$fitted.values[tyrrell$male==1],col=2)
lines(tyrrell$GEST_C[tyrrell$male==0],linear_model$fitted.values[tyrrell$male==0],col=3)
points(tyrrell$GEST_C,tyrrell$BWTG_C)

# M-estimation
library(MASS)
fit2 <- rlm(BWTG_C ~ GEST_C+male, data=tyrrell)
summary(fit2)

fit2_weights = data.frame(bwt = tyrrell$BWTG_C, gest = tyrrell$GEST_C, 
                          resid=fit2$resid, weight=fit2$w)
fit2_weights[order(fit2$w)[c(1:5, (length(fit2$w)-5):length(fit2$w))],]

fit2_weights = data.frame(bwt = tyrrell$BWTG_C, gest = tyrrell$GEST_C, 
                          resid=fit2$resid, weight=fit2$w)
fit2_weights[order(fit2$w)[c(1:5, (length(fit2$w)-5):length(fit2$w))],]


