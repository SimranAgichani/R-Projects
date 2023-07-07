#' Tobit Regression (for censored data)
#' Data AirlinePassengerSatisfaction.csv (100,000 obs x 21 variables)
#' Objective: To predict customer satisfaction with airline service
#' DV measured on a 1-5 scale (terrible/bad/neutral/good/excellent)
rm(list = ls())
setwd("C:/Users/simra/Downloads")
library(readxl)
d <- read_excel("LungCancer.xlsx")

str(d)
View(d)

colSums(is.na(d)) 
attach(d)
hist(days)

library(PerformanceAnalytics)
df <- d[,]
str(df)
chart.Correlation(df) 

ols = lm(days ~ Treatment + Cell.type + Status + Karnofsky.score 
         + Months.from.Diagnosis + Age.in.years + Prior.chemotherapy, data=d)

library(AER)
tobit = tobit(Survival.in.days ~ Treatment + Cell.type + Status + Karnofsky.score 
              + Months.from.Diagnosis + Age.in.years + Prior.chemotherapy, left=1, right=999, data=d)
summary(tobit)

d$prior.chemo <- ifelse(d$prior.chemo=='10', 1, 0) #transforming for ease of understanding
library(dplyr)
d$celltype = recode_factor(d$celltype, "1"="Squamous", "2"="Small Cell", "3"="adeno", "4"="large")
d$eth = relevel(df$eth, "White")
# Kaplan-Meier non-parametric analysis
# install.packages("survival")
library(survival)
y <- Surv(d$days, d$event)              # Y is a combination of time and event

#km <- survfit(y ~ 1)   tried base model without treatment
#plot(km1, xlab="Time", ylab="Survival Probability") 

# Kaplan-Meier non-parametric analysis by treatment group
km1 <- survfit(y ~ d$treatment)
summary(km1)
summary(days)
plot(km1, xlab="Time", ylab="Survival Probability")

# Cox proportional hazard model - coefficients and hazard rates
cox <- coxph(y ~ Treatment + Cell.type + Karnofsky.score 
             + Months.from.Diagnosis + Age.in.years + Prior.chemotherapy, data = d)
summary(cox)

# Exponential, Weibull, and log-logistic parametric model coefficients
exp <- survreg(y ~ Treatment + Cell.type + Karnofsky.score 
               + Months.from.Diagnosis + Age.in.years + Prior.chemotherapy, data = d, dist="exponential")
summary(exp)

weibull <- survreg(y ~ Treatment + Cell.type + Karnofsky.score 
                   + Months.from.Diagnosis + Age.in.years + Prior.chemotherapy, data = d, dist="weibull")
summary(weibull)

loglogistic <- survreg(y ~ Treatment + Cell.type + Karnofsky.score 
                       + Months.from.Diagnosis + Age.in.years + Prior.chemotherapy, data = d, dist="loglogistic")
summary(loglogistic)

library(censReg)
tobit3 <- censReg(Survival.in.days ~ Treatment + Cell.type + Status + Karnofsky.score 
                  + Months.from.Diagnosis + Age.in.years + Prior.chemotherapy, left=1, right=999, data=d)

library(stargazer)
stargazer(ols, tobit, tobit3,cox, weibull,loglogistic, exp, type="text", single.row=TRUE)
AIC(ols, tobit, tobit3,cox, weibull,loglogistic, exp)
BIC(ols, tobit, tobit3,cox, weibull,loglogistic, exp)

#' Note: If you want to do left censoring only but not right censoring 
#' (e.g., analyzing wages which is left censored at 0 but has no right censor),
#' you can use right=Inf in the above formulas.

