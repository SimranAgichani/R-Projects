#' SDM A1: Credit Rating

getwd()
# setwd("C:/Users/abhatt/Desktop/SDM/Data")
setwd("/Users/ab/Desktop/SDM/Data")

library(readxl)
d <- read_excel("CreditCardTransactions.xlsx", sheet="Data")
str(d)
View(d)
which(! complete.cases(d)) 

#' Data Preprocessing

d$Month <- factor(d$Month)
d$WealthTag <- factor(d$WealthTag)
d$CardType <- factor(d$CardType)
d$RevolvingIndicator <- factor(d$RevolvingIndicator)
d$SpendCategory <- factor(d$SpendCategory)

d$WealthTag <- relevel(d$WealthTag, "MassMarket")
d$CardType <- relevel(d$CardType, "Blue")
d$RevolvingIndicator <- relevel(d$RevolvingIndicator, "Transactor")
d$SpendCategory <- relevel(d$SpendCategory, "Grocery")

#' Descriptive Statistics

table(d$CardType)
table(d$SpendCategory)
table(d$WealthTag)
table(d$RevolvingIndicator)

lapply(d[,sapply(d, is.factor)], table)

cor(d$TransCount, d$TransAmount, use="complete.obs")

#' Almost every predictor is a factor variable; the only two continuous variables are
#' TransCount and TransAmount. However the correlation between them is 0.97, i.e., 
#' they are almost the same variable. So we can't use TransCount as a predictor 
#' of TransAmount

#' Data Visualization

hist(d$TransAmount, prob = TRUE, main="Transaction Amount", xlab="Transaction amount")
hist(d$TransCount, prob = TRUE, main="Transaction Count", xlab="Transaction count")

hist(log(d$TransAmount), prob = TRUE, main="Log-Transformed Transaction Amount", xlab="log(Transaction amount)")
hist(log(d$TransCount), prob = TRUE, main="Log-Transformed Transaction Count", xlab="log(Transaction count)")

#' Regression Models

Count_Model <- lm(log(TransCount) ~ Month + WealthTag + CardType + 
           RevolvingIndicator + SpendCategory, data=d) 
Amount_Model <- lm(log(TransAmount) ~ Month + WealthTag + CardType + 
           RevolvingIndicator + SpendCategory, data=d) 

library(stargazer)
stargazer(Count_Model, Amount_Model, type="text", single.row=TRUE)

#' Assumptions Tests

plot(Count_Model)
plot(Amount_Model)

shapiro.test(Count_Model$res)               # Shapiro-Wilk's test of multivariate normality
shapiro.test(Amount_Model$res)

bartlett.test(list(Count_Model$res, Count_Model$fit))  # Bartlett's test of homoskedasticity
bartlett.test(list(Amount_Model$res, Amount_Model$fit))

library("car")                              # Multicollinearity test
vif(Count_Model)
vif(Amount_Model)

library(lmtest)
dwtest(Count_Model)                         # Durbin-Watson test of autocorrelation
dwtest(Amount_Model)

#' Note: The above models are incorrect, because of violation of the independence
#' assumption (correlated errors within ClientNum). The correct models should be 
#' as follows, even if these models may fail to estimate certain beta estimates

d$ClientNum <- factor(d$ClientNum)
Count_Model <- lm(log(TransCount) ~ Month + ClientNum + WealthTag + CardType + 
                    RevolvingIndicator + SpendCategory, data=d) 
Amount_Model <- lm(log(TransAmount) ~ Month + ClientNum + WealthTag + CardType + 
                     RevolvingIndicator + SpendCategory, data=d) 

