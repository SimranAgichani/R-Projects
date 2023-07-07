
#Simran Agichani

rm(list=ls())
library(rio)



df=import("HRanalytics.xlsx",sheet='Data')
colnames(df)=tolower(make.names(colnames(df)))
df$dateofhire <- as.Date(df$dateofhire, origin = "1899-12-30")
df$dateoftermination <- as.Date(df$dateoftermination, origin = "1899-12-30")
df$dob <- as.Date(df$dob, origin = "1899-12-30")
df$lastperformancereview_date <- as.Date(df$lastperformancereview_date, origin = "1899-12-30")
df$empdays <- ifelse(is.na(df$dateoftermination),as.numeric(difftime(Sys.Date(), df$dateofhire, units = "days")), as.numeric(difftime(df$dateoftermination, df$dateofhire, units = "days")))
df[, c("position", "positionid",'maritaldesc','department','managername','recruitmentsource','performancescore','state')] <- lapply(df[,  c("position", "positionid",'maritaldesc','department','managername','recruitmentsource','performancescore','state')], factor)

#hist perf
hist(df$perfscoreid, main = NULL)
table(df$perfscoreid)


#satisfaction
hist(df$empsatisfaction, main = NULL) #categorical hence histogram doesn't tell anything
table(df$empsatisfaction)

#absence
hist(df$absences,main = NULL)
hist(log(df$absences),main=NULL) #doesn't seem to be normally distributed

#corrplot
df$empid<- NULL
df$employee_name <- NULL
df$managerid <- NULL 
df$termreason <- NULL
df$deptid<- NULL
df$marriedid<- NULL
df$maritalstatusid<- NULL
df$genderid<- NULL
df$empstatusid<- NULL
df$fromdiversityjobfairid<- NULL
df$positionid <- NULL
df$zip <- NULL
df$maritaldesc <- relevel(df$maritaldesc, ref = "Single")

library(corrplot)
df_corr <- df[, sapply(df, is.numeric)]

correlations <- cor(df_corr)

corrplot(correlations, method = "number", type = "lower")



#Q3: Probabilities of continuing the employment after 5 years

attach(df)
#install.packages("survival")
library(survival)
km <- Surv(df$empdays, df$termd)
prob <- survfit(km~department, data=df)
summary(prob,times = 1825,single.row=TRUE)

#Kaplan Meier Curve 

par(mar = c(2, 2, 2, 2))
plot(prob,col=c("red","blue","cyan","brown","black","orange"))


#Q1: tobit models for perf, satisf as values are censored 
library(AER)
t.perf = tobit(perfscoreid ~ salary + employmentstatus + department + managername + recruitmentsource 
               + engagementsurvey+ empsatisfaction+ dayslatelast30, left=0, right=5, data=df)
summary(t.perf)

 
#assumptions
plot(residuals(t.perf),pch=19,cex=0.8, ylab="residuals")

#qqplots
qqnorm(residuals(t.perf),pch=19,cex=0.8)
qqline(residuals(t.perf),lwd=2)


library(lmtest)
resid.perf <- residuals(t.perf)
durbinWatsonTest(resid.perf)

vif(t.perf)


#for empsatisfaction
t.satisf = tobit(empsatisfaction ~ perfscoreid + salary  + sex + maritaldesc + citizendesc+ employmentstatus 
                 + department + managername + recruitmentsource + engagementsurvey+ specialprojectscount + absences, left=0, right=6, data=df)
summary(t.satisf)

plot(residuals(t.satisf),pch=19,cex=0.8, ylab="residuals")

qqnorm(residuals(t.satisf),pch=19,cex=0.8)
qqline(residuals(t.satisf),lwd=2)

resid.satisf <- residuals(t.satisf)
durbinWatsonTest(resid.satisf)

#multicoleanirity
vif(t.satisf)

#absence DV

abs = glm(absences ~ perfscoreid+ salary + maritaldesc +managername
          + department +  engagementsurvey  + specialprojectscount  ,data = df ,family = "poisson" (link = log))
summary(abs)

dispersiontest(abs)

vif(abs)

resid_abs <- residuals(abs)
durbinWatsonTest(resid_abs)

AIC(t.perf, t.satisf, abs)

library(stargazer)
stargazer(t.perf, t.satisf, abs, type='text', single.row=TRUE)

#Q4 salary
hist(df$salary,main = NULL)
hist(log(df$salary),main=NULL)

df[, c("perfscoreid", "empsatisfaction")] <- lapply(df[,  c("perfscoreid", "empsatisfaction")], factor)

sal = lm(log(salary) ~ sex*department +  racedesc*department + sex*performancescore + sex*empsatisfaction +  performancescore*department + empsatisfaction*department + maritaldesc  + 
           specialprojectscount +empdays ,data = df)
summary(sal)



