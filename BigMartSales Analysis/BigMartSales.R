#U03922317 Simran
rm(list=ls())
library(readxl)
df <- read_excel("C:/Users/simra/Downloads/BigMartSales.xlsx", sheet="Data")
colnames(df)=tolower(make.names(colnames(df)))
str(df) 

#checking Dv's distribution

summary(df$item_sales)

library(ggplot2)

p = ggplot(df, aes(x=item_sales)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(item_sales)),
           color="blue", linetype="dashed", lwd=1)
p

p_log = ggplot(df, aes(x=log(item_sales))) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(log(item_sales))),
             color="blue", linetype="dashed", lwd=1)
p_log

#converting all chr variables to factors

colSums(is.na(df)) 
str(df)
df$outlet_id <- factor(df$outlet_id)
df$outlet_size <- factor(df$outlet_size)
df$city_type <- factor(df$city_type)
df$outlet_size <- replace(df$outlet_size, is.na(df$outlet_size), "Small")
df$outlet_type <- factor(df$outlet_type)
df$item_sales <- trunc(df$item_sales)

str(df)

#checking correlation
library(corrplot)
df_num=df[,c(4,6,12)]
cor_df_num <- cor(df_num)
corrplot(cor_df_num, type = "lower" , method = "number")

attach(df)

base <- glm(item_sales ~ item_visibility + item_mrp  + outlet_type + city_type
            + outlet_id + outlet_size , data = df ,family=poisson(link=log))
summary(base)

library(AER)
dispersiontest(base) 

library(lme4)
library(lmtest)
m1 <- glmer (item_sales ~ item_visibility + outlet_size + item_mrp + city_type + (1|outlet_id) + (1|outlet_type), data = df,family=poisson(link=log))
summary(m1)
ranef(m1)
vif(m1)
durbinWatsonTest(m1)

library(MASS)
m2 <- glmer.nb(item_sales ~ item_visibility + outlet_size + item_mrp + city_type + ( 1 | outlet_type) + (1 | outlet_id) ,data = df)
summary(m2)
ranef(m2)
vif(m2)

library(stargazer)
stargazer(base,m1,m2, type="text", single.row=TRUE)

durbinWatsonTest(m2)
durbinWatsonTest(resid(m2))
