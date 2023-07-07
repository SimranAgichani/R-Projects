
rm(list=ls())

#reloading the existing libraries
library("rio")
library("corrplot")
library(car)
#library(tidyr)
#library(GGally)
library(MASS)
library(lmtest)
#library(pscl)
#library(AER)
library(stargazer)
library(readxl)


setwd("C:/Users/simra/Downloads")
#loading store data
sheet1 <- read_excel("SnackChain.xlsx", sheet = "stores")
colnames(sheet1)=tolower(make.names(colnames(sheet1)))
#finding duplicate values
dup <- subset(sheet1, duplicated(sheet1[c("store_id", "store_name")]) | duplicated(sheet1[c("store_id", "store_name")], fromLast = TRUE))
dup
#considering only mainstream values based on mean of avg_basket_size for both segments
sheet1 <- sheet1[!(sheet1$segment=='UPSCALE' & sheet1$store_name=='FLOWER MOUND'), ]
sheet1 <- sheet1[!(sheet1$segment=='UPSCALE' & sheet1$store_name=='ROCKWALL'), ]
nrow(sheet1)
#loading products data
sheet2 <- read_excel("SnackChain.xlsx", sheet = "products")
colnames(sheet2)=tolower(make.names(colnames(sheet2)))
#reading transaction data
sheet3 <- read_excel("SnackChain.xlsx", sheet = "transactions")
colnames(sheet3)[colnames(sheet3) == "STORE_NUM"] ="STORE_ID" #making the column names same for joining the data
colnames(sheet3)=tolower(make.names(colnames(sheet3)))

# Join on upc
t1 <- merge(sheet3, sheet2, by = "upc")

# Join on store_id
df <- merge(t1, sheet1, by = "store_id")
df <- df[df$category != "ORAL HYGIENE PRODUCTS", ] #removing oral hygiene products as they are out of scope

library(openxlsx)
#making a new file for analysis
write.xlsx(df, file = "SnackChain_merge.xlsx", sheetName = "Sheet1", rowNames = FALSE)

# missing values

colSums(is.na(df)) 
which(is.na(df$price))
df$price<-ifelse(is.na(df$price),df$spend/df$units,df$price)
colSums(is.na(df)) 
df$price<-ifelse(df$spend=='0'&df$units=='0',df$base_price,df$price)
colSums(is.na(df)) 

#on exploring the data 
#found that tpr=0 for all missing values of baseprice
# therefore replacing it with price as there are no promos
count <- sum(is.na(df$base_price) & df$tpr_only=='0')
count
df$base_price<-ifelse(is.na(df$base_price),df$price,df$base_price)
colSums(is.na(df))

#zeroes in spend
num_zeroes <- sum(df$spend == 0)
num_zeroes

#multilevel data models
df<-df[df$spend!=0,] #removing zeroes in spend because lmer 
sum(df$spend == 0)


#convert to as factor :

df$upc <- as.factor(df$upc)
df$store_id <- as.factor(df$store_id)
df$category <- as.factor(df$category)
df$description <- as.factor(df$description)
df$manufacturer <- as.factor(df$manufacturer)
df$sub_category <- as.factor(df$sub_category)
df$city <- as.factor(df$city)
df$state <- as.factor(df$state)
df$msa <- as.factor(df$msa)
df$segment <- as.factor(df$segment)
df$store_name <- as.factor(df$store_name)


df$product_size <- as.numeric(gsub("[^0-9.]", "", df$product_size))
colnames(df)[2] <- "product_code" #for better understanding
colnames(df)[24] <- "avg_wb"
#corrplot
df$parking<-NULL
df_c <- df[, sapply(df, is.numeric)]
corr <- cor(df_c)
corrplot(corr,method="number",number.cex=0.75,t1.cex=0.5) #pie, ellipse, color,number,square, method="circle",type="upper"

#histogram of target variable : spend
par(mar = c(6, 5, 5, 3) + 0.1)

hist(df$spend,col="orange",main=NULL,
     probability = TRUE)
lines(density(df$spend),lwd=2,col="blue")

hist(log(df$spend),col="orange",main=NULL,
     probability = TRUE)
lines(density(log(df$spend)),lwd=2,col="blue")

#histogram of target variable : units

hist(df$units,col="orange",main=NULL,
     probability = TRUE)
lines(density(df$units),lwd=2,col="blue")

hist(log(df$units),col="orange",main=NULL,
     probability = TRUE)
lines(density(log(df$units)),lwd=2,col="blue")


#histogram of target variable : hhs
hist(df$hhs,col="orange",main=NULL,
     ylim=c(0,.03),probability = TRUE)
lines(density(df$hhs),lwd=2,col="blue")

hist(log(df$hhs),col="orange",main=NULL,
     probability = TRUE)
lines(density(log(df$hhs)),lwd=2,col="blue")


library(lattice)
bwplot(spend ~ category, data=df)
bwplot(spend ~ segment, data=df)

bwplot(log(spend) ~ category, data=df)
bwplot(log(spend) ~ segment, data=df)


##Units , visits ,hhs, spend highly corelated

#Q1

#spend
library(lme4)
m1_s = lmer(log(spend) ~ display*category + feature*category + tpr_only*category +
              display*segment + feature*segment + tpr_only*segment + manufacturer +
              base_price + product_size + ( 1 | state) + ( 1 | store_id ), data = df)

vif(m1_s)
#manufacturer is having high vif, hence dropping it

m2_s = lmer(log(spend) ~ display*category + feature*category + tpr_only*category +
              display*segment + feature*segment + tpr_only*segment +
              base_price + product_size + ( 1 | state) + ( 1 | store_id ), data = df)
summary(m2_s)
ranef(m2_s)
vif(m2_s)

#units

m1_u = lmer(log(units) ~ display + feature + tpr_only + base_price+ category +
              product_size + segment  + ( 1 | store_id ) + (1 | state), data = df)

summary(m1_u)
ranef(m1_u)
vif(m1_u)


#hhs

m1_h = lmer(log(hhs) ~ display + feature + tpr_only + base_price+ category +
              product_size + segment + ( 1 | store_id ) + (1 | state), data = df)

summary(m1_h)
ranef(m1_h)
vif(m1_h)


#stargazer
stargazer(m2_s, m1_u ,m1_h , title="DV's", type="text", single.row=TRUE)



#Q3

elas <- numeric() #empty vector to store elasticities

for (product in unique(df$product_code)) {
  
  subset <- df %>% filter(product_code == product)  # Subset data
  e1 <- glm(units ~ base_price, data = subset, family = poisson(link = log))
  coef <- coef(e1) # Extract coefficients and standard errors
  std_err <- summary(e1)$coefficients[,2]
  
  elasticity <- coef[2]*mean(subset$base_price)/mean(subset$units)
  elas <- c(elas, elasticity)
}

length(elas)

# Combine results into data frame
final_abs <- data.frame(Product = unique(df$product_code),
                      Elasticity = abs(elas))
final_abs_sorted <- final_abs[order(final_abs$Elasticity), ]
final_abs_sorted
final_abs_sorted_desc <- final_abs[order(final_abs$Elasticity,decreasing = TRUE), ]
final_abs_sorted_desc


# Sort by the absolute value of elasticity in ascending order
arranged <- final_abs[order(final_abs, decreasing = FALSE),]

head(arranged)
tail(arranged)

max_elas <- tail(arranged, 5)
max_elas

min_elas <- head(arranged, 5)
min_elas








