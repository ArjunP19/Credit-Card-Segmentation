#Clear the environment
rm(list = ls())

rm(library)

#Load the Libraries
library(DataCombine)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(magrittr)
library(corrplot)
library(corrgram)
library(psych)
library(GPArotation)
library(paran)
library(NbClust)

#Set Working directory
setwd("C:/Users/rnp/Desktop/Arjun/Data Science/Project3")


#Load the Credit card dataset

credit_df<- read.csv('credit-card-data.csv',header = T)

head(credit_df)
summary(credit_df)

View(credit_df)

dim(credit_df)

#Distribution of data attributes

hist_plot1 = ggplot(credit_df, aes(BALANCE))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of Balance")+theme(text = element_text(size = 10))  
hist_plot2 = ggplot(credit_df, aes(BALANCE_FREQUENCY))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of BALANCE_FREQUENCY")+theme(text = element_text(size = 10))  
hist_plot3 = ggplot(credit_df, aes(PURCHASES))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of PURCHASES")+theme(text = element_text(size = 10))  
hist_plot4 = ggplot(credit_df, aes(ONEOFF_PURCHASES))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of ONEOFF_PURCHASES")+theme(text = element_text(size = 10))  
hist_plot5 = ggplot(credit_df, aes(INSTALLMENTS_PURCHASES))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of INSTALLMENTS_PURCHASES")+theme(text = element_text(size = 10))  
hist_plot6 = ggplot(credit_df, aes(CASH_ADVANCE))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of CASH_ADVANCE")+theme(text = element_text(size = 10))  
hist_plot7 = ggplot(credit_df, aes(PURCHASES_FREQUENCY))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of PURCHASES_FREQUENCY")+theme(text = element_text(size = 10))  
hist_plot8 = ggplot(credit_df, aes(ONEOFF_PURCHASES_FREQUENCY))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of ONEOFF_PURCHASES_FREQUENCY")+theme(text = element_text(size = 10))  
hist_plot9 = ggplot(credit_df, aes(PURCHASES_INSTALLMENTS_FREQUENCY))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of PURCHASES_INSTALLMENTS_FREQUENCY")+theme(text = element_text(size = 10))  
hist_plot10 = ggplot(credit_df, aes(CASH_ADVANCE_FREQUENCY))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of CASH_ADVANCE_FREQUENCY")+theme(text = element_text(size = 10))  
hist_plot11 = ggplot(credit_df, aes(CASH_ADVANCE_TRX))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of CASH_ADVANCE_TRX")+theme(text = element_text(size = 10))  
hist_plot12 = ggplot(credit_df, aes(PURCHASES_TRX))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of PURCHASES_TRX")+theme(text = element_text(size = 10))  
hist_plot13 = ggplot(credit_df, aes(CREDIT_LIMIT))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of CREDIT_LIMIT")+theme(text = element_text(size = 10))  
hist_plot14 = ggplot(credit_df, aes(MINIMUM_PAYMENTS))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of MINIMUM_PAYMENTS")+theme(text = element_text(size = 10))  
hist_plot15 = ggplot(credit_df, aes(PRC_FULL_PAYMENT))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of PRC_FULL_PAYMENT")+theme(text = element_text(size = 10))  
hist_plot16 = ggplot(credit_df, aes(TENURE))+theme_bw()+geom_histogram(fill='blue', bins = 20)+ggtitle("Distribution of TENURE")+theme(text = element_text(size = 10)) 

gridExtra::grid.arrange(hist_plot1,hist_plot2,hist_plot3,hist_plot4,hist_plot5,hist_plot6,hist_plot7,hist_plot8,hist_plot9,
                        hist_plot10,hist_plot11,hist_plot12,hist_plot13,hist_plot14,hist_plot15,hist_plot16,nrow=4,ncol=4)

# Find Missing values
missing_value<-data.frame(missing_value=apply(credit_df,2,function(x){sum(is.na(x))}))
missing_value

#impute missing values with mean
credit_df$CREDIT_LIMIT[is.na(credit_df$CREDIT_LIMIT)] <- mean(credit_df$CREDIT_LIMIT, na.rm = TRUE)
credit_df$MINIMUM_PAYMENTS[is.na(credit_df$MINIMUM_PAYMENTS)] <- mean(credit_df$MINIMUM_PAYMENTS, na.rm = TRUE)


# Check for Missing values after imputation
missing_value<-data.frame(missing_value=apply(credit_df,2,function(x){sum(is.na(x))}))
missing_value

#******** Outlier detection  ********#
box_plot1 = ggplot(aes_string(y = credit_df$BALANCE), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$BALANCE)+ggtitle(paste("Box plot for Balance"))

box_plot2 = ggplot(aes_string(y = credit_df$BALANCE_FREQUENCY), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$BALANCE_FREQUENCY)+ggtitle(paste("Box plot for BALANCE_FREQUENCY"))

box_plot3 = ggplot(aes_string(y = credit_df$PURCHASES), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$PURCHASES)+ggtitle(paste("Box plot for PURCHASES"))

box_plot4 = ggplot(aes_string(y = credit_df$ONEOFF_PURCHASES), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$ONEOFF_PURCHASES)+ggtitle(paste("Box plot for ONEOFF_PURCHASES"))

box_plot5 = ggplot(aes_string(y = credit_df$INSTALLMENTS_PURCHASES), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$INSTALLMENTS_PURCHASES)+ggtitle(paste("Box plot for INSTALLMENTS_PURCHASES"))

box_plot6 = ggplot(aes_string(y = credit_df$CASH_ADVANCE), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$CASH_ADVANCE)+ggtitle(paste("Box plot for CASH_ADVANCE"))

box_plot7 = ggplot(aes_string(y = credit_df$PURCHASES_FREQUENCY), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$PURCHASES_FREQUENCY)+ggtitle(paste("Box plot for PURCHASES_FREQUENCY"))

box_plot8 = ggplot(aes_string(y = credit_df$ONEOFF_PURCHASES_FREQUENCY), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$ONEOFF_PURCHASES_FREQUENCY)+ggtitle(paste("Box plot for ONEOFF_PURCHASES_FREQUENCY"))

box_plot9 = ggplot(aes_string(y = credit_df$PURCHASES_INSTALLMENTS_FREQUENCY), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$PURCHASES_INSTALLMENTS_FREQUENCY)+ggtitle(paste("Box plot for PURCHASES_INSTALLMENTS_FREQUENCY"))

box_plot10 = ggplot(aes_string(y = credit_df$CASH_ADVANCE_FREQUENCY), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$CASH_ADVANCE_FREQUENCY)+ggtitle(paste("Box plot for CASH_ADVANCE_FREQUENCY"))

box_plot11 = ggplot(aes_string(y = credit_df$CASH_ADVANCE_TRX), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$CASH_ADVANCE_TRX)+ggtitle(paste("Box plot for CASH_ADVANCE_TRX"))

box_plot12 = ggplot(aes_string(y = credit_df$PURCHASES_TRX), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$PURCHASES_TRX)+ggtitle(paste("Box plot for PURCHASES_TRX"))

box_plot13 = ggplot(aes_string(y = credit_df$CREDIT_LIMIT), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$CREDIT_LIMIT)+ggtitle(paste("Box plot for CREDIT_LIMIT"))

box_plot14 = ggplot(aes_string(y = credit_df$PAYMENTS), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$PAYMENTS)+ggtitle(paste("Box plot for PAYMENTS"))

box_plot15 = ggplot(aes_string(y = credit_df$MINIMUM_PAYMENTS), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$MINIMUM_PAYMENTS)+ggtitle(paste("Box plot for MINIMUM_PAYMENTS"))

box_plot16 = ggplot(aes_string(y = credit_df$PRC_FULL_PAYMENT), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$PRC_FULL_PAYMENT)+ggtitle(paste("Box plot for PRC_FULL_PAYMENT"))

box_plot17 = ggplot(aes_string(y = credit_df$TENURE), data = credit_df)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,outlier.size=1, notch=FALSE) + theme(legend.position="bottom")+labs(y=credit_df$TENURE)+ggtitle(paste("Box plot for TENURE"))


gridExtra::grid.arrange(box_plot1,box_plot2,box_plot3,box_plot4,nrow=2,ncol=2)
gridExtra::grid.arrange(box_plot5,box_plot6,box_plot7,box_plot8,nrow=2,ncol=2)
gridExtra::grid.arrange(box_plot9,box_plot10,box_plot11,box_plot12,nrow=2,ncol=2)
gridExtra::grid.arrange(box_plot13,box_plot14,box_plot15,box_plot16,box_plot17,nrow=3,ncol=2)


#Advance KPI's

#1.monthly average purchase and cash advance amount
credit_df$MONTHLY_AVG_PURCHASE <- credit_df$PURCHASES/credit_df$TENURE
credit_df$CASH_ADV_AMOUNT <- credit_df$CASH_ADVANCE/credit_df$TENURE

#2.Purchased by Type (one-off, instalments)
### 4 types of purchase behaviour has been seen
###### 1. People not done both type of purchases (one-off, instalments)
###### 2. People done both type of purchases (one-off, instalments)
###### 3. People done only one-off payment type of purchases
###### 4. People done only installments payment type of purchases

purchase <- function(one_purchase,instal_purchase){
    if ((one_purchase == 0) && (instal_purchase == 0)) {
    return ('p_none')
  } else if ((one_purchase > 0) && (instal_purchase > 0)) {
    return ('p_both') 
  } else if ((one_purchase > 0) && (instal_purchase == 0)) {
    return ('p_oneoff_installment')
  } else if ((one_purchase == 0) &&  (instal_purchase > 0)) {
    return ('p_installment')
  }
}

one_purchase = credit_df$ONEOFF_PURCHASES
instal_purchase = credit_df$INSTALLMENTS_PURCHASES

credit_df$PURCHASE_TYPE <- mapply(purchase, one_purchase,instal_purchase)
credit_df$PURCHASE_TYPE

table(credit_df$PURCHASE_TYPE)

#3.Limit usage (balance to credit limit ratio in percentage)

credit_df$LIMIT_USAGE= (credit_df$BALANCE/credit_df$CREDIT_LIMIT)*100
credit_df$LIMIT_USAGE


#4.payments to minimum payments ratio in percentage

credit_df$MIN_PAYMENT=(credit_df$PAYMENTS/credit_df$MINIMUM_PAYMENTS)*100
credit_df$MIN_PAYMENT

#5 Minimum payment ratio

credit_df$MIN_PAYMENTS_RATIO <- credit_df$PAYMENTS/credit_df$MINIMUM_PAYMENTS
credit_df$MIN_PAYMENTS_RATIO


####Insights on new KPI

#Plot graph on Purchase types for counts
bar_plot = ggplot(credit_df, aes(PURCHASE_TYPE))+theme_bw()+geom_bar(fill='blue')+ggtitle("Total counts by Purchase Type")+theme(text = element_text(size = 10))  
plot(bar_plot)


#Average cash advance taken by customers on different Purchase type

credit_df %>% group_by(PURCHASE_TYPE) %>% summarise(mean_cash_adv=mean(CASH_ADV_AMOUNT))

# Output
#### A tibble: 4 x 2
#### PURCHASE_TYPE        mean_cash_adv
#### <chr>                        <dbl>
#### 1 p_both                      67.8
#### 2 p_installment               38.4
#### 3 p_none                      183. 
#### 4 p_oneoff_installment        79.0

#Plot a graph
credit_df %>% 
group_by(PURCHASE_TYPE) %>% 
summarise(mean_cash_adv=mean(CASH_ADV_AMOUNT)) %>%

ggplot(aes(x = PURCHASE_TYPE, y = mean_cash_adv, fill = PURCHASE_TYPE)) + geom_bar(stat = "identity") + theme_classic() +
  labs(x = "Purchase Type", y = "Mean cash advance amount", title = paste("Mean cash advance amount summarise by Purchase Type"))



#Customer average Limit usage vs Purchase type

credit_df %>% group_by(PURCHASE_TYPE) %>% summarise(mean_cash_adv=mean(LIMIT_USAGE))

# Output
#### A tibble: 4 x 2
#### PURCHASE_TYPE        mean_cash_adv
#### <chr>                        <dbl>
#### 1 p_both                        35.4
#### 2 p_installment                 27.2
#### 3 p_none                        57.4
#### 4 p_oneoff_installment          38.1

#Plot a graph
credit_df %>% 
  group_by(PURCHASE_TYPE) %>% 
  summarise(mean_limit_usage=mean(LIMIT_USAGE)) %>%
  
  ggplot(aes(x = PURCHASE_TYPE, y = mean_limit_usage, fill = PURCHASE_TYPE)) + geom_bar(stat = "identity") + theme_classic() +
  labs(x = "Purchase Type", y = "Mean Limit usage", title = paste("Mean Limit usage summarise by Purchase Type"))




# Identifying Outliers
mystats <- function(x) {
  missing_val<-sum(is.na(x))
  val <- x[!is.na(x)]
  mn <- mean(val)
  len <- length(val)
  std <- sd(val)
  min <- min(val)
  p1<-quantile(val,0.01)
  q1<-quantile(val,0.25)
  q2<-quantile(val,0.5)
  q3<-quantile(val,0.75)
  p90<-quantile(val,0.90)
  p95<-quantile(val,0.95)
  p99<-quantile(val,0.99)
  max <- max(val)
  UC <- mn+2*std
  LC <- mn-2*std
  outlier_flag<- max>UC | min<LC
  return(c(n=len, missing_val=missing_val, outlier_flag=outlier_flag, mean=mn, stdev=std, min = min, p1=p1,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

colnames(credit_df)

DS_Vars <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "PRC_FULL_PAYMENT",
  "TENURE",
  "MONTHLY_AVG_PURCHASE",
  "CASH_ADV_AMOUNT",
  "LIMIT_USAGE",
  "MIN_PAYMENT",
  "MIN_PAYMENTS_RATIO"
  )


Outliers<-t(data.frame(apply(credit_df[DS_Vars], 2, mystats)))
View(Outliers)


# Outlier Treatment
credit_df$BALANCE[credit_df$BALANCE>5727.539]<-5727.539
credit_df$BALANCE_FREQUENCY[credit_df$BALANCE_FREQUENCY>1.351079]<-1.351079
credit_df$PURCHASES[credit_df$PURCHASES>5276.474]<-5276.474
credit_df$ONEOFF_PURCHASES[credit_df$ONEOFF_PURCHASES>3912.213]<-3912.213
credit_df$INSTALLMENTS_PURCHASES[credit_df$INSTALLMENTS_PURCHASES>2219.744]<-2219.744
credit_df$CASH_ADVANCE[credit_df$CASH_ADVANCE>5173.199]<-5173.199
credit_df$PURCHASES_FREQUENCY[credit_df$PURCHASES_FREQUENCY>1.293092]<-1.293092
credit_df$ONEOFF_PURCHASES_FREQUENCY[credit_df$ONEOFF_PURCHASES_FREQUENCY>0.7991298]<-0.7991298
credit_df$PURCHASES_INSTALLMENTS_FREQUENCY[credit_df$PURCHASES_INSTALLMENTS_FREQUENCY>1.159333]<-1.159333
credit_df$CASH_ADVANCE_FREQUENCY[credit_df$CASH_ADVANCE_FREQUENCY>0.535387]<-0.535387
credit_df$CASH_ADVANCE_TRX[credit_df$CASH_ADVANCE_TRX>16.89812]<-16.89812
credit_df$PURCHASES_TRX[credit_df$PURCHASES_TRX>64.42513]<-64.42513
credit_df$CREDIT_LIMIT[credit_df$CREDIT_LIMIT>11771.67]<-11771.67
credit_df$PAYMENTS[credit_df$PAYMENTS>7523.271]<-7523.271
credit_df$MINIMUM_PAYMENTS[credit_df$MINIMUM_PAYMENTS>5525.383]<-5525.383
credit_df$PRC_FULL_PAYMENT[credit_df$PRC_FULL_PAYMENT>0.738713]<-0.738713
credit_df$TENURE[credit_df$TENURE>14.19398]<-14.19398
credit_df$MONTHLY_AVG_PURCHASE[credit_df$MONTHLY_AVG_PURCHASE>447.1927] <- 447.1927
credit_df$CASH_ADV_AMOUNT[credit_df$CASH_ADV_AMOUNT>475.2502] <- 475.2502
credit_df$LIMIT_USAGE[credit_df$LIMIT_USAGE>116.8327] <- 116.8327
credit_df$MIN_PAYMENT[credit_df$MIN_PAYMENT>24538.99] <- 24538.99
credit_df$MIN_PAYMENTS_RATIO[credit_df$MIN_PAYMENTS_RATIO>245.3899] <- 245.3899



#Check for collinearity using correlation graph
corrgram(credit_df, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#computes the correlation coefficient.
Col_nums <- credit_df[DS_Vars]
comp_corr<- cor(Col_nums)    
View(comp_corr)


#Parallel Analysis Scree Plot
parallel <- fa.parallel(comp_corr, fm = 'minres', fa = 'fa')

scree(comp_corr, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE)

#Factor Analysis

FA_5 <- fa(r=comp_corr, 5, rotate = "Varimax" , fm="minres")

#View the values
FA_SORT<-fa.sort(FA_5)      
FA_SORT$loadings


#Standardizing the data
credit_scaled <- credit_df[DS_Vars]
credit_scaled <- data.frame(scale(credit_scaled))

head(credit_scaled)


#Extract number of cluster to build

#NBclsut_ext <- NbClust(comp_corr, min.nc = 5, max.nc = 8, method = "kmeans")


#Bulid the model with K-mean clustering
#With no. of cluster 4,5,6,7

Kmean_4 <- kmeans(credit_scaled, 4, nstart = 20)
Kmean_5 <- kmeans(credit_scaled, 5, nstart = 20)
Kmean_6 <- kmeans(credit_scaled, 6, nstart = 20)
Kmean_7 <- kmeans(credit_scaled, 7, nstart = 20)

#summarise the cluster data
Kmean_4
Kmean_5
Kmean_6
Kmean_7


####Marketing Strategy Suggested:
  
####1.Group with both type of purchases . They are potential target customers who are paying dues and doing purchases and maintaining comparatively good credit score ) -- we can increase credit limit or can lower down interest rate -- Can be given premium card /loyality cards to increase transactions

####2.Group not done both type of purchases . They have poor credit score and taking only cash on advance. We can target them by providing less interest rate on purchase transaction

####3.Group done both type of purchases . This group is has minimum paying ratio and using card for just oneoff transactions (may be for utility bills only). This group seems to be risky group.

####4.Group done only installments payment type of purchases . This group is performing best among all as cutomers are maintaining good credit score and paying dues on time. -- Giving rewards point will make them perform more purchases.


