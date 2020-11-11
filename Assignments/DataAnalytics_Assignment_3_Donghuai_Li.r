setwd("C:/Users/donal/OneDrive/RPI/2020 Fall/Data Analytic")
library(readr)
library(ggplot2)
library(dplyr)
nyt27 <- read_csv("dds_ch2_nyt/nyt27.csv")
nyt28 <- read_csv("dds_ch2_nyt/nyt28.csv")
nyt29 <- read_csv("dds_ch2_nyt/nyt29.csv")
nyt30 <- read_csv("dds_ch2_nyt/nyt30.csv")
nyt31 <- read_csv("dds_ch2_nyt/nyt31.csv")

dfs=c('nyt27','nyt28','nyt29','nyt30','nyt31')
boxplot(nyt27$Impressions,nyt28$Impressions,nyt29$Impressions,nyt30$Impressions,nyt31$Impressions,names=dfs,main='Box plot of Impressions')
boxplot(nyt27$Age,nyt28$Age,nyt29$Age,nyt30$Age,nyt31$Age,names=dfs,main='Box plot of age')

hist(nyt27$Age,main = 'Histogram of Ages in nyt27')
hist(nyt28$Age,main = 'Histogram of Ages in nyt28')
hist(nyt29$Age,main = 'Histogram of Ages in nyt29')
hist(nyt30$Age,main = 'Histogram of Ages in nyt30')
hist(nyt31$Age,main = 'Histogram of Ages in nyt31')


hist(nyt27$Impressions,main = 'Histogram of Impressions in nyt27')
hist(nyt28$Impressions,main = 'Histogram of Impressions in nyt28')
hist(nyt29$Impressions,main = 'Histogram of Impressions in nyt29')
hist(nyt30$Impressions,main = 'Histogram of Impressions in nyt30')
hist(nyt31$Impressions,main = 'Histogram of Impressions in nyt31')
plot(ecdf(nyt27$Age))

plot(ecdf(nyt27$Age),main='ECDF of Age in nyt27')
plot(ecdf(nyt28$Age),main='ECDF of Age in nyt28')
plot(ecdf(nyt29$Age),main='ECDF of Age in nyt29')
plot(ecdf(nyt30$Age),main='ECDF of Age in nyt30')
plot(ecdf(nyt31$Age),main='ECDF of Age in nyt31')
library(car)
help(qqPlot)
qqPlot(nyt27$Age,main='QQ plot of age in nyt27')
qqPlot(nyt28$Age,main='QQ plot of age in nyt28')
qqPlot(nyt29$Age,main='QQ plot of age in nyt29')
qqPlot(nyt30$Age,main='QQ plot of age in nyt30')
qqPlot(nyt31$Age,main='QQ plot of age in nyt31')

plot(ecdf(nyt27$Impressions),main='ECDF of Impressions in nyt27')
plot(ecdf(nyt28$Impressions),main='ECDF of Impressions in nyt28')
plot(ecdf(nyt29$Impressions),main='ECDF of Impressions in nyt29')
plot(ecdf(nyt30$Impressions),main='ECDF of Impressions in nyt30')
plot(ecdf(nyt31$Impressions),main='ECDF of Impressions in nyt31')

qqPlot(nyt27$Impressions,main='QQ plot of Impressions in nyt27')
qqPlot(nyt28$Impressions,main='QQ plot of Impressions in nyt28')
qqPlot(nyt29$Impressions,main='QQ plot of Impressions in nyt29')
qqPlot(nyt30$Impressions,main='QQ plot of Impressions in nyt30')
qqPlot(nyt31$Impressions,main='QQ plot of Impressions in nyt31')



shapiro.test(sample(nyt27$Age,30))
shapiro.test(sample(nyt28$Age,30))
shapiro.test(sample(nyt29$Age,30))
shapiro.test(sample(nyt30$Age,30))
shapiro.test(sample(nyt31$Age,30))

boxplot(nyt27$Signed_In)
table(rbind(nyt27$Signed_In,nyt27$Signed_In))
gender_signed_in=table(nyt27$Gender,nyt27$Signed_In)
colnames(gender_signed_in)=c('not_signed_in','signed_in')
rownames(gender_signed_in)=c('Male','Female')
gender_signed_in

table(nyt28$Gender,nyt28$Signed_In)
table(nyt27$Age,nyt27$Signed_In)
table(nyt27$Clicks,nyt27$Signed_In)
ggplot()

table(nyt27$Impressions,nyt27$Signed_In)

nyt27 = nyt27 %>% filter(Signed_In==1)
nyt28 = nyt28 %>% filter(Signed_In==1)

hist(nyt27$Age,main = 'Histogram of Ages in nyt27')
hist(nyt28$Age,main = 'Histogram of Ages in nyt28')
hist(nyt27$Impressions,main = 'Histogram of Impressions in nyt27')
hist(nyt28$Impressions,main = 'Histogram of Impressions in nyt28')
plot(ecdf(nyt27$Age),main='ECDF of Age in nyt27')
plot(ecdf(nyt28$Age),main='ECDF of Age in nyt28')
plot(ecdf(nyt27$Impressions),main='ECDF of Impressions in nyt27')
plot(ecdf(nyt28$Impressions),main='ECDF of Impressions in nyt28')

shapiro.test(sample(nyt27$Age,30))
shapiro.test(sample(nyt28$Age,30))
