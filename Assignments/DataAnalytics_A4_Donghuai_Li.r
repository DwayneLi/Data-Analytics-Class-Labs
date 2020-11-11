library(readr)
library(readxl)
library(ggplot2)
setwd("C:/Users/donal/OneDrive/RPI/2020 Fall/Data Analytic/Assignments")
sales =  read_excel("rollingsales_brooklyn.xls",skip = 4)
df =  read_excel("rollingsales_brooklyn.xls",skip = 4)

# explore the distribution of variables.
summary(df)
unique(df$NEIGHBORHOOD)
unique(df$`TAX CLASS AT PRESENT`)
table(df$LOT)
table(df$`BUILDING CLASS AT PRESENT`)

ggplot(df,aes(x=`BUILDING CLASS AT PRESENT`))+geom_bar()

df$simplebuildingclass=gsub('[^A-Z]','',df$`BUILDING CLASS AT PRESENT`)
ggplot(df,aes(x=`simplebuildingclass`))+geom_bar()
df$simplebuildingclass=substring(df$`BUILDING CLASS AT PRESENT`,1,1)
ggplot(df,aes(x=`simplebuildingclass`))+geom_bar()
df$buildingclassnum=gsub('[^0-9]','',df$`BUILDING CLASS AT PRESENT`)
ggplot(df,aes(x=`buildingclassnum`))+geom_bar()


dfyear=filter(df,!is.na(df$`YEAR BUILT`))
ggplot(dfyear,aes(x=`YEAR BUILT`))+geom_histogram(binwidth = 1 )

sort(table(dfyear$`YEAR BUILT`))

dfyear=df[!is.na(df$`YEAR BUILT`) &df$`YEAR BUILT`>0,]

colSums(is.na(df))

summary(df$`EASE-MENT`)
df$`EASE-MENT`=NULL


sum(df$`LAND SQUARE FEET`==0)#8027
sum(df$`COMMERCIAL UNITS`==0)#20564
sum(df$`YEAR BUILT`==0)#3099
sum(df$`SALE
PRICE`==0)#8791
table(df$`ZIP CODE`)
as.Date.date(df$`SALE DATE`)
class(df$`SALE DATE`)
df$saledate=as.Date(df$`SALE DATE`)
df$saleyear=format(df$saledate,'%Y')
df$salemonth=format(df$saledate,'%m')

table(df$saleyear)

ggplot(dfyear,aes(x=`YEAR BUILT`))+geom_histogram(binwidth = 1 )
#select land square feet, sale year and sale month,built years to predict sale price.

df =  read_excel("rollingsales_brooklyn.xls",skip = 4)
sort(table(df$NEIGHBORHOOD))
sort(table(df$`BUILDING CLASS CATEGORY`))

df = df[,c(4,15,17,18,20,21)]
names(df)=c('taxclassnow','land','builtyear','taxclassbegin',
            'price','saledate')
#names(df)=c('taxclassnow','buildingclassnow','land','builtyear','taxclassbegin',  'buildingclassbegin','price','saledate')
qnt = quantile(df[df$price>0,]$price,probs=c(.01,.99))
qnt2 = quantile(df[df$land>0,]$land,probs=c(.01,.99))

df=df[df$builtyear>=1800 & df$price >qnt[1] & df$price<qnt[2]& df$land >qnt2[1]& df$land<qnt2[2],]
df$saledate=as.Date(df$saledate)
df$saleyear=format(df$saledate,'%Y')
df$salemonth=format(df$saledate,'%m')

#df$builtyeargroup = cut(df$`YEAR BUILT`,c(-1,1895,1930,1975,2000))

ggplot(df,aes(builtyear,price))+geom_line()
ggplot(df,aes(land,price))+geom_line()

max(df$price)
layout(matrix(c(1,1)))
boxplot(df$price)
barplot(df$land)
smp_size = floor(0.7 * nrow(df))

set.seed(2020)
train_ind = sample(seq_len(nrow(df)),size = smp_size)
train = df[train_ind,]
test = df[-train_ind,]
fit = lm(price ~ taxclassnow + taxclassbegin+ land + builtyear+saleyear+salemonth,data=train)


residuals(fit)
anova(fit)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)
summary(fit)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

pre = predict(fit,test[,-5])
RMSE(test[,5],pre)
qqplot(test[,5],pre)
test$pre=pre
ggplot(data=transform())
ggplot(test,aes(x=land,y=price))+geom_point()+geom_smooth(method="lm") + geom_line(aes(y = pre),size =1)
