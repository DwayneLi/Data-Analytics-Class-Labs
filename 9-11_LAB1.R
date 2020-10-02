setwd("C:/Users/donal/OneDrive/RPI/2020 Fall/Data Analytic")

#####Exercises-getting data in####
library(readr)

df=read_csv("GPW3_GRUMP_SummaryInformation_2010.csv")
View(df)

#####exploring the distribution####

EPI_data <- read_csv("EPI_data.csv")
View(EPI_data.csv)
attach(EPI_data)
EPI
tf=is.na(EPI)
summary(EPI)
summary(EPI_data)
hist(DALY)
#other data

GRUMP_data=read_csv("GPW3_GRUMP_SummaryInformation_2010.csv")
View(GRUMP_data)

#####Distribution####
plot(ecdf(EPI),do.points=F,verticals=T)
par(pty='s')
qqnorm(EPI)
qqline(EPI)

x=rt(250,df=5)
?rt
hist(x)
qqnorm(x)
qqline(x)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

pp=ppoints(250)
hist(pp)
?qt

help(distributions)
poi=dpois(3:20, lambda = 1)
hist(poi)
rug(poi)
poi
?dpois
?dbinom


bi=rbinom(100,20,0.5)
bi
mean(bi)
qqnorm(bi)
qqline(bi)

#####filter####
EPI[!Landlock]  #select EPI rows where exclude Landlock=1, which Landlock=0 and !Landlock=1,
#exclammation mark ! is the logical-NOT operator in R.
attach(EPI_data)
EPILand= EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
Eland
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=T)


#####SUMMARY####

summary(EPI) 	
fivenum(EPI,na.rm=TRUE)
help(stem)
stem(EPI)		 # stem and leaf plot
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.)) 
help(rug)
rug(EPI) 

EPI[!Desert]
EPI_regions
EPI_South_Asia=EPI[EPI_regions=="South Asia"]
EPI_South_Asia
hist(EPI_South_Asia)





