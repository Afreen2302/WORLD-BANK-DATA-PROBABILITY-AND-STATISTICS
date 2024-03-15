install.packages("WDI")
library(tidyverse)  
library(WDI)        
data=WDI(indicator = c(elec="EG.ELC.ACCS.ZS", # access to electricity
                       cab="BN.CAB.XOKA.GD.ZS", # current account balance
                       edb="IC.BUS.DFRN.XQ", # ease of doing business
                       cpi="FP.CPI.TOTL.ZG", # CPI
                       rate="FR.INR.LNDP"), # interest rate spread
         start = 1960, end = 2020) %>% as_tibble()
summary(data)#provides descriptive statistics
str(data)#structure of data set
mean(data$elec,na.rm=TRUE)
median(data$cab,na.rm=TRUE)
mode(data$edb) 

# Extract data related to Kuwait
kuwait=subset(data,data$elec=="Kuwait",na.rm=TRUE)
kuwait
# Extract data related to Tuvalu
tuvalu=subset(data, data$elec=="Tuvalu",na.rm=TRUE)
tuvalu
# Creating table (one-way)
table1=table(data$elec)
table1
table2=table(data$cab)
table2
# Creating table (two-way)
table3=table(data$elec, data$cab)
table3
# Graphical representation (scatter plot)
plot(data$elec,type="p",main="Access to electricity (% of population)",xlab="x-axis",ylab="% of population",col="red")
# Graphical representation (Pie chart)
pie(table1)
# Graphical representation (Box plot)
boxplot(data$elec~data$cab,col=c('red','blue'))

# Correlation and Regression
#Variance
v1=var(data$elec,na.rm=TRUE); v1
v2=var(data$cab,na.rm=TRUE); v2
s11=sqrt(v1); s11
s21=sqrt(v2); s21
s1=sd(data$elec,na.rm=TRUE);s1
s2=sd(data$cab,na.rm=TRUE);s2
corr=cor(data$elec,data$cab); corr
# Covariance between "elec" and "cab"
covariance=cov(data$elec,data$cab); covariance
r=corr/(s1&s2);r # Karl pearson's coefficient
cd=corr*corr; cd
# Visualize the samples
plot(data$elec,data$cab)
regression=lm(data$elec~data$cab); regression
abline(regression)
summary(regression)

# Multiple regression
x=data$elec; x
y=data$cab; y
z=data$edb; z
reg1=lm(z~x+y); reg1
summary(reg1);
library(scatterplot3d)
graph=scatterplot3d(x,y,z)
graph$plane3d(reg1) 

#Binomial Distribution
n=13
p=nrow(data[data$elec == "Turkiye" & data$year == "2000",])/nrow(data[data$elec == "Turkiye",]);p
dbinom(6,n,p)
#PMF
x = 0:n
pmf = dbinom(x,n,p);pmf
plot(x,pmf,main="Probability mass function");
pbinom(9,n,p);
#CDF
cdf = pbinom(x,n,p);cdf
plot(x,cdf,type = "s",main = "CDF")
mu = n*p;mu
var = n*p*(1-p);var
sd = sqrt(var); sd

#Poisson Distribution-X
n=20;n
ps=nrow(data[data$edb == "Afghanistan" & data$year == "2014",])/nrow(data[data$edb == "Afghanistan",]);ps
lambda=n*ps;lambda
xn=0:n
pxn=dpois(xn,lambda);pxn
plot(xn,pxn,type="p",xlab="Values of x",ylab="Probability distribution of x",main="Poisson Distribution")
Ex=weighted.mean(xn,pxn);Ex
Var=weighted.mean(xn*xn,pxn)-Ex^2;Var

# Hypothesis Testing
mu = nrow(data[data$cpi == "Afghanistan" & data$year == "2014",])/nrow(data[data$cpi == "Afghanistan",]);mu
n = 35;
x_bar = nrow(data[data$cpi == "Turkiye" & data$year == "2000",])/nrow(data[data$cpi == "Turkiye",]);x_bar
sig = 2.5;
alpha = 0.05;
z = (x_bar-mu)/(sig/sqrt(n));z; #test statistic
#two tailed critical value
zhalfalpha = qnorm(1-(alpha/2));zhalfalpha;
#qnorm takes the cumulative probability and gives the corresponding z-value
c(-zhalfalpha,zhalfalpha); #vector representation
#comparison
if(-(zhalfalpha)<z | z<zhalfalpha){print("Accept Null hypothesis")}else{print("Reject Null hypothesis")}


