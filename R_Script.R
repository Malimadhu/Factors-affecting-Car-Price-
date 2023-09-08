

#Data set is about factors affecting car price
CarPrice <- read.delim("C:/Users/madhu/OneDrive/Desktop/ssi/CarPrice.txt")
View(CarPrice)
d=CarPrice
library(readxl)
d=read.table(file.choose(),header=T)
d
attach(d)
fix(d)
View(d)
head(d)
dim(d)
str(d)      #nature of variables
y=as.factor(d$price);
x1=as.factor(d$citympg);
x2=as.factor(d$highwaympg);
x3=as.factor(d$horsepower);
x4=as.factor(d$peakrpm);
x5=as.factor(d$compressionratio);
x6=as.factor(d$stroke);
x7=as.factor(d$boreratio);
x8=as.factor(d$enginesize);
x9=as.factor(d$curbweight);
x10=as.factor(d$carheight);
x11=as.factor(d$carlength);
x12=as.factor(d$wheelbase);
x13=as.factor(d$carwidth);
a=data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
m=as.matrix(d)
library(pastecs)
stat.desc(d)         #descriptive statistic parameters
n=cbind.data.frame(d$price,d$enginesize)
c=cor(n)
c
library(corrplot)
corr_plot=corrplot(c,method="circle")
#It has strong positive correlation as correlation between price and enginesize lies between 0.8 and 1(ie. 0.8741448).
#Multicollinearity is present
plot(d$price,d$enginesize)     #scatter plot between price and enginesize
#Interpretation:the scatter plot shows positive linear relationship betwwen price of car and the engine size of the car. 
#Fitting of SLRM with independent variable as price and dependent variable as enginesize.
model<-lm(d$price~d$enginesize)
model
#Estimates of parameters involved in SLRM : Intercept:-8005.4 & Slope: 167.7 
#Equation of fitted regression model is yij=-8005.4+ 167.7x+error
a<-summary(model)
a
#Testing significance at 5% los:-
cat("H0:Beta1=0\n Vs\n H1:Beta1!=0\n")
cat("H0:Beta0=0\n Vs\n H1:Beta0!=0\n")
#pvalue is approx <2.2e-16
if(2.2e-16<0.05)
{cat("Reject H0 of slope\n")}else
{cat("Accept H0 of slope\n")}
if(2.2e-16<0.05)
{cat("Reject H0 of intercept\n")}else
{cat("Accept H0 of intercept\n")}
#As slope and intercept both lie within CI.Also we reject H0 at 95% CI ie.there is linear relationship between price and enginesize.
#Interpretation from R- squared value:The adjusted R-squared value is  0.763. It means around 76.3% of variability in price is explained by enginesize.
ci=confint(model,level = 0.95)    #95% Confidence interval
ci
residuals(model)
rstandard(model)
par(mfrow=c(2,2))
plot(model)
#NTERPRETATIONS:
#1)Residual vs Fitted: Here we can see an outward opening funnel which indicate that assumption of constant variance is violated.
#2)Normal Q-Q: Flat extremes indicate that observations are from a distribution having tails thinner than normal distribution.
#3)Scale-Location:The plot shows that residuals are not spread equally along the range of predictor.
#4)Residual vs Leverage: we can observe 1 observations 50 lie beyond Cooke's distance and exceed -3 standard deviation.
#ANOVA
as=stack(list(b1=d$price,b2=d$enginesize))
names(as)
av=aov(values~ind,data=as)
av
summary(av)
#F(1,408,0.05)=3.864 < F(cal)=555.4. Therefore we reject H0(ie there exists a linear relationship between Price and Enginesize)






#FITTING OF MLRM 
df=data.frame(d$price,d$enginesize,d$horsepower,d$carwidth,d$highwaympg)
library(ggplot2)
library(GGally)
ggpairs(df)      #Scatter plot 
#Interpretation: We see that enginesize ,horsepower and,carwidth has a positive linear relationship with price whereas,highwaympg has negative realtion.
n=cbind.data.frame(d$price,d$enginesize,d$horsepower,d$carwidth,d$highwaympg)
c1=cor(n)
c1
library(corrplot)
corr_plot1=corrplot(c1,method="circle")
#There is a positive correlation between price and enginesize.horsepower, &, carwidth. And there is negative correaltion between price and highwaympg.
#Multicollinearity is present(It can be removed by calaculating VIF values)
#Fitting of MLRM with price as independent variable and enginesize,horsepower,carwidth and highwaympg as dependent variable.
model1=lm(d$price~d$enginesize+d$horsepower+d$carwidth+d$highwaympg)
model1
##Estimates of parameters involved in MLRM : Intercept:-57780.22 & Slope1: 95.13 ,Slope2: 50.50 ,Slope3: 825.40 and slope4: -21.95.  
#Equation of fitted regression model is yij=-57780.22+95.13*(enginesize)+50.50*(horsepower)+ 825.40*(carwidth) -21.95*(highwaympg) +error
a1=summary(model1)
a1
#As Adjusted R-squared is 0.8133 ie. 81.33% of variability in price is explained by the 4 dependent variables.
# We can see that we got 3 dependent variable as significant.
#Also the pvalue is less than 0.05 los so rejecting the null hypothesis.As slopes and intercept both do not lie within CI so reject H0 at 95% CI
ci_1=confint(model1,level = 0.95)    #95% Confidence interval
ci_1
#All values lie within confidence interval.
par(mfrow=c(2,2))
plot(model1)
#INTERPRETATION:
#1)Residual vs Fitted: Here we can see an outward opening funnel which indicate that assumption of constant variance is violated.
#2)Normal Q-Q: Flat extremes indicate that observations are from a distribution having tails thinner than normal distribution.
#Scale-Location:The plot shows that residuals are not spread equally along the range of predictor.
#4)Residual vs Leverage: We see that all lie above Cook's distance.
#ANOVA
a_s=stack(list(b1=d$price,b2=d$enginesize,b3=d$horsepower,b4=d$carwidth,b5=d$highwaympg))
names(a_s)
av1=aov(values~ind,data=a_s)
av1
summary(av1)
#F(table value)(4,1020,0.05)=2.381 which is smaller than calculated value.Therefore,we reject null hypothesis(ie. there exists linear relationship between dependent variable and predictors.)

##Test for autocorrelation.
library(car)
durbinWatsonTest(model1)
#We get p value zero indicating that autocorelation is present 1
#Testing for normality
hist(model1$residuals, main= "Histogram")
library(olsrr)
ols_test_normality(model1)
#Since our observation is greater than 30,so we consider Kolmogorov-Smirnov test. 
#p value is less than 0.05 so therefore we do not accept null hypothesis. Therefore, the observation are normal.
#Checking for heteroscedasticity
library(lmtest)
lmtest::bptest(model1)
#Since p value is less than 0.05, it indicates that the difference in variance is significant.



