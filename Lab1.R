VG=read.csv("D:\\Downloads\\video_games_fall_2021.csv")
install.packages("visreg")
Library("visreg")

attach(VG)
#1
##Critcal_Score##

#summary
summary(critic_score)

#boxplot
boxplot(critic_score,col="green")

#histogram
hist(critic_score
     ,breaks = 20,col="green")


##global Sales##

#summary
summary(global_sales)

#boxplot
boxplot(global_sales,col="orange")

#histogram
hist(global_sales,breaks = 500,col="orange")

##year_of_release##
#summary
summary(year_of_release)

#boxplot
boxplot(year_of_release,col="purple")

#histogram
hist(year_of_release,breaks = 25,col="purple")

##critic_count##
#summary
summary(critic_count)

#boxplot
boxplot(critic_count,col="blue")

#histogram
hist(critic_count,breaks = 15,col="blue")


##scatterPlot##
genre = as.factor(genre)

par(mfrow=c(3,1))
plot(global_sales, critic_score, col=genre)
plot(year_of_release, critic_score, col=genre)
plot(critic_count, critic_score, col=genre)
par(mfrow=c(1,1))



#LINEAR REGRESSIONt Score & Global Sales

lm.fit=lm(critic_score~global_sales)
b0_1=coef(lm.fit)[1]
b1_1=coef(lm.fit)[2]
summary(lm.fit)

confint(lm.fit,"global_sales",level=0.95)

require(visreg)
visreg(lm.fit, alpha=0.05)
plot(global_sales,critic_score)
abline(lm.fit)


#LINEAR REGRESSION critscore vs yearof release
lm2.fit=lm(critic_score~year_of_release)
b0_2=coef(lm2.fit)[1]
b1_2=coef(lm2.fit)[2]

summary(lm2.fit)

confint(lm2.fit,"year_of_release",level=0.95)

visreg(lm2.fit,alpha=0.05)

#LINEAR REGRESSION critscore vs critic_count
lm2.fit=lm(critic_score~critic_count)
b0_3=coef(lm2.fit)[1]
b1_3=coef(lm2.fit)[2]

summary(lm2.fit)

confint(lm2.fit,"critic_count",level=0.95)

visreg(lm2.fit,alpha=0.05)


#3
#predicitons
#A
pred1=b0_1+0.5*b1_1
print(pred1)
#b
pred2=b0_2+2013*b1_2
print(pred2)
#c
pred3=b0_3+200*b1_3
print(pred3)

#Multiple linear regression
#4A
attach(VG)
mreg1=lm(critic_score~global_sales+year_of_release+critic_count)
summary(mreg1)

b0_4=coef(mreg1)[1]
b1_4=coef(mreg1)[2]
b2_4=coef(mreg1)[3]
b3_4=coef(mreg1)[4]
print(b1_4)
print(b2_4)
print(b3_4)

Predict4=b0_4+0.5*b1_4+ 2013*b2_4+ 200*b3_4
print(Predict4)

#5A
VG$Nintendo=ifelse(publisher=="Nintendo",1,0)
VG$Nintendo=as.factor(VG$Nintendo)
attach(VG)
levels(Nintendo)
table(Nintendo)
mreg2=lm(critic_score~year_of_release+Nintendo)
summary(mreg2)

plot(year_of_release,critic_score,pch=2,col=ifelse(Nintendo==1,"green","blue"))
legend("bottomleft",pch=3,col=c("green","blue"),c("Nintendo","Non_Nintendo"))

b0=coef(mreg2)[1]
b1=coef(mreg2)[2]
b2=coef(mreg2)[3]

abline(b0+b2,b1,col="green",lwd=3,lty=2)
abline(b0,b1,col="blue",lwd=3,lty=2)

#6
#A
VG$genre=as.factor(VG$genre)
levels(genre)
table(genre)


#B
genre = relevel(genre, ref="Shooter")
detach(VG)
attach(VG)
mreg3 = lm(critic_score~genre)
summary(mreg3)

#C

#D


#7
#A
VG$Strategy=ifelse(VG$genre=="Strategy",1,0)
attach(VG)
mreg4=lm(critic_score~Nintendo+Strategy+Nintendo*Strategy)
summary(mreg4)
#B Response
##From the data I can conclude that Nintendo Strategy Games have a higher critic_score by 5.386 for the videogame while holding everything the same. We should also note that regarding the interaction model b3 coeffcient this value only comes with b1 and b2 being in the model.





#C
mreg5=lm(critic_score~year_of_release+Nintendo+year_of_release*Nintendo)
summary(mreg5)

plot(year_of_release,critic_score,pch=0,col=ifelse(Nintendo=="Yes","green","blue"))
legend("bottomleft",pch=0,col=c("green","blue"),c("Nintendo","Non_Nintendo"))

b0=coef(mreg5)[1]
b1=coef(mreg5)[2]
b2=coef(mreg5)[3]
b3=coef(mreg5)[4]
abline(b0+b2,b1+b3,col="green",lty=2)
abline(b0,b1,col="blue",lty=2)

#D
#As time goes on Nintendo game steadily decrease in critic score throughout the time by -0.41103 which is faster than typical Non Nintendo games which are decreasing by -0.01444


#8
install.packages("car")
library("car")
#A
attach(VG)
mreg6=lm(critic_score~year_of_release+global_sales+critic_count)
plot(predict(mreg6),residuals(mreg6))
abline(0,0,lty=2)
residualPlots(mreg6)
#B
#Predictors with p-value 0.05 do not satisfy linearity assumption which in my case are year_of_release,global_sales, and critic_count

#C
#The Turkey Test is a linearity test for the whole model, since it is less than 0.05 this will lead us to the conclusion that th model is not a good fit. In order to improve the model we should remove the nonlinear variables, remove outliters, check for multicollinearity,check hetroskedasticty,or try a different regression.

#9
#A 
# Heteroskedasticity is when a non-constant variance is in the resodial plot. Visually it looks like a funnel where residuals are small then get larger later on. The effect of  heteroskedasticity on model is it affects the value of the predcitor along the horizontal access. This will lead to resaults in biased standard errors for the linear regression, bad estiamtes for t-stat and p value, and constant coefficients.

#B
mreg7=lm(critic_score~critic_count)
summary(mreg7)
#C
plot(predict(mreg7),residuals(mreg7))
abline(0,0)
#this has a funnel shape

#d
ncvTest(mreg7)

#e
install.packages("lmtest")
install.packages("plm")
require(lmtest)
require(plm)
coeftest(mreg7,vcov=vcovHC(mreg7,type = "HC1"))

#f
#the p valuedid not change, the t value changed a little bit.


#10
#A
attach(VG)
mreg8=lm(critic_score~year_of_release+global_sales)
outlierTest(mreg8)
#B
#Observation Wii Fit and Wii Party are the outliers 

#C
VG1=VG[-6558,-2639,]
mreg9=lm(critic_score~year_of_release+global_sales,data=VG1)
summary(mreg8)
summary(mreg9)
#The Models are different since 1) T value Increased 
#2)R squared Increased 
#3)coefficent of global sales release increased while year_of_release increased

#11
#A
#Colineraity is problematic when there is high correlation between different predictors. This will lead to double counting as the high collinearity will a flawed model. Ideally we should remove 1 out of the two collinear columns.

#B
mreg10=lm(critic_score~year_of_release+global_sales+na_sales+critic_count)
install.packages("psych") 
require(psych)
quantvars=VG[,c(3,6,10,12)]
pairs.panels(quantvars)
#C
#Global Sales and na_sales are collinear

#D
vif(mreg10)
#Both global and na sales since the vf>11

#This is likely because na sales are counted in global sales therefore we are double counting for the sale. 
#F
mreg11=lm(critic_score~year_of_release+global_sales+critic_count)
vif(mreg11)
#yes the value dropped from 11.6 to 1.09
summary(mreg10)
summary(mreg11)