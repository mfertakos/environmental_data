#Numerical Exploration
delomys<-read.csv(here("data","delomys.csv"))
summary(delomys$body_mass)
summary(delomys$body_length)
shapiro.test(delomys$body_mass)
shapiro.test(delomys$body_length)
#p value = < 2.2e-16 aka non-normal

#graphical
require(palmerpenguins)
par(mfrow = c(3, 2))
#scatterplot
plot(body_length ~ body_mass, data = delomys,main="scatterplot of Delomys body mass \n as explained by body length",xlab="body mass (g)",ylab="body length")
#histogram of body mass
hist(delomys$body_mass, main="histogram of Delomys body mass", xlab="body mass (g)")
#histogram of body length
hist(delomys$body_length, main="histogram of Delomys body length", xlab="body length")
#boxplot of body mass, conditioned on species
boxplot(formula = body_mass ~ binomial, data=delomys,ylab="body mass (g)",main="boxplot of Delomys body mass \n as explained by species",xlab="")
#boxplot of body mass conditioned on sex
boxplot(formula = body_mass ~ sex,data=delomys,main="boxplot of body mass \n as explained by sex", ylab="body mass (g)")
#boxplot of body mass conditioned on both sex and species
boxplot(formula = body_mass ~ sex*binomial,data=delomys,ylab="body mass (g)",xlab="", names=c("female \n D. dorsalis","male \n D. dorsalis","female \n D. sublineatus","male \n D. sublineatus"),main="boxplot of body mass \n as explained by sex and species", cex.axis=0.6)

#Model building
#simple linear regression
fit1<-lm(body_length ~ body_mass, data=delomys)
#1-way ANOVA
fit2<-lm(body_mass ~ sex, data=delomys)
anova(fit2)
#1-way ANOVA
fit3<-lm(body_mass ~ binomial, data=delomys)
anova(fit3)
#2-way additive ANOVA
fit4<-lm(body_mass ~ sex + binomial, data=delomys)
anova(fit4)
#2-way factorial ANOVA
fit5<-lm(body_mass ~ sex * binomial, data=delomys)
anova(fit5)

#Model Diagnostics
par(mfrow=c(3,2))
hist(residuals(fit1))
hist(residuals(fit2))
hist(residuals(fit3))
hist(residuals(fit4))
hist(residuals(fit5))

shapiro.test(fit1$residuals)
shapiro.test(fit2$residuals)
shapiro.test(fit3$residuals)
shapiro.test(fit4$residuals)
shapiro.test(fit5$residuals)

#Model interpretation
knitr::kable(coef(summary(fit1)))
