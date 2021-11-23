t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"),alternative="less")

#ANOVA
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
shapiro.test(dat_chinstrap$body_mass_g)
aggregate(body_mass_g ~ species, data = penguins, FUN = function(x) shapiro.test(x)$p.value)

fit_species = lm(body_mass_g ~ species, data = penguins)

summary(fit_species)

anova(fit_species)

#two-way additive ANOVA
boxplot(body_mass_g ~ sex+species, data = penguins)
fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

#two-way interactive ANOVA
fit_interactive = lm(body_mass_g ~ sex*species,data=penguins)

#Question 1 (should this be + instead of *?)
boxplot(formula = body_mass_g ~ sex*species,data=penguins,ylab="body mass (g)",names=c("female \n Adelie","male \n Adelie","female \n Chinstrap","male \n Chinstrap","female \n Gentoo","male \n Gentoo"),las=2,xlab="",main="boxplot of body mass as explained by sex and species")

#Question 2
#Based on the boxplots, I think that male Gentoo penguins are significantly heavier than female Gentoos, while the other two species are not sgnificantly different.
#I believe this because of the distance between the median weights of male and female Gentoos are far apart, and the minimum male weight is still higher than the median female weight.

#Question 3 (finish)
#I think adding sex to the model of species will improve the fit. If you compare the boxplot created with only species and the one created with species and sex as predictors, you can see that 

#Question 4
fit_both = lm(body_mass_g ~ sex*species, data = penguins)

#Question 5
#The base case is female*Adelie (Adelie females)

#Question 6
#The intercept and speciesChinstrap.

#Question 7
#3368.84+(1)158.37=3527.21g

#Question 8
aggregate(
  x = penguins$body_mass_g,
  by = list(penguins$species), 
  FUN = "mean",na.rm=TRUE)
#=3733.088
