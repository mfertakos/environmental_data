#droplevels should always go with subset it doesnt hurt, basically removes remnants of unused levels
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")
#Q1
boxplot(dat_ade$body_mass_g~dat_ade$sex)
#Q2
dat_ade_fem = droplevels(subset(dat_ade,sex=="female"))
t.test(dat_ade_fem$body_mass_g,alternative="two.sided",mu=0)
#Q3
#A p-value is very low and therefore significant. This means the null
#hypothesis that female Adelie penguins have a body mass equal to
# 0 is not true, the alternative hypothesis is supported and they 
#infact have a body mass greater than 0.
#Q4
dat_ade_male=droplevels(subset(dat_ade,sex=="male"))
t.test(dat_ade_male$body_mass_g,alternative="greater",mu=4000)
#Q5
#high p value means the null hypothesis is supported but the 
#alternative hypothesis is not. This means male Adelie penguins
#do not have a mean mody mass greater than 4000 grams. The null
#hypothesis that male Adelie penguins have a mean body mass of 
#less than 4000 grams is supported.

#Q6
#two sample because its males and females not just 1 at a time
t.test(body_mass_g ~ sex,data=dat_ade)

#Q8
t.test(body_mass_g ~ sex,data=dat_ade,alternative="g")
t.test(body_mass_g ~ sex,data=dat_ade,alternative="l")
