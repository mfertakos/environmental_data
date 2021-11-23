catrate<-read.csv(here("eco_634_2021","data","catrate.csv"))
head(catrate)
summary(catrate)
#Q1
png(filename=here("using_models1_Q1.png"),width=1500,height=1600,units="px",res=180)  
hist(catrate$cat.rate,xlab="catastrophe rate",main="frequency of salamander catastrophe rates")
dev.off()
#Q2
shapiro.test(catrate$cat.rate)
#p=0.041
#Q3
#The null hypothesis of a shapiro test is that the distribution is normal
#Q4
#There is strong evidence (p=0.041) that the null hypothesis that the distribution is normal is not supported. Therefore, there is strong evidence the distribution is not normally distributed.
#One sample test: T-test
#Q5
t.test(x=catrate$cat.rate,mu=2/7)
#p=0.01054 shows that the null  is not supported. 
#Q6
#The null hypothesis is that the observed catastrophic rate and the expected rate from the pond late-filling rate are equal.
#Q7
#This is a two-tailed test

#Q8 
#p=0.01054 is equivalent to the false positive rate that 1% of the time we will observe a catastrophic 
#rate that is in fact NOT different from the pond late-filling rate 

#Q9
#CI: 0.3526 to 0.7261 - It does not include 0.

#Q10
#p=0.01054 means the alternative hypothesis that the observed rate is greater than the expected is supported, and consequently
#provides strong evidence to reject the null hypothesis.

#One sided alternative hypothesis
t.test(x=catrate$cat.rate,mu=0.28,alternative="greater")

#The Wilcoxon Rank Sum Test
#Q11
wilcox.test(catrate$cat.rate, mu = 2 / 7)

#Q12
#The wilcox test gives a p value of 0.006, which is much smaller than the p value obtained from the t-test (0.01054).


#Q13
#The p-value of 0.006 from the wilcox rank sum test provides strong evidence to reject the null that the distribution that the observed and expected rates are the same.

#Q14
#The p-values from the t-test and the wilcoxon rank sum test both reject the null that the observed catastophric rate is the same as the expected catastrophic rate.
#The Wilcoxon rank sum test provides stronger evidence to reject the null,though, because the  p value from this test is much smaller, and is therefore a smaller false positive rate.

#Q15
#Both tests work for small sample sizes, but the Wilcoxon rank sums test is for datasets that are not normally distributed. Because the catastophic salamander dataset appears non-normal when plotted as a histogram,
#the Wilcoxon rank sums test is better. This is verified by a shapiro test which has a p value of 0.041, meaning the alternative hypothesis that they are non-normally distributed is supported.

#Comparing two sample means
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

#Q16
#shapiro test
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

#Q17
#The p value of 0.72 from the shaprio test of the flipper length of Adelie penguins supports the null hypothesis that they are normal.
#Furthermore, the p value of 0.81 from the shaprio test of the flipper length of Chinstrap penguins supports the null hypothesis that they are normal.
#parametric and nonparametric tests

#Q18
png(filename=here("using_models1_Q18.png"),width=2500,height=1600,units="px",res=180)  
par(mfrow = c(1, 2))
hist(dat_adelie$flipper_length_mm,xlab="flipper length",main="Adelie penguin flipper length frequencies")
hist(dat_chinstrap$flipper_length_mm,xlab="flipper length",main="Chinstrap penguin flipper length frequencies")
dev.off()

#Q19
#The alternative hypothesis is that the average flipper length of Adelie penguins are larger OR smaller than the average flipper length of Chinstrap penguins.

#Q20
t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)
