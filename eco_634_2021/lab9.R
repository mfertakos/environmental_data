catrate<-read.csv(here("eco_634_2021","data","catrate.csv"))
head(catrate)

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

#one-sided
binom.test(
  n_success,
  n_years,
  p = 5/7,
  alternative='less')

#F-distribution example
veg = read.csv(here("eco_634_2021","data", "vegdata.csv"))
head(veg)
boxplot(pine ~ treatment, data = veg)

#variance test
var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])

#flidner-killeen test
fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))
fligner.test(pine ~ treatment, data = veg)


#bartlett test
bartlett.test(pine ~ treatment, data=veg)

#T-test
t.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

#wilcox test
wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

#test for paired samples
#paired t-test
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']
t.test(control, clipped, paired=TRUE)
#paired wilcox test
wilcox.test(control, clipped, paired=TRUE)

#correlation
disp = read.csv(here("eco_634_2021","data", "dispersal.csv"))
disp
plot(disp$disp.rate.ftb, disp$disp.rate.eb)
#normal
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')
#non-normal
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#comparing two distributions
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
#kolmogrov-smirnov test
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

prop.test(c(4,16),c(40,250))

#chisquare
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)
#fisher test
fisher.test(owls)

#bird habitat data
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

#Question 1
#The null hypothesis is that there is no relationship between brown creeper presences at the forest edge and the interior aka no habitat preference
#Question 2
#p<0.001 meaning there is strong evidence to reject the null, meaning there is strong evidence that Brown Creepers show a habitat preference. But we dont know which way because its a two sided test.
#Question 3
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)
#Question 4
fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)
#Question 5
fit_both = 
  lm(
    formula = body_mass_g ~ sex*species,
    data = penguins)
#Question 6
boxplot(formula = body_mass_g ~ species,data=penguins,ylab="body mass(g)",main="boxplot of body mass as explained by species")
#Question 7
boxplot(formula = body_mass_g ~ sex,data=penguins,main="boxplot of body mass as explained by sex")
#Question 8
boxplot(formula = body_mass_g ~ sex*species,data=penguins,ylab="body mass (g)",names=c("female \n Adelie","male \n Adelie","female \n Gentoo","male \n Gentoo","female \n Chinstrap","male \n Chinstrap"),las=2,xlab="",main="boxplot of body mass as explained by sex and species")
#Question 9
#I am not as certain the homogeneity assumption is met for the model of body mass as explained by species. The box for Gentoo penguins in this plot is larger than the other two species. 
#The chinstrap box is also very narrow, while the Adelie is sized in between the box widths of Gentoo and Adelie. This variation makes me unsure the variances are the same. The boxes in the
#other models appear similar enough that I am comfortable assuming homogeneity. These conclusions should be confirmed with a Bartlett test to be certain.
#Queston 10
#The null hypothesis of the Bartlett test is that there is no difference in the variances amoung the treatment levels. It assumes homogeneity.
#Question 11
bartlett.test(body_mass_g ~ species, data=penguins)
#p=0.0501
#Question 12
bartlett.test(body_mass_g ~ sex, data=penguins)
#p=0.0319
#Question 13=
dat_sex = aggregate(
  body_mass_g ~ sex*species,
  data = penguins,
  FUN = c)

bartlett.test(dat_sex$body_mass_g)

#Question 14
#For the model testing body mass as explained by species and the model testing body mass 
#as explained by sex, the p values were less than 0.05 (0.0501 and 0.0319 respectively). This means the null hypothesis that 
#they are homogeneous is not supported, and their variances are in fact heterogeneous. For my two way model testing the relationship
#between body mass as explained by sex AND species, the p value of 0.1741 accepts the null hypothesis that the variances are
#homogenous. This means the variances are homogeneous and will not provide any issues when performing group 1 models.


