require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

install.packages('simpleboot')
library(simpleboot)
samp1=subset(penguin_dat, species=='Adelie')
adelfliplength=samp1$flipper_length_mm
samp2=subset(penguin_dat,species=='Chinstrap')
chinfliplength=samp2$flipper_length_mm
pen_boot = two.boot(sample1=adelfliplength, sample2=chinfliplength, FUN=mean, R=10000,na.rm=TRUE)
mean(pen_boot$t,na.rm=TRUE)

#Q1
sd(pen_boot$t)
#1.007668

#Q2
png(filename=here("eco_634_2021","lab_08_Q2.png"),width=1500,height=1600,units="px",res=180)  
hist(pen_boot$t, xlab="Difference in mean flipper lengths",main="Histogram of difference in mean flipper lengths")
dev.off()

#Q3
quantile(pen_boot$t,c(0.025,0.975),na.rm=TRUE)
#2.5%     97.5% 
#-7.816329 -3.904232 

#use these in Q4
mean(pen_boot$t)
#-5.863978
median(pen_boot$t)
#-5.856643
#No I do not think my resampled differences in means follow a skewed distribution.
#This is because the mean (-5.86) and median (-5.85) are close in value, and the histogram
#of the mean differences appears normally distributed. If this was a skewed distribution
#the bars of the histogram would concentrate on one side or the other, and the median would be 
#further apart in value from each other.

#Q5
pen_ecdf<-ecdf(pen_boot$t)
#Q6
1-pen_ecdf(-4.5)
#0.0881
#Q7
pen_ecdf(-8)
#0.0149
#Q8
#Null: There is no difference in mean flipper lengths between the two penguin species.
#Alternative: There is a significant difference in mean flipper lengths between the two penguins. 


#other question in lab (?)
1-pen_ecdf(0)

#Trees
veg = read.csv(here("eco_634_2021","data","vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)
table(dat_tree$treatment)
#wilcox test
#Q9
wilcox.test(data=dat_tree,pine~treatment)
#p=0.1005
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )
boot.ci(tree_boot)
hist(tree_boot$t, main = "Bootstrap sampling distribution")
#Q10
quantile(tree_boot$t, c(0.025,0.975))
#4.25 to 30.00

#Q11
mean(tree_boot$t)
#The observed difference in mean tree counts was 16.01 and it does fall within the range of the 95% bootstrap CI

#Q12
dat_bird<-read.csv(here("eco_634_2021","data","bird.sub.csv"))
dat_habitat<-read.csv(here("eco_634_2021","data","hab.sub.csv"))
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))
head(dat_all[, c("b.sidi", "s.sidi")])
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

#Q13
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

#plot
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#simple linear regression
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#the slope coefficient
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))
#resampling the data
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

#plot
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#randomization loop
#Q14
m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1),replace = TRUE)
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
} 

#plot
#Q15
png(filename=here("eco_634_2021","lab_08_Q15.png"),width=1500,height=1600,units="px",res=180)  
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = critical_slope, lty=3, col="blue",lwd= 2)
dev.off()

#critical slope value
#Q16
critical_slope <- quantile(result, c(.05))
#=-0.01309726
#slope observed was -0.02437 (less than critical)
sum(as.numeric(result<= -0.02437))/10000
#Q17
#There is a significant negative relationship between vegetation cover diversity
#and bird diversity. This is supported as true because our observed slope is less than
#the critial slope value created through the randomization loop. In the slopes outputted by the
#10000 replicate randomization loop, only 17 are equal to or less than the observed slope. This means
#there is a p value of 0.0017, which is significant. There is an extremely small chance on randomization of the data
#we would observe the slope we observed.
