sse_mean = function(x)
{
  sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
}

require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

#t test
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

#resampling
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#difference of means
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

  #aggregate function
set.seed(54321)
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

#sample sizes
table(dat_pen$species)
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

#simulation function
two_group_resample = function(x, n_1, n_2) 
  {
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  difference_in_means = mean(dat_1,na.rm = TRUE) - mean(dat_2,na.rm=TRUE)
  return(difference_in_means)
}

set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm,68,152)

#histogram
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

#retrieving named elements
t_test = t.test(flipper_shuffled ~ dat_pen$species)
str(t_test)
t_test$estimate

#Q1
rm(list = ls())
sse_mean = function(x)
{
  sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
}
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#Q2
two_group_resample = function(x, n_1, n_2) 
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  difference_in_means = mean(dat_1,na.rm = TRUE) - mean(dat_2,na.rm=TRUE)
  return(difference_in_means)
}

#Q3
#This function simulates the null hypothesis because it takes a random number of
#points and shuffles them to create a random dataset. This is an example of Monte Carlo
#sampling because in the sampling associations between the flipper length and species are destroyed.
#The observed data is then compared to this to see if the trend is significant.

#Q4
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
png(filename=here("eco_634_2021","lab_06_q_4.png"),width=1500,height=1600,units="px",res=180)  
hist(mean_differences,xlab="mean differences",main="Histogram of random resampled differences of mean flipper lengths (mm)
     between Adelie and Chinstrap penguins")
dev.off()

#Q5
mean_differences[which(abs(mean_differences)>5.8)]
#None are greater than 5.8 (or lower than -5.8)

#Q6
#At least 10 million or more simulations.

#Q7
png(filename=here("eco_634_2021","lab_06_q_7.png"),width=1500,height=1600,units="px",res=180)  
boxplot(bill_length_mm ~ species, data = dat_pen,main="Boxplot of bill length for Adelie and Chinstrap penguins",ylab="bill length (mm)")
dev.off()

#Q8
  #group means: 38.79139 & 48.83382

agg_means = aggregate(
  bill_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
agg_means


  
  #difference between the means 10.04
diff_crit = diff(agg_means[, 2])
diff_crit

#Q9 
t.test_1 = t.test(bill_length_mm ~ dat_pen$species)
t.test_1

#The p value is less than 2.2 x 10^-16. This value means that there is an EXTREMELY small chance that the result obtained
#by the oberserved values would occur if there is in fact no association between bill length and species. 
#This chance is equal to less than 1 in a quadrillion. As a result, we can interpret this as there beind a significant difference
#between bill length in these two species.

#Q10 
n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$bill_length_mm, 68, 152)
  )
}

mean_differences[which(abs(mean_differences)>diff_crit)]
#0

#Q11
png(filename=here("eco_634_2021","lab_06_q_11.png"),width=1500,height=1600,units="px",res=180)  
hist(mean_differences,xlab="mean differences",main="Histogram of random resampled differences in mean bill lengths 
     of Adelie and Chinstrap penguins")
dev.off()
