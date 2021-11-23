dat=matrix(1:49,nrow=7,ncol=7)
print(dat)
apply(dat,MARGIN=1,FUN=min)
apply(dat,MARGIN=1,FUN=max)
apply(dat,MARGIN=2,FUN=mean)

moths = read.csv(here("eco_634_2021","data","moths.csv"))
head(moths)

hist(moths$anst)

#calculate CI
sse_mean = function(x)
{
  sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
}

sse_mean(moths$anst)

#bootstrapping
m=10000
result=numeric(m)
head(result)

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

mean(result)
quantile(result,c(0.025,0.975))

#bootstap interval using boot()
install.packages("boot")
require(boot)
boot(data,statistic,R)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)
mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.025, 0.975))

#setting up the bootstrap
moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

#################
rm(list = ls())

# Re-read my data:
moths = read.csv(here("eco_634_2021","data","moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

moths = read.csv(here("eco_634_2021","data","moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

#next steps
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

#plot the curve
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

###QUESTIONS###
#Q1
require(palmerpenguins)
dat_gentoo = subset(penguins, species == "Gentoo")
as.data.frame(dat_gentoo)->dat_gentoo
length(dat_gentoo$species)-length(sum(is.na(dat_gentoo)))
#n=123


#Q2
sd(dat_gentoo$bill_length_mm,na.rm=TRUE)
#sd=3.081857

#Q3
#alpha=0.05
#df=122
lower_qt = qt(0.05/2,122,lower.tail=TRUE)
upper_qt = qt(1-(0.05/2),122,lower.tail=TRUE)
#lower critical t = -1.9796
#upper critical t = 1.9796

#Q4
sse_mean = function(x)
{
  sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
}

SSE = sse_mean(dat_gentoo$bill_length_mm)
SSE = 0.2778817

#Q5
mean_gentoo = mean(na.omit(dat_gentoo$bill_length_mm))

mean_gentoo + (SSE*lower_qt)
mean_gentoo + (SSE*upper_qt)
#CI= 47.50488 +/- 0.5500946
#or 46.95 to 48.05

#Q6
#The CI, an abreviation for confidence interval, is an interval found to countain the true value
#95% of the time when recalculated on x repeated samplings.
#CI is 46.97338 to 48.05970

#Q7
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
my_boot=boot(data=dat_gentoo$bill_length_mm, statistic=boot_mean, R=10000)

#Q8
quantile(
  my_boot$t,
  c(0.025, 0.975))

#CI is 46.97338 to 48.05970

#Q9
moths = read.csv(here("eco_634_2021","data","moths.csv"))
moth_dat = moths[,-1]
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}


rarefact = rarefaction_sampler(moth_dat, 10000)

#Q10 The most difficult part of the function to build for me was figuring out how to fix the code so it knew what n was. 
#This turn out to be the n_input_rows object which represents the number of rows in the input data. 

#Q11
rarefact = rarefaction_sampler(moth_dat, 10000)

#Q12
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))
png(filename=here("eco_634_2021","lab_07_curve.png"),width=1500,height=1600,units="px",res=180)  
matplot(
  rare,
  type='l',
  col=c("black","red","blue"),
  lwd=c(2,2,2),
  xlab='Number of sampling plots',
  ylab='Species richness of rare moths',
  main='Rarefaction Curve of Rare Moths')

legend(
  'bottomright',
  legend=c('mean # of rare moth species','2.5% confidence interval','97.5% confidence interval'),
  lty=c(1,2,3),col=c("black","red","blue"),lwd=c(2,2,2),inset=c(.1,.1))
dev.off()

#Q13 
#About 15 plots because that is the x-value where the rarefaction curve reaches all 10 species on the y-axis.
