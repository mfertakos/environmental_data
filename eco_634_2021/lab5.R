ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

exp_fun = function(x, a, b)
  {
  return(a* exp(-b*x))
}

curve(
  exp_fun(x, 1, 1), 
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

#Normal errors 2
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

#Exponentially-distributed errors
y_observed_3 = 
  y_pred + 
  rexp(n_pts,rate=1.2)
plot(x_sim, y_observed_3, main = "Exponentially Distributed Errors\n Non-Constant Variance", xlab = "", ylab = "")

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")

#Choosing a model
par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)




#Assignment
dat_dist = read.csv(
  here("eco_634_2021","data","dispersal.csv")
)
head(dat_dist)

plot(dat_dist$disp.rate.ftb,dat_dist$dist.class)

#Q1 #create exp_fun()
exp_fun = function(x, a, b)
{
  return(a* exp(-b*x))
}

#create curves
#curve 1
png(filename=here("eco_634_2021","lab_05_q_2.png"),width=1500,height=1600,units="px",res=180)  
curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 20, 
  col="black", lty="solid", ylim=c(0,2), xlab="x",ylab="f(x)",main="Exponential function curves")
  
#curve 2
curve(
  exp_fun(x, 1.9, 0.3),
  col="black", lty="dotted", add=TRUE)
  
#curve 3
curve(
  exp_fun(x, 1.2, 0.2), 
  col="red", lty="solid", add=TRUE)
 
#curve 4
curve(
  exp_fun(x, 1.2, 0.4), 
  col="red", lty="dotted", add=TRUE)
  
dev.off()
#Q2 is above figure
#Q3
#When a decreases the curve becomes steeper, meaning the slope

#creater Ricker Function
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

png(filename=here("eco_634_2021","lab_05_q_5.png"),width=1500,height=1600,units="px",res=180)  
#curve 1
curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 70,
  ylab = "f(x)", xlab = "x",
  col="black", lty="solid", add=FALSE,
  main="Ricker function curves")
#curve 2
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 70, 
  col="black", lty="dotted", add=TRUE)
#curve 3
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 70, 
  col="black", lty="dotted", add=TRUE)

#curve 4
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 70, 
  col="red", lty="solid", add=TRUE)

#curve 5
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 70, 
  col="red", lty="dotted", add=TRUE)

#curve 6
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 70, 
  col="red", lty="dotted", add=TRUE)
dev.off()

#Q8
png(filename=here("eco_634_2021","lab_05_q_9.png"),width=1500,height=1600,units="px",res=180)  
plot(dat_dist$dist.class,dat_dist$disp.rate.ftb,main="Scatter plot with linear model",xlab="Distance Class",ylab="Dispersal Rate")
guess_x=700
guess_y=0.3
guess_slope=-0.00035
x=dat_dist$dist.class
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dev.off()

#Q10
png(filename=here("eco_634_2021","lab_05_q_11.png"),width=1500,height=1600,units="px",res=180)  
plot(dat_dist$dist.class,dat_dist$disp.rate.ftb,main="Scatter plot with exponential model",xlab="Distance Class",ylab="Dispersal Rate")
curve(
  exp_fun(x, 75, 0.015), add = TRUE, from = 0, to = 1500, 
  col="black", lty="solid", ylim=c(0,0.8))
dev.off()

#Q12
png(filename=here("eco_634_2021","lab_05_q_13.png"),width=1500,height=1600,units="px",res=180)  
plot(dat_dist$dist.class,dat_dist$disp.rate.ftb,main="Scatter plot with Ricker model",xlab="Distance Class",ylab="Dispersal Rate",xlim=c(-100,1500))
curve(
  ricker_fun(x, 0.010, 0.004), 
  from = 0, to = 1500,
  col="black", lty="solid", add=TRUE)
dev.off()

#calculating residuals
dat_dist$linpred<-line_point_slope(dat_dist$dist.class,guess_x,guess_y,guess_slope)
dat_dist$resids_linear<-dat_dist$disp.rate.ftb-dat_dist$linpred
dat_dist$exppred<-exp_fun(dat_dist$dist.class,75,0.015)
dat_dist$resids_exp<-dat_dist$disp.rate.ftb-dat_dist$exppred
dat_dist$rickpred<-ricker_fun(dat_dist$dist.class,0.010,0.004)
dat_dist$resids_ricker<-dat_dist$disp.rate.ftb-dat_dist$rickpred
#create histogram
png(filename=here("eco_634_2021","lab_05_q_15.png"),width=1500,height=1600,units="px",res=180)  
par(mfrow = c(1, 3))
hist(dat_dist$resids_linear,main="Linear residuals",xlab="residual values")
hist(dat_dist$resids_exp,main="Exponential residuals",xlab="residual values")
hist(dat_dist$resids_ricker,main="Ricker residuals",xlab="residual values")
dev.off()
