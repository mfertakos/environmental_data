dnorm(): #the probability density height of the curve at x

pnorm(): #the cumulative probability density area under the curve left of x

qnorm(): #the quantile function

rnorm(): #function to generate random, normally-distributed numbers.
  
  
  x=c(-1.96,-1,0,1.96)
  y=pnorm(x,mean=0,sd=1)
  
  plot(x,y,type="l")
  abline(h=0)
  
  x = seq(-3, 3, length.out = 1000)
  y = dnorm(x)
  
  plot(x, y, main = "Normal PDF", type = "l")
  abline(h = 0)
  
  #penguins example
  require(palmerpenguins)
  hist(
    penguins$body_mass_g,
    main="Histogram of Penguin Body Mass",
    xlab = "Body Mass (g)")
  
mean(penguins$body_mass_g,na.rm=TRUE)
sd(penguins$body_mass_g,na.rm=TRUE)
nrow(penguins)

dat_1=rnorm(n=344,mean=4202,sd=802)
dat_2=rnorm(n=344,mean=4202,sd=802)
dat_3=rnorm(n=344,mean=4202,sd=802)
dat_4=rnorm(n=344,mean=4202,sd=802)

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)
  
set.seed(12)
dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)

#lets calculate some residuals
set.seed(123)
n=17
slope=0.7
intcp=0.2

guess_x=6
guess_y=4
guess_slope=0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

  # Calculates the value of y for a linear function, given the coordinates
  # of a known point (x1, y1) and the slope of the line.
  line_point_slope = function(x, x1, y1, slope)
  {
    get_y_intercept = 
      function(x1, y1, slope) 
        return(-(x1 * slope) + y1)
    
    linear = 
      function(x, yint, slope) 
        return(yint + x * slope)
    
    return(linear(x, get_y_intercept(x1, y1, slope), slope))
  }
  
#plot
  set.seed(123)
  n_pts = 10
  x_min = 1
  x_max = 10
  x = runif(n = n_pts, min = x_min, max = x_max)
  
  dat = data.frame(x = x, y_observed = rnorm(n_pts))
  guess_x= 5
  guess_y= 0
  guess_slope=0.1
  
  plot(y_observed ~ x, data = dat, pch = 8)
  #fit a linear line
  curve(line_point_slope(x,guess_x,guess_y,guess_slope),add=T)
  
  #calculate predicted y values based on y guess of model parameters
  line_point_slope(dat$x, guess_x, guess_y, guess_slope)
  dat$y_predicted<-line_point_slope(dat$x, guess_x, guess_y, guess_slope)
  dat$resids<-dat$y_predicted-dat$y_observed
  sum(dat$resids)
  sum(abs(dat$resids))
  
  plot(x=dat$y_observed,y=dat$y_predicted)
  plot(x=dat$y_observed,y=dat$resids)
  hist(dat$resids)

#worked with Heather
#Q1
  mean=10.4
  sd=2.4
  norm_17<-rnorm(n=17,mean,sd)
  norm_30<-rnorm(n=30,mean,sd)
  norm_300<-rnorm(n=300,mean,sd)
  norm_3000<-rnorm(n=3000,mean,sd)
  
#Q2
  png(filename=here("eco_634_2021","lab_04_hist_01.png"),width=1500,height=1600,units="px",res=180)
  par(mfrow=c(2,2))
  hist(norm_17,main="Hist.of 17 normally distributed random pnts.")
  hist(norm_30,main = "Hist. of 30 normally distr. random pnts.")
  hist(norm_300,main ="Hist. of 300 normally distr. random pnts.")
  hist(norm_3000,main ="Hist. of 3000 normally distr. random pnts.")
  dev.off()
  
#Q3
  #upload figure
  
#Q4
#With increased sample size the distribution appears more normal. The histograms with smaller samples
  #are more prone variation from the mean. This is representative in my historgrams as the histogram with
  #the smallest sample size (17) looks the least normally distrubted (around the mean) than the histogram with
  #a sample of 3000. The histogram with a sample size of 30 and 300 appear progressivly more normal.
  
#Q5
  #This occurs because histograms with larger sample sizes are less prone to variation from the mean. As sample size
  #increases, the distribution appears more normal. More representatives are provide a better picture of the distribution.
  
#Q6
  #The parameters and their values for the standard normal distribution are the mean and the standard deviation.
  #The values for the histograms we just made were mean=10.4, and sd=2.4.
  
#Q7
  x = seq(-20, 20, length.out = 1000)
  y = dnorm(x,mean=10.4,sd=2.4)
  svg(filename=here("eco_634_2021","norm_1.svg"),width=7,height=9)
  plot(x, y, type = "l", xlim = c(0, 20),main="normal distribution with a mean 10.4 and sd 2.4")
  abline(h = 0)
  dev.off()
  
#Q8
  #upload file
  
#Q9
  set.seed(100)
  n_pts = 100
  x_min = -10
  x_max = 10
  x = runif(n = n_pts, min = x_min, max = x_max)
  dat = data.frame(x = x, y_observed = rpois(n_pts,15))

  
#create color
  mycol <- rgb(100, 80,20 , max = 100, alpha = 50)
  mycol2 <- rgb(20, 60, 20, max = 100, alpha = 80)
  mycol3<- rgb(100,0,80,max=100,alpha=25)

png(filename=here("eco_634_2021","lab_04_q_10.png"),width=1500,height=1600,units="px",res=180)  
par(mfrow = c(2, 2))
#scatterplot
plot(dat,cex=0.8,col=mycol,pch=19)
plot(dat,cex=2,col=mycol2,pch=18)
#histogram
hist(dat$x,col=mycol3)
#boxplot
boxplot(dat)
dev.off()

#Q10
#upload image

#Q11 #NEED HELP
set.seed(100)
n_pts = 100
x_min = -10
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rpois(n_pts,15))
#create color
mycol <- rgb(100, 80,20 , max = 100, alpha = 50)

png(filename=here("eco_634_2021","lab_04_q_12.png"),width=1500,height=1600,units="px",res=180)  
plot(dat,cex=0.8,col=mycol,pch=19)
guess_x<-0
guess_y<-15
guess_slope<-0.2
x = runif(n = n, min = 1, max = 10)
#line_point_slope<-Vectorize(line_point_slope)
curve(line_point_slope(x,guess_x,guess_y,guess_slope),add=T)
dev.off()

#Q12
#upload image

#Q13
#column of predicted y-values
dat$y_predicted<-line_point_slope(dat$x,guess_x,guess_y,guess_slope)
#column of residuals
dat$resids<-dat$y_predicted-dat$y_observed

#Q14
#histogram of the models residuals
png(filename=here("eco_634_2021","lab_04_q_14_histogram.png"),width=1500,height=1600,units="px",res=180)  
hist(dat$resids,main="Histogram of Model Residuals",xlab="Residual Values", ylab="Frequency")
dev.off()
#scatterplot of the models predicted values on x and residuals on y
png(filename=here("eco_634_2021","lab_04_q_14_scatterplot.png"),width=1500,height=1600,units="px",res=180)  
plot(dat$y_predicted,dat$resids,xlab="predicted y values",ylab="residual values",main="Residual Scatterplot")
dev.off()