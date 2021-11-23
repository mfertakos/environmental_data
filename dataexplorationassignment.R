install.packages("here")
require("here")

read.csv(here("eco_634_2021","data","habitat.sta.csv"))->dat_habitat
elevation<-dat_habitat$elev
aspect<-dat_habitat$aspect
slope<-dat_habitat$slope

par(mfrow=c(3,1))
hist(elevation,breaks=5)
hist(aspect)
hist(slope,breaks=7)


totalba<-dat_habitat$ba.tot


#all 6 combined
#function to make linear line
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

png(file=here("dataexplorationassignmentfigure.png"),width=1200,height=1000)
par(mfrow=c(2,3))
hist(elevation,breaks=5)
hist(aspect)
hist(slope,breaks=7)
plot(y=dat_habitat$ba.tot,x=dat_habitat$elev,cex=0.3,ylab="basal area (m2 per ha)",xlab="elevation")
data_center_x=mean(dat_habitat$elev)
data_center_y=mean(dat_habitat$ba.tot)
curve(line_point_slope(x, data_center_x, data_center_y, 0.3), add = TRUE)
abline(lm(ba.tot~elev,dat_habitat),col="red")
plot(y=dat_habitat$ba.tot,x=dat_habitat$aspect,cex=0.3,ylab="basal area (m2 per ha",xlab="aspect")
abline(lm(ba.tot~aspect,dat_habitat),col="red")
plot(y=dat_habitat$ba.tot,x=dat_habitat$slope,cex=0.3,ylab="basal area (m2 per ha)",xlab="slope")
x=dat_habitat$slope
#curve(line_point_slope(x,x1=3.5,y1=1.25,slope=0.4),add=TRUE)
abline(lm(ba.tot~slope,dat_habitat),col="red")
dev.off()



#Q2