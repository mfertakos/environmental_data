#the below code reads in the data from the github site and saves it as two separate objects
dat_birds = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/bird.sta.csv")
dat_habitat = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/hab.sta.csv")

#pairplot this code creates a pairplot of the data
pairs(dat_habitat[,c(21:24)]) #this code selects columns 21 to 24 and creates a pairs plot.

pairs(dat_birds) #this line creates an error because it contains columns with non numeric data
pairs(dat_habitat[,c(12:24)]) #this code selects the columns with numerica data columns 12 to 24 to create a pairs plot.


#histogram: this code creates a histogram for the kind of bird specified after dat_birds$. I include a title with xlab, 
#and use the breaks function to define the number of ticks on the x-axis and that they should be centered to each bar in the histogram.
hist(dat_birds$CBCH,xlab="Number of birds counted",breaks=0:7-0.5)
hist(dat_birds$AMRO,xlab="Number of birds counted",breaks=0:6-0.5)
hist(dat_birds$DOWO,xlab="Number of birds counted",breaks=0:4-0.5)

#Q2 most of the data points are congregated in the lower left hand side of the plots, which means ...
#snag.dc4 and snag.l seem to have a positive linear relationship because as x increases, y increases across the span of the graph.

#Q3
