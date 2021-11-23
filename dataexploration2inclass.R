install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)

penguins = data.frame(penguins)
mean(penguins$body_mass_g,na.rm=TRUE)
head(penguins)
summary(penguins)
pairs(penguins)
plot(x=penguins$bill_length_mm,y=penguins$body_mass_g)
hist(x=penguins$body_mass_g,breaks=5)
boxplot(penguins$bill_depth_mm~sex,data=penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | island, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | bill_length_mm, data = penguins)

png(file=here("basic_histogram.png"),width=800,height=600)
hist(penguins$flipper_length_mm,breaks=8,xlab= "flipper length", main="frequency of different flipper lengths")
dev.off()