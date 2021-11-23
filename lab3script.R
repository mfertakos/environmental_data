install.packages("psych")
require(psych)
pairs.panels(iris)
install.packages("here")
require(here)

dat_bird = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")
dat_habitat = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")

write.csv(dat_bird,"bird.sta.csv",row.names=TRUE)
write.csv(dat_habitat,"habitat.sta.csv",row.names = TRUE)

dat_bird = read.csv(
  here("eco_634_2021","data","bird.sta.csv")
)
head(dat_bird)

dat_habitat = read.csv(
  here("eco_634_2021","data","habitat.sta.csv")
)
head(dat_habitat)

dat_all <-merge(dat_bird,dat_habitat)

sample(dat_all$CEWA, 100)

#presence absense
as.numeric(dat_all$CEWA > 1)->cewa_present_absent
plot(x = dat_all$elev, y = cewa_present_absent)

#logistic fit 1
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

#logistic fit 2
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

#logistic fit 3
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)


pairs.panels(dat_habitat[c(7:9,18)])

png(filename=here("eco_634_2021","bushtitpresvbasalarea.png"))
as.numeric(dat_all$BUSH > 0)->bush_present_absent
plot(x=dat_all$ba.tot,y=bush_present_absent,xlab="Basal area (m2 per ha)",ylab="Bushtit presence absense",main="Bushtit presence compared to basal area")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.5), add = TRUE)
dev.off()

png(filename=here("eco_634_2021","goldencrkingletpresvbasalarea.png"))
as.numeric(dat_all$GCKI > 0)->gcki_present_absent
plot(x=dat_all$ba.tot,y=gcki_present_absent,xlab="Basal area (m2 per ha)",ylab="Golden-cr Kinglet presence absense",main="Golden-cr Kinglet presence compared to basal area")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -1), add = TRUE)
dev.off()

#Q1 Basal area is the average amount of a designated area that has tree stems present. It tells us the number of trees in a designated area.

#Q2 

sum(dat_all$GRJA)

as.numeric(dat_all$GRJA>0)->GRJAamounts
length(GRJAamounts[which(GRJAamounts=='1')])
sum(GRJAamounts)
