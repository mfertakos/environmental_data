dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)
hist(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 7)
hist(dat_all$WIWA, breaks = 0:7-0.5)

#Q1
wiwa_counts = c(2, 6)
sum(log(dpois(x = wiwa_counts, lambda = 4)))

#Q2
dat_all$WIWR->wiwr_counts
sum(log(dpois(x=wiwr_counts,lambda = 1.46)))
hist(wiwr_counts)

#Q3
sum(log(dbinom(x=wiwr_counts,6,0.24)))
