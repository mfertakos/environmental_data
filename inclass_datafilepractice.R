dat_catrate<-read.csv(here("data","catrate.csv"))
dat_delomys<-read.csv(here("data","delomys.csv"))
dat_rope<-read.csv(here("data","rope.csv"))

head(dat_catrate)
#3 columns are numeric
plot(dat_catrate)

head(dat_delomys)
#4 columns are numeric
png(file=here("datafilepractice.png"),width=600,height=500)
plot(x=dat_delomys$body_length,y=dat_delomys$body_mass,main="Matt Fertakos")
dev.off()

head(dat_rope)
#5 columns are numeric
plot(dat_rope)
