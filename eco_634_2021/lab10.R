rm(list = ls())
rope<-read.csv(here("data","rope.csv"))
class(rope$rope.type)
rope$rope.type<-as.factor(rope$rope.type)
levels(rope$rope.type)
                        
n_obs = length(rope$blade)
n_groups = length(levels(rope$rope.type))

ss_tot = sum((rope$p.cut-mean(rope$p.cut))^2)                        

df_tot = n_obs-1

png(filename=here("eco_634_2021","lab_10_Q2.png"),width=1500,height=1600,units="px",res=180)
par(mfrow=c(1,2))
boxplot(rope$p.cut,xlab="all ropes",ylab="percent rope cut", main="percent rope cut by all ropes")
boxplot(rope$p.cut~rope$rope.type,xlab="rope type",ylab = "percent rope type",main="percent rope cut by rope type")
dev.off()   

resid_function = function(x){
  x-mean(x)
}

agg_resids =  aggregate(
      x = rope$p.cut,
      by = list(rope$rope.type), 
      FUN = resid_function)

sumsqresid_function = function(x){
  sum((x-mean(x))^2)
}

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = sumsqresid_function)
                        

ss_within = sum(agg_sq_resids$x) #4.875
df_within = n_obs-n_groups
                        
ss_among = ss_tot - ss_within #0.472
df_among = n_groups - 1
                        
ms_within = ss_within / (n_obs - n_groups) #0.0424
                        
ms_among  = ss_among / (n_groups - 1) #0.0946
                        
#dividing by degrees of freedom accounts for sample size and allows for a better comparison
                       
f_ratio = ms_among/ms_within
                    
f_pval = pf(f_ratio,n_groups - 1,n_obs - n_groups,lower.tail=FALSE)  
#0.0558

#anova
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)
str(anova_fit_1)
anova_fit_1$`Sum Sq`

bartlett.test(p.cut~rope.type,data=rope)

#question 5
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)
