ginkgo<-read.csv(here("data","ginkgo_data_2021.csv"))
boxplot(ginkgo$notch_depth~ginkgo$seeds_present)

#Question 2
#

#Question 3
plot(ginkgo$max_width~ginkgo$max_depth)

#Question 4
#As max depth increases max width increases. Looks like it has a positive slope.

#Question 5
#Our data collection could have violated the fixed x assumption because tehre could have been measurement error
#especially because multiple people were making the measurements. It is also possible people collected the 
#leaves non-randomly around the tree.

#Question 6
