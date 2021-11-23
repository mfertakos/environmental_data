require(palmerpenguins)

image_file = "ugly_histogram.png"
png(height=1000,width=1200,filename=(here("eco_634_2021","images",image_file)))
hist(penguins$flipper_length_mm)
dev.off()


save_png_1<-function(image_file)
{
require(here)
png(height=1000,width=1200,filename=(here("eco_634_2021","images",image_file)))
}

save_png_1("ugly_histo_2.png")
  
hist(penguins$body_mass_g)
dev.off()

dat_vec = penguins$body_mass_g
my_title="Matt's Histogram"
xlabel="Matt's Data"
hist(dat_vec,col="steelblue",main=my_title,xlab=xlabel)

matts_hist_fun = function(dat_vec,my_title,xlabel)
{
  hist(dat_vec,col="steelblue",main=my_title,xlab=xlabel)
}

matts_hist_fun(dat_vec=sample(x=1:100,size=1000,replace=TRUE),
               my_title="Matt's random numbers",
               xlabel="x-values")


sample(x=1:100,size=1000,replace=TRUE)


#aggregate example
aggregate(penguins$flipper_length_mm,
          list(penguins$species),
          FUN=mean,na.rm=TRUE)

#flipper length ~ as explained by ~ species
aggregate(flipper_length_mm ~ species, 
          data = penguins,
          FUN=mean,
          na.rm=TRUE)
