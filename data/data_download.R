# I don't normally do this, but we need to start with zero objects in the global environment:
rm(list = ls()) 

library(agridat)

data(cochran.crd)
data(chinloy.fractionalfactorial)
data(burgueno.alpha)
data(cochran.latin)
data(durban.splitplot)
data(archbold.apple)
data(beall.webworms)  
data(stroup.nin)

datasets <- ls()

lapply(datasets, function(x) {
  temp <- mget(x, envir = .GlobalEnv)[[1]]
  name <- gsub("\\.", "_", x)
  write.csv(temp, paste0("data/", name, ".csv"), row.names = FALSE)
})

