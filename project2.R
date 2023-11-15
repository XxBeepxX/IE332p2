data <- read.csv("training_set.csv")
#data <- training_set.2

run_time <- system.time({
  
  
  
  
posinst <- subset(data, class == "pos")
neginst <- subset(data, class == "neg")
})[3]
print(paste("Run Time:", run_time, "seconds"))
