
install.packages("ggmap")
require(ggmap)
require(tidyverse)

gKey <- "AIzaSyDXxR0Ta4eLE8tWBKlo-_f9R4BjNj94v2E"
register_google(key = gKey)
data <- read.csv("US Mass Shootings.csv", header = TRUE)

for(i in 1:nrow(data))
{
  # Print("Working...")
  if (data$Location[i] == "") {
    result <- revgeocode(location = c(data$Longitude[i], data$Latitude[i]), output = "all", source = "google")
    address <- result$results[[1]]$address_components
    data$Location[i] <- Filter(function(u) "locality" %in% u$types, address)[[1]]$long_name
  }
  # data$Location[i] <- as.character(result[3])
}
rm(list = ls())

