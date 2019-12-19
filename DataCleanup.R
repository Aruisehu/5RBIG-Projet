
install.packages("ggmap")
require(ggmap)
require(tidyverse)
require(ggplot2)
rm(list = ls())

gKey <- "AIzaSyDXxR0Ta4eLE8tWBKlo-_f9R4BjNj94v2E"
register_google(key = gKey)
data <- read.csv("US Mass Shootings.csv", header = TRUE, stringsAsFactors = FALSE)

for(i in 1:nrow(data))
{
  # Print("Working...")
  if (data$Location[i] == "") {
    result <- revgeocode(location = c(data$Longitude[i], data$Latitude[i]), output = "all", source = "google")
    address <- result$results[[1]]$address_components
    city <- Filter(function(u) "locality" %in% u$types, address)[[1]]$long_name
    data$Location[i] <- city
  }
  
  if(is.na(data$Longitude[i]) || is.na(data$Latitude[i])) {
    address <- paste(data$Location[i], data$State[i], sep = ", ")
    result <- geocode(address, output = "latlona", source = "google")
    data$Longitude[i] <- as.numeric(result[1])
    data$Latitude[i] <- as.numeric(result[2])
  }
}

# Make map of school shootings
# qmplot(Longitude, Latitude, data = data, maptype = "toner-lite", color = Race) +
#   facet_wrap(~ Race)
