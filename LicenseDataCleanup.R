Sys.setlocale("LC_TIME", "English")

require(ggmap)
require(tidyverse)
require(ggplot2)
require(lubridate)
require(scales)
require(usmap)
require(grid)

rm(list = ls())
gKey <- "AIzaSyDXxR0Ta4eLE8tWBKlo-_f9R4BjNj94v2E"
register_google(key = gKey)
# gun_license <- read_delim('firearm_licences_january_2017.tsv', delim="\t")
gun_license <- read_csv(file = 'US_License_Cleaned.csv')

gun_license <- gun_license %>% mutate(Address = paste(`Premise City`, " ", `Premise State`, " ", `Premise Zip Code`))

for(i in 15000:nrow(gun_license))
{
  if(is.na(gun_license$Latitude[i])) {
    result <- geocode(gun_license$Address[i], output = "latlona", source = "google")
    gun_license$Longitude[i] <- as.numeric(result[1])
    gun_license$Latitude[i] <- as.numeric(result[2])
  }
  
  if( i %% 500 == 0) {
    write_csv(gun_license, "./US_License_Cleaned.csv")
  }
}

write_csv(gun_license, "./US_License_Cleaned.csv")
