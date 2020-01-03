Sys.setlocale("LC_TIME", "English")

require(ggmap)
require(tidyverse)
require(ggplot2)
require(lubridate)

rm(list = ls())

gKey <- "AIzaSyDXxR0Ta4eLE8tWBKlo-_f9R4BjNj94v2E"
register_google(key = gKey)
shootings <- read_csv(file = "./US Mass Shootings.csv" )

for(i in 1:nrow(shootings))
{
  # Print("Working...")
  if (is.na(shootings$Location[i]) || shootings$Location[i] == "") {
    result <- revgeocode(location = c(shootings$Longitude[i], shootings$Latitude[i]), output = "all", source = "google")
    address <- result$results[[1]]$address_components
    city <- Filter(function(u) "locality" %in% u$types, address)[[1]]$long_name
    shootings$Location[i] <- city
  }
  
  if(is.na(shootings$Longitude[i]) || is.na(shootings$Latitude[i])) {
    address <- paste(shootings$Location[i], shootings$State[i], sep = ", ")
    result <- geocode(address, output = "latlona", source = "google")
    shootings$Longitude[i] <- as.numeric(result[1])
    shootings$Latitude[i] <- as.numeric(result[2])
  }
}

# Make map of school shootings
# qmplot(Longitude, Latitude, shootings = shootings, maptype = "toner-lite", color = Race) +
#   facet_wrap(~ Race)

# Remove unused column
shootings$S. <- NULL

# Remove spaces from shootings frame columns names
names(shootings) <- make.names(names(shootings), unique = TRUE)

# Cleaning of columns types
shootings$Fatalities <- as.integer(shootings$Fatalities)
shootings$Injured <- as.integer(shootings$Injured)
shootings$Total.victims <- as.integer(shootings$Total.victims)
shootings$Policeman.Killed <- as.integer(shootings$Policeman.Killed)


# Cleaning of age
shootings$Age <- map(shootings$Age, function(age) sapply(seq(from=1, to=nchar(age), by=2), function(i) substr(shootings$Age, i, i + 1)))
shootings$Age <- gsub("(.{2})", "\\1;", as.character(shootings$Age));

# Cleaning Gender column
shootings$Gender <- tolower(shootings$Gender)
shootings <- shootings %>% mutate(Gender = replace(Gender, Gender == "m", "male"))
shootings <- shootings %>% mutate(Gender = replace(Gender, Gender == "f", "female"))
shootings <- shootings %>% mutate(Gender = replace(Gender, Gender %in% c("m/f", "male/female"), "unknown"))

# Cleaning of Target column
shootings$Target <- tolower(shootings$Target)
shootings$Target <- gsub("\\+|&|/", ";", shootings$Target)
shootings$Target <- gsub(" ; ", ";", shootings$Target)

# Cleaning of Race column
shootings$Race <- tolower(shootings$Race)
shootings <- shootings %>% mutate(Race = replace(Race, Race == "some other race", "other"))
shootings <- shootings %>% mutate(Race = replace(Race, Race == "two or more races", "multiple"))

shootings$Race <- gsub("/some other race|/unknown", "", shootings$Race) # not sure if we should remove or place in "multiple" instead

# Cleaning of Mental Health Issue colum
shootings$Mental.Health.Issues <- tolower(shootings$Mental.Health.Issues)

# Cleaning of Cause column
shootings$Cause <- tolower(shootings$Cause)
shootings <- shootings %>% mutate(Cause = replace(Cause, Cause == "domestic disputer", "domestic dispute"))

# Cleaning of Open.Close column
shootings$Open.Close.Location <- tolower(shootings$Open.Close.Location)
shootings$Open.Close.Location <- gsub("\\+", ";", shootings$Open.Close.Location)

# Cleaning of Weapon.Types
shootings$Weapon.Type <- tolower(shootings$Weapon.Type)
shootings$Weapon.Type <- gsub(", ", ";", shootings$Weapon.Type)
shootings <- shootings %>% mutate(Weapon.Type = replace(Weapon.Type, Weapon.Type == "semi-automatic;handgun", "semi-automatic handgun")) # not sure but the corresponding shootings mention only one semi auto handgun

# Separate Date in three columns
shootings$Date <- parse_date_time2(shootings$Date, "mdy", cutoff_2000 = 30)
shootings$Day <- format(shootings$Date, format = "%A")
shootings$Month <- format(shootings$Date, format ='%B')
shootings$Year <- as.integer(format(shootings$Date, format = "%Y"))
shootings$Date <- NULL
