Sys.setlocale("LC_TIME", "English")

require(ggmap)
require(tidyverse)
require(ggplot2)
require(lubridate)

rm(list = ls())

gKey <- "AIzaSyDXxR0Ta4eLE8tWBKlo-_f9R4BjNj94v2E"
register_google(key = gKey)
shootings <- read_csv(file = "./US Mass Shootings.csv" )
shootingsSave <- read_csv(file = "./US Mass Shootings.csv" )

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
# qmplot(Longitude, Latitude, data = shootings, maptype = "toner-lite", color = Race) +
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
shootings$Age2 <- sapply(map(shootings$Age, function(age) unlist(strsplit(gsub("(.{2})", "\\1;", as.character(age)), ";")[1])[2]), FUN = paste)
shootings$Age <- sapply(map(shootings$Age, function(age) unlist(strsplit(gsub("(.{2})", "\\1;", as.character(age)), ";")[1])[1]), FUN = paste)
shootings$Age[shootings$Title == "Ferguson, MO Drive by"] <- NA

# Cleaning Gender column
shootings$Gender <- tolower(shootings$Gender)
shootings <- shootings %>% mutate(Gender = replace(Gender, Gender == "m", "male"))
shootings <- shootings %>% mutate(Gender = replace(Gender, Gender == "f", "female"))
shootings <- shootings %>% mutate(Gender = replace(Gender, Gender %in% c("m/f", "male/female"), "unknown"))
shootings$Gender <- replace_na(shootings$Gender, "unknown")

# Cleaning of Target column
shootings$Target <- tolower(shootings$Target)
shootings$Target <- gsub("\\+|&|/", ";", shootings$Target)
shootings$Target <- gsub(" ; ", ";", shootings$Target)

# Cleaning of Race column
shootings$Race <- tolower(shootings$Race)
shootings$Race <- gsub("/some other race|/unknown", "", shootings$Race) # not sure if we should remove or place in "multiple" instead
shootings <- shootings %>% mutate(Race = replace(Race, Race == "some other race", "other"))
shootings <- shootings %>% mutate(Race = replace(Race, Race == "two or more races", "multiple"))
shootings <- shootings %>% mutate(Race = replace(Race, Race == "black american or african american", "black"))
shootings <- shootings %>% mutate(Race = replace(Race, Race == "white american or european american", "white"))
shootings <- shootings %>% mutate(Race = replace(Race, Race == "asian american", "asian"))
shootings <- shootings %>% mutate(Race = replace(Race, Race == "native american or alaska native", "native american"))
shootings$Race <- replace_na(shootings$Race, "unknown")

# Cleaning of Mental Health Issue colum
shootings$Mental.Health.Issues <- tolower(shootings$Mental.Health.Issues)
shootings$Mental.Health.Issues <- replace_na(shootings$Mental.Health.Issues, "unknown")

# Cleaning of Cause column
shootings$Cause <- tolower(shootings$Cause)
shootings <- shootings %>% mutate(Cause = replace(Cause, Cause == "domestic disputer", "domestic dispute"))
shootings$Cause <- replace_na(shootings$Cause, "unknown")

# Cleaning of Open.Close column
shootings$Open.Close.Location <- tolower(shootings$Open.Close.Location)
shootings$Open.Close.Location <- gsub("\\+", ";", shootings$Open.Close.Location)
shootings$Open.Close.Location <- replace_na(shootings$Open.Close.Location, "unknown")

# Cleaning of Weapon.Types
shootings$Weapon.Type <- tolower(shootings$Weapon.Type)
shootings$Weapon.Type <- gsub(", ", ";", shootings$Weapon.Type)
shootings <- shootings %>% mutate(Weapon.Type = replace(Weapon.Type, Weapon.Type == "semi-automatic;handgun", "semi-automatic handgun")) # not sure but the corresponding shootings mention only one semi auto handgun
shootings$Weapon.Type <- replace_na(shootings$Weapon.Type, "unknown")

# Cleaning of Incident Area
shootings$Incident.Area <- tolower(shootings$Incident.Area)
shootings$Incident.Area <- gsub("\\+", ";", shootings$Incident.Area)

shootings <- shootings %>%
  mutate(Incident.Area = case_when(
    # (^|\\s) permit us to ensure that a word start the string or is preceded with a whitespace
    # (\\s|$) permit us to ensure that a word end the string or is followed with a whitespace
    # both regular expression permit us to be sure that a detected word isn't a part of a another one, hence corrupting our data
    str_detect(Incident.Area, "(^|\\s)airport(\\s|$)") ~ "airport",
    str_detect(Incident.Area, "(^|\\s)association(\\s|$)") ~ "association",
    str_detect(Incident.Area, "(^|\\s)bus stop(\\s|$)|(^|\\s)bus station(\\s|$)") ~ "street",
    str_detect(Incident.Area, "(^|\\s)club(\\s|$)|(^|\\s)nightclub(\\s|$)|(^|\\s)movie theatre(\\s|$)|(^|\\s)spa(\\s|$)") ~ "place of entertainment",
    str_detect(Incident.Area, "(^|\\s)drive-by(\\s|$)") ~ "drive-by",
    str_detect(Incident.Area, "(^|\\s)elementary school(\\s|$)") ~ "elementary school",
    str_detect(Incident.Area, "(^|\\s)gas station(\\s|$)|(^|\\s)shop(\\s|$)") ~ "shop",
    str_detect(Incident.Area, "(^|\\s)high school(\\s|$)") ~ "high school",
    str_detect(Incident.Area, "(^|\\s)hospital(\\s|$)") ~ "hospital",
    str_detect(Incident.Area, "(^|\\s)multiple homes(\\s|$)|(^|\\s)apartment(\\s|$)") ~ "home",
    str_detect(Incident.Area, "(^|\\s)middle school(\\s|$)") ~ "middle school",
    str_detect(Incident.Area, "(^|\\s)party(\\s|$)|(^|\\s)concert(\\s|$)") ~ "event",
    str_detect(Incident.Area, "(^|\\s)post office(\\s|$)") ~ "administrative building",
    str_detect(Incident.Area, "(^|\\s)protest(\\s|$)") ~ "protest",
    str_detect(Incident.Area, "(^|\\s)restaurant(\\s|$)|(^|\\s)cafeteria(\\s|$)|(^|\\s)coffee(\\s|$)|(^|\\s)pub(\\s|$)") ~ "restaurant",
    str_detect(Incident.Area, "(^|\\s)river(\\s|$)|(^|\\s)forest(\\s|$)|(^|\\s)forests(\\s|$)|(^|\\s)campsite(\\s|$)") ~ "nature",
    str_detect(Incident.Area, "(^|\\s)street(\\s|$)|(^|\\s)sidewalk(\\s|$)|(^|\\s)bridge(\\s|$)|(^|\\s)park(\\s|$)|(^|\\s)square(\\s|$)|(^|\\s)parking(\\s|$)") ~ "street",
    str_detect(Incident.Area, "(^|\\s)temple(\\s|$)|(^|\\s)monastery(\\s|$)|(^|\\s)church(\\s|$)") ~ "place of worship",
    str_detect(Incident.Area, "(^|\\s)township(\\s|$)") ~ "township",
    str_detect(Incident.Area, "(^|\\s)university(\\s|$)|(^|\\s)college(\\s|$)") ~ "university",
    str_detect(Incident.Area, "(^|\\s)wal-mart(\\s|$)|(^|\\s)mall(\\s|$)|(^|\\s)grocery(\\s|$)") ~ "market",
    str_detect(Incident.Area, "(^|\\s)workplace(\\s|$)") ~ "company",
    TRUE ~ Incident.Area
  )
)

# Specific tratments for non standard values
# Using title and descriptions of the shootings
nursing <- c("a nursing home", "nursing home")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% nursing, "hospital"))

highSchools <- c("chardon high scool", "outside gym", "successtech academy")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% highSchools, "high school"))

university <- c("dormitory", "nursing classroom", "school campus", "lecture hall", "los angeles computer school", "appalachian school of law")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% university, "university"))

driveBy <- c("along a highway", "outside of liquor store")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% driveBy, "drive-by"))

protests <- c("black lives matter encampment")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% protests, "protest"))

events <- c("capitol hill neighborhood of seattle", "banquet hall", "south shore", "opening of motorcycle season")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% events, "event"))

homes <- c("home in rural alabama", "outside the house", "backyard of a house", "in home")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% homes, "home"))
            
laws <- c("county office building", "city hall building", "smith county courthouse", "edmond, oklahoma")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% laws, "administrative building"))

workpalces <- c(
  "accent signage systems building",
  "atlantis plastics factory",
  "esl in sunnyvale",
  "standard gravure printing company",
  "edgewater technology",
  "r.r. phelon company",
  "remodeling store",
  "ups facility",
  "manufacturer fiamma inc.",
  "warehouse",
  "entrance of building",
  "law firm"
)
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% workpalces, "company"))

townships <- c("amarillo, texas", "tulsa, oklahoma")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% townships, "township"))

restaurants <- c("chuck e. cheese")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% restaurants, "restaurant"))

supermakets <- c("cosmetics section of a macys department store")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% supermakets, "market"))

millitaryFacilities <- c("fort hood army post", "military facilities")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% millitaryFacilities, "millitary facility"))

stores <- c("liquor store", "village west apartments", "hair salon")
shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area %in% stores, "shop"))

shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area == "crown", NA))

shootings <- shootings %>% mutate(Incident.Area = replace(Incident.Area, Incident.Area == "interstate in hazelwood", "street"))

# Fix des valeurs
shootings$Incident.Area[shootings$Title == "Case Western Reserve University"] <- "university"
shootings$Incident.Area[shootings$Title == "Westside Middle School killings"] <- "middle school"
shootings$Incident.Area[shootings$Title == "Pennsylvania supermarket shooting"] <- "market"
shootings$Incident.Area[shootings$Title == "Pinellas Park High School"] <- "high school"
shootings$Incident.Area[shootings$Title == "Nellis Plaza"] <- "restaurant;supermaket"
shootings$Incident.Area[shootings$Title == "Parkland Coffee Shop"] <- "restaurant"
shootings$Incident.Area[shootings$Title == "Marysville-Pilchuck High School"] <- "high school"
shootings$Incident.Area[shootings$Title == "Youth With A Mission and New Life Church"] <- "association;place of whorship"
shootings$Incident.Area[shootings$Title == "Dearborn Post Office"] <- "administrative building"
shootings$Incident.Area[shootings$Title == "Offices of All-Tech Investment Group and Momentum Securities"] <- "home;company"
shootings$Incident.Area[shootings$Title == "Planned Parenthood clinic"] <- "street"
shootings$Incident.Area[shootings$Title == "Massachusetts Abortion Clinic"] <- "hospital"



# Separate Date in three columns
shootings$Date <- parse_date_time2(shootings$Date, "mdy", cutoff_2000 = 30)
shootings$Day <- format(shootings$Date, format = "%A")
shootings$Month <- format(shootings$Date, format ='%B')
shootings$Year <- as.integer(format(shootings$Date, format = "%Y"))
shootings$Date <- NULL

# Second point first step
shootings.cleaned <- shootings %>% mutate(Ten.Casualities.Min = if_else(Total.victims < 10, 0, 1))

write_csv(shootings.cleaned, "./US_Shootings_Cleaned.csv")

