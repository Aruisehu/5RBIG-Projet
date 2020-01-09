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

gun_license <- read_csv(file="Us_License_Cleaned.csv")


gun_license <- gun_license %>% mutate(Address = paste(`Premise City`, " ", `Premise State`, " ", `Premise Zip Code`))

for(i in 1:nrow(gun_license))
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

gun_license[,"State"] <- as.character(NA)

# Alabama - AL
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Alaska - AK
gun_license$State[gun_license$`Premise State` == "AK"] <- "Alaska"
# Arizona - AZ
gun_license$State[gun_license$`Premise State` == "AZ"] <- "Arizona"
# Arkansas - AR
gun_license$State[gun_license$`Premise State` == "AR"] <- "Arkansas"
# California - CA
gun_license$State[gun_license$`Premise State` == "CA"] <- "California"
# Colorado - CO
gun_license$State[gun_license$`Premise State` == "CO"] <- "Colorado"
# Connecticut - CT
gun_license$State[gun_license$`Premise State` == "CT"] <- "Connecticut"
# Delaware - DE
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Florida - FL
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Georgia - GA
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Hawaii - HI
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Idaho - ID
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Illinois - IL
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Indiana - IN
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Iowa - IA
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Kansas - KS
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Kentucky - KY
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Louisiana - LA
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Maine - ME
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Maryland - MD
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Massachusetts - MA
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Michigan - MI
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Minnesota - MN
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Mississippi - MS
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Missouri - MO
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Montana - MT
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Nebraska - NE
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Nevada - NV
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# New Hampshire - NH
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# New Jersey - NJ
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# New Mexico - NM
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# New York - NY
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# North Carolina - NC
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# North Dakota - ND
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Ohio - OH
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Oklahoma - OK
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Oregon - OR
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Pennsylvania - PA
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Rhode Island - RI
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# South Carolina - SC
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# South Dakota - SD
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Tennessee - TN
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Texas - TX
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Utah - UT
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Vermont - VT
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Virginia - VA
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Washington - WA
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# West Virginia - WV
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Wisconsin - WI
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Wyoming - WY
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# American Samoa - AS
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# District of Columbia - DC
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Federated States of Micronesia - FM
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Guam - GU
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Marshall Islands - MH
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Northern Mariana Islands - MP
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Palau - PW
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Puerto Rico - PR
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"
# Virgin Islands - VI
gun_license$State[gun_license$`Premise State` == "AL"] <- "Alabama"



write_csv(gun_license, "./US_License_Cleaned.csv")
