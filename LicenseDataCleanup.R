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

gun_license <- read_csv(file="US_License_Cleaned.csv")

# 
# gun_license <- gun_license %>% mutate(Address = paste(`Premise City`, " ", `Premise State`, " ", `Premise Zip Code`))
# 
# for(i in 1:nrow(gun_license))
# # DO NOT EXECUTE AGAIN FOR THE SAKE OF MY GCP BALANCE ACCOUNT (80000 costs 80€)
# {
#   if(is.na(gun_license$Latitude[i])) {
#     result <- geocode(gun_license$Address[i], output = "latlona", source = "google")
#     gun_license$Longitude[i] <- as.numeric(result[1])
#     gun_license$Latitude[i] <- as.numeric(result[2])
#   }
#   
#   if( i %% 500 == 0) {
#     write_csv(gun_license, "./US_License_Cleaned.csv")
#   }
# }

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
gun_license$State[gun_license$`Premise State` == "DE"] <- "Delaware"
# Florida - FL
gun_license$State[gun_license$`Premise State` == "FL"] <- "Florida"
# Georgia - GA
gun_license$State[gun_license$`Premise State` == "GA"] <- "Georgia"
# Hawaii - HI
gun_license$State[gun_license$`Premise State` == "HI"] <- "Hawaii"
# Idaho - ID
gun_license$State[gun_license$`Premise State` == "ID"] <- "Idaho"
# Illinois - IL
gun_license$State[gun_license$`Premise State` == "IL"] <- "Illinois"
# Indiana - IN
gun_license$State[gun_license$`Premise State` == "IN"] <- "Indiana"
# Iowa - IA
gun_license$State[gun_license$`Premise State` == "IA"] <- "Iowa"
# Kansas - KS
gun_license$State[gun_license$`Premise State` == "KS"] <- "Kansas"
# Kentucky - KY
gun_license$State[gun_license$`Premise State` == "KY"] <- "Kentucky"
# Louisiana - LA
gun_license$State[gun_license$`Premise State` == "LA"] <- "Louisiana"
# Maine - ME
gun_license$State[gun_license$`Premise State` == "ME"] <- "Maine"
# Maryland - MD
gun_license$State[gun_license$`Premise State` == "MD"] <- "Maryland"
# Massachusetts - MA
gun_license$State[gun_license$`Premise State` == "MA"] <- "Massachusetts"
# Michigan - MI
gun_license$State[gun_license$`Premise State` == "MI"] <- "Michigan"
# Minnesota - MN
gun_license$State[gun_license$`Premise State` == "MN"] <- "Minnesota"
# Mississippi - MS
gun_license$State[gun_license$`Premise State` == "MS"] <- "Mississippi"
# Missouri - MO
gun_license$State[gun_license$`Premise State` == "MO"] <- "Missouri"
# Montana - MT
gun_license$State[gun_license$`Premise State` == "MT"] <- "Montana"
# Nebraska - NE
gun_license$State[gun_license$`Premise State` == "NE"] <- "Nebraska"
# Nevada - NV
gun_license$State[gun_license$`Premise State` == "NV"] <- "Nevada"
# New Hampshire - NH
gun_license$State[gun_license$`Premise State` == "NH"] <- "New Hampshire"
# New Jersey - NJ
gun_license$State[gun_license$`Premise State` == "NJ"] <- "New Jersey"
# New Mexico - NM
gun_license$State[gun_license$`Premise State` == "NM"] <- "New Mexico"
# New York - NY
gun_license$State[gun_license$`Premise State` == "NY"] <- "New York"
# North Carolina - NC
gun_license$State[gun_license$`Premise State` == "NC"] <- "North Carolina"
# North Dakota - ND
gun_license$State[gun_license$`Premise State` == "ND"] <- "North Dakota"
# Ohio - OH
gun_license$State[gun_license$`Premise State` == "OH"] <- "Ohio"
# Oklahoma - OK
gun_license$State[gun_license$`Premise State` == "OK"] <- "Oklahoma"
# Oregon - OR
gun_license$State[gun_license$`Premise State` == "OR"] <- "Oregon"
# Pennsylvania - PA
gun_license$State[gun_license$`Premise State` == "PA"] <- "Pennsylvania"
# Rhode Island - RI
gun_license$State[gun_license$`Premise State` == "RI"] <- "Rhode Island"
# South Carolina - SC
gun_license$State[gun_license$`Premise State` == "SC"] <- "South Carolina"
# South Dakota - SD
gun_license$State[gun_license$`Premise State` == "SD"] <- "South Dakota"
# Tennessee - TN
gun_license$State[gun_license$`Premise State` == "TN"] <- "Tennessee"
# Texas - TX
gun_license$State[gun_license$`Premise State` == "TX"] <- "Texas"
# Utah - UT
gun_license$State[gun_license$`Premise State` == "UT"] <- "Utah"
# Vermont - VT
gun_license$State[gun_license$`Premise State` == "VT"] <- "Vermont"
# Virginia - VA
gun_license$State[gun_license$`Premise State` == "VA"] <- "Virginia"
# Washington - WA
gun_license$State[gun_license$`Premise State` == "WA"] <- "Washington"
# West Virginia - WV
gun_license$State[gun_license$`Premise State` == "WV"] <- "West Virginia"
# Wisconsin - WI
gun_license$State[gun_license$`Premise State` == "WI"] <- "Wisconsin"
# Wyoming - WY
gun_license$State[gun_license$`Premise State` == "WY"] <- "Wyoming"
# American Samoa - AS
gun_license$State[gun_license$`Premise State` == "AS"] <- "American Samoa"
# District of Columbia - DC
gun_license$State[gun_license$`Premise State` == "DC"] <- "District of Columbia"
# Federated States of Micronesia - FM
gun_license$State[gun_license$`Premise State` == "FM"] <- "Federated States of Micronesia"
# Guam - GU
gun_license$State[gun_license$`Premise State` == "GU"] <- "Guam"
# Marshall Islands - MH
gun_license$State[gun_license$`Premise State` == "MH"] <- "Marshall Islands"
# Northern Mariana Islands - MP
gun_license$State[gun_license$`Premise State` == "MP"] <- "Northern Mariana Islands"
# Palau - PW
gun_license$State[gun_license$`Premise State` == "PW"] <- "Palau"
# Puerto Rico - PR
gun_license$State[gun_license$`Premise State` == "PR"] <- "Puerto Rico"
# Virgin Islands - VI
gun_license$State[gun_license$`Premise State` == "VI"] <- "Virgin Islands"

write_csv(gun_license, "./US_License_Cleaned.csv")
