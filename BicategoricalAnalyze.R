Sys.setlocale("LC_TIME", "English")

require(ggmap)
require(tidyverse)
require(ggplot2)
require(lubridate)
require(scales)
require(usmap)
require(grid)

rm(list = ls())

shootings <- read_csv(file = "./US_Shootings_Cleaned.csv")

#######################
## Gender/Race study ##
#######################

shootings.GenderRace <- shootings %>% 
  group_by(.dots=c("Gender","Race")) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`)
shootings.GenderRace$Label <- scales::percent(shootings.GenderRace$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution


shootings.GenderRace.ChiTest <- chisq.test(table(shootings$Race, shootings$Gender))

ggplot(shootings.GenderRace, aes(x=reorder(Race, -ShootingsPct), y=ShootingsPct, fill=Race))+
  geom_bar(width = 1, stat = "identity") +
  theme(axis.text.x=element_blank()) +
  ggtitle("US Shootings Frequencies By Gender and Race") +
  facet_wrap(~Gender)

###############################
## Cause/Incident Area study ##
###############################

shootings.CauseIncidentArea <- shootings %>% 
  group_by(.dots=c("Cause","Incident.Area")) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`)
shootings.CauseIncidentArea$Label <- scales::percent(shootings.CauseIncidentArea$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

shootings.CauseIncidentArea.ChiTest <- chisq.test(table(shootings$Cause, shootings$Incident.Area))

ggplot(shootings.CauseIncidentArea, aes(x=reorder(Incident.Area, -ShootingsPct), y=ShootingsPct, fill=Incident.Area))+
  geom_bar(width = 1, stat = "identity") +
  theme(axis.text.x=element_blank()) +
  ggtitle("US Shootings Frequencies By Cause and Incident.Area") +
  facet_wrap(~Cause)

###############################
## Target/Race Area study ##
###############################

shootings.TargetRace <- shootings %>% 
  group_by(.dots=c("Target","Race")) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`)
shootings.TargetRace$Label <- scales::percent(shootings.TargetRace$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

shootings.TargetRace.ChiTest <- chisq.test(table(shootings$Target, shootings$Race))

ggplot(shootings.TargetRace, aes(x=reorder(Target, -ShootingsPct), y=ShootingsPct, fill=Target))+
  geom_bar(width = 1, stat = "identity") +
  theme(axis.text.x=element_blank()) +
  ggtitle("US Shootings Frequencies By Target and Ethnicities") +
  facet_wrap(~Race)
