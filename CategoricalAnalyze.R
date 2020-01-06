## Differents catgorical variables and their meaning
## Title: Shooting press designation. No analyse to provide
## Location: Where the incident took place. maybe to analyse, by state should be more pertinent
## State: Us State where the shootings took place. Analyse
## Incident.Area: Type of area that was targeted. Analyse
## Open.Close.Location: Indicate if the Location was public or private (??). Analyse
## Target: People targeted by the shooter. Analyse
## Cause: Why the shooter acted. Analyse
## Summary: Incident summary. No analyse to provide
## Weapon.Type: What kind of weapon was used. Analyse
## Mental.Health.Issues: If and what kind of mental issues had the shooter. Analyse
## Race: Shooter enthnicity. Analyse
## Gender: Shooter assumed gender #RIPLGBTQ+. Analyse
## Day / Month: Date on which the shooting take place. Analyse??
## Ten.Casualities.Min: Total.Victims > 10. Analyse.

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

##################
## Gender study ##
##################

shootings.Gender <- shootings %>% 
  group_by(Gender) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Gender))
shootings.Gender$Label <- scales::percent(shootings.Gender$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.Gender), times=nrow(shootings.Gender))

shootings.Gender.ChiTest <- chisq.test(shootings.Gender$Shootings, p = uni_dist)

ggplot(shootings.Gender, aes(x="", y=ShootingsPct, fill=Gender))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1, y = cumsum(ShootingsPct) - ShootingsPct/3, label=Label)) +
  ggtitle("US Shootings Frequencies By Gender")

#####################
## Ethnicity study ##
#####################

shootings.Race <- shootings %>% 
  group_by(Race) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Race))
shootings.Race$Label <- scales::percent(shootings.Race$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.Race), times=nrow(shootings.Race))

shootings.Race.ChiTest <- chisq.test(shootings.Race$Shootings, p = uni_dist)

ggplot(shootings.Race, 
       aes(x = reorder(Race, -ShootingsPct),
           y = ShootingsPct)) + 
  geom_bar(stat = "identity", 
           fill = "lightblue", 
           color = "black") +
  geom_text(aes(label = Label), 
            hjust = -0.15) +
  scale_y_continuous(labels = percent) +
  labs(x = "Race", 
       y = "Percent", 
       title  = "US Shootings Frequencies By Ethnicity") +
  coord_flip()

#########################
## Incident area study ##
#########################

shootings.IncidentArea <- shootings %>% 
  group_by(Incident.Area) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Incident.Area))
shootings.IncidentArea$Label <- scales::percent(shootings.IncidentArea$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.IncidentArea), times=nrow(shootings.IncidentArea))

shootings.IncidentArea.ChiTest <- chisq.test(shootings.IncidentArea$Shootings, p = uni_dist)

ggplot(shootings.IncidentArea, 
       aes(x = reorder(Incident.Area, -ShootingsPct),
           y = ShootingsPct)) + 
  geom_bar(stat = "identity", 
           fill = "lightblue", 
           color = "black") +
  geom_text(aes(label = Label), 
            hjust = -0.15) +
  scale_y_continuous(labels = percent) +
  labs(x = "Area of incident", 
       y = "Percent", 
       title  = "US Shootings Frequencies By Area of incident") +
  coord_flip()


######################
## Open Close study ##
######################

shootings.OpenClose <- shootings %>% 
  group_by(Open.Close.Location) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Open.Close.Location))
shootings.OpenClose$Label <- scales::percent(shootings.OpenClose$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.OpenClose), times=nrow(shootings.OpenClose))

shootings.OpenClose.ChiTest <- chisq.test(shootings.OpenClose$Shootings, p = uni_dist)

ggplot(shootings.OpenClose, aes(x="", y=ShootingsPct, fill=Open.Close.Location))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1, y = cumsum(ShootingsPct) - ShootingsPct/3, label=Label)) +
  ggtitle("US Shootings Frequencies By Accessibility")

##################
## Target study ##
##################

shootings.Target <- shootings %>% 
  group_by(Target) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Target))
shootings.Target$Label <- scales::percent(shootings.Target$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution: does the shooting target distribution is uniform alongst target type

uni_dist <- rep(1/nrow(shootings.Target), times=nrow(shootings.Target))

shootings.Target.ChiTest <- chisq.test(shootings.Target$Shootings, p = uni_dist)

ggplot(shootings.Target, 
       aes(x = reorder(Target, -ShootingsPct),
           y = ShootingsPct)) + 
  geom_bar(stat = "identity", 
           fill = "lightblue", 
           color = "black") +
  geom_text(aes(label = Label), 
            hjust = -0.15) +
  scale_y_continuous(labels = percent) +
  labs(x = "Type of target", 
       y = "Percent", 
       title  = "US Shootings Frequencies By Target") +
  coord_flip()

#################
## Cause study ##
#################

shootings.Cause <- shootings %>% 
  group_by(Cause) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Cause))
shootings.Cause$Label <- scales::percent(shootings.Cause$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.Cause), times=nrow(shootings.Cause))

shootings.Cause.ChiTest <- chisq.test(shootings.Cause$Shootings, p = uni_dist)

ggplot(shootings.Cause, 
       aes(x = reorder(Cause, -ShootingsPct),
           y = ShootingsPct)) + 
  geom_bar(stat = "identity", 
           fill = "lightblue", 
           color = "black") +
  geom_text(aes(label = Label), 
            hjust = -0.15) +
  scale_y_continuous(labels = percent) +
  labs(x = "Cause", 
       y = "Percent", 
       title  = "US Shootings Frequencies By Cause") +
  coord_flip()

#######################
## Weapon Type study ##
#######################

shootings.WeaponType <- shootings %>% 
  group_by(Weapon.Type) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Weapon.Type))
shootings.WeaponType$Label <- scales::percent(shootings.WeaponType$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.WeaponType), times=nrow(shootings.WeaponType))

shootings.WeaponType.ChiTest <- chisq.test(shootings.WeaponType$Shootings, p = uni_dist)


ggplot(shootings.WeaponType, 
       aes(x = reorder(Weapon.Type, -ShootingsPct),
           y = ShootingsPct)) + 
  geom_bar(stat = "identity", 
           fill = "lightblue", 
           color = "black") +
  geom_text(aes(label = Label), 
            hjust = -0.15) +
  scale_y_continuous(labels = percent) +
  labs(x = "Type of weapon", 
       y = "Percent", 
       title  = "US Shootings Frequencies By Weapon Type") +
  coord_flip()

################################
## Mental Health Issues study ##
################################

shootings.MentalHealthIssues <- shootings %>% 
  group_by(Mental.Health.Issues) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Mental.Health.Issues))
shootings.MentalHealthIssues$Label <- scales::percent(shootings.MentalHealthIssues$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.MentalHealthIssues), times=nrow(shootings.MentalHealthIssues))

shootings.MentalHealthIssues.ChiTest <- chisq.test(shootings.MentalHealthIssues$Shootings, p = uni_dist)

ggplot(shootings.MentalHealthIssues, 
       aes(x = reorder(Mental.Health.Issues, -ShootingsPct),
           y = ShootingsPct)) + 
  geom_bar(stat = "identity", 
           fill = "lightblue", 
           color = "black") +
  geom_text(aes(label = Label), 
            hjust = -0.15) +
  scale_y_continuous(labels = percent) +
  labs(x = "Mental disease", 
       y = "Percent", 
       title  = "US Shootings Frequencies By mental disease") +
  coord_flip()

###############################
## Ten Casualities Min study ##
###############################

shootings.TenCasualitiesMin <- shootings %>% 
  group_by(Ten.Casualities.Min) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>% 
  arrange(desc(Ten.Casualities.Min))
shootings.TenCasualitiesMin$Label <- scales::percent(shootings.TenCasualitiesMin$ShootingsPct)

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.TenCasualitiesMin), times=nrow(shootings.TenCasualitiesMin))

shootings.TenCasualitiesMin.ChiTest <- chisq.test(shootings.TenCasualitiesMin$Shootings, p = uni_dist)

ggplot(shootings.TenCasualitiesMin, aes(x="", y=ShootingsPct, fill=Ten.Casualities.Min))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank(), axis.title.y = element_blank()) +
  geom_text(aes(x=1, y = cumsum(ShootingsPct) - ShootingsPct/3, label=Label)) +
  ggtitle("US Shootings Frequencies By Severity")

#################
## State study ##
#################
 
shootings.State <- shootings %>% 
  group_by(State) %>%
  count() %>%
  ungroup() %>%
  mutate(ShootingsPct=`n`/sum(`n`)) %>% 
  mutate(Shootings=`n`) %>%
  arrange(desc(State))

shootings.State.TV <- shootings %>% 
  group_by(State) %>%
  summarise(Total.victims = sum(Total.victims)) %>%
  arrange(desc(State))

# Chi Squared test of goodness fit to a uniform distribution

uni_dist <- rep(1/nrow(shootings.State), times=nrow(shootings.State))

shootings.State.ChiTest <- chisq.test(shootings.State$Shootings, p = uni_dist)

shootings.State$state <- shootings.State$State
shootings.State.TV$state <- shootings.State.TV$State

plot_usmap(data = shootings.State, values = "n", color = "black") + 
  scale_fill_continuous(low = "lightgoldenrodyellow", high = "red", name = "Shootings", label = scales::comma) + 
  theme(legend.position = "right") +
  ggtitle("US Shootings By State")

plot_usmap(data = shootings.State.TV, values = "Total.victims", color = "black") + 
  scale_fill_continuous(low = "lightgoldenrodyellow", high = "red", name = "Victims", label = scales::comma) + 
  theme(legend.position = "right") +
  ggtitle("# of victims in Shootings in the US By State")

qmplot(Longitude, Latitude, data = shootings, maptype = "toner-lite", color = State)
