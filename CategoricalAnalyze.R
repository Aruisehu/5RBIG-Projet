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

rm(list = ls())

shootings <- read_csv(file = "./US_Shootings_Cleaned.csv")
shootings.Gender <- shootings %>% 
  group_by(Gender) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Shootings=`n`/sum(`n`)) %>% 
  arrange(desc(Gender))
shootings.Gender$Label <- scales::percent(shootings.Gender$Shootings)

shootings.Race <- shootings %>% 
  group_by(Race) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Shootings=`n`/sum(`n`)) %>% 
  arrange(desc(Race))
shootings.Race$Label <- scales::percent(shootings.Race$Shootings)

shootings.State <- shootings %>% 
  group_by(State) %>%
  count() %>%
  ungroup() %>%
  mutate(Shootings=`n`/sum(`n`)) %>%
  arrange(desc(State))

shootings.State.TV <- shootings %>% 
  group_by(State) %>%
  summarise(Total.victims = sum(Total.victims)) %>%
  arrange(desc(State))

shootings.State$state <- shootings.State$State
shootings.State.TV$state <- shootings.State.TV$State

ggplot(shootings.Gender, aes(x="", y=Shootings, fill=Gender))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1, y = cumsum(Shootings) - Shootings/3, label=Label)) +
  ggtitle("US Shootings Frequencies By Gender")

ggplot(shootings.Race, aes(x="", y=Shootings, fill=Race))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1, y = cumsum(Shootings) - Shootings/3, label=Label)) +
  ggtitle("US Shootings Frequencies By Ethnicity")

plot_usmap(data = shootings.State, values = "n", color = "black") + 
  scale_fill_continuous(low = "lightgoldenrodyellow", high = "red", name = "Shootings", label = scales::comma) + 
  theme(legend.position = "right") +
  ggtitle("US Shootings By State")

plot_usmap(data = shootings.State.TV, values = "Total.victims", color = "black") + 
  scale_fill_continuous(low = "lightgoldenrodyellow", high = "red", name = "Victims", label = scales::comma) + 
  theme(legend.position = "right") +
  ggtitle("# of victims in Shootings in the US By State")

qmplot(Longitude, Latitude, data = shootings, maptype = "toner-lite", color = State)
