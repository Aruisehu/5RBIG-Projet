Sys.setlocale("LC_TIME", "English")

require(ggmap)
require(tidyverse)
require(ggplot2)
require(lubridate)
require(scales)
require(usmap)
require(grid)

rm(list = ls())

gun_license <- read_csv(file="US_License_Cleaned.csv")

shootings <- read_csv(file="US_Shootings_Cleaned.csv")

gun_license <- gun_license[-which(is.na(gun_license$State)),]

shootings.State <- shootings %>%
  group_by(State) %>%
  count() %>%
  ungroup() %>%
  mutate(ShootingsPct=`n`/sum(`n`)) %>%
  mutate(Shootings=`n`) %>%
  arrange(desc(State))

gun_license.State <- gun_license %>%
  group_by(State) %>%
  count() %>%
  ungroup() %>%
  mutate(ShopsPct=`n`/sum(`n`)) %>%
  mutate(Shops=`n`) %>%
  arrange(desc(State))

Data <- merge(shootings.State[,-2], gun_license.State[,-2],by=c("State")) 

cor(Data$ShootingsPct, Data$ShopsPct)
rel <- lm(ShootingsPct ~ ShopsPct, data = Data)

plot(Data$ShootingsPct, Data$ShopsPct)

qmplot(Longitude, Latitude, data = gun_license, maptype = "toner-background", zoom = 4,
       colour = I("yellow"), alpha = I(0.4), size = I(0.01))

ggmap(get_stamenmap(bbox = c(left = -170, bottom = 24, right = -50, top = 70), zoom = 4,
        color = "color",
        maptype = "toner-lite", force = FALSE
)) + geom_point(aes(x = Longitude, y = Latitude), color = "lightblue3", data = gun_license, size = 0.01, alpha = 0.1) + 
  theme(legend.position="bottom")
