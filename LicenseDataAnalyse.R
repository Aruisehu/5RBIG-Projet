Sys.setlocale("LC_TIME", "English")

require(ggmap)
require(tidyverse)
require(ggplot2)
require(lubridate)
require(scales)
require(usmap)
require(grid)

rm(list = ls())

gun_license <- read_csv(file="Us_License_Cleaned.csv")

qmplot(Longitude, Latitude, data = gun_license, maptype = "toner-background", zoom = 4,
       colour = I("yellow"), alpha = I(0.4), size = I(0.01))

ggmap(get_stamenmap(bbox = c(left = -170, bottom = 24, right = -50, top = 70), zoom = 4,
        color = "color",
        maptype = "toner-lite", force = FALSE
)) + geom_point(aes(x = Longitude, y = Latitude, color="blue"), data = gun_license, size = 0.01, alpha = 0.05) + 
  theme(legend.position="bottom")
