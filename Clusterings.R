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

shootings.TotalVictims <- shootings[rep(seq_len(dim(shootings)[1]), shootings$Total.victims), 1:26]

clusters <- kmeans(shootings.TotalVictims[c('Latitude', 'Longitude')], 5)

# Save the cluster number in the dataset as column 'Borough'
shootings.TotalVictims$Clusters <- as.factor(clusters$cluster)

hierachical.cluster <- hclust(dist(shootings.TotalVictims[c('Latitude', 'Longitude')]))
plot(hierachical.cluster)

qmplot(Longitude, Latitude, data = shootings.TotalVictims, maptype = "terrain-background", color = Clusters)

# shootings[rep(seq_len(dim(shootings)[1]), shootings$Total.victims), 1:26]
