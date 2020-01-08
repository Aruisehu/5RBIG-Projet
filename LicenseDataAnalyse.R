Sys.setlocale("LC_TIME", "English")

require(ggmap)
require(tidyverse)
require(ggplot2)
require(lubridate)
require(scales)
require(usmap)
require(grid)

rm(list = ls())

gun_license <- read_delim('firearm_licences_january_2017.tsv', delim="\t")

gun_license <- gun_license %>% mutate(Address = paste(`Premise City`, " ", `Premise State`, " ", `Premise Zip Code`))