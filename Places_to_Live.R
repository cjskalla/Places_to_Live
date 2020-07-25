library(ggplot2)
library(dplyr)
library(tidyr)
library(kableExtra)
library(lattice)
library(tinytex)
library(plotly)

cost_of_living <- read.csv("Data/Cost of living index by country 2020.csv") %>% 
  select(c(-Rent.Index, -Local.Purchasing.Power.Index))
names(cost_of_living)[names(cost_of_living) == 'ï..Country'] <- 'Country'

age_structure <- read.csv("Data/Coutries age structure.csv") 
names(age_structure)[names(age_structure) == 'ï..Country'] <- 'Country'

population_density <- read.csv("Data/Pupulation density by countries.csv") %>% 
  select(c(-ï..Rank, -Area.km2, -Area.km2, -Area.mi2, -Population, 
           -Density.pop..km2, -Date, -Population.source))
names(population_density)[names(population_density) == 
                            'Country..or.dependent.territory.'] <- 'Country'

quality_of_life <- read.csv("Data/Quality of life index by countries 2020.csv")
names(quality_of_life)[names(quality_of_life) == 'ï..Country'] <- 'Country'

join_data <- inner_join(quality_of_life, population_density, by = "Country")
join_data <- inner_join(join_data, age_structure, by = "Country")

full_data <- inner_join(join_data, crime, by = "Country")
full_data <- full_data[, c(1, 2, 6, 7, 3, 5, 9, 10, 4, 8, 11, 12, 13, 14)]

summary(full_data)
pairs(full_data)

