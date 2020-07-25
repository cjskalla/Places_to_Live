library(ggplot2)
library(dplyr)
library(tidyr)
library(kableExtra)
library(lattice)
library(tinytex)
library(plotly)
library(GGally)
library(scales)

#Reading in all of the data
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


#Joining datasets to gather all the necessary columns
join_data <- inner_join(quality_of_life, population_density, by = "Country")
second_join_data <- inner_join(join_data, age_structure, by = "Country")

full_data <- second_join_data[, c(1, 2, 6, 7, 3, 5, 9, 10, 4, 8, 11, 12, 13, 14)]


#Gathering knowledge on the dataset
summary(full_data)



#Quality of Live vs Cost of Living with their respective leaders
qol_data <- filter(full_data, Quality.of.Life.Index > 142)
ggplot(qol_data, aes(y = reorder(Country, Quality.of.Life.Index), 
                      x = Quality.of.Life.Index, size = Cost.of.Living.Index)) +
  geom_point(color = "black")

col_data <- filter(full_data, Cost.of.Living.Index < 45)
ggplot(col_data, aes(y = reorder(Country, -Cost.of.Living.Index), 
                       x = Cost.of.Living.Index, size = Quality.of.Life.Index)) +
  geom_point(color = "black")


#Property Price to Income Graph
ppi_data <- filter(full_data, Property.Price.to.Income.Ratio < 8)
ggplot(ppi_data) +
  geom_col(aes(y = Property.Price.to.Income.Ratio, 
               x = reorder(Country, -Property.Price.to.Income.Ratio)),
           fill = "green") +
  labs(y = "Property Price to Income ", x = NULL) + coord_polar()


#Calculate a personal well-being rating off of col(6:9)
colnames(full_data)
summary(full_data$Health.Care.Index)
summary(full_data$Pollution.Index)
summary(full_data$Climate.Index)
summary(full_data$Safety.Index)

wellbeing_data <- full_data[,c(6:9)]
country_data <- as.data.frame(full_data[,1])

rescale.many <- function(dat, column.nos) { 
  nms <- names(dat) 
  for(col in column.nos) { 
    name <- paste(nms[col],".rescaled", sep = "") 
    dat[name] <- rescale(dat[,col]) 
  } 
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n")) 
  dat 
} 

scaled_wellbeing_data <- as.data.frame(rescale.many(wellbeing_data, c(1,2,3,4)))
scaled_wellbeing_data <- scaled_wellbeing_data[, c(5:8)]

binded_data <- cbind(country_data, scaled_wellbeing_data)
colnames(binded_data) = c("Country", "Health", "Pollution", "Climate",
                          "Safety")
equation_data <- mutate(binded_data, "wellbeing" = Health - Pollution + 
                          Climate + Safety) %>% filter(wellbeing > 1.09)

#Graphs for wellbeing data
ggplot(equation_data, aes(x = wellbeing, y = reorder(Country, wellbeing))) +
  geom_point(color = "blue", size = 2, 
             fill = alpha("lightblue", 0.5), shape = 21, stroke = 2)  +
  geom_segment(aes(x = 0, xend = wellbeing, y = Country, yend = Country)) +
  labs(x = "Total Wellbeing", y = NULL) 
