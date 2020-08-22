library(ggplot2)
library(dplyr)
library(tidyr)
library(kableExtra)
library(lattice)
library(tinytex)
library(plotly)
library(GGally)
library(scales)
library(ggthemes)
library(ggrepel)

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
full_data <- filter(full_data, Quality.of.Life.Index > 135)

country_data <- as.data.frame(full_data[,1])
number_data <- as.data.frame(full_data[, c(2, 6, 7, 3, 5, 9, 4, 8)])

#Scaling data to minmax to essentially make percentiles
rescale.many <- function(dat, column.nos) { 
  nms <- names(dat) 
  for(col in column.nos) { 
    name <- paste(nms[col],".rescaled", sep = "") 
    dat[name] <- rescale(dat[,col]) 
  } 
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n")) 
  dat 
} 

scaled_number_data <- as.data.frame(rescale.many(number_data, c(1,2,3,4,5,6,7,8)))
scaled_number_data <- as.data.frame(scaled_number_data[, c(9:16)])
scaled_full_data <- cbind(country_data, scaled_number_data)
scaled_full_data <- scaled_full_data[, c(1, 2, 5, 6, 8, 3, 7, 4, 9)]

colnames(scaled_full_data) = c("Country", "Quality", "Cost", "Purchasing.Power", 
                               "Property.Price.to.Income", "Health.Care", 
                               "Safety", "Pollution", "Climate")
scaled_full_data <- mutate(scaled_full_data, "Quality.Cost" = Quality/Cost, 
                           "WellBeing" = Health.Care - Pollution + Climate + 
                             Safety) %>% select(-Health.Care, -Safety, -Pollution, 
                                                -Climate)

scaled_full_data <- as.data.frame(rescale.many(scaled_full_data, c(6,7))) %>% 
  select(-Quality.Cost, -WellBeing)

scaled_full_data[36, 6] = 1.0
scaled_full_data[22, 6] = 0.95

#Gathering knowledge on the dataset
summary(full_data$Quality.of.Life.Index)


#Quality of Life Plot
qol_data <- scaled_full_data %>% arrange(desc(Quality))
qol_plot <- ggplot(qol_data[1:10,], aes(y = reorder(Country, Quality), 
                                              x = Quality)) +
  geom_point(color = "black", size = 5) + labs(title = "Top 10 Qualities of Life", 
                                               x = "Quality of Life Percentile", 
                                               y = "Country")
qol_plot


#Cost of Living Plot
col_data <- scaled_full_data %>% arrange(desc(-Cost))
col_plot <- ggplot(col_data[1:10,], aes(y = reorder(Country, -Cost), 
                                              x = Cost)) +
  geom_point(color = "black", size = 5) + labs(title = 
                                                 "Top 10 Cheapest Costs of Living", 
                                               x = "Cost of Living Percentile", 
                                               y = "Country")

# to make lollipop chart => geom_segment(aes(x = 0, xend = Cost.of.Living.Index, 
#y = Country, yend = Country))



col_plot



#Property Price to Income Graph
ppi_data <- scaled_full_data %>% arrange(desc(-Property.Price.to.Income))
ppi_plot <- ggplot(ppi_data[1:10,]) +
  geom_col(aes(y = Property.Price.to.Income, 
               x = reorder(Country, -Property.Price.to.Income)),
           fill = "green") +
  labs(y = "Property Price to Income ", x = NULL) + coord_polar()


ppi_plot


#Ratio Plot
ratio_data <- scaled_full_data %>% arrange(desc(Quality.Cost.rescaled))
ratio_plot <- ggplot(ratio_data[1:10,], aes(y = reorder(Country, 
                                                        Quality.Cost.rescaled), 
                                                  x = Quality.Cost.rescaled)) +
  geom_point(aes(y = reorder(Country, Quality.Cost.rescaled), 
                                             x = Quality.Cost.rescaled, 
                                             size = WellBeing.rescaled*10), 
             color = "blue", fill = alpha("lightblue", 0.5), shape = 21, stroke = 2)  +
  geom_segment(aes(x = 0, xend = Quality.Cost.rescaled, y = Country, yend = Country), 
               color = "white") + 
  labs(title = "Top 10 Quality over Cost", 
       x = "Quality over Cost Percentile", y = "Country") + 
  theme_solarized_2(light=FALSE) + scale_colour_solarized('blue') +
  theme(axis.text.y = element_text(angle = 360))


ratio_plot




#Graph for wellbeing data
well_being_data <- scaled_full_data %>% arrange(desc(WellBeing.rescaled))
wellbeing_plot <- ggplot(well_being_data[1:10,], aes(x = WellBeing.rescaled, 
                                                     y = reorder(Country, 
                                                                 WellBeing.rescaled))) +
  geom_col(aes(y = WellBeing.rescaled, 
               x = reorder(Country, WellBeing.rescaled)), fill = "#800000", 
           width = 0.5)  + 
  theme_wsj()+ scale_colour_wsj("colors6") + coord_flip() +
  labs(title = "Top 10 Well-being", 
       x = "Total Well-being", y = "Country", caption = 
         "Well-being Percentile calculated by aggregating indexes of: Healthcare, 
         Pollution, Climate, 
       and Safety")

wellbeing_plot


#Scatterplot of Well-Being vs Quality over Cost
mean(scaled_full_data$Quality.Cost.rescaled)
colnames(scaled_full_data)
wb_qoc_plot <- ggplot(scaled_full_data) +
  geom_point(aes(x = Quality.Cost.rescaled, 
                 y = WellBeing.rescaled, 
                 size = 10*Property.Price.to.Income, 
                 color = Purchasing.Power)) + 
  geom_text_repel(data = subset(scaled_full_data, Quality.Cost.rescaled > 0.6 |
                                         WellBeing.rescaled > 0.82), 
                           aes(x = Quality.Cost.rescaled, 
                               y = WellBeing.rescaled, label = Country)) + 
  geom_hline(yintercept = mean(scaled_full_data$WellBeing.rescaled), 
             linetype = "dashed") + 
  geom_vline(xintercept = mean(scaled_full_data$Quality.Cost.rescaled),
             linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red", na.value = NA) 
  

wb_qoc_plot

