library(ggplot2)
library(readr)
library(tidyverse)
library(ggprism)
library(rworldmap)
library(countrycode)

# read in the data

demographic_data <- read_csv("Data/users.csv") %>% 
  filter(age >= 18)%>% 
  group_by(age, gender, country)%>%
  tally() %>% 
  mutate(population = n)

# plot age and gender

demographic_data$population <- ifelse(demographic_data$gender == "M", -1*demographic_data$population, demographic_data$population)

ggplot(demographic_data, aes(x = age, fill = gender, y = population))+
geom_bar(data = subset(demographic_data, gender == "F"), stat = "identity") +
  geom_bar(data = subset(demographic_data, gender == "M"), stat = "identity")+
  geom_bar(data = subset(demographic_data, gender == "Unknown"), stat = "identity")+
  scale_fill_manual(labels = c("Female", "Male", "Unknown"), values = c("#bf6371","#f27853", "#ffa600"))+
theme_prism()+
  xlab("Age")+
  ylab("Count")+
  scale_y_continuous(limits = c(-120, 120), breaks = c(-120, -80, -40, 0, 40, 80, 120), labels = c(120, 80, 40, 0, 40, 80, 120))+ 
  coord_flip()+
  expand_limits(x = c(18,87))+
  theme(legend.position = "top")

ggsave("Figures/population_pyramid.png", width = 6, height = 6, units = "in", type = "cairo", dpi = 600)

# read in ISO2 codes
ISO_data <- read_csv("Data/ISO_codes.csv")

# read in country data
country_data <- read_csv("Data/users.csv") %>% 
dplyr::rename(Code = country)

# join ISO codes and data

full_data <- left_join(ISO_data, country_data, by = "Code") %>%
  mutate(Code = ifelse(is.na(Code), "NA", Code)) %>% 
  complete(Code, fill = list(n = 0)) %>% 
  group_by(Code) %>% 
  tally()

joinData <- joinCountryData2Map(full_data,
                                 joinCode = "ISO2",
                                 nameJoinColumn = "Code", 
                                verbose = TRUE)

new_world <- subset(joinData, continent != "Antarctica")

# plot world map
# save map

png(file = "Figures/world_map.png",
    width = 1024, height = 768)

par(mai=c(0,0,0.2,0))

theMap <- mapCountryData(new_world, nameColumnToPlot="n", 
                         xlim = NA, 
                         ylim = NA,
                         addLegend = T,
                         catMethod = "logFixedWidth",
                         colourPalette = c("#ffdc8a","#ffc885", "#ffb584", "#ff8e71", "#e38d84", "#d27e86", "#bd7187"),
                         #oceanCol="lightblue",
                         missingCountryCol="white",
                         mapTitle = "")

theMap
 # add legend

do.call(addMapLegend, list(legendLabels="all",
                          legendWidth=0.5,
                          legendIntervals="data",
                          legendMar=2))

dev.off()


