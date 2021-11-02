
library(tidyverse)
library(ggplot2)

data <- read_delim("biodiversity_data.csv", delim = ',')

# cleaning the data
clean_data <- data %>%
  select(abiotic_hab, species, n) %>%
  group_by(abiotic_hab)

summary(clean_data)

# Cole's code for species richness per quadrat
quad_richness <- data %>%
  drop_na(n) %>%
  group_by(vertical_transect,quadrat) %>%
  summarize(n_spp = n())

# finding species richness for each abiotic habitat type
boulder_richness <- clean_data %>%
  filter(abiotic_hab == "boulder") %>%
  distinct()

bedrock_richness <- clean_data %>%
  filter(abiotic_hab == "bedrock") %>%
  distinct()

cobble_richness <- clean_data %>%
  filter(abiotic_hab == "cobble") %>%
  distinct()

pebbles_richness <- clean_data %>%
  filter(abiotic_hab == "pebbles") %>%
  distinct()

sand_richness <- clean_data %>%
  filter(abiotic_hab == "sand") %>%
  distinct()

# total n of each species for each substrate type 
aggboulder<- aggregate(list(boulder_data$n), by = list(boulder_data$species), sum)

bedrock_data <- clean_data %>%
  filter(abiotic_hab == "bedrock")

aggbedrock <- aggregate(list(bedrock_data$n), by = list(bedrock_data$species), sum)

cobble_data <- clean_data %>%
  filter(abiotic_hab == "cobble")

aggcobble <- aggregate(list(cobble_data$n), by = list(cobble_data$species), sum)

pebbles_data <- clean_data %>%
  filter(abiotic_hab == "pebbles")

aggpebbles <- aggregate(list(pebbles_data$n), by = list(pebbles_data$species), sum)

sand_data <- clean_data %>%
  filter(abiotic_hab == "sand")

aggsand <- aggregate(list(sand_data$n), by = list(sand_data$species), sum)

ggplot(data, aes(x = abiotic_hab, y = species)) +
  geom_boxplot()
