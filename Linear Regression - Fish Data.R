install.packages("fst")
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("lubridate")
library(fst)
library(tidyverse)
library(ggrepel)
library(lubridate)

# Set working directory
setwd("C:/Users/hande/Downloads/datacamp/projects/12 - Linear Regression")
# Get working directory
getwd()
# Show folders in working directory
dir() 


fish <- read.csv("fish.csv")


# Simple Linear Regression: Predicting mass of bream using length

str(fish)

fish %>% 
  group_by(species) %>% 
  summarise()

bream <- fish %>% 
  filter(species == "Bream")

ggplot(bream, aes(length_cm, mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # + coord_fixed(0.02)

mdl_bream <- lm(mass_g ~ length_cm, data = bream)

explanatory_data_bream <- tibble(length_cm = seq(15, 42, 5))

prediction_data_bream <- explanatory_data_bream %>% 
  mutate(mass_g = predict(mdl_bream, explanatory_data_bream))

ggplot(bream, aes(length_cm, mass_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(
    data = prediction_data_bream, 
    color = "red") 


# Transforming variables: Predicting mass of perch using length

perch <- fish %>% 
  filter(species == "Perch")

# Relationship does not seem like a straight line here:

ggplot(perch, aes(length_cm, mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # + coord_fixed(0.02)

# So we transform it using "exponentiation" to fit a linear regression model
# I tried ^2 ^3 and ^4 and it seems like ^3 is a better choice for fitting a LR

ggplot(perch, aes(length_cm^3, mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # + coord_fixed(0.02)


# We apply the same in the model and wrap length in I()

mdl_perch <- lm(mass_g ~ I(length_cm^3), data = perch)


explanatory_data_perch <- tibble(length_cm = seq(10, 40, 5))

prediction_data_perch <- explanatory_data_perch %>% 
  mutate(mass_g = predict(mdl_perch, explanatory_data_perch))

# Visualize as linear (^3)

ggplot(perch, aes(length_cm^3, mass_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(
    data = prediction_data_perch, 
    color = "red", size = 3.5) 

# Visualize as non-linear (orijinal)

ggplot(perch, aes(length_cm, mass_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(
    data = prediction_data_perch, 
    color = "red", size = 3.5) 






