############
# Egg and Chick Project
# Justin Benjamin 
############

# Here is a project I am working on to explore if there are any patterns or 
# relationships between the size of Pūkeko (Porphyrio melanotus melanotus) eggs 
# (i.e. length and width), the order of laying and hatching, and the size of 
# chicks at hatching. I have been putting together a multi-year data set from
# 2008, 2010, 2013-19, 2022-23. Some years are missing some egg size data. 
# Otherwise I have data egg size, length from the end of the shield to the tip 
#of the beak, and length of the left outer tarsus at hatching measured in 
# millimeters using digital calipers. I also have the mass of the chicks at 
# hatching using a spring scale. The chicks were placed in small sacs and the 
# mass is the total mass minus the mass of the bag. The laying order is only 
# known when additional eggs were found in the nest during monitoring. 
# For example, there were 3 eggs when the nest was first found, and 4 eggs the 
# next day, we know which egg was laid 4th. Pūkeko can have varying degrees of 
# hatching synchrony with some clutches all hatching at the same time and other 
# nests that all hatch on different days. The hatching order is only known when 
# there were not synchronous hatching of multiple chicks at the same time. 
# Sometimes we can tell which chick came from which egg based on which eggs are 
# left in the nest when chicks are found, or we observe them actively hatching. 
# When this occurs, we can also tell the hatching order of the chicks. 
# Most of my analyses will be using this complete data set with known egg size, 
# hatching order, and chick morphometric measurements at hatching.

# Hypothesis: We hypothesize that females vary their pre-hatching reproductive 
# investment based on their dominance status and laying order relative to other 
# females in the clutch to increase their reproductive success. 

library(ggplot2)
library(lme4)
library(glmmTMB)
library(performance)
library(tidyr)
library(dplyr)
library(patchwork)
library(ggpubr)

## BMB: once you are working in a version control system you should
##  **not** have different, dated versions of data files ...

## BMB: don't use built-in names such as 'data' for variable names.
##  Mostly harmless but occasionally causes very confusing error messages.
data <- read.csv("Egg_chick_data_May_22.csv")
View(data)

## BMB: why do this? it's almost always best practice to keep variables
##  inside a data frame, so they stay consistent e.g. if you filter the data
##  set.

Width <- data$Width
Length <- data$Length

data <- data %>% 
        mutate(Volume = (0.525*Length*(Width^2)/1000))
# Volume calculation is from Narushin (2005) but we could change the volume 
# formula if necessary. 

long_data <- pivot_longer(data, 
                          cols = c("Shield.to.Tip", "Tarsus", "Mass"),
                          names_to = "Measurement",     
                          values_to = "Value")
View(long_data)

Year <- long_data$Year
Nest_ID <- long_data$Nest_ID
Egg_ID <- long_data$Egg_ID
Hatch_order <- long_data$Hatch_order
Measurement <- long_data$Measurement
Value <- long_data$Value


# Function to plot morphometric measurements to different predictor variables and 
# print the correlation test. Here I am making plots with egg volume and hatch order
# but other predictor variables could be added later. 

plot_and_correlate <- function(Data, measurement, predictor) {
  filtered_data <- Data %>%
    filter(Measurement == measurement) %>%
    filter(!is.na(Volume) & !is.na(Value) & !is.na(Hatch_order))
  
  plot <- ggplot(filtered_data, aes(x = !!sym(predictor), y = Value)) +
      geom_point() +
      theme_classic() +
      ylab(measurement) +
      geom_smooth(method = "lm") +
      ggpubr::stat_cor()
  
  ## cor_test_result <- cor.test(filtered_data[[predictor]], filtered_data$Value)
  ## return(cor_test_result)
  return(plot)
}

## BMB: how about
pairs(~Mass+Tarsus+Shield.to.Tip + Volume, data = data, gap = 0)
car::scatterplotMatrix(~Mass+Tarsus+Shield.to.Tip + Volume, data = data, gap = 0)


# Prediction 1: Larger eggs will hatch larger chicks.
p1 <- plot_and_correlate(long_data, "Mass", "Volume")
p2 <- plot_and_correlate(long_data, "Tarsus", "Volume")
p3 <- plot_and_correlate(long_data, "Shield.to.Tip", "Volume")
p1 + p2 + p3

## or even
ggplot(long_data, aes(Volume, Value)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggpubr::stat_cor() +
    facet_wrap(~Measurement, scale = "free")


# Prediction 2: Earlier hatched eggs will hatch larger chicks.
plot_and_correlate(long_data, "Mass", "Hatch_order")
plot_and_correlate(long_data, "Tarsus", "Hatch_order")
plot_and_correlate(long_data, "Shield.to.Tip", "Hatch_order")


# Prediction 3: There's an interaction between hatch order and egg volume on the 
# size of the chick. # The female (A, B, or C), 1st or 2nd clutch of the season, climate data such as 
# mean temperature and precipitation in the 30 days before laying could also be 
# added to these models. A challenge I'll face is that egg sizes may not be 
# independent between years as some of these data from different years may be from
# the same female. 

Mass_model <- glmmTMB(Value ~ Hatch_order*Volume + 
                     (1|Year/Nest_ID), data = long_data %>% filter(Measurement == "Mass"))
check_model(Mass_model)

Tarsus_model <- glmmTMB(Value ~ Hatch_order*Volume + 
                       (1|Year/Nest_ID), data = long_data %>% filter(Measurement == "Tarsus"))
check_model(Tarsus_model)

Shield_tip_model <- glmmTMB(Value ~ Hatch_order*Volume + 
                           (1|Year/Nest_ID), data = long_data %>% filter(Measurement == "Shield.to.Tip"))
check_model(Shield_tip_model)






#### Prediction 4: Earlier laid eggs are larger than later laid eggs. 

# Model looking at the effects of females and laying order on the size of eggs. 
# It's missing a lot of the data since we find most nests with eggs so not too sure
# how to proceed. This can come from a different dataset that doesn't include 
# chick data. Might be interesting to add some climate data for the yearly variation, 
# and maybe add first or second clutch of the season to the model. 

#Egg_size_model <- glmmTMB(Egg size ~ Laying_order + female + laying_order*female + (1|Year/Nest ID)





#### Prediction 5: Larger later-laid eggs will hatch earlier than smaller later-laid eggs

# Model exploring any pattern between laying order, female, and egg size on the 
# hatching order. Earlier laid eggs will obviously hatch earlier if there is 
# no delayed incubation, but maybe larger eggs are better at "catching up" 
# with development to hatch more synchronously?

# Hatch_order_model <- glmmTMB(Hatch_order ~ Laying_order + female + Egg_size + 
                            # Laying_order*female + Laying_order*Egg_size +
                            # female*Egg_size + (1|Year/Nest ID)


# To what extent does an individual female dictate the size of the eggs
# variation within females
# Egg pattern most abundant in the nest relative to egg size

# Prediction 4: Dominant females lay larger eggs than subordinate females.

# Perhaps even just the dominance status could alter the size of the eggs. This 
# prediction is related to earlier eggs are larger. 



