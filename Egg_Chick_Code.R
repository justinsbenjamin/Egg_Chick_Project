###########
# Egg and Chick Project
# Justin Benjamin 
###########

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


library(ggplot2)
library(lme4)
library(glmmTMB)
library(performance)

data <- read.csv("All_data_May_2.csv")
View(data)

Nest_ID <- data$Nest_ID
Egg_ID <- data$Egg_ID
Length <- data$Length
Width <- data$Width
Shield_tip <- data$Shield.to.Tip
Tarsus <- data$Tarsus
Mass <- data$Mass
Hatch_order <- data$Hatch_order












