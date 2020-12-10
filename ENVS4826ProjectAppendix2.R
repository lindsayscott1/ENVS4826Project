# ENVS 4826 Project
# Lindsay Scott, Pavlos Skarvelis, Madison Silver
# Code that we did not use, as it was not useful for our project

library(ggplot2)
library(dplyr)
faodata <- read.csv("C:/Users/Keegan & Lindsay/Documents/SMU/Data Science/Project/ENVS4826Project/FAOSTAT_data_12-2-2020.csv")

# We made this faceted graph to try and look at everything at once, but it did not work out
ggplot() +
  geom_line(data = faodata, aes(x = Year, y = Value, color = ï..Area)) +
  facet_wrap(.~ faodata$Item)

# So then we tried to look at one item but with all of the areas, and the scales were so different it was too hard to see the changes
faodata1 <- faodata %>%
  filter(Item == "Agriculture total")
ggplot() +
  geom_line(data = faodata1, aes(x = Year, y = Value, color = ï..Area))
# Then we thought maybe facets would work for some reason
ggplot() +
  geom_line(data = faodata1, aes(x = Year, y = Value)) +
  facet_wrap(.~ faodata1$ï..Area)
# After these failed graphs, we then found what did work for the final code, separating each area, and no facets

# We then tried and failed at linear regression models before we realized the best way was to make new csv documents that were reorganized for our needs
faodata2 <- faodata %>%
  filter(ï..Area == "World")
lmworld <- lm(Value ~ Item, data = faodata2)
summary(lmworld)
# We wanted to look at each of the Items against Ag total, and for that to work properly they each had to be in their own column. We did not know how to do this in R so we did it in Excel and imported the new csv's.