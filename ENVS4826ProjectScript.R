# ENVS 4826 Project
# Lindsay Scott, Pavlos Skarvelis, Madison Silver

library(ggplot2)
library(dplyr)
faodata <- read.csv("C:/Users/Keegan & Lindsay/Documents/SMU/Data Science/Project/ENVS4826Project/FAOSTAT_data_12-2-2020.csv")

# We need to rename the Area column in order for the script to run
faodata <- faodata %>%
  rename(Area = ï..Area)

# Figure 1 code, graph of World Total Agricultural Methane Emissions
figure1 <- faodata %>%
  filter(Item == "Agriculture total", Area == "World")
ggplot() +
  geom_line(data = figure1, aes(x = Year, y = Value), size = 1) +
  ylab("Value (Gg)") +
  scale_x_continuous(breaks = seq(1960, 2018, 5)) +
  theme_classic()

# Figure 2 code, graph of Canadian Total Agricultural Methane Emissions
figure2 <- faodata %>%
  filter(Item == "Agriculture total", Area == "Canada")
ggplot() +
  geom_line(data = figure2, aes(x = Year, y = Value), size = 1) +
  ylab("Value (Gg)") +
  scale_x_continuous(breaks = seq(1960, 2018, 5)) +
  theme_classic()

# Figure 3 code, graph of United States Total Agricultural Methane Emissions
figure3 <- faodata %>%
  filter(Item == "Agriculture total", Area == "United States of America")
ggplot() +
  geom_line(data = figure3, aes(x = Year, y = Value), size = 1) +
  ylab("Value (Gg)") +
  scale_x_continuous(breaks = seq(1960, 2018, 5)) +
  theme_classic()

# Figure 4 code, graph of World Agricultural Methane Emissions by Item type
figure4 <- faodata %>%
  filter(Item != "Agriculture total", Area == "World")
ggplot() +
  geom_line(data = figure4, aes(x = Year, y = Value, color = Item), size = 2) +
  ylab("Value (Gg)") +
  scale_x_continuous(breaks = seq(1960, 2018, 10)) +
  theme_classic()

# Figure 5 code, graph of Canadian Agricultural Methane Emissions by Item type
figure5 <- faodata %>%
  filter(Item != "Agriculture total", Area == "Canada")
ggplot() +
  geom_line(data = figure5, aes(x = Year, y = Value, color = Item), size = 2) +
  ylab("Value (Gg)") +
  scale_x_continuous(breaks = seq(1960, 2018, 10)) +
  theme_classic()

# Figure 6 code, graph of United States Agricultural Methane Emissions by Item type
figure6 <- faodata %>%
  filter(Item != "Agriculture total", Area == "United States of America")
ggplot() +
  geom_line(data = figure6, aes(x = Year, y = Value, color = Item), size = 2) +
  ylab("Value (Gg)") +
  scale_x_continuous(breaks = seq(1960, 2018, 10)) +
  theme_classic()

# Statistics time
# First we upload another csv, that has been edited in excel from the original dataset to make each of the Items into their own column
worlddata <- read.csv("C:/Users/Keegan & Lindsay/Documents/SMU/Data Science/Project/ENVS4826Project/FAOWorldData.csv")
# Then we can run the linear model, this one is for the world data
lmworld <- lm(Atot ~ EF + RC + BC + MM, data = worlddata)
summary(lmworld)
# And check the assumptions:
worldresids <- resid(lmworld)
shapiro.test(worldresids)
# The null hypothesis of normal distribution is accepted: there is no significant difference (p > 0.05) from a normal distribution
plot(lmworld)
# Using the scale-location graph of these plots, the points are close to evenly spread although the line is not horizontal. It is close, but not perfectly homoscedastic.
# We cannot know for sure if the observations are independent, but I think they are based on my knowledge of the data.

# The same thing as above, but now we need the data for just Canada
canadadata <- read.csv("C:/Users/Keegan & Lindsay/Documents/SMU/Data Science/Project/ENVS4826Project/FAOCanadaData.csv")
lmcanada <- lm(Atot ~ EF + BC + MM, data = canadadata)
summary(lmcanada)
# Now to check assumptions
canadaresids <- resid(lmcanada)
shapiro.test(canadaresids)
# This data is significantly different from normal distribution, p < 0.05, it does not meet the first assumption
plot(lmcanada)
# similar to the world data, the scale-location graph shows that there is some variance of the residuals and fitted values so it does not really meet the assumption of homoscedasticity
# The third assumption is the same as for world data, as all of this is from the same dataset just edited to be used for linear models.

# Finally we need to run a linear model for the USA data
usdata <- read.csv("C:/Users/Keegan & Lindsay/Documents/SMU/Data Science/Project/ENVS4826Project/FAOUSData.csv")
lmusa <- lm(Atot ~ EF + RC + BC + MM, data = usdata)
summary(lmusa)
# And check the assumptions for this
usaresids <- resid(lmusa)
shapiro.test(usaresids)
# The null hypothesis of normal distribution is accepted: there is no significant difference (p > 0.05) from a normal distribution
plot(lmusa)
# Once again, similar to the other two, the third plot of scale-location shows that the data is not homoscedastic and fails this test
# Also similar to before we are unsure of independence but believe this to be met.