options(stringsAsFactors = FALSE) # turn-off character as factors
library(tidyverse)

## Import data
vanuatu.dat <- read.csv('Datasets/Vanuatu report 65.csv')
van1 <- dplyr::select(vanuatu.dat, 
                      trip_date, sp_name, sp_code, 
                      catch_n=sp_n, catch_kg=sp_kg)
focussp <- "DEEPWATER RED SNAPPER"

## Our goal: plot monthly average catch (in kg) 
## for this focal species

## First step, create a month column
## Use the package lubridate
library(lubridate)
van1$datefrm <- mdy_hms(van1$trip_date)
van1$Month <- month(van1$datefrm)
van1$Day <- day(van1$datefrm)

# Find year in dataset with the most observations
van1$Year <- year(van1$datefrm)
table(van1$Year)

# Now that we have a month column, let's filter for focussp
vanfilt <- filter(van1, sp_name==focussp)
# Introducing................ %>% !!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
vanfilt <- van1 %>% filter(sp_name==focussp)

# Using the pipe, filter van1 to keep years greater than 2020
apple <- van1 %>% filter(Year > 2020)

# Calculate the average catch (in kg) per month
# (all years aggregated)
moncatch <- vanfilt %>% 
  group_by(Month) %>% 
  summarise(catch=mean(catch_kg), 
            catchsd=sd(catch_kg), 
            maxn=max(catch_n))

## Calculate the average catch (in kg) per year
yrcatch <- vanfilt %>% 
  group_by(Year) %>% 
  summarise(catch=mean(catch_kg), 
            catchsd=sd(catch_kg), 
            maxn=max(catch_n))

## Average by year *and* month
ymoncatch <- vanfilt %>% 
  group_by(Year, Month) %>% 
  summarise(catch=mean(catch_kg), 
            catchsd=sd(catch_kg), 
            maxn=max(catch_n))

van1 %>% filter(sp_name==focussp) %>%
  group_by(Month) %>%
  summarise(catch=mean(catch_kg))

van1 %>% 
  group_by(sp_name, Month) %>%
  summarise(catch=mean(catch_kg)) %>% head

##############################
# Making plots ####
# Make a plot of average catch for deepwater red snapper
# dataset: moncatch
library(ggplot2)
theme_set(theme_bw()) # set ggplot theme for rest of session
# aes() = aesthetics --> **variables** that go into plot
ggplot(data=moncatch, 
       aes(x=Month, y=catch)) +
  geom_point(colour='dodgerblue') + 
  geom_line(colour='green') +
  theme_bw() +
  ggtitle('Average catch')


