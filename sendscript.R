#analysis of climbing progress based on mountain project ticks

library(tidyverse)

#set wd

wd <- setwd("/Users/sagepletka/Documents/data projects/climbing.progression")
setwd("/Users/sagepletka/Documents/data projects/climbing.progression")
getwd()
 
#import data from ticks and clean
ticks <- read_csv("/Users/sagepletka/Documents/data projects/climbing.progression/mtn.projs.ticks.csv",
                 skip = 1 )

#using my suggested grade or mtn projs default if i didn't grade
cleanticks <- ticks %>% select(Date, Route, Rating, `Your Rating`) %>%
  rename(mygrade = `Your Rating`) %>%
  mutate(grade = ifelse(is.na(mygrade), Rating, mygrade)) %>%
  select(-Rating, -mygrade)

#remove sport grades, convert v grades to integers
#v-easy left out, slash grades were given the lower grade for the sandbag
#remove all grades below v5- they don't particularly contribute and make the data unnecessarily messy
#they aren't important for an analysis of progression since I climbed v5 my first year 
cleanticks2 <- cleanticks %>% filter(str_starts(grade, "V")) %>% 
  mutate(grade = str_sub(grade, 2, 3))
cleanticks2$grade <- as.numeric(cleanticks2$grade)
finalticks <- cleanticks2 %>% filter(grade >= 5) %>%
  mutate(year = str_sub(Date, 1, 4))

#manually change life after death star to v8
finalticks$grade[61] <- 8

#visuzalize
#raw sends
allsends <- ggplot(finalticks, aes(Date, grade)) + geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)
#send density per year
sends_by_year <- finalticks %>% ggplot(aes(grade)) + 
  geom_histogram(bins = 6, aes(fill = year), color = "black") +
  facet_grid(~ year,)
#max grade each year
maxgrade <- finalticks %>% group_by(year) %>%
  summarise(maxgrade = max(grade)) %>% ggplot(aes(year, maxgrade)) +
  geom_col() + ylim(0, 16)

#can i make a better estimator? raw sends penalizes climbing submaximally
#max grade will also penalize non-projecting sends
#sends by year is a bit more unbiased but if anything creates an illusion of bias
#towards more submaximal sends
#we might get a better picture of my ability to send if we track sends from a
#total volume perspective

#lets find cumulative (v5 and up) v points per year
vpoints_per_year <- finalticks %>% group_by(year) %>% summarise(vpoints = sum(grade))
vpoint_per_year_plot <- ggplot(vpoints_per_year, aes(year, vpoints)) + 
  geom_col()
#this is interesting as my total points went down




#in summary it seems that in 2022 my total v points and adjusted volume points
#both went down- likely atleast in part due to moving to indianapolis
#and despte feeling stronger I just sent less

#exponential scale
expon_scale_points <- finalticks %>% mutate(expon = 2^grade) %>% group_by(year) %>% 
  summarise(per_year = sum(expon)) %>%
  ggplot(aes(year, per_year)) + geom_col()


#number of boulders climbed each year 
bouldernumb <- finalticks %>% group_by(year) %>% summarize(sendnumb = length(Route)) %>%
  ggplot(aes(year, sendnumb)) + geom_col()

#look at sendnumber per year versus vpoints and exponentailly scaled point
library(patchwork)

grid <- (allsends + sends_by_year) / (bouldernumb + 
                                        vpoint_per_year_plot + 
                                        expon_scale_points)
grid2 <- bouldernumb + vpoint_per_year_plot + expon_scale_points


#avg grade per year
avg_grade <- finalticks %>% group_by(year) %>% summarize(average = mean(grade))


