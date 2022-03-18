# Loading Library 
library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)

# Reading Excel file
my_data <- read_excel("CriminalOffences.xlsx")

# Question 1

## Part 1

# Since charts produced by faceting did not fit in the window, 
# I've created 3 different charts instead each showing the primary violations by year for 
# 3 different criminal offence categories.

# Insights: Assaults and Other violations are the two biggest categories having the highest number of incidents
# Incidents in both these categories witnessed a rise between 2016 and 2019,
# while incident count in other categories remained almost constant
my_data %>%
  filter(Crime_Offences_Category=="Crimes Against Person (1000)") %>% 
  group_by(Crime_Offences_Category, Primary_Violation, Year,) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=Year, y=n, color=Primary_Violation)) +
    geom_line() +
    guides(color=guide_legend(nrow=4)) + theme(legend.position='top')

# Theft of $5000 and Under is the biggest category with the most incidents throughout the years,
# Theft $5000 and Under violation witnessed a fall in number of incidents from 2019 to 2020
my_data %>%
  filter(Crime_Offences_Category=="Crimes Against Property (2000)") %>% 
  group_by(Crime_Offences_Category, Primary_Violation, Year,) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=Year, y=n, color=Primary_Violation)) +
  geom_line() +
  guides(color=guide_legend(nrow=4)) + theme(legend.position='top')

# Other Criminal Code violation was rising from 2016 to 2019, while Offensive Weapons violation remained the same throughout the years
# Data for "Prostitution" violation is missing after 2017
my_data %>%
  filter(Crime_Offences_Category=="Other Criminal Code Offences (3000)") %>% 
  group_by(Crime_Offences_Category, Primary_Violation, Year,) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=Year, y=n, color=Primary_Violation)) +
  geom_line() +
  guides(color=guide_legend(nrow=4)) + theme(legend.position='top')



## Part 2

# In 2019, number of incidents for crimes against property rose from February to July
# Post July, incidents for crime against property started declining
# Crime against property was at its peak in the month of July in 2019
my_data %>%
  filter(Crime_Offences_Category=='Crimes Against Property (2000)' & Year==2019) %>%
  mutate(month=str_extract(Occurrence_Date, "(?<=-).*(?=-)")) %>% 
  mutate(month=factor(month, levels=month.abb)) %>% 
  group_by(month) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=month, y=n)) + geom_line(stat='identity', group=1)  


# Question 2

# I first looked at hourly data and observed that the hours could be
# bucketed together into 'early morning hours' from 00-05,
# 'morning hours' from 06-08, 'office hours' from 09-17, and 
# 'night hours' from 18-23.
# Early morning hours and office hours, contribute to 
# 64% of break and enter violation, but only 50% of motor vehicle thefts, indicating
# that break and enter incidents might occur during those times when people are sleeping or are at work
my_data %>% 
  filter(Primary_Violation%in%c('Break and Enter', 'Theft - Motor Vehicle')) %>% 
  filter(Year==2020) %>% 
  mutate(hour=hour(strptime(Occurrence_Time, '%I:%M:%S %p'))) %>%
  mutate(hour_buckets=ifelse(hour<=5, '00-05',
                      ifelse(hour<=8, '06-08',
                      ifelse(hour<=17, '09-17',
                      '18-23')))) %>% 
  group_by(hour_buckets, Primary_Violation) %>% 
  summarise(count=n()) %>% 
  group_by(Primary_Violation) %>%
  mutate(perc=100*count/sum(count)) %>% 
  ggplot(aes(x=hour_buckets, y=perc, fill=Primary_Violation)) +
      geom_col(position='dodge') + 
      geom_text(aes(label=round(perc, 2)), position=position_dodge(.9),
                vjust=1.5, colour='white')

# Question 3

# There is a lot of variation in crimes across the neighborhoods from
# less than 10 in some neighborhoods to greater than 500 in other neighborhoods with most 
# criminal incidents reported in Centretown neighborhood. 
# Given more time, I would have plotted this on a map to see whether there
# is clustering of neighborhoods and would have also analysed how these incidents are 
# related to population and poverty in the neighborhoods.
my_data%>%
  filter(Crime_Offences_Category=='Crimes Against Person (1000)' & Year==2018) %>%
  group_by(ONS_Neighbourhood_2019) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(ONS_Neighbourhood_2019, n),y=n)) +
    geom_col(fill='red') +
    coord_flip()
ggsave('~/Desktop/file.png', height=15, width=10, dpi=1600)

