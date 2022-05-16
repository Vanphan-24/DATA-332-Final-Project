library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)


#set the working directory
rm(list = ls())
setwd("/Users/vanphan/Desktop/DATA332/cabbage_butterfly-main")

#read the file
df_RawData <- read_excel("data/CompletePierisData_2022-03-09.xlsx", sheet = 1) %>%
  #rename country column
  dplyr::rename("Country" = "dwc:country") %>%
  #cleaning the data to have consistent names
  dplyr::select(coreid, Institution, SexUpdated, LWingLength, LWingWidth, LBlackPatchApex, RWingLength, RWingWidth, RBlackPatchApex, YearUpdated, Country) %>%
  dplyr::mutate(SexUpdated = ifelse(SexUpdated == "M", "male", SexUpdated)) %>%
  dplyr::mutate(SexUpdated = ifelse(SexUpdated == "male?", "male", SexUpdated)) %>%
  dplyr::mutate(SexUpdated = ifelse(SexUpdated == "Male", "male", SexUpdated)) %>%
  dplyr::mutate(SexUpdated = ifelse(SexUpdated == "F", "female", SexUpdated)) %>%
  dplyr::mutate(SexUpdated = ifelse(SexUpdated == "F?", "male", SexUpdated)) %>%
  dplyr::mutate(SexUpdated = ifelse(SexUpdated == "Female", "female", SexUpdated)) %>%
  dplyr::mutate(Country = ifelse(Country == "U.S.A.", "USA", Country)) %>%
  dplyr::mutate(Country = ifelse(Country == "United States", "USA", Country)) %>%
  dplyr::mutate(YearUpdated = ifelse(YearUpdated == "200", "2000", YearUpdated))

##Putting the years into their respective decades   
df_RawData$YearUpdated <- as.integer(df_RawData$YearUpdated)
df_RawData$decade <- df_RawData$YearUpdated
df_RawData$first_three <- substr(df_RawData$YearUpdated, 1, 3)
df_RawData$digit <- paste(0)
df_RawData$decade <- paste0(df_RawData$first_three, df_RawData$digit)


#This is where we are filtering the blank data out of the table 
df <- df_RawData %>%
  dplyr::group_by(Institution, SexUpdated, LWingLength, LWingWidth, LBlackPatchApex, RWingLength, RWingWidth, RBlackPatchApex, YearUpdated, Country) %>%
  dplyr::filter(!is.na(Country)) %>%
  dplyr::filter(!is.na(SexUpdated)) %>%
  dplyr::filter(SexUpdated == 'male' | SexUpdated == 'female') %>%
  dplyr::filter(!is.na(YearUpdated)) %>%
  dplyr::filter(!is.na(Institution)) %>%
  dplyr::filter(!is.na(LWingLength)) %>%
  dplyr::filter(!is.na(LWingWidth)) %>%
  dplyr::filter(!is.na(LBlackPatchApex)) %>%
  dplyr::filter(!is.na(RWingLength)) %>%
  dplyr::filter(!is.na(RWingWidth)) %>%
  dplyr::filter(!is.na(RBlackPatchApex))


#Create a pivot table that groups the sex of the butterflies and the country where they were found
df_pivot1 <- df %>%
  dplyr::group_by(SexUpdated, Country) %>%
  dplyr::summarise(number_of_sexes = n()) 

#Create a chart that shows the amount of the butteflies by male and female in the countries they were found in 
ggplot(df_pivot1, aes(fill=SexUpdated, y=number_of_sexes, x=Country)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Country") +
  ylab("Number of Insects Found") +
  ggtitle("Countries Where Male and Female Butterflies Were Found")

#Create a pivot table that groups the sex of the butterflies by amount and the institution that found them  
df_pivot2 <- df %>%
  dplyr::group_by(SexUpdated, Institution) %>%
  dplyr::summarise(number_in_institution = n()) 

#Create a chart that hsows the amount of butteflies by male and female in the institutions that found them 
ggplot(df_pivot2, aes(fill=SexUpdated, x=number_in_institution, y=Institution)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Number of Insects Found") +
  ylab("Institution") +
  ggtitle("Institution Where Male and Female Butterflies Were Found")

#Create a pivot table to group by male and female butterflies by decade
df_pivot3 <- df %>%
  dplyr::group_by(SexUpdated, decade) %>%
  dplyr::summarise(number_by_year = n()) %>%
  dplyr::filter(decade == "1960" | decade == "1970" | decade == "1980" | decade == "1990" | decade == "2000" | decade == "2010" | decade == "2020") 

#Create a chart to group by male and female butterflies by decade
ggplot(df_pivot3, aes(fill=SexUpdated, x=decade, y=number_by_year)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Decade") +
  ylab("Number of Butterflies") +
  ggtitle("Number of Male and Female Butterflies by Decade")

#make the right wing length column into an integer
df$RWingLength <- as.double(df$RWingLength)

#find the average length of the right wing by male and female butterflies
df_RWingLength <- df %>%
  dplyr::filter(!is.na(RWingLength)) %>%
  dplyr::group_by(SexUpdated) %>%
  dplyr::summarise(average_rw = mean(RWingLength)) 

#make the left wing length column into an integer
df$LWingLength <- as.double(df$LWingLength)

#find the average length of the left wing by male and female butterflies
df_LWingLength <- df %>%   
  dplyr::filter(!is.na(LWingLength)) %>%
  dplyr::group_by(SexUpdated) %>%
  dplyr::summarise(average_lw = mean(LWingLength)) %>%
  #combine the data from the right wing into this data frame
  left_join(df_RWingLength, by = c("SexUpdated"))


#make the right wing width column into an integer
df$RWingWidth <- as.double(df$RWingWidth)

#find the average width of the right wing by male and female butterflies
df_RWingWidth <- df %>%
  dplyr::filter(!is.na(RWingWidth)) %>%
  dplyr::group_by(SexUpdated) %>%
  dplyr::summarise(average_rw = mean(RWingWidth))

#make the left wing width column into an integer
df$LWingWidth <- as.double(df$LWingWidth)

#find the average width of the left wing by male and female butterflies
df_LWingWidth <- df %>%   
  dplyr::filter(!is.na(LWingWidth)) %>%
  dplyr::group_by(SexUpdated) %>%
  dplyr::summarise(average_lw = mean(LWingWidth)) %>%
  #combine the data from the right wing into this data frame
  left_join(df_RWingWidth, by = c("SexUpdated"))

#Create the T Test for the right wing length of male and female butterflies
#mu=30 because looking at all the data there seems to be a variety of wing lengths from 20-30
t.test(df_LWingLength$average_rw, mu=30) %>%
  tidy()
