library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(haven)
library(knitr)
library(broom)

rm(list = ls())
setwd("/Users/vanphan/Desktop/DATA332/Project-Insect-Carnivore-main")

df_Ladybug <- read_excel("data/Ladybug Data.xlsx", sheet = 1)
df_ScanLadybug <- read_excel("data/Scan Ladybug Data.xlsx", sheet = 1) %>%
  #Clean data so that values are consistent
  dplyr::mutate(stateProvince = ifelse(stateProvince == "IL", "Illinois", stateProvince)) %>%
  dplyr::mutate(stateProvince = ifelse(stateProvince == "Il", "Illinois", stateProvince)) %>%
  dplyr::mutate(stateProvince = ifelse(stateProvince == "IA", "Iowa", stateProvince)) %>%
  dplyr::mutate(stateProvince = ifelse(stateProvince == "Ia", "Iowa", stateProvince)) 

##Putting the years into their respective decades   
df_ScanLadybug$year <- as.integer(df_ScanLadybug$year)
df_ScanLadybug$decade <- df_ScanLadybug$year
df_ScanLadybug$first_three <- substr(df_ScanLadybug$year, 1, 3)
df_ScanLadybug$digit <- paste(0)
df_ScanLadybug$decade <- paste0(df_ScanLadybug$first_three, df_ScanLadybug$digit) 

#Selecting the columns that we want to work with 
df <- df_ScanLadybug %>%
  dplyr::select(decade, scientificName, stateProvince)

#Create the pivot table
df <- df %>%
  dplyr::group_by(decade, scientificName, stateProvince) %>%
  dplyr::summarise(number_of_species = n()) %>%
  #dplyr::summarise(average_number = mean(number_of_species)) %>%
  dplyr::filter(decade == "2020" | decade == "2010" | decade == "2000") %>%
  dplyr::filter(!is.na(scientificName)) %>%
  dplyr::filter(!is.na(decade))

#Number of Specific Species caught 2000-2020
#Create a chart
ggplot(df, aes(y=number_of_species, x=scientificName)) + 
  geom_bar(stat = "identity") +
  xlab("Scientific Name") +
  ylab("Number of Insects Found") +
  ggtitle("Species Found in 2000-2020") +
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))

#Species caught in Illinois and Iowa in 2000-2020
#Create a chart
ggplot(df, aes(y=number_of_species, x=stateProvince)) + 
  geom_bar(stat = "identity") +
  xlab("State") +
  ylab("Number of Insects Found") +
  ggtitle("Total Number of Insects Found in Illinois vs Iowa during 2000-2020") +
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))

#Number of total species caught in 2000-2020
ggplot(df, aes(y=number_of_species, x=decade)) + 
  geom_bar(stat = "identity") +
  xlab("Decade") +
  ylab("Number of Insects Found") +
  ggtitle("Number of Insects Found by Decade") +
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))

#Selecting the columns that will be used in calculating the average
dfAvg <- df_ScanLadybug %>%
  dplyr::select("decade", "scientificName")
