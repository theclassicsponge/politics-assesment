# QUESTIONS TO ASK:
# 1. Do richer countries do better or worse than poorer countries (and why).
# 2. Are countries led by women are more successful than those led by men.
# 3. Is the success in dealing with the pandemic is related to the proportion of older people living in the
# country.
# 4. Are democratic countries are more successful than authoritarian countries.

#SPECIFIC TASKS:
# 1. Create a Table with all the variables, the relevant measure of central tendency (MCT), and the relevant
# measure of dispersion (MD).
# 2. Create the relevant plot for every variable.
# 3. Create a justification for the proposed relationship. 
# 4. Create a conditional distribution plot.
# 5. Create a two-sample t-test.


#### INSTALL LIBRARYS: ####
library("ggplot2")
library(tidyverse)
library(tidyr)
#### INPUT DATASET: ####
dataset <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/Covid2020.csv")
dataset_omit <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/Covid2020.csv")
dataset_omit <- drop_na(dataset_omit, extreme_poverty)
head(dataset)

#### EXAMINE THE RELEVENT VARIABLES: ####

# Total Deaths Per Million:

# MCT:
  
tdm_mean <- mean(dataset_omit$total_deaths_per_million)
tdm_mean

# MD:

sd(dataset_omit$total_deaths_per_million)

# GDP Per Captia:
# MCT:

gdp_per_cap <- mean(dataset_omit$gdp_per_capita)
gdp_per_cap

# MD:

sd(dataset_omit$gdp_per_capita)

# Female Head of Government: 

# MCT:

fem_hog <- table(dataset_omit$female_HoG)
fem_hog

# MD:

fem_hog_dev <- prop.table(table(dataset_omit$female_HoG))
fem_hog_dev

# Aged 65 of Older:

# MCT:

old_age <- mean(dataset_omit$aged_65_older)
old_age

# MD:

sd(dataset_omit$aged_65_older)

# VDEM Polyarchy:

# MCT:

vdem <- mean(dataset_omit$vdem_polyarchy)
vdem

# MD:

sd(dataset_omit$vdem_polyarchy)

#### A TABLE WITH ALL THE VARIBALE AND MEASURE OF CENTRAL TENDENCY: ####

# This will be added in Rmd

#### A RELEVENT PLOT FOR EVERY VARIABLE ####

# Total Deaths Per Million:
gg_total_deaths_per_mil <- ggplot(dataset_omit, aes(x = total_deaths_per_million))
gg_total_deaths_per_mil + geom_histogram(binwidth = 10)

# GDP Per Capita:
gg_gdp_per_cap <- ggplot(dataset_omit, aes(x = gdp_per_capita))
gg_gdp_per_cap + geom_histogram()

# Female Head of Government:
gg_female_HoG_deaths <- ggplot(data = dataset_omit, aes(x = female_HoG)) +
  geom_histogram(binwidth = 20)

geom_text(size = 2, 
          colour = "red", 
          aes(label = location))

gg_female_HoG_deaths + facet_wrap(~ female_HoG)

# Aged 65 or Older:
gg_aged_65_plus <- ggplot(data = dataset_omit, aes(x = aged_65_older))
gg_aged_65_plus + geom_histogram()

# VDEM Polyarchy:
gg_vdem <- ggplot(data = dataset_omit, aes(x = vdem_polyarchy))
gg_vdem + geom_histogram()

#### RECODES FOR VARIBELS OF INTREST ####

#### RELATIONSHP BETWEEN TOTAL COVID 19 DEATHS AND THE RELEVENT VARIABLES ####

  #### CONDISIONAL DISTRIBUTION PLOTS ####

  #### TWO SAMPLE T-TESTS ####