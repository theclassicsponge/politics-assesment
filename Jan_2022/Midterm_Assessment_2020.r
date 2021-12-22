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
gg_total_deaths_per_mil <- ggplot(dataset_omit, aes(x = total_deaths_per_million)) +
  ggtitle("The Total Number of COVID-19 Related Deaths per Million Inhabitants")
gg_total_deaths_per_mil + geom_histogram(binwidth = 10)

# GDP Per Capita:
gg_gdp_per_cap <- ggplot(dataset_omit, aes(x = gdp_per_capita)) +
  ggtitle("Gross Domestic Product (GDP) per capita")
gg_gdp_per_cap + geom_histogram()

# Female Head of Government:
gg_female_HoG_deaths <- ggplot(data = dataset_omit, aes(x = female_HoG)) +
  geom_histogram(binwidth = 20) + 
  ggtitle("The Number of Female Heads of Government in the World (0 = Male, 1 = Female)")

geom_text(size = 2, 
          colour = "red", 
          aes(label = location))

gg_female_HoG_deaths + facet_wrap(~ female_HoG)

# Aged 65 or Older:
gg_aged_65_plus <- ggplot(data = dataset_omit, aes(x = aged_65_older)) + 
  ggtitle("Estimated Share of the Population Aged 65 and Over")
gg_aged_65_plus + geom_histogram()

# VDEM Polyarchy:
gg_vdem <- ggplot(data = dataset_omit, aes(x = vdem_polyarchy)) + 
  ggtitle("Level of Liberal Democracy, Negative Values Represent Lower Levels of Democracy, Higher Values Represent Higher Levels")
gg_vdem + geom_histogram()

#### RECODES FOR VARIBELS OF INTREST (BINARY)####

# calculate median GDP Per Capita
med_gdp <- median(dataset_omit$gdp_per_capita)

# create new variable using case_when()
dataset_omit <- dataset_omit %>% # pipe the dataset
  mutate( # create new variable
    gdp_var = # name the new variable
      factor(case_when(
        gdp_per_capita > med_gdp ~ 1,
        gdp_per_capita <= med_gdp ~ 0)))

# calculate median 65 or Older
med_65_plus <- median(dataset_omit$aged_65_older)

# create new variable using case_when()
dataset_omit <- dataset_omit %>% # pipe the dataset
  mutate( # create new variable
    sixty_five_var = # name the new variable
      factor(case_when(
        aged_65_older > med_65_plus ~ 1,
        aged_65_older <= med_65_plus ~ 0)))

# calculate median vdem Polyarchy
med_vdem <- median(dataset_omit$vdem_polyarchy)

# create new variable using case_when()
dataset_omit <- dataset_omit %>% # pipe the dataset
  mutate( # create new variable
    vdem_var = # name the new variable
      factor(case_when(
        vdem_polyarchy > med_vdem ~ 1,
        vdem_polyarchy <= med_vdem ~ 0)))



#### RELATIONSHP BETWEEN TOTAL COVID 19 DEATHS AND THE RELEVENT VARIABLES ####

  #### CONDITIONAL DISTRIBUTION PLOTS ####

# Distribution of Deaths per Million Conditional on GDP per Capita

gdp_dist <- ggplot(data = dataset_omit, aes(total_deaths_per_million, group = gdp_var)) + 
  geom_density(aes(colour = gdp_var)) +
  labs(x = "Total Deaths Per Million", # clearer x axis label
       y = "Density", # clearer y axis label
       title = "Distribution of Deaths per Million Conditional on GDP per Capita") + # title 
  scale_color_discrete(name = "GDP Per Capita", # change legend title
                       labels = c("Below or equal median", # change legend labels
                                  "Above median")) + 
  theme_minimal()

gdp_dist

# Distribution of Deaths per Million Conditional on median 65 or Older

sixtyfive_dist <- ggplot(data = dataset_omit, aes(aged_65_older, group = sixty_five_var)) + 
  geom_density(aes(colour = sixty_five_var)) +
  labs(x = "Total Deaths Per Million", # clearer x axis label
       y = "Density", # clearer y axis label
       title = "Distribution of Deaths per Million Conditional on the Estimate Share of the Population aged 65 or Older") + # title 
  scale_color_discrete(name = "Over aged 65", # change legend title
                       labels = c("Below or equal median", # change legend labels
                                  "Above median")) + 
  theme_minimal()

sixtyfive_dist

# Distribution of Deaths per Million Conditional on median vdem Polyarchy

vdem_dist <- ggplot(data = dataset_omit, aes(vdem_polyarchy, group = vdem_var)) + 
  geom_density(aes(colour = vdem_var)) +
  labs(x = "Total Deaths Per Million", # clearer x axis label
       y = "Density", # clearer y axis label
       title = "Distribution of Deaths per Million Conditional on the Level of Liberal Democracy") + # title 
  scale_color_discrete(name = "Level of Liberal Democracy", # change legend title
                       labels = c("Below or equal median", # change legend labels
                                  "Above median")) + 
  theme_minimal()

vdem_dist

  #### TWO SAMPLE T-TESTS ####

# T-Test: GDP

t.test(vdem_polyarchy ~ vdem_var, # formula y ~ x
       data = dataset_omit, # dataset where the variables are found
       mu = 0, # difference under the null hypothesis
       alt = "two.sided",  # two sided test (difference in means could be smaller or larger than 0)
       conf = 0.95) # confidence interval

# T-Test: Aged 65 Plus

t.test(aged_65_older ~ sixty_five_var, # formula y ~ x
       data = dataset_omit, # dataset where the variables are found
       mu = 0, # difference under the null hypothesis
       alt = "two.sided",  # two sided test (difference in means could be smaller or larger than 0)
       conf = 0.95) # confidence interval

# T-Test: VDEM Polyarchy

t.test(vdem_polyarchy ~ vdem_var, # formula y ~ x
       data = dataset_omit, # dataset where the variables are found
       mu = 0, # difference under the null hypothesis
       alt = "two.sided",  # two sided test (difference in means could be smaller or larger than 0)
       conf = 0.95) # confidence interval
