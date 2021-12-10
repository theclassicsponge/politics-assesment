####LIBRARIES####
library(tidyverse)
library(tidyr)
library(ggplot2)
####DATASETS####
dataset1 <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/Covid2020.csv")
dataset_omit <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/Covid2020.csv")
dataset_omit <- drop_na(dataset_omit, extreme_poverty)

####MEASURE OF CENTRAL TENDENCY####

#total deaths per million MCT
tdm_mean <- mean(dataset_omit$total_deaths_per_million)
tdm_mean

#GDP per capita MCT
gdp_per_cap <- mean(dataset_omit$gdp_per_capita)
gdp_per_cap

#Female HOG MCT
fem_hog <- table(dataset_omit$female_HoG)
fem_hog

#aged 65 plus MCT
old_age <- mean(dataset_omit$aged_65_older)
old_age

#vdem polyarchy MCT
vdem <- mean(dataset_omit$vdem_polyarchy)
vdem

#Human Development Index 
hdm_index <-mean(dataset_omit$human_development_index)
hdm_index

#Life Expectancy 
life_ex <- mean(dataset_omit$life_expectancy)
life_ex

#Extreme poverty
ex_pov <- mean(dataset_omit$extreme_poverty)
ex_pov

#population density
pop_dens <- mean(dataset_omit$population_density)
pop_dens

#Median Age
mid_age <- mean(dataset_omit$median_age)
mid_age

#population
pop <- mean(dataset_omit$population)
pop

#cases per million
case_per_mil<- mean(dataset_omit$total_cases_per_million)
case_per_mil

#total deaths
all_deaths <- mean(dataset_omit$total_deaths)
all_deaths

#total cases
all_case <- mean(dataset_omit$total_cases)
all_case

####MEASURE OF DISPERSION####

#total deaths per million 
sd(dataset_omit$total_deaths_per_million)

#GDP per capita
sd(dataset_omit$gdp_per_capita)

#female HOG 
fem_hog_dev <- prop.table(table(dataset_omit$female_HoG))
fem_hog_dev


#aged 65 plus 
sd(dataset_omit$aged_65_older)

#vdem polyarchy
sd(dataset_omit$vdem_polyarchy)

#Human Development Index 
sd(dataset_omit$human_development_index)

#Life Expectancy
sd(dataset_omit$life_expectancy)

#Extreme poverty 
sd(dataset_omit$extreme_poverty)

#Total Deaths
sd(dataset_omit$total_deaths)

#Median Age
sd(dataset_omit$median_age)

#Population 
sd(dataset_omit$population)

#Total cases per million 
sd(dataset_omit$total_cases_per_million)

#population density
sd(dataset_omit$population_density)

#Total cases
sd(dataset_omit$total_cases)


####PLOTS####

#Total cases per million





gg_total_cases_per_mil + geom_point()




#GDP per Capita

gg_gdp_per_cap <- ggplot(data = dataset_omit, 
                         mapping = aes(x =gdp_per_capita , y = total_cases_per_million)) +
  geom_text(size = 2, 
            colour = "red", 
            aes(label = location))

gg_gdp_per_cap + geom_point()

#Total Deaths Per Million GDP

gg_gdp_deaths_per_cap <- ggplot(data = dataset_omit, 
                         mapping = aes(x = total_deaths, y = gdp_per_capita)) +
  geom_text(size = 2, 
            colour = "red", 
            aes(label = location))

gg_gdp_deaths_per_cap + geom_point()



#Female HoG
gg_female_HoG <- ggplot(data = dataset_omit, aes(x = total_cases_per_million)) +
  geom_histogram(bins = 30)

geom_text(size = 2, 
          colour = "red", 
          aes(label = location))

gg_female_HoG + facet_wrap(~ female_HoG)

#Female HoG Deaths
gg_female_HoG_deaths <- ggplot(data = dataset_omit, aes(x = total_deaths)) +
  geom_histogram(bins = 30)

geom_text(size = 2, 
          colour = "red", 
          aes(label = location))

gg_female_HoG_deaths + facet_wrap(~ female_HoG)

#Population over 65

gg_pop_65_plus <- ggplot(data = dataset_omit, 
                         mapping = aes(x = aged_65_older , y = total_cases_per_million)) +   
  geom_text(size = 2, 
  colour = "red", 
  aes(label = location))

gg_pop_65_plus + geom_point()

#Population over 65 Deaths 

gg_pop_65_plus_deaths <- ggplot(data = dataset_omit, 
                         mapping = aes(x = aged_65_older , y = total_deaths_per_million)) +   
  geom_text(size = 2, 
            colour = "red", 
            aes(label = location))

gg_pop_65_plus_deaths + geom_point()

#VDEM

gg_vdem <- ggplot(data = dataset_omit, 
                         mapping = aes(x = vdem_polyarchy , y = total_cases_per_million)) +   
  geom_text(size = 2, 
            colour = "red", 
            aes(label = location))

gg_vdem + geom_point()


#VDEM Deaths

gg_vdem <- ggplot(data = dataset_omit, 
                  mapping = aes(x = vdem_polyarchy , y = total_deaths)) +   
  geom_text(size = 2, 
            colour = "red", 
            aes(label = location))

gg_vdem + geom_point()

#Extreme poverty

gg_extreme_poverty <- ggplot(data = dataset_omit, 
                         mapping = aes(x = total_cases_per_million, y = extreme_poverty)) +
  geom_text(size = 2,
            colour ="red",
            aes(label = location))

gg_extreme_poverty + geom_point()

#Total Deaths 

gg_total_deaths <- ggplot(data = dataset_omit, 
                         mapping = aes(x = total_cases_per_million, y = total_deaths)) +
  geom_text(size = 2,
            colour ="red",
            aes(label = location))


gg_total_deaths + geom_point()

#median Age 

gg_median_age <- ggplot(data = dataset_omit, 
                          mapping = aes(x = total_cases_per_million, y = median_age)) +
  geom_text(size = 2,
            colour ="red",
            aes(label = location))

gg_median_age + geom_point()






####AREAS OF INTREST####

##TEST##

#GDP per capita binary
median_gdp_p_c <- median(dataset_omit$gdp_per_capita)

gdp_oneOrZero <- dataset_omit %>%
  mutate(
    gdp_oneOrZero_Median =
      factor(case_when(
        gdp_per_capita > median_gdp_p_c ~ 1,
        gdp_per_capita<= median_gdp_p_c ~ 0)))

#vdem polyarchy binary 
median_vdemBinary <- median(dataset_omit$vdem_polyarchy)

Vdem_Median <- dataset_omit %>%
  mutate(
    Vdem_Median_Median =
      factor(case_when(
        vdem_polyarchy > median_vdemBinary ~ 1,
        vdem_polyarchy<= median_vdemBinary ~ 0)))

#aged 65 plus binary
median_sixtyFiveBinary <- median(dataset_omit$aged_65_older)

sixtyFive_Median <- dataset_omit %>%
  mutate(
    SixtyFive_Median_Median =
      factor(case_when(
        aged_65_older > median_sixtyFiveBinary ~ 1,
        aged_65_older<= median_sixtyFiveBinary ~ 0)))

###CONDITIONAL DISTRBUTION PLOT####

#gdp per capita




gdp_1or0_dist <- ggplot(data = gdp_oneOrZero, aes(total_deaths_per_million, group = gdp_oneOrZero_Median)) +
geom_density(aes(color = gdp_oneOrZero_Median)) +
labs(x = "Total Deaths Per Million",
     y = "Density",
title = "Distribution of Deaths per Million Conditional on GDP per Capita") + # title
scale_color_discrete(name = "GDP per Capita",
labels = c("Below or equal median", "Above median")) + theme_minimal()

gdp_1or0_dist

#vdem polyarchy binary


vdem_dist <- ggplot(data = Vdem_Median, aes(total_deaths_per_million, group = Vdem_Median_Median)) +
  geom_density(aes(color = Vdem_Median_Median)) +
  labs(x = "Vdem Polyarchy",
       y = "Density",
       title = "Distribution of Deaths per Million Conditional on Vdem Polyarchy") + # title
  scale_color_discrete(name = "GDP per Capita",
                       labels = c("Below or equal median", "Above median")) + theme_minimal()

vdem_dist

#aged 65 plus binary

aged65_dist <- ggplot(data = sixtyFive_Median, aes(total_deaths_per_million, group = SixtyFive_Median_Median)) +
  geom_density(aes(color = SixtyFive_Median_Median)) +
  labs(x = "Aged 65 or Over",
       y = "Density",
       title = "Distribution of Deaths per Million Conditional on Population aged 65 or Older") + # title
  scale_color_discrete(name = "GDP per Capita",
                       labels = c("Below or equal median", "Above median")) + theme_minimal()

aged65_dist

###TWO SAMPLE T-TESTS####

#gdp per capita 

t.test(total_deaths_per_million ~ gdp_oneOrZero_Median ,
       data = gdp_oneOrZero ,
       mu = 0,
       alt = "two.sided",
       conf = 0.95)

# vdem polyarchy 

t.test(total_deaths_per_million ~ Vdem_Median_Median  ,
       data = Vdem_Median,
       mu = 0,
       alt = "two.sided",
       conf = 0.95)

# aged 65 plus

t.test(total_deaths_per_million ~ SixtyFive_Median_Median ,
       data = sixtyFive_Median,
       mu = 0,
       alt = "two.sided",
       conf = 0.95)

### Bar Chart ###

#Total cases per million

total_cases_per_mil <- ggplot(data = dataset_omit, aes(x = location, label = location)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Countries",
       y = "location",
       title = "Countries by Total Cases per Million"
       ) +
  theme_minimal()
  

total_cases_per_mil

