#Load a dataset in a csv file
pop <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/census-historic-population-borough.csv")
# Make sure tidyverse is available to use
library(tidyverse)
#Subset dataset only to have years since 2000

pop_21 <- select(pop, Area.Code, Area.Name, Persons.2001, Persons.2011)

# Apply all changes to dataset using pipe

pop_21_200k <- pop %>% # call the dataset 
  select( # keep only the variables we want, renaming them
    area_name = Area.Name,
    area_code = Area.Code,
    persons_2001 = Persons.2001,
    persons_2011 = Persons.2011) %>%
  filter( # keep only rows with 200,000+ population
    persons_2011 >= 200000) %>% 
  mutate( # create new variable of difference between 2011 and 2001
    persons_diff = persons_2011-persons_2001
  )

# take a look at the new data
head(pop_21_200k)
