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

# new dataset
# load perception of non-western foreigners data
load(url("https://github.com/QMUL-SPIR/Public_files/raw/master/datasets/BSAS_manip.RData"))

# look at the first six rows of the dataset
head(data2)

# create dataset of people who overestimated immigration levels, with reformatting
over_estimators <- data2 %>% # call the original dataframe
  filter( # only keep people who overestimated immigration
    over.estimate == 1
  ) %>%
  mutate( # create single partisanship indicator
    party_id =
      case_when( # function creating variable depending on other variable values
        Cons == 1 ~ "con", # if 'Cons' value is 1, party_id value should be 'con'
        Lab == 1 ~ "lab",
        SNP == 1 ~ "snp",
        Ukip == 1 ~ "ukip",
        BNP == 1 ~ "bnp",
        GP == 1 ~ "green",
        party.other == 1 ~ "other",
        TRUE ~ "none"
      )
  ) %>%
  select( # subset variables (columns)
    sex = RSex,
    age = RAge,
    household = Househld,
    party_id,
    perceived_immigrants = IMMBRIT
  )

# take a look at the new data
head(over_estimators)


# to deal with missing values
# install and load naniar
install.packages('naniar', repos = "http://cran.us.r-project.org")

library(naniar)

# load in quality of government data
qog <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/QoG2012.csv")

# inspect the variable h_j

table(qog$h_j)
# number does not add up to 194 because of missing values
n_miss(qog$h_j)
n_complete(qog$h_j)
pct_miss(qog$h_j)
pct_complete(qog$h_j)
# dataset must remain rectangular so missing data means you must delete the entire row 
# make sure tidyr loaded
library(tidyr)

# remove NAs from h_j
qog <- drop_na(qog, h_j)

# alternatively, we could have piped this!

# qog <- qog %>% drop_na(h_j)

# Distributions
# a marginal distribution is the distribution of a variable by itself
# we see the range, interquartile range, mean, median and the number of NAs
summary(qog$undp_hdi)
# drop NAs
qog <- qog %>% # call dataset
  drop_na(undp_hdi) %>% # drop NAs
  rename(hdi = undp_hdi) # rename() works like select() but keeps all other variables

# get the mean of hdi 
hdi_mean <- mean(qog$hdi)

hdi_mean

# hdi_mean is the mean in the sample 
# we need to know what the sampling distribution looks like
# the standard error is the standard deviation of the sampling distribution

#to find the standard error 
se_hdi <- sd(qog$hdi) / sqrt(nrow(qog))

se_hdi

# we know that sampling distribution is aproximatly normal 
# 95% of all observations are within 1.96 standard deviations (standard errors ) of the mean

lower_bound <- hdi_mean - 1.96 * se_hdi
lower_bound

upper_bound <- hdi_mean + 1.96 * se_hdi
upper_bound

hdi_mean_draws <- rnorm(1000, # 1000 random numbers
                        mean = hdi_mean, # numbers should have same mean as hdi
                        sd = se_hdi) # numebrs should have same sd as hdi

# we have drawn 100 mean values a random from the distribution 
# this can be plotted as a histogram 

p1 <- ggplot(mapping = aes(hdi_mean_draws)) + 
  geom_histogram() + 
  ggtitle("Sampling Distribution of HDI means") + # add title
  theme_minimal() + # change plot 'theme' 
  labs(x = "Plausible mean HDI levels", # change x axis label
       y = "Frequency") # change y axis label
p1

# lets add the 95% confidence inverval around our estimate 
p1 <- p1 + 
  geom_vline( # add vertical line at lower_bound
    xintercept = lower_bound, 
    linetype = "dashed") + 
  geom_vline( # add vertical line at upper_bound
    xintercept = upper_bound, 
    linetype = "dashed")

p1

pnorm(0.74, 
      mean = hdi_mean, 
      sd = se_hdi)

1 - pnorm(0.74, 
          mean = hdi_mean, 
          sd = se_hdi)

# conditional distribution plots

# plot the marginal distribution of hdi 
p2 <- ggplot(qog, aes(hdi)) + 
  geom_density() + 
  ggtitle("Marginal Distribution of HDI") + 
  theme_minimal()
p2
# the distribution looks bimodal 
# plot conditional distributions
p3 <- ggplot(qog, aes(hdi, group = former_col)) + 
  geom_density(aes(colour = factor(former_col))) + 
  ggtitle("Distribution of HDI conditional on former colony status") + 
  theme_minimal()

p3

# add more infomation to the conditional distribution plot 
p3 <- p3 +
  labs(x = "Human Development Index (HDI)", # clearer x axis label
       y = "Density") + # clearer y axis label 
  scale_color_discrete(name = "Former colony", # change legend title
                       labels = c("Never colonised", # change legend labels
                                  "Colonised"))

p3



# Exercises: 
#1 load the dataset
qog <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/QoG2012.csv")
#2 Rename the variable wdi_gdpc to gdpc using rename() from the tidyverse.
head(qog)
qog <- qog %>% 
  rename(gdpc = wdi_gdpc)
# 3 Delete all rows with missing values on gdpc
qog <- qog %>%
  drop_na(undp_hdi)
# 4 Inspect former_col and delete rows with missing values on it.
