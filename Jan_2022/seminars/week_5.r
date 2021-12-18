# don't have tidyverse installed? Run:
# install.packages('tidyverse')

# load tidyverse
library(tidyverse)

# generate random data 
set.seed(100)

# Creates vector of negative binomial distributed data
skewed <- rnbinom(100000, 
                  mu = 0.5,
                  size = 0.1) 
# We can get the mean and standard deviation for this object.
mean(skewed)

sd(skewed)


# make sure ggplot2 is loaded
library(ggplot2)

# plot distribution
qplot(skewed) +
  theme_minimal() +
  labs(x = "Number of crimes committed",
       y = "Frequency in data")

sum(skewed > 0)

# put this variable in a dataframe and create a new categorical variable identifying whether someone offended over the past year (e.g., anybody with a count of crime higher than 0).
# create dataframe with our data as one column
fake_population <- data.frame(crime = skewed)

# create 'offender' variable - dummy indicator of whether someone has committed one or more crimes, rather than zero
fake_population <- fake_population %>% # load and pipe dataset
  mutate(
    offender = factor(
      case_when( # create new factor variable following certain conditions
        crime > 0 ~ 1, # value of 1 if crimes committed greater than zero
        TRUE ~ 0 # value of 0 otherwise
      )))

# Let's check the results
table(fake_population$offender)

# create age variable depending on committing crime or not

# subset to offenders, create age
offenders <- fake_population %>% # pipe dataset
  filter(offender == 1) %>% # subset to offenders
  mutate(age = round(rnorm(nrow(.), 
                           mean = 20, 
                           sd = 4))) # random numbers from normal distribution rounded to whole numbers

# subset to non-offenders, create age
non_offenders <- fake_population %>% # pipe dataset
  filter(offender == 0) %>% # subset to non-offenders
  mutate(age = round(rnorm(nrow(.), 
                           mean = 30, 
                           sd = 10))) # random numbers from normal distribution rounded to whole numbers

# bring both subsets back together
fake_population <- rbind(offenders, 
                         non_offenders)

# average age across all respondents
mean(fake_population$age)


# dataset of means for each group independently

age_means <- fake_population %>% # pipe dataset
  group_by(offender) %>% # group so that function can be applied to offenders and non-offenders separately
  summarise(age = mean(age)) # get mean of each group

# plot 

gg_age_crimes <- ggplot(fake_population, 
                        aes(x = age, 
                            group = factor(offender),
                            colour = offender)) + 
  geom_density() +
  geom_vline(data = age_means, 
             mapping = aes(xintercept = age, colour = offender),
             linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Distribution of age conditional on offender status",
       x = "Age in 2019",
       y = "Density") +
  scale_colour_discrete(name = "Committed crime in 2019",
                        labels = c("No", "Yes"))

gg_age_crimes