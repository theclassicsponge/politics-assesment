# QUESTIONS TO ASK:
# 1. Do richer countries do better or worse than poorer countries (and why).
# 2. Are countries led by women are more succesful than those led by men.
# 3. Is the success in dealing with the pandemic is related to the proportion of older people living in the
# country.
# 4. Are democratic countries are more succesful than authoritarian countries.

#SPECIFIC TASKS:
# 1. Create a Table with all the variables, the relevant measure of central tendency (MCT), and the relevant
# measure of dispersion (MD).
# 2. Create the relevant plot for every variable.
# 3. Create a justification for the proposed relationship. 
# 4. Create a conditional distribution plot.
# 5. Create a two-sample t-test.


#### INSTALL LIBRARYS: ####
library("ggplot2")
#### INPUT DATASET: ####
dataset <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/Covid2020.csv")
head(dataset)
#### EXAMINE THE RELEVENT VARIABLES: #### 

# Total Deaths per Million:
all_deaths <- (dataset$total_deaths_per_million)
head(all_deaths)
plot(dataset$total_deaths_per_million, dataset$median_age)

gg_all_deaths <- ggplot(data = dataset, 
                        mapping = aes(x = total_deaths_per_million, y = median_age)) +
                        geom_text(size = 2, 
                                  colour = "red", 
                                  aes(label = location))
gg_all_deaths 2+ geom_point()