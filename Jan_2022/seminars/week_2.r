# Chapter 2 Getting to Know Your Data: Central Tendency and Dispersion

# Central Tendency 

# The way of understanding what is a typical value of a variable
# The appropriate measure of central tendency depends on the level of measurement of the variable

# Continuous = Arithmetic Mean(average)
# Ordinal = Median(Middle value)  
# Nominal = mode(Most occurring value)

# Mean:

# The grades of students from a statistics exam:
grades <- c(80, 90, 85, 71, 69, 85, 83, 88, 99, 81, 92)

# Take the sum of the grades:
sum_grades <- sum(grades)
# Take the number of grades:
num_grades <- length(grades)
# The mean is the sum of the grades over the number of grades
sum_grades / num_grades
# R can do this all to get the mean automatically
mean(grades)

# Sequences

# To create a sequence use this
1:10
# seq does the same thing
seq(from = 1, to = 10)

# Display odd numbers between 1 and 100
odd_numbers <- seq(1, 100, 2)
odd_numbers

#show length of list
length(odd_numbers)

#Median
# The appropriate measure of central tendency for ordinal variables
# Ordinal means that there is an unequal value between variables but there is a hierarchy
# i.e education
edu <- c( rep(x = 0, times = 1), 
          rep(x = 1, times = 5), 
          rep(x = 2, times = 55),
          rep(x = 3, times = 20), 
          rep(4, 10), rep(5, 9)) # works without 'x =', 'times ='
median(edu)

# Mode 
# The appropriate measure of central tendency for nominal variables
# mode means the most occuring value
# nominal means there isnt any kind of ordering 
# i.e. votes 
stay <- c(rep(0, 509), 
          rep(1, 491))
table(stay)
# mode would be 509 (0)

#Dispersion
# to measure the difference between two sets of data you can use measures of dispersion

# Continuous = variance or standard deviation
# Ordinal = range or interquartial range
# Nominal = proportion of each category 

#Variance and standard deviation:

# they tell us how much, on average observations will differ from the average observation
# varience: 
var(grades)
# Standard Deviation: 
sd(grades)

# range and interquartile range:

# the proper measure of dispersion for ordinal variables 
range(edu)
# use quantile() to get percentials 
quantile(edu, 0.25) # 25th percentile 
quantile(edu, 0.75) # 75th percentile

# proportion in each category: 

# to describe the distribution of nominal varibles we use the proporitios in each category
table(stay)
# to get proportion in each category we devide the vales of the table by the sum of the table i.e.:
# 509 and 491 /1000
table(stay)/ sum(table(stay))
# r has a built in function for this
prop.table(table(stay))

#central tenedency and dispersion with data

# create a character vector containing state names
state <- c(
  "North Rhine-Westphalia",
  "Bavaria",
  "Baden-Wurttemberg",
  "Lower Saxony",
  "Hesse",
  "Saxony",
  "Rhineland-Palatinate",
  "Berlin",
  "Schleswig-Holstein",
  "Brandenburg",
  "Saxony-Anhalt",
  "Thuringia",
  "Hamburg",
  "Mecklenburg-Vorpommern",
  "Saarland",
  "Bremen"
)

population <- c(
  17865516,
  12843514,
  10879618,
  7926599,
  6176172,
  4084851,
  4052803,
  3670622,
  2858714,
  2484826,
  2245470,
  2170714,
  1787408,
  1612362,
  995597,
  671489
)

popdata <- data.frame( 
  state,
  population
)

head(popdata)
names(popdata)
str(popdata)
# central tendancy of the data 
mean(popdata$population)
#dispersion of the data
sd(popdata$population)

#loading data 
# load perception of non-western foreigners data
load(url("https://github.com/QMUL-SPIR/Public_files/raw/master/datasets/BSAS_manip.RData"))
dim(data2)
head(data2, n = 10)
mean(data2$RAge)
table(data2$RSex)
median(data2$urban)

# EXERCIESES
# Use square brackets to access the first 10 rows in the 4th column of data2, which you should have loaded above. (You may need to load it again using load(url("https://github.com/QMUL-SPIR/Public_files/raw/master/datasets/BSAS_manip.RData")).)
data2[0:10, 4]
# Use the dollar sign to access the Househld variable in data2.
data2$Househld
# How would you describe the level of measurement of
# the religious variable
 # nominal
# the IMMBRIT variable
 # continuous
# the health.good variable
 # ordinal 
# Find and describe the central tendency of the religious variable in data2, thinking about which measure is appropriate.
religiousyn <- c(data2$religious)
table(religiousyn)