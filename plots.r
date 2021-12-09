# load a dataset in a csv file
pop <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/census-historic-population-borough.csv")
head(pop)
# left of the comma is the x-axis, right is the y-axis. Also note how we  are using the $ command to select the columns of the data frame we want.

plot(pop$Persons.1811,pop$Persons.1911)
library("ggplot2")
gg_pops <- ggplot(data = pop, 
                  mapping = aes(x = Persons.1811, y = Persons.1911))

gg_pops + geom_point()