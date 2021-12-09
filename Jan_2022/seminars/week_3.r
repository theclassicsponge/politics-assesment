# Describing data using plots 
# loading datasets using csvs 
# load a dataset 
# load a dataset in a csv file
pop <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/census-historic-population-borough.csv")
dim(pop)
names(pop)
head(pop)
# plotting data with R 
#standard r plotting 
# left of the comma is the x-axis, right is the y-axis. Also note how we  are using the $ command to select the columns of the data frame we want.

plot(pop$Persons.1811,pop$Persons.1911)
library("ggplot2")
gg_pops <- ggplot(data = pop, 
                  mapping = aes(x = Persons.1811, y = Persons.1911))
gg_pops + geom_point()
# add color 
gg_pops + geom_point(aes(colour =  "red", Persons.2011))
gg_pops + geom_point(aes(colour = Persons.2011))
# change size of points
gg_pops + geom_point(aes(size = Persons.2011))
# add names to points 
gg_pops + 
  geom_point(aes(size = Persons.2011)) + 
  geom_text(size = 2, 
            colour = "red", 
            aes(label = Area.Name))
#save plot as a pdf
ggsave("first.ggplot.pdf", scale = 2)

# Histograms 

# Questions 
# read in the penguins datafile
# 1. 
penguins <- readRDS(url("https://github.com/QMUL-SPIR/Public_files/blob/master/datasets/penguins.rds?raw=true"))
head(penguins)
dim(penguins)
names(penguins)
# 3. Create a histogram of penguin flipper length.
flip_len <- ggplot(penguins, aes(x = flipper_length_mm))
name <- flip_len + geom_histogram()
name
# 4. How does the distribution of flipper length vary by species? Take your histogram and add a facet_wrap layer to see this.
name + facet_wrap(~ species)
# 5. Add a title to the faceted plot using the ggtitle() layer. If you need help, try ?ggtitle.
name + facet_wrap(~ species) + ggtitle("Flipper lengths of different species of penguin")
# 6. Create a scatterplot to show the relationship between bill depth and bill length. How would you describe this relationship?
bill_scatter <- ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point() 

# hint: adding a layer of geom_smooth(method = "lm") 
# will help you work out whether the relationship is positive or negative

bill_scatter
# 7. How does this relationship change when we break it down into species? Rewrite your scatterplot code so that the colour of the points varies by species.
bill_scatter_species <- ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + geom_point(aes(colour = species))
bill_scatter_species
# 8. Save this plot using ggsave().
ggsave("bill_scatter_species.pdf", scale = 2)

# 9. 
# Now, we can see that, within each species of penguin, there is a positive relationship between bill depth and length. This relationship is masked by the fact that different species of penguin have different sized bills overall. What we have uncovered here, through data visualisation, is an example of Simpson's paradox.
