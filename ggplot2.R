library(ggplot2)
library(tidyverse)
library(ggpubr)

pokemon = read_csv("C:/Users/mc/Documents/pokemon_go_captures.csv")


#Find out which `height_bin` has the smallest `combat_power` on average.

#create a table to view the different bin heights
table(pokemon$height_bin)

#create 3 datasets, seperating the pokemon into their respective bins
small_height <- pokemon %>%
  filter(height_bin == 'extra_small')

normal_height <- pokemon %>%
  filter(height_bin == 'normal')

large_height <- pokemon %>%
  filter(height_bin == 'extra_large')

#compute the average combat powers for the different bins
mean(small_height$combat_power)
mean(normal_height$combat_power)
mean(large_height$combat_power)


#Assume that the total awesomeness of a pokemon is the product of `combat_power` and `hit_points`. 
#Which pokemon species has the highest awesomeness?

awesomeness <- pokemon %>%
  #add another column for awesomeness
  mutate(awesomeness = (combat_power * hit_points)) %>%
  #arrange in descending order
  arrange(desc(awesomeness))


#Plot height vs weight of all pokemons. Make appropriate transformations of axes and points. 
#Add a correlation coefficient and regression line to the plot.


#save a plot using pokemon data with an x axis of height, and y of weight
poke_plot <- ggplot(data = pokemon, aes(x = height_m, y = weight_kg)) +
  #plot the points
  geom_point() + 
  #perform logrithmic transformations on the x and y axises
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") + 
  #label the x and y axises, and give the plot a title
  labs(x = "Weight", 
       y = "Height", 
       title = "Height x Weight of Pokemon") + 
  #calculate the R-squared of line
  stat_cor(aes(label = ..rr.label..)) +
  #plot a linear regression line
  geom_smooth(method = "lm")


#Make a boxplot of `combat_power` (y) for `weight_bin` (x) category. Compare the means of 
#`combat_power` betwen "extra_large" and "extra_small" `weight_bin` and 
#plot the p-value on the boxplot.
  
#plot of pokemon data of weight categories against combat power
poke_boxplot <- ggplot(data = pokemon, aes(x = weight_bin, y = combat_power)) + 
  #turn it into a boxplot
  geom_boxplot() +
  #do a logrithmic transformation on the y axis
  scale_y_continuous(trans = 'log10') +
  #add in labels and a title
  labs(x = "Weight Category", 
       y = "Combat Power", 
       title = "Boxplot ") +
  #do t.test only on the extra large and small weight bins, and plot the p-value
  stat_compare_means(comparisons = list(c("extra_small", "extra_large")), method = "t.test")
                     
#Some pokemons have a unique fast_attack (not shared by any other pokemon). 
#How many pokemons and which pokemons have a unique fast_attack?

unique_attack <- pokemon %>%
  group_by(fast_attack) %>%
  tally %>%
  filter(n == 1) %>%
  pull(fast_attack)

poke1 = which(pokemon$fast_attack == unique_attack[1])
poke2 = which(pokemon$fast_attack == unique_attack[2])
poke3 = which(pokemon$fast_attack == unique_attack[3])
poke4 = which(pokemon$fast_attack == unique_attack[4])

pokemon[poke1,]
pokemon[poke2,]
pokemon[poke3,]
pokemon[poke4,]
