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
  #group by their attacks
  group_by(fast_attack) %>%
  #tally and select those who have a unique fast attack
  tally %>%
  filter(n == 1) %>%
  #pull the name of the fast attack
  pull(fast_attack)

#find the position in the dataset where those unique attacks are 
poke1 = which(pokemon$fast_attack == unique_attack[1])
poke2 = which(pokemon$fast_attack == unique_attack[2])
poke3 = which(pokemon$fast_attack == unique_attack[3])
poke4 = which(pokemon$fast_attack == unique_attack[4])

#print the line for those pokemon with unique attacks
pokemon[poke1,]
pokemon[poke2,]
pokemon[poke3,]
pokemon[poke4,]



###### All R code from working at the Rabadan lab follows #########

# create tis df of data with 0 and 1 as homozygous for a spot
```{r}

Acral_Optitype <- read_excel("Downloads/Acral_Optitype.xlsx")

p1 <- data.frame(Acral_Optitype[1:34,])[c(-1,-3)]
myCols <- colnames(p1)
myCols[1] <- "Sample"

p2 <- Acral_Optitype[35:52,][c(-2,-3)]

colnames(p1) <- myCols
colnames(p2) <- myCols


t1 <- p1 %>%
  filter(str_detect(p1$Sample, "tumour"))
c1 <- p1 %>%
  filter(str_detect(p1$Sample, "normal"))

t2 <- p2 %>%
  filter(str_detect(p2$Sample, "T"))

c2 <- p2 %>%
  filter(str_detect(p2$Sample, "C"))


myTis <- rbind(t1, t2)
myCel <- rbind(c1, c2)

myTis
myCel
```

# Tissue Plots
```{r}
# convert the data frame to a long format
#mutate df so columns contain 0 (no homo), or 1(homo) for each allele

tisDF <- myTis %>%
  mutate(A_Homo = (A1 == A2)) %>%
  mutate(B_Homo = (B1 == B2)) %>%
  mutate(C_Homo = (C1 == C2))

plotTis <- tisDF[c(1,10,11,12)]
df_tis <- reshape2::melt(plotTis,na.rm=TRUE, id.vars="Sample")

ggplot(df_tis, aes(x=Sample, y=as.numeric(value), fill=factor(variable))) + geom_col(na.rm=TRUE) + theme_classic() +  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(title="HLA Homozygosity for Tis",  x="Samples", y="Count of Genes with HLA Homozygosity") 

# create the stacked histogram using ggplot2
ggplot(df_tis, aes(x=Sample, y=as.numeric(value))) + geom_col(na.rm=TRUE) + theme_classic() +  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(title="HLA Homozygosity for Tis",  x="Samples", y="Count of Genes with HLA Homozygosity") 


```


```{r}

# convert the data frame to a long format
#mutate df so columns contain 0 (no homo), or 1(homo) for each allele

celDF <- myCel %>%
  mutate(A_Homo = (A1 == A2)) %>%
  mutate(B_Homo = (B1 == B2)) %>%
  mutate(C_Homo = (C1 == C2))


celDF

plotCel <- celDF[c(1,10,11,12)]
df_cel <- reshape2::melt(plotCel,na.rm=TRUE, id.vars="Sample")

ggplot(df_cel, aes(x=Sample, y=as.numeric(value), fill=factor(variable))) + geom_col(na.rm=TRUE) + theme_classic() +  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(title="HLA Homozygosity for Normal",  x="Samples", y="Count of Genes with HLA Homozygosity") 

# create the stacked histogram using ggplot2
ggplot(df_cel, aes(x=Sample, y=as.numeric(value))) + geom_col(na.rm=TRUE) + theme_classic() +  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(title="HLA Homozygosity for Normal",  x="Samples", y="Count of Genes with HLA Homozygosity") 


```
# Mutations - what genes are being mutated? What samples? 
```{r}

# convert the data frame to a long format
#mutate df so columns contain 0 (no homo), or 1(homo) for each allele
jointDF <- rbind(celDF, tisDF)
mutDF = data.frame(c(0), nrow=1, ncol=4)

 
for (i in 1:nrow(celDF)) {
  
  # 6 for 2 alleles for HLA-A, B, C
  for (j in 2:7) {
    
    if (celDF[i,j] != tisDF[i,j]) {

      print(paste0(celDF[i, 1], ": ",celDF[i,j], " -> ", tisDF[i,j]))
      rowLen = nrow(mutDF) + 1
      
      print(rowLen)
      mutDF[rowLen, 1] <- celDF[i,1]
      mutDF[rowLen, 2] <- celDF[i,j]
      mutDF[rowLen, 3] <- tisDF[i,j]
    }
  }
}

colnames(mutDF) <- c("Sample", "Normal", "Mutation")
mutDF[-1,]

```

#EC2 INSTANCE PLOTS 
```{r}
mj_inst <- data.frame(ec2_instance[9,])

mj_inst[1,15] = 0
mj_inst[1,16] = 0

mj_inst[2,] <- colnames(mj_inst)
myOut <- data.frame(t(mj_inst))
myOut <- myOut[-1,]

myOut$X1 <- as.numeric(as.character(myOut$X1))
myOut$X2 <- factor(myOut$X2, levels = unique(myOut$X2[order(myOut$X1)]))

p1 <- ggplot(myOut, aes(x = X2, y = X1)) +
  geom_bar(stat = "identity", fill = "steelblue") +  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(x="Lab Members", y="May / June Averaged Instance Cost") +
  ggtitle("Bar Graph")
```


#EC2 OTHER PLOTS 
```{r}
other <- data.frame(ec2_other[9,])

other[2,] <- colnames(other)

myOther <- data.frame(t(other))

myOther <- myOther[-1,]
myOther$X1 <- as.numeric(as.character(myOther$X1))
myOther$X2 <- factor(myOther$X2, levels = unique(myOther$X2[order(myOther$X1)]))

p2 <- ggplot(myOther, aes(x = X2, y = X1)) +
  geom_bar(stat = "identity", fill = "steelblue") +  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(x="Lab Members", y="Cost ($)", title ="May / June Averaged Volume Cost per User")
```
