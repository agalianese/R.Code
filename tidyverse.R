---
title: "R Notebook"
output: html_notebook
---
```{r}
#load in library
library(tidyverse)

#read in data
data <- read_tsv('C:/Users/mc/Documents/homework_data.tsv')
quiz_data <- read_tsv('C:/Users/mc/Documents/quiz_data.tsv')
tcga <- read_csv("C:/Users/mc/Documents/tcga_stats.csv")
data2 <- read_tsv('C:/Users/mc/Documents/quiz02_data1.tsv')
```

#Create a new data frame with only genes longer than 400 codons and containing utrs.
```{r}
#create a new data drame
long.df <- data %>%
  #filter so it only contains those longer than 400 codons
  filter(Length > 400) %>%
  #filter so NA's are removed
  filter(!is.na(utr))

long.df
```

# What is the difference in the average RPF values of genes with and without uATGs?
```{r}
#create new datasets based on filter for T or F for whether the genes have uATGs
with_uATG <- data %>%
  filter(uATG == T)
without_uATG <- data %>%
  filter(uATG == F)

#comparisons 
mu_with = mean(with_uATG$RPF) 
mu_without = mean(without_uATG$RPF)
diff = mu_with - mu_without


#print statements
sprintf("The average RPF value for genes with uATGS is %f.", mu_with)
sprintf("The average RPF value for genes without uATGS is %f.", mu_without)
sprintf("The different in average RPF values is %f.", diff)

```

#What is the length of a genome which has only the genes with mRNA abundances greater than 250?
```{r}
#create dataset with an mRNA abundance > 250
large_mRNA = data %>%
  filter(mRNA > 250)

#sum the gene lengths to get the length of the fake genome
sum(large_mRNA$Length)

```

#What proportion of total mRNA is due to all short genes (less than 250 codons)?
```{r}
#find the total amount of mRNA
total_mRNA = sum(data$mRNA)

#filter for short genes
short_genes = data %>%
  filter(Length < 250)

#find the total amount of mRNA due to the short genes
short_mRNA = sum(short_genes$mRNA)

#find the proportion of mRNA due to short genes
prop = (short_mRNA / total_mRNA)
print(prop)
```


#What is the average GC content of utr containing, short genes (less than 250) ?
```{r}
#create a dataset of short genes that contain a utr
gc_content = short_genes %>%
  filter(!is.na(utr))

#find the average GC content for those genes
mean(gc_content$gc)
```

#What is the size of the genome comprising of only the last 100 genes in the dataset?
```{r}
#find the end of the genome
end_point = nrow(quiz_data)

#start point so as to include the 100 genes
start = nrow(quiz_data) - 99

#sum the lengths of the last 100 genes
genome_size = sum(quiz_data$Length[start:end_point])
genome)prprint(genome_size)
````


#Where in the dataset are the following genes - YBL006C, YBL025W, and YAL025C?
````{r}
#find the positions of the 3 genes and print them
pos1 <- which(quiz_data == "YBL006C")
pos2 <- which(quiz_data == 'YBL025W')
pos3 <- which(quiz_data == 'YAL025C')

sprintf("Gene YBL006C is at position %i. Gene YBL025W is at %i. Gene YAL025C is at %i.", pos1, pos2, pos3)
````


#How many biospecimen files have been collected for all instances of kidney cancer in the dataset?
```{r}
#filter for kidney cancer
kidney_cancer <- tcga %>%
  filter(primary == 'Kidney')

#sum the amount of biospecimen files, removing NA
sum(kidney_cancer$biospecimen, na.rm = T)
````

#Which primary cancer site occurs the most among unreported genders (that is, anything that is not male or female)?
```{r}

#view what types of genders are in the table
table(tcga$gender)


unreported_gender <- tcga %>%
  #filter out females
  filter(gender != 'female') %>%
  #filter out males
  filter(gender != 'male') %>%
  #group the cancers
  group_by(primary) %>%
  #tally the amount in each group
  tally() %>%
  #arrange in descending order
  arrange(desc(n))
````


#Submissions come with a unique ID in the submitter column, have any submitters have submitted more than one entry?
```{r}

unique_ID <- tcga %>%
  #group by who submitted the entry
  group_by(submitter) %>%
  #tally the amount of submissions per person
  tally() %>%
  #filter leaving only those who submitted more than 1
  filter(n > 1)

````
#Which project_id has submitted the most entries? For that ID, what primary site did they submit the most entries for?


````{r}


most_project <- tcga %>%
  #group by who submitted the entry
  group_by(project_id) %>%
  #tally the amount of submissions per project
  tally() %>%
  #arrange in descending order
  arrange(desc(n)) %>% 
  #grab the project id and put it as the variable
  pull(project_id) %>%
  .[1]

which_primary <- tcga %>%
  #filter the project ids by the one we just found
  filter(project_id == most_project) %>%
  #group by the types of cancer
  group_by(primary) %>%
  #tally the results and arrange them in descending order
  tally() %>%
  arrange(desc(n)) %>%
  #grab the name of the primary
  pull(primary) %>%
  .[1]

````

# Which submitter has the most number of sequencing datasets for females? 
head(data2)
most_female <- data2 %>%
  #filter for females
  filter(gender == 'female') %>% 
  #group by the submitter
  group_by(submitter) %>%
  #tally the amount of sequencing datasets per submitter
  tally(sequencing_reads) %>%
  #arrange in descending order
  arrange(desc(n))



#Write a function where the user provides two inputs - (i) the current dataset and (ii) project id 
#and returns the sum of all transcriptome_profiling files for that project. 
```{r}
#create new function with inputes of a dataset and project id
data_project <- function(dataset, new_project_id) {
  
  temp <- dataset %>%
    #filter through the chosen dataset to isolate those with chosen project id
    filter(project_id == new_project_id)
  
  #return the sum of the transcriptome profiling with NA's removed
  return(sum(temp$transcriptome_profiling, na.rm = T))
}

data_project(data2, 'BEATAML1.0-COHORT')

````

