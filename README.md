# R_Code
This repository represents all of my coding done in the R language. 

Tidyverse and ggplot represent some of my course projects.

numInd_function, BIC_Calculations, HWE_checker, and chisq_gof, come from my research. My research focuses on analyzing which factors most significantly affect the probability of finding gnees that influence quantitative traits. Our overall goal is to identify important genes and map their position in the genome. Additionally, we hope to examine gene-environment interactions and evaluate sample size requirements. 

We began by simulating genotype and phenotype data using the numInd_function. It  simualtes three three normal distributions laid over one another under the Fisher-Falconer Model. The data is then combined into a larger dataset where it is analyzed for significant differences between the genotypic means. 

Some of the data analysis done includes the HWE_checker. This function checks that the data follows Hardy-Weinberg Equilibrium using the Chi-Square Test for Independence. If more than 5% of replicates fail this test, then the data is re-simulated. Next an ANOVA is run for significance. If the data fails and is significant, p-value counters are logged. The counters are then used to create a binomial confidence interval indicating the true power of the sample.

Our current work involves adding environmental effects of age and aneuploidy rate into a threshold-based model. We then hope to repeat the process and analyze which traits most significantly contribute to phenotype and to identify the best statistical method to model gene-environment interactions. 

BIC_calculations and chisq_gof have been used in the next stage of the research. The ChiSq_GOF function checks that the threshold data is being simulated correctly under the chosen model. BIC_calculations attempt to identify the underlying trends of the data, for example, given a sample, how many various groups comprise the sample?

Our current work involves attempting to fit a dataset into the model using a logistic distribution. This is done to see what happens if you remove the assumption of normality from a sample, and how the data is then compromised if you continue to operate under the normality assumption. 
