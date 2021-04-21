##  VARIABLES ## 

#inital input variables of QTL Variance, dominance-additivity ratio, increaser and null allele frequency
var = .005
da = -.12
IAF = .5
NAF = 1 - IAF

#number of indiviuals is decided by the increaser allele frequency
pq2 = (2*IAF*NAF) 
HWMin = min(IAF^2, pq2, NAF^2)
numInd = max((10/HWMin), 1000)
 
#Number of Replications
numReps = 10

#Additive Term
at = sqrt(var/(2*IAF*(1-IAF)*(1+da*(1-2*IAF))^2+4*IAF^2*(1-IAF)^2*da^2))
#Dominance Term
dt = at*da
#Mean
mean = -(IAF^2*at+2*IAF*(1-IAF)*dt-((1-IAF)^2)*at)

#Mean + Additive Term - genotype 2
mat = mean + at
#Mean + Dominance Term - genotype 1
mdt = mean + dt
#Mean - Additive Term - genotype 0
negmat = mean - at
  
#Residual Variance
rVar = 1-var
#Residual Standard Deviation
rsd = sqrt(rVar)
  
#counters
chi.count = 0
pValCount1 = 0
pValCount05 = 0
pValCount01 = 0
pValCount001 = 0

#simulate over numReps replications
for (i in seq_len(numReps)){
  #simulate numInd individuals per replicates based on the genetic input variables
  repData <- t(replicate(numInd, numInd.function(var,da,IAF)))
  
  #counters for genotypes of null (0), heterozygous (1), and increaser(2)
  counter0 = as.numeric(sum(repData[,1] == 0, na.rm=TRUE))
  counter1 = as.numeric(sum(repData[,1] == 1, na.rm = T))
  counter2 = as.numeric(sum(repData[,1] == 2, na.rm = T))
  
  
  #HWE Function - chi squared test for independence using HWE
  #writes out failed matrix to excel to examine those which failed the chi-square test and increases the count
  chi.mat <- HWE.function(counter0, counter1, counter2, numInd)
  if (chi.mat[11] < .05) {
    write.table(t(chi.mat), "chi.mat.failed.csv", sep = ',', append = T)
    chi.count = chi.count + 1
  }
 
  #ANOVA will begin
    #seperate the genotypes into their own datasets 
    #geno 0
    gen0 <- matrix(nrow = counter0, ncol = 3)
    gen0 <- repData[repData[,1] == 0,]
    qtl0 <- gen0[,2]
    
    #geno 1
    gen1 <- matrix(nrow = counter1, ncol = 3)
    gen1 <- repData[repData[,1] == 1,]
    qtl1 <- gen1[,2]
    
    #geno 2
    gen2 <- matrix(nrow = counter2, ncol = 3)
    gen2 <- repData[repData[,1] == 2,]
    qtl2 <- gen2[,2]
    
    
    #ANOVA testing
    aov.out = summary(aov(qtls ~ genos))
    repPValue <- aov.out[[1]][["Pr(>F)"]][[1]]
    
   #p-value counters
    if (repPValue <.001) {
      pValCount1 = pValCount1 + 1
      pValCount05 = pValCount05 + 1
      pValCount01 = pValCount01 + 1
      pValCount001 = pValCount001 + 1
    } # end of elif 1
    else if (repPValue <.01)  {
      pValCount1 = pValCount1 + 1
      pValCount05 = pValCount05 + 1
      pValCount01 = pValCount01 + 1
    } #end of elif 2
    else if (repPValue <.05) {
      pValCount1 = pValCount1 + 1
      pValCount05 = pValCount05 + 1
    } # end of elif3
    else if (repPValue <.1) pValCount1 = pValCount1 + 1

    
    #progress checker
    if  (i%%50 == 0) print(i)

   }#end of numReps loop
  

### OUTPUT TABLE ###
#confidence interval calculations
CI.1 <- binom.test(pValCount1, n = abs(numReps), conf.level = .9)
CI.05 <- binom.test(pValCount05, n = numReps, conf.level = .95)
CI.01 <- binom.test(pValCount01, n = numReps, conf.level = .99)
CI.001 <- binom.test(pValCount001, n = numReps, conf.level = .999)

#prints how many replicates failed chi sqaured under HWE
sprintf("%s replicates failed chi sqaured", chi.count/numReps)

#creation of table for vector of parameters
vecpar <- matrix(nrow = 4, ncol = 5)
colnames(vecpar) <- c("Number of Replicates", "Alpha", "Proportion Rejected", "Lower CI", "Upper CI")
vecpar[,1] <- numReps
vecpar[1,2] <- .1
vecpar[2,2] <- .05
vecpar[3,2] <- .01
vecpar[4,2] <- .001
vecpar[1,3] <- (pValCount1 / numReps)
vecpar[2,3] <- (pValCount05 / numReps)
vecpar[3,3] <- (pValCount01 / numReps)
vecpar[4,3] <- (pValCount001 / numReps)
vecpar[1,4] <- CI.1$conf.int[1][1]
vecpar[2,4] <- CI.05$conf.int[1][1]
vecpar[3,4] <- CI.01$conf.int[1][1]
vecpar[4,4] <- CI.001$conf.int[1][1]
vecpar[1,5] <- CI.1$conf.int[2][1]
vecpar[2,5] <- CI.05$conf.int[2][1]
vecpar[3,5] <- CI.01$conf.int[2][1]
vecpar[4,5] <- CI.001$conf.int[2][1]

#this output is a power calculator with the proportion rejected equating to the estimated power given a sufficiently high number of replications
print(vecpar)
