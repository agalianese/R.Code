library(mclust)

#BIC Calculations
classification <- anueploidy$Class_Anue
class_age <- anueploidy$Class_Age
table(classification)
fitted <- anueploidy$`(O - F)`

#returns scatterplot for each variable pair
clPairs(fitted, classification)
#evaluates BIC based on different groupings, returning the best 3 models
#E means equal variance, V is variable or unequal variance
BIC <- mclustBIC(fitted, modelNames = c("E","V"))

##plots and summarizes best BIC values
plot(BIC)
summary(BIC)


#BIC for SQRT values
sqrt <- anueploidy$SQRT
sqrt_BIC <- mclustBIC(sqrt, modelNames = c("E","V"))
plot(sqrt_BIC)
summary(sqrt_BIC)
sqrt_BIC


#BIC for Cube Root Values
cube <- anueploidy$`Cube Root`
cube_BIC <- mclustBIC(cube, modelNames = c("E","V"))
plot(cube_BIC)
summary(cube_BIC)
cube_BIC



clPairs(anueploidy[,6:7], classification)
