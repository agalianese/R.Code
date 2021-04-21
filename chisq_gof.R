#case is upper threshold, control is lower
low.count == control0 + control1 + control2
up.count == case0 + case1 + case2

#actual matrix
actual.mat <- matrix(data = c(control0, control1, control2, case0, case1, case2), nrow = 2,  ncol = 3)
colnames(actual.mat) <- c("Geno 0", "Geno 1", "Geno 2")
rownames(actual.mat) <- c('Control', 'Case')

control.total <- sum(actual.mat[1,])
case.total <- sum(actual.mat[2,])
g0.total <- sum(actual.mat[,1])
g1.total <- sum(actual.mat[,2])
g2.total <- sum(actual.mat[,3])
actual.total = control.total + case.total


#expected matrix
expected.mat <- matrix(c((control.total*g0.total)/actual.total,
                         (case.total*g0.total) / actual.total,
                         (control.total*g1.total)/actual.total,
                         (case.total*g1.total) / actual.total,
                         (control.total*g2.total)/actual.total,
                         (case.total*g2.total) / actual.total), nrow = 2)

#Chi squared
chi.gof = (actual.mat - expected.mat)^2 / expected.mat
df = (nrow(expected.mat) - 1)*(ncol(expected.mat) - 1)
gof.pval = 1 - pchisq(sum(chi.gof), df)
print(gof.pval)
