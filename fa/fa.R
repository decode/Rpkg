library(gdata)
library(psych)
source("../test/kmo.R")

# example
covariances = ability.cov$cov
correlations = cov2cor(covariances)
fa.parallel(correlations, n.obs=112, fa="fa", n.iter=100,show.legend=FALSE)
fa = fa(correlations,nfactors=2,rotate="varimax",fm="pa" )


xls <- read.xls('~/develop/git/aubluo/tools/fa.xls', header=FALSE)
head(xls)
fa.parallel(xls, fa="fa")#, n.iter=100,show.legend=FALSE)
fa = fa(xls,nfactors=5,rotate="varimax",fm="pa" )
fa
f = factanal(xls)


# 计算相关系数
corMat <- cor(xls)
corMat

# KMO检验
xls <- read.xls('~/develop/git/aubluo/tools/fa.xls', header=FALSE)
k <- kmo(xls[,c(1,3,4, 5)])
k$overall
k$report

# Bartlett球度检验
bartlett.test(xls)

