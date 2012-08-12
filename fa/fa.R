library(gdata)
library(psych)
source("../test/kmo.R")

# example
covariances = ability.cov$cov
correlations = cov2cor(covariances)
fa.parallel(correlations, n.obs=112, fa="fa", n.iter=100,show.legend=FALSE)
fa = fa(correlations,nfactors=2,rotate="varimax",fm="pa" )


xls <- read.xls('~/develop/git/aubluo/tools/fa.xlsx', header=F)
xls.t <- t(as.matrix(xls))
head(xls)

xls <- read.xls('~/develop/git/aubluo/tools/fa.xlsx', sheet=1, header=F)
xls <- xls[5:215,]

# 计算相关系数
corMat <- cor(xls)
corMat

# KMO检验
k <- kmo(xls)
k <- kmo(t(as.matrix(xls)))
k$overall
k$report

# Bartlett球度检验
bartlett.test(xls)

# 平行分析
fa.parallel(xls, fa="fa")#, n.iter=100,show.legend=FALSE)

# 因子旋转
fa = fa(xls,nfactors=5,rotate="varimax",fm="pa")
fa
f = factanal(xls)



