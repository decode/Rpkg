library('gdata')
library('eRm')

# Example
data(pcmdat2)
res.rsm <- RSM(pcmdat2)
thresholds(res.rsm)
plotICC(res.rsm, mplot=TRUE, legpos=FALSE,ask=FALSE)

res.pcm <- PCM(pcmdat2)
plotPImap(res.pcm, sorted = TRUE)
pres.pcm <- person.parameter(res.pcm)
itemfit(pres.pcm)


lr <- 2*(res.pcm$loglik-res.rsm$loglik)
df<- res.pcm$npar-res.rsm$npar
pvalue<-1-pchisq(lr,df)
cat("LR statistic: ", lr, " df =",df, " p =",pvalue, "\n")

# -------------------------------------------

xls <- read.xls('~/develop/git/aubluo/tools/fa.xlsx', header=F)
xls.rsm <- RSM(xls-1)
xls.rsm <- RSM(xls[, 1:4]-1)
summary(xls.rsm)
thresholds(xls.rsm)
test_info(xls.rsm)
item_info(xls.rsm)
pres <- person.parameter(xls.rsm)
itemfit(pres)

plotINFO(xls.rsm, type='item')
plotICC(xls.rsm, mplot=TRUE, legpos=FALSE,ask=FALSE)
plotPImap(xls.rsm, sorted = TRUE)
pres.pcm <- person.parameter(xls.rsm)
plotICC(pres.pcm, mplot=TRUE, legpos=FALSE,ask=FALSE)

xls.pcm <- PCM(xls[, 1:4]-1)
xls.pcm <- PCM(xls-1)
plotPImap(xls.pcm, sorted = TRUE)
pres.pcm <- person.parameter(xls.pcm)
itemfit(pres.pcm)
thresholds(xls.pcm)
plotICC(xls.pcm, mplot=TRUE, legpos=FALSE,ask=FALSE)


library(prefmod)
data(music)
p <- PCM(music)
head(music)

library(mixRasch)
# Example data included with mixRasch
data(SimMix)
test1 <- mixRasch(SimMix,1,50, conv.crit=.0001, n.c=1)
test2 <- mixRasch(SimMix,1,500, conv.crit=.0001, n.c=2)
# The 3 class solution may take a while to converge or need
# to be restarted if it fails to converge.
test3 <- mixRasch(SimMix,1,1000, conv.crit=.0001, n.c=3)
# Notice that the AIC and BIC are lowest for the 2 class solution
rbind(test1$info.fit,test2$info.fit,test3$info.fit)
# Notice that the two "difficulty" columns are ordered differently
# The results reflect that the two groups in the data set have a reversed
# scale from one another.
test2

test <- mixRasch(xls-1, 5)
test
