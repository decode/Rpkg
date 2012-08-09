library('gdata')
library('eRm')

# Example
data(pcmdat2)
res.rsm <- RSM(pcmdat2)
thresholds(res.rsm)
plotICC(res.rsm, mplot=TRUE, legpos=FALSE,ask=FALSE)

# -------------------------------------------

xls <- read.xls('~/develop/git/aubluo/tools/fa.xls', header=F)
xls.rsm <- RSM(xls[, 1:20])
thresholds(xls.rsm)
plotICC(xls.rsm, mplot=TRUE, legpos=FALSE,ask=FALSE)
pres.pcm <- person.parameter(xls.rsm)
plotICC(pres.pcm, mplot=TRUE, legpos=FALSE,ask=FALSE)

xls.pcm <- PCM(xls[, 1:20])
plotPImap(xls.pcm, sorted = TRUE)
pres.pcm <- person.parameter(xls.pcm)
itemfit(pres.pcm)
thresholds(pres.pcm)
plotICC(pres.pcm, mplot=TRUE, legpos=FALSE,ask=FALSE)

res.pcm <- PCM(pcmdat2)
plotPImap(res.pcm, sorted = TRUE)
pres.pcm <- person.parameter(res.pcm)
itemfit(pres.pcm)


lr <- 2*(res.pcm$loglik-res.rsm$loglik)
df<- res.pcm$npar-res.rsm$npar
pvalue<-1-pchisq(lr,df)
cat("LR statistic: ", lr, " df =",df, " p =",pvalue, "\n")
