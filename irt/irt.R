library('gdata')
library('eRm')

# Example
example <- function() {
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
}

# -------------------------------------------

xls <- read.xls('~/develop/git/aubluo/tools/fa.xlsx', header=F)
rsm <- function() {
  #txls <- t(xls)
  xls.rsm <- RSM(xls-1)
  #xls.rsm <- RSM(xls[, 1:4]-1)
  #summary(xls.rsm)

  test_info(xls.rsm)
  item_info(xls.rsm)
  pres <- person.parameter(xls.rsm)
  fit <- itemfit(pres)
  pfit <- personfit(pres)
  a <- cbind(fit$i.fit, fit$i.df, fit$i.outfitMSQ, fit$i.infitMSQ, fit$i.outfitZ, fit$i.infitZ)
  colname <- c('chisq', 'df', 'outfit msq', 'infit msq', 'outfit t', 'infit t')
  b <- rbind(colname, a)
  write.csv(b, '~/develop/git/aubluo/tools/fit.csv')

  xls.thresh <- thresholds(xls.rsm)
  write.csv(xls.thresh$threshtable, '~/develop/git/aubluo/tools/thresh.csv')

  plotINFO(xls.rsm, type='item')
  plotICC(xls.rsm, mplot=TRUE, legpos=FALSE,ask=FALSE)
  plotPImap(xls.rsm, sorted = TRUE)
}

pcm <- function() {
  xls.pcm <- PCM(xls[, 1:4]-1)
  xls.pcm <- PCM(xls-1)
  plotPImap(xls.pcm, sorted = TRUE)
  pres.pcm <- person.parameter(xls.pcm)
  itemfit(pres.pcm)
  thresholds(xls.pcm)
  plotICC(xls.pcm, mplot=TRUE, legpos=FALSE,ask=FALSE)
}

#library(prefmod)
#data(music)
#p <- PCM(music)
#head(music)

mixrasch <- function() {
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
}

library('ltm')
grm_example <- function() {
  res <- grm(Science[c(1,3,4,7)])
  plot(res)
  margins(res)
  des <- descript(xls)
  write.csv(des$alpha, '~/develop/git/aubluo/tools/des.csv')
  res <- grm(xls)
  write.csv(res$coefficients, '~/develop/git/aubluo/tools/grm.csv')
}

seperate <- function(x, sep=3) {
  if(x<sep) return(0) else return(1)
}

trans <- function(x, sep=3) {
  res <- array(0, c(nrow(x), ncol(x)))
  for(j in 1:ncol(x)) {
    for(i in 1:nrow(x)) {
      res[i, j] = seperate(x[i,j], sep)
    }
  }
  return(res)
}

ctpm <- function() {
  tt <- trans(xls, 2)
  t1 <- tpm(tt)
  factor.scores(t1)
  write.csv(coef(t1), '~/develop/git/aubluo/tools/tpm.csv')
  tt <- trans(xls, 3)
  t2 <- tpm(tt)
  factor.scores(t2)
  write.csv(coef(t2), '~/develop/git/aubluo/tools/tpm1.csv')
  tt <- trans(xls, 4)
  t3 <- tpm(tt)
  factor.scores(t3)
  write.csv(coef(t3), '~/develop/git/aubluo/tools/tpm2.csv')
}
