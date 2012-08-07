pca.load_csv <- function(source_file) {
  data <- read.csv(source_file, sep=";", header=TRUE)
  nlength = nrow(data)
  nwidth = ncol(data)
  #res <- array(0, dim=c(nwidth-4, nlength))
  res <- data[, 5:nwidth]
  return(res)
}

pca.pricom <- function(x){ 
  temp = scale(x); 
  #print("====================== after scale ======================")
  #print(temp)
  cor = cor(temp); 
  #print("====================== cor: ==========================")
  #print(cor)
  eig = eigen(cor); 
  #print("====================== eig: ==========================")
  #print(eig);
  #print("====================== eig$va ==========================")
  #print(eig$va)
  cm = sweep(eig$ve,2,sqrt(eig$va),"*"); 
  par(mfrow=c(1,2)); 
  plot(eig$va,type="b",pch=22,col="red",ylab="EigenValue", xlab="Component Numbers",main="Scree Plot"); 
  plot(cm[,1],cm[,2],pch=22,ylab="Component2",xlab="Component1",col="green", ylim=c(-1,1),xlim=c(-1,1),main="Component Plot"); 
  abline(h=0,v=0,col="blue"); 
  rownames(cm)=colnames(x); 
  colnames(cm)=paste("Comp",c(1:dim(x)[2])); 
  write.csv(cm,"Component Matrix.csv",row.names=T) 
  cm 
} 

pca.scores <- function(x){ 
  temp = scale(x); 
  cor = cor(temp); 
  eig = eigen(cor); 
  cm = sweep(eig$ve,2,sqrt(eig$va),"*"); 
  cm2 = sweep(cm,2,eig$va/sum(eig$va),"*"); 
  scores = temp %*% cm2; 
  write.csv(scores,"scores.csv",row.names=T) 
  scores; 
} 

#原文件为result.csv
result <- pca.load_csv('newresult.csv')
#result <- pca.load_csv('result.csv')
#print(summary(result))
#result <- princomp(data)
#result <- matrix(rnorm(600),nr=100)
#print(result)
print("========================= pricom ===========================")
pr <- pca.pricom(result)
print(pr)
print("========================= scores ===========================")
print(pca.scores(result))

#for(i in 1:ncol(result)) {
#  print(i)
#  cat(var(result[,i]))
#}   
#print(var(result[,24]))


print("========================= princomp ===========================")
x <- scale(result)
x.pr <- princomp(x, core=T)
print(summary(x.pr))#, loadings=T)
#print("========================= load ===========================")
#x.load <- loadings(x.pr)
#print(x.load)
#print("========================= predict ===========================")
#print(predict(x.pr))


# 使用线性加权法计算得分
y <<- scale(result, center=F)
write.csv(y, "normal.csv")
s <- array(0, c(40,0))

#data <- read.csv('result2sort_new.csv', sep=",", header=TRUE)
data <- read.csv('result2sort_new.csv', sep=",", header=F)
nlength = nrow(y)
nwidth = ncol(y)
#res <- array(0, dim=c(nwidth-4, nlength))
#res <- data[, 6:nwidth]

z <- scale(t(data[,6]), center=F)
for(i in 1:nlength) {
  t <- 0
  for(j in 1:nwidth) {
    if(j==15 || j==16 || j==18 || j==19 ||j==21 || j==22) {
      y[i,j] <- 1- y[i,j]/sum(y[,j])
      #print(sum(y[i,j]))
    }
    t <- t + (y[i,j] * z[j])
  }
  s[i] = (t)
}
write.csv(s, "normal_sum.csv")

y <<- scale(result)
for(i in 1:nwidth) {
  #print((var(y[,i])))
}
