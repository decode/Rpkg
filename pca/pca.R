pca.load_csv <- function(source_file) {
  data <- read.csv(source_file, sep=",", header=TRUE)
  nlength = nrow(data)
  nwidth = ncol(data)
  #res <- array(0, dim=c(nwidth-4, nlength))
  res <- data[, 5:nwidth]
  return(res)
}

pca.pricom <- function(x){ 
  temp = scale(x); 
  print("====================== after scale ======================")
  print(temp)
  cor = cor(temp); 
  #print("====================== cor: ==========================")
  #print(cor)
  eig = eigen(cor); 
  print("====================== eig: ==========================")
  print(eig);
  print("====================== eig$va ==========================")
  print(eig$va)
  cm = sweep(eig$ve,2,sqrt(eig$va),"*"); 
  par(mfrow=c(1,2)); 
  plot(eig$va,type="b",pch=22,col="red",ylab="EigenValue", xlab="Component Numbers",main="Scree Plot"); 
  plot(cm[,1],cm[,2],pch=22,ylab="Component2",xlab="Component1",col="green", ylim=c(-1,1),xlim=c(-1,1),main="Component Plot"); 
  abline(h=0,v=0,col="blue"); 
  rownames(cm)=colnames(x); 
  colnames(cm)=paste("Comp",c(1:dim(x)[2])); 
  write.csv(cm,"Component Mtrix.csv",row.names=T) 
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

result <- pca.load_csv('result.csv')
print(result)
#result <- princomp(data)
#result <- matrix(rnorm(600),nr=100)
#print(result)
pca.pricom(result)
pca.scores(result)
