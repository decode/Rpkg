# create some file
library('lsa')
td = tempfile()
dir.create(td)
write( c("dog", "cat", "mouse"), file=paste(td, "D1", sep="/"))
write( c("hamster", "mouse", "sushi"), file=paste(td, "D2", sep="/"))
write( c("dog", "monster", "monster"), file=paste(td, "D3", sep="/"))
write( c("dog", "mouse", "dog"), file=paste(td, "D4", sep="/"))

# create matrices
myMatrix = textmatrix(td, minWordLength=1)
myLSAspace = lsa(myMatrix, dims=dimcalc_share())
myNewMatrix = as.textmatrix(myLSAspace)
# calc associations for mouse
associate(myNewMatrix, "mouse")
# clean up
unlink(td, recursive=TRUE)

data(corpus_training)
data(corpus_essays)
data(corpus_scores)

f = file("data.txt", 'r')

td = tempfile()
dir.create(td)

line = readLines(f, n=1)
i = 1
while( length(line)!=0 ) {
  write.table( strsplit(line, " "), file=paste(td, i, sep="/")) 
  line = readLines(f, n=1)
  i = i + 1
}
close(f)

m = textmatrix(td, minWordLength=1)
s = lsa(m)
n = as.textmatrix(s)


check <- function(m, res, current) {
  start = current + 1
  if(start > ncol(m))
    return(res)
  for(i in start:ncol(m)) {
    if(res[i] == 0 && (cor(m[,as.character(current)], m[,as.character(i)])> 0.7)) {
      res[i] = res[current]
      print(paste("same type:", paste(i, type)))
    }
  }
  return(res)
}
res = array(0, dim=c(ncol(m)))
type = 0
for(i in 1:ncol(m)) {
  if(res[i] == 0) {
    type = type + 1
    res[i] = type
    res = check(m, res, i)
  }
}
print(res)
print(type)
print(cor(m[, '1'], m[,'2']))


unlink(td, recursive=TRUE)

#trm = lw_bintf(trm) * gw_idf(trm)
#space = lsa(trm)

