# create some file
library('lsa')

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
      #print(paste("same type:", paste(i, type)))
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
print(paste("Type count: ", type))

unlink(td, recursive=TRUE)

br = array(0)
for(i in 1:type) {
  cur = 1
  ar = array(0)
  for(j in 1:length(res)) {
    if(res[j] == i) {
      ar[cur] = j
      cur = cur + 1
    }
  }
  print(paste("Type ", i))
  print(ar)
  br[i] = ar
}
print(type)
#trm = lw_bintf(trm) * gw_idf(trm)
#space = lsa(trm)

