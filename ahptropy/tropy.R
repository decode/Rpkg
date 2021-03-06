# 读取基本数据
tropy.load_data <- function(source_file) {
  data <- read.csv(source_file, sep=";", header=TRUE)
  return(data)
}

# 计算标准化矩阵
tropy.normalize <- function(input_data) {
  nlength = nrow(input_data)
  nwidth = ncol(input_data)
  data = array(0, dim=c(nlength, nwidth))
  for(i in 1:nlength) {
    for(j in 1:nwidth) {
      data[i, j] <- input_data[i, j] / sum(input_data[, j])
    }
  }
  print("Normalize Data:")
  print(data)
  return(data)
}

# 计算标准化矩阵 使用方差计算标准化矩阵
tropy.normalize_sd <- function(input_data) {
  nlength = nrow(input_data)
  nwidth = ncol(input_data)
  data = array(0, dim=c(nlength, nwidth))
  for(i in 1:nlength) {
    for(j in 1:nwidth) {
      data[i, j] <- input_data[i, j] / sd(input_data[, j])
    }
  }
  print("Normalize Data:")
  print(data)
  return(data)
}

# 根据标准化评价矩阵计算信息熵
tropy.entropy <- function(input_data) {
  nlength = nrow(input_data)
  nwidth = ncol(input_data)
  e = array(0, dim=c(1, nwidth))
  for(i in 1:nwidth) {
    temp <- 0
    for(j in 1:nlength) {
      if(input_data[j, i] == 0)
        l = 0
      else
        l = log(input_data[j, i])
      temp  <- temp + (input_data[j, i] * l)
    }
    e[i] <- -(1 / log(nwidth)) * temp
  }
  print("Entropy:")
  #print(e)
  return(e)
}

# 根据信息熵计算权重
tropy.weight <- function(input_data) {
  nlength = nrow(input_data)
  nwidth = ncol(input_data)
  w = array(0, dim=c(1, nwidth))
  print((input_data))
  for(i in 1:nwidth) {
    w[i] <- input_data[i] / sum(input_data[])
  }
  print("Weight:")
  #print(w)
  return(w)
}

data <- tropy.load_data("~/Downloads/Taobao_1.csv")
new_data <- data[,8:(ncol(data))]
normal <- tropy.normalize(new_data)
e <- tropy.entropy(normal)
w <- tropy.weight(e)
t <- array(0, dim=c(2, ncol(w)))
t[1,] = names(new_data)
t[2,] = w
print(t(t))
write.table(t(t), file="result_1.csv", sep=",")
#plot(1:33, w);text(1.8:33.8, w, 1:33)
plot(1:16, w);text(1.8:ncol(data)-0.6, w, 1:ncol(data))

x <- new_data
width <- ncol(x)
for(i in 1:nrow(x)) x[i,width+1]<-sum(x[i,1:width])
y<-c(1:width+1)
y<-as.data.frame(y)

for(i in 1:width+1)(y[i,1]<-var(x[,i]))
a<-(width/(width-1))*(1-sum(y[1:width,1])/y[width+1,1])


#p <- array(0, dim=c(0, 109))
#w <- array(0, dim=c(0, 109))
#for(i in 1:109) {
  #p[i] <- (runif(1, 1, 4.3))
#}
#for(i in 1:109) {
  #w[i] <- p[i]/sum(p)
#}
