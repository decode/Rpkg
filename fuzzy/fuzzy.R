#信用模糊评价模型
fuzzy.load_source <- function(source_file) {
  #读取数据
  data <- read.csv(source_file, sep=",", header=TRUE)
  nlength = nrow(data)
  nwidth = ncol(data)
  return(data)
}

fuzzy.judge_matrix <- function(source) {
  judge  <- source
  nlength = nrow(judge)
  nwidth = ncol(judge)
  #judge <- judge / nlength
  judge_length <- 5
  temp <- array(0, dim=c(nwidth, judge_length))
  for(j in 1:nwidth) {
    for(i in 1:nlength) { 
      #print(as.numeric(judge[i, j]))
      temp[j, judge[i, j]+1] <- temp[j, as.numeric(judge[i, j])+1] + 1
    }
  }
  temp <- temp / nlength
  return(temp)
}

fuzzy.synthesize <- function(weight_data, judge_data) {
  temp <- weight_data %*% judge_data
  return(temp)
}

# 根据评价人经验和购买次数计算相应权重
fuzzy.normalize_sd <- function(input_data) {
  nlength = nrow(input_data)
  nwidth = ncol(input_data)
  data = array(0, dim=c(nlength, 1))
  for(i in 1:nlength) {
    data[i] <- input_data[i, 1] + input_data[i, 2] * 0.9 + 0.5
  }
  result = array(0, dim=c(nlength, 1))
  for(i in 1:nlength) {
    result[i] <- data[i] / sum(data)
  }
  return(result)
}
weight_data <- read.csv("result.csv", sep=",", header=FALSE)
pre <- fuzzy.load_source("~/Downloads/Taobao.csv")
judge_data <- fuzzy.judge_matrix(pre[, 7:39])
judge <- fuzzy.synthesize(t(weight_data[, 3]), judge_data)

normal <- fuzzy.normalize_sd(pre[, 2:3])
#print(t(normal))
temp <- (pre[, 7:39])
print(t(temp))
origin <- array(0, dim=c(1, ncol(temp)))
for(i in 1:ncol(temp)) {
  origin[1, i] <- sum(temp[, i])
}
print(origin)

temp1 <- array(0, dim=c(nrow(temp), ncol(temp)))
for(i in 1:nrow(temp)) {
  for(j in 1:ncol(temp)) {
    temp1[i, j] <- temp[i, j]
  }
}
judge <- fuzzy.synthesize(t(normal), temp1)
#print(t(normal) %*% pre[,7:39])
#print(sum(judge))
new_judge <- weight_data[,3] * judge
print("After weight plus judge:")

new_data <- pre[,7:(ncol(pre))]
t <- array(0, dim=c(5, ncol(new_data)))
t[1,] = names(new_data)
t[2,] = origin
t[3,] = weight_data[, 3]
t[4,] = judge
t[5,] = new_judge
print(t(t))
write.table((t), file="result1.csv", sep=",")
write.table(t(t), file="result2.csv", sep=",")
