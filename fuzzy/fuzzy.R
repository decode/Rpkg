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
  print(temp)
  return(temp)
}

fuzzy.synthesize <- function(weight_data, judge_data) {
  temp <- weight_data %*% judge_data
  return(temp)
}

weight_data <- read.csv("result.csv", sep=",", header=FALSE)
pre <- fuzzy.load_source("~/Downloads/Taobao.csv")
judge_data <- fuzzy.judge_matrix(pre[, 7:39])
judge <- fuzzy.synthesize(t(weight_data[, 3]), judge_data)
print(judge)

print(pre[, 2:3])
weight <- array(c(1, 0.775, 0.75, 0.75, 0.05, 0.75, 1, 1, 0.7, 0.525), dim=c(1,10))

