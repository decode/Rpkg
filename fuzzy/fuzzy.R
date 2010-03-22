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
<<<<<<< HEAD
  print(nwidth)
  temp <- temp / nlength
  print(temp)
  return(judge)
=======
  temp <- temp / nlength
  print(temp)
  return(temp)
>>>>>>> cd27836b7a2cff4cd86eeee67424673aad7a4300
}

fuzzy.synthesize <- function(weight_data, judge_data) {
  temp <- weight_data %*% judge_data
  return(temp)
}

weight_data <- read.csv("result.csv", sep=",", header=FALSE)
<<<<<<< HEAD
#nlength = nrow(weight_data)
#nwidth = ncol(weight_data)
pre <- fuzzy.load_source("~/Downloads/Taobao.csv")
print(pre)

judge_data <- fuzzy.judge_matrix(pre[, 7:39])

#nlength = nrow(judge_data)
#nwidth = ncol(judge_data)

judge <- fuzzy.synthesize(t(weight_data[, 3]), pre)
=======
pre <- fuzzy.load_source("~/Downloads/Taobao.csv")
judge_data <- fuzzy.judge_matrix(pre[, 7:39])
judge <- fuzzy.synthesize(t(weight_data[, 3]), judge_data)
>>>>>>> cd27836b7a2cff4cd86eeee67424673aad7a4300
print(judge)
