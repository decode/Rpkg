# 读取基本数据
tropy.load_data <- function(source_file) {
  data <- read.csv(source_file, sep=",", header=TRUE)
  return(data)
}

# 计算标准化矩阵
tropy.nomalize <- function(datafile) {
  nlength = nrow(data)
  nwidth = ncol(data)
  data = array(0, dim=c(nlength, nwidth))
  for(i in 1:nwidth) {
    for(j in 1:nlength) {
      data[i, j] <- datafile[i, j] / sum(datafile[, j])
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
      temp  <- temp + (input_data[j, i] * log(input_data[j, i]))
    }
    e[i] <- -(1 / log(nwidth)) * temp
  }
  print("Entropy:")
  print(e)
  return(e)
}

# 根据信息熵计算权重
tropy.weight <- function(input_data) {
  nlength = nrow(input_data)
  nwidth = ncol(input_data)
  w = array(0, dim=c(1, nwidth))
  for(i in 1:nwidth) {
    w[i] <- input_data[i] / sum(input_data)
  }
  print("Weight:")
  print(w)
  return(w)
}

data <- tropy.load_data("~/Downloads/Taobao.csv")
new_data <- data[,7:(ncol(data)-1)]
print(new_data)
