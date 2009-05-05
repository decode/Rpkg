# Function Name: 数据标准化
# Author:
# Date: 2009/05/03
# Version: 0.1
ahptropy.base_transform <- function(source_file, precision=4) {
  #读取数据
  data <- read.table(source_file, sep=" ", header=TRUE)
  nlength = nrow(data)
  nwidth = ncol(data)

  #创建输出数组
  target <- array(0, dim=c(nlength, 5))

  for (i in 1:nlength) {
    a1 = data[i,1]
    b1 = (a1 + data[i,2]) / 2
    c1 = (data[i,2] + data[i,3]) /2
    d1 = data[i,3]
    d = data[i,4]

    #计算标准差
    std = sd( c(a1,b1,c1,d1) )

    target[i,1] = round(a1/std, precision)
    target[i,2] = round(b1/std, precision)
    target[i,3] = round(c1/std, precision)
    target[i,4] = round(d1/std, precision)
    target[i,5] = round(d/std,  precision)
  }
  print(target)
  return(target)
  #输出到文件
  #write.table(target, file="target.txt", eol="\n")
}

ahptropy.step_load <- function(step, source) {
  nlength <- nrow(source)
  if (nlength %% step != 0) return
  nwidth <- ncol(source)
  data <- array(0, dim=c(nlength*nwidth/step, step))

  count <- 0
  for(j in 1:nwidth) {
    for(i in 1:nlength) {
      cur <- ifelse(i%%step==0, step, i%%step)
      count <- ifelse(cur == 1, count + 1, count)
      data[count,cur] <- source[i,j]
    }
  }
  print("Converted Data: ")
  print(data)
  return(data)
}

# Function Name: Entropy熵权法求值
# Author:
# Date: 2009/05/03
# Version: 0.2
ahptropy.entropy <- function(target, step, precision=4) {
  data <- ahptropy.step_load(step, target)
  nlength = nrow(data)
  nwidth = ncol(data)

  compare = nrow(target) / step
  entropy_result <- array(0, dim=c(nlength/compare, nwidth))
  
  for(i in 1:(nlength/compare)) {
    temp_data <- array(0, dim=c(compare, nwidth))
    if(compare == 1) {
      temp_data[compare,] <- data[i,]
    }
    if(compare != 1) {
      # Bugs here...
      #temp_data <- rbind(data[compare*i-1,], data[compare*i,])
      # Fixed it but not test
      temp_data <- data[(compare*(i-1)+1):(compare*i),]
    }
    print("After combine:")
    print(temp_data)
    entropy_result[i,] <- ahptropy.entropy_compute(temp_data, precision)
  }
  
  print("Entropy Data: ")
  print(entropy_result)

  return(entropy_result)
  # 输出到文件
  #write.table(entropy_result, file="entropy_target.txt")
}

ahptropy.entropy_compute <- function(data, precision=4) {
  nlength = nrow(data)
  nwidth = ncol(data)

  constK <- 1 / log(nwidth)
  k <- array(0, dim=c(nlength, nwidth))
  entropy_result <- array(0, dim=c(1, nwidth))

  for (j in 1:nwidth) {
    sumK <- sum(data[,j])
    for (i in 1:nlength) {
      k[i,j] <- data[i,j] / sumK
      k[i,j] <- k[i,j] * log(k[i,j])
    }
    entropy_result[j] <- -constK * sum(k[,j])
  }

  total <- sum(entropy_result)
  for(i in 1:nwidth) {
    entropy_result[i] <- round((1 - entropy_result[i]) / (nwidth - total), precision)
  }
  return(entropy_result)
}

# Name: 计算组合权重
# Author:
# Date: 2009/05/03
# Version: 0.1
ahptropy.combination <- function(source_file, entropy_result, precision=4) {
  #读取ahp法数据,一般只有一行
  normal_data <- read.table(source_file, sep=" ", header=TRUE)
  if (ncol(normal_data) != ncol(entropy_result)) {
    print("Data input error")
    return
  }
  
  data <- array(0, dim=c(nrow(normal_data), ncol(normal_data)))
  for(i in 1:ncol(normal_data)) {
    data[1,i] <- normal_data[1,i]
  }
  print("Readed data from ahp: ")
  print(data)
  
  result_data <- array(0, dim=c(nrow(entropy_result), ncol(entropy_result)))
  for(i in 1:nrow(result_data)) {
    data <- rbind(data, entropy_result[i,])

    nlength = nrow(data)
    nwidth = ncol(data)

    result <- array(0, dim=c(1,nwidth))
    total <- 0

    for(j in 1:nwidth) {
      result[1,j] <- prod(data[,j])
    }
    total <- sum(result[1,])
    for(j in 1:nwidth) {
      result[1,j] <- round(prod(data[,j]) / total, precision)
    }
    result_data[i,] <- result[1,]
  }
  print("Final Combination Result: ")
  print(result_data)
  return(result_data)
  # 输出到文件
  # write.table(result_data, file="comb_target.txt")
}

# Function Name: AHP/熵权法复合求权重
# Author:
# Date: 20090505
# Version: 0.1
# Params:
#    source_file: 待计算的文件
#    normal_file: AHP法取得的权重值文件
#    step:        待计算的文件中的指标数
#    precision:   保留的数据精度(小数点位数),默认为4位
#    writable:    是否输出到文件,默认不输出
ahptropy.caculate <- function(source_file, normal_file, step, precision=4, writable=FALSE) {
  base_data <- ahptropy.base_transform(source_file, precision)
  entropy_result <- ahptropy.entropy(base_data, step, precision)
  result <- ahptropy.combination(normal_file, entropy_result, precision)
  if (writable == TRUE) {
    write.table(result, file="ahptropy_result.txt")
  }
}

ahptropy.caculate("source.txt", "standard.txt", 6)