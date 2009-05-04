

# Step 1
# Name: 数据标准化
# Author:
# Date: 2009/05/03
# Version: 0.1
base_transform <- function(source_file) {
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

    target[i,1] = a1 / std
    target[i,2] = b1 / std
    target[i,3] = c1 / std
    target[i,4] = d1 / std
    target[i,5] = d  / std
  }
  print(target)
  target
  #输出到文件
  #write.table(target, file="target.txt", eol="\n")
}

step_load <- function(step, source) {
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
  data
}

# Step 2
# Name: Entropy
# Author:
# Date: 2009/05/03
# Version: 0.2
entropy <- function(target, step) {
  data <- step_load(step, target)
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
      temp_data <- rbind(data[compare*i-1,], data[compare*i,])
    }
    print("After combine:")
    print(temp_data)
    entropy_result[i,] <- entropy_compute(temp_data)
  }
  
  print("Entropy Data: ")
  print(entropy_result)

  entropy_result
  # 输出到文件
  #write.table(entropy_result, file="entropy_target.txt")
}

entropy_compute <- function(data) {
  nlength = nrow(data)
  nwidth = ncol(data)

  constK <- 1 / log(nwidth)
  k <- array(0, dim=c(nlength, nwidth))
  entropy_result <- array(0, dim=c(1, nwidth))

  for (j in 1:nwidth) {
    #sumK <- 0
    # 计算所有列的值
    #for (i in 1:nlength) {
      #k[i,j] <- data[i,j]
      #sumK <- sumK + data[i,j]
    #}
    sumK <- sum(data[,j])
    for (i in 1:nlength) {
      #k[i,j] <- k[i,j] / sumK
      k[i,j] <- data[i,j] / sumK
      k[i,j] <- k[i,j] * log(k[i,j])
    }
    entropy_result[j] <- -constK * sum(k[,j])
  }

  total <- sum(entropy_result)
  for(i in 1:nwidth) {
    entropy_result[i] <- (1 - entropy_result[i]) / (nwidth - total)
  }
  entropy_result
}

# Name: Combination
# Author:
# Date: 2009/05/03
# Version: 0.1
combination <- function(source_file, entropy_result) {
  #读取数据
  normal_data <- read.table(source_file, sep=" ", header=TRUE)
  if (ncol(normal_data) != ncol(entropy_result)) {
    print("Data input error")
    return
  }

  result_data <- array(0, dim=c(nrow(entropy_result), ncol(entropy_result)))
  
  data <- array(0, dim=c(nrow(normal_data), ncol(normal_data)))
  for(i in 1:ncol(normal_data)) {
    data[1,i] <- normal_data[1,i]
  }
  print("Readed data: ")
  print(data)

  for(i in 1:nrow(result_data)) {
    data <- rbind(data, entropy_result[i,])

    nlength = nrow(data)
    nwidth = ncol(data)
    #comp <- array(0, dim=c(nlength, nwidth))
    result <- array(0, dim=c(1,nwidth))
    #debug <- array(0, dim=c(nwidth))
    total <- 0

    for(j in 1:nwidth) {
      result[1,j] <- prod(data[,j])
      #debug[j] <- prod(data[,j])
    }
    total <- sum(result[1,])
    for(j in 1:nwidth) {
      # Only reserve 8 digits number
      result[1,j] <- round(prod(data[,j]) / total, 8)
    }
    print("result: ")
    print(result)
    result_data[i,] <- result[1,]
  }
  print("Final Combination Result: ")
  print(result_data)
  
  # 输出到文件
  write.table(result_data, file="comb_target.txt")
}

ahptropy <- function(source_file, normal_file, step) {
  base_data <- base_transform(source_file)
  entropy_result <- entropy(base_data, 6)
  combination(normal_file, entropy_result)
}

ahptropy("source.txt", "standard.txt", 6)
