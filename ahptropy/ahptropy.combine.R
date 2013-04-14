# Function Name: 数据标准化
# Author:
# Date: 2009/05/03
# Version: 0.1
# 2013.4.12: 存在Bug,如果只有一年的数据计算会出错，必须至少两年的数据 in ahptropy.entropy, 补充line-109
ahptropy.base_transform <- function(source_file, precision=4, prepare=TRUE) {
  #读取数据
  data <- read.table(source_file, sep=" ", header=TRUE)
  nlength = nrow(data)
  nwidth = ncol(data)

  #创建输出数组
  target <- array(0, dim=c(nlength, 4))

  for (i in 1:nlength) {
    a1 = data[i,1]
    b1 = data[i,2]
    c1 = data[i,3]
    d  = data[i,4]

    #计算标准差
    std = sd( c(a1,b1,c1) )

    print(round(scale(a1, F), precision))
    target[i,1] = round(scale(a1, F), precision)
    target[i,2] = round(scale(b1, F), precision)
    target[i,3] = round(scale(c1, F), precision)
    target[i,4] = round(scale(d, F),  precision)
  }
  print('Standard matrix:')
  print(target)
  return(target)
  #输出到文件
  #write.table(target, file="target.txt", eol="\n")
}

ahptropy.step_load <- function(step, source) {
  nlength <- nrow(source)
  if (nlength %% step != 0) {
    print("dataset row%%indicator!=0")
    return
  }
  nwidth <- ncol(source)
  data <- array(0, dim=c(nlength*nwidth/step, step))
  count <- 0
  cur <- 0
  # 将几年的数据输入重新排列,将指标的数据转到一列存储,若只有一年的数据,则直接为矩阵转置
  for(j in 1:nwidth) {
    for(i in 1:nlength) {
      cur <- ifelse(cur%%step==0, 1, cur + 1)
      count <- ifelse(cur == 1, count + 1, count)
      data[count,cur] <- source[i,j]
    }
  }
  print("Converted Data: ")
  print(data)
  return(data)
}

ahptropy.ahp_data <- array(0, dim=c(0,0))

# Function Name: Entropy熵权法求值
# Author:
# Date: 2009/05/03
# Version: 0.2
ahptropy.entropy <- function(target, step, precision=4) {
  data <- ahptropy.step_load(step, target)
  nlength = nrow(data)
  nwidth = ncol(data)

  compare = nrow(target) / step
  resort_table <- array(0, dim=c(nlength, nwidth))
  data_height <- nlength/compare

  # re-sort the table from year to year
  for(i in 1:compare) {
    sub_sequence <- seq(i, nlength, by=compare)
    dest_sequence <- seq((i-1)*data_height+1, i*data_height)
    resort_table[dest_sequence,] <- data[sub_sequence,]
  }
  # Store in global varity
  ahptropy.ahp_data <<- resort_table
  
  # =========== For Debug only ============ #
  #print("Re-sort table: ")
  #print(resort_table)

  year_result <- array(0, dim=c(data_height, nwidth))  
  entropy_result <- array(0, dim=c(nlength, nwidth))

  if(compare > 1) { # 多于一年的计算方法
    for(sub_compare in 1:compare) {
      for(i in 1:data_height) {
        temp_data <- array(0, dim=c(sub_compare, nwidth))
        print(temp_data)
        if (sub_compare == 1) {
          #temp_data[sub_compare,] <- data[(i-1)*sub_compare+1,]
          temp_data <- data[((sub_compare+1)*(i-1)+1):((sub_compare+1)*i),] #结果如1:2, 3:4, 5:6, 7:8
        }
        if (sub_compare != 1) {
          temp_data <- data[(sub_compare*(i-1)+1):(sub_compare*i),]
        }
        # =========== For Debug only ============ #
        #print("After combine:")
        #print(temp_data)
        year_result[i,] <- ahptropy.entropy_compute(temp_data, precision)
      }
      entropy_result[(data_height*(sub_compare-1)+1):(data_height*sub_compare),] <- year_result
    }
  }
  else {
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
    temp_data <- rbind(data, entropy_result[i,])

    nlength = nrow(temp_data)
    nwidth = ncol(temp_data)

    result <- array(0, dim=c(1,nwidth))
    total <- 0

    for(j in 1:nwidth) {
      result[1,j] <- prod(temp_data[,j])
    }
    total <- sum(result[1,])
    for(j in 1:nwidth) {
      result[1,j] <- round(prod(temp_data[,j]) / total, precision)
    }
    result_data[i,] <- result[1,]
  }
  print("Final Combination Result: ")
  print(result_data)
  return(result_data)
  # 输出到文件
  # write.table(result_data, file="comb_target.txt")
}

ahptropy.last <- function(source_data, combine_data, precision=4) {
  temp <- source_data*combine_data
  print("The result after ahp * combination weight:")
  print(temp)
  result <- array(0, dim=c(nrow(temp), 1))
  for(i in 1:nrow(temp)) {
    result[i,] <- round(sum(temp[i,]), precision)
  }
  #print("Last Result: ")
  #print(result)
  return(result)
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
  print("The standard value: ")
  print(ahptropy.ahp_data)
  last_result <- ahptropy.last(ahptropy.ahp_data, result, precision)
  return(last_result)
}

ahptropy.convert <- function(source_data, length) {
  nlength <- nrow(source_data)
  nwidth <- ncol(source_data)
  target <- array(0, dim=c(nwidth,nlength))
  for(i in 1:nwidth) {
    target[i,] <- source_data[,i]
  }
  return(target)
}

ahptropy.re_caculate <- function(source_file, normal_file, step, precision=4, writable=FALSE) {
  base_data <- read.table(source_file, sep=" ", header=TRUE)
  entropy_result <- ahptropy.entropy(base_data, step, precision)
  result <- ahptropy.combination(normal_file, entropy_result, precision)
  if (writable == TRUE) {
    write.table(result, file="ahptropy_result.txt")
  }
  print("The ahp result: ")
  print(ahptropy.ahp_data)

  last_result <- ahptropy.last(ahptropy.ahp_data, result, precision)
  return(last_result)
}

ahptropy.caculate("source.txt", "standard.txt", 6)
ahptropy.caculate("source.txt", "standard.txt", 4)
