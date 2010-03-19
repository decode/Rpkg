# 读取数据
fuzzy.read_input <- function(source_file) {
  data <- read.table(source_file, sep=" ", header=TRUE)
  nlength = nrow(data)
  nwidth = ncol(data)

  return(data)
}

fuzzy.calc <- function(input_matrix) {
  p <- input_matrix

  nlength = nrow(p)
  nwidth = ncol(p)
  if(nlength != nwidth) {
    print("Error, your input is NOT a squere matrix!!")
    return
  }
  new_p <- p
  v <- array(0, c(nlength, nwidth))

  # 构造V矩阵
  for(i in 1:nlength) {
    for(j in 1:nwidth) {
      if((p[i,j] - 0.5) > 0)
        v[i,j] <- 1
      else
        v[i,j] <- 0
    }
  }
  #print("V Matrix: ")
  #print(v)

  # 构造T矩阵
  t <- v %*% v %*% v
  #print("Generate T Matrix")
  #print(t)

  for(i in 1:nlength) {
    # 一致性检验
    if(t[i,i] != 0) {
      print("...Test Failed...")
      new_p <- fuzzy.regenerate(p)
      new_p <- fuzzy.calc(new_p)
      break
    }
  }
  return(new_p)
}

# 重新构造P矩阵
fuzzy.regenerate <- function(input_matrix) {
  p <- input_matrix
  new_p <- p
  nlength = nrow(p)
  nwidth = ncol(p)
  
  r <- array(0, c(nlength, nwidth))
  for(i in 1:nlength) {
    for(j in 1:nwidth) {
      r[i,j] <- 0.5 + (sum(p[i,]) - sum(p[j,])) / nlength 
    }
  }
  #print("R Matrix: ")
  #print(r)
  # 构建C矩阵
  c <- p - r
  #print("C Matrix: ")
  #print(c)

  max_value <- max(abs(c))
  for(i in 1:nlength) {
    for(j in 1:nwidth) {
      # 必须使用round限制小数点位数,否则会出现异常
      if(abs(round(c[i,j],6)) == round(max_value, 6)) {
        new_p[i,j] <- new_p[i,j] - 0.04 * c[i,j]/abs(c[i,j])
      }
    }
  }
  #print("Regenerate P Matrix")
  #print(new_p)
  return(new_p)
}

# 计算排序值向量
fuzzy.weight <- function(input_matrix) {
  p <- input_matrix
  nlength = nrow(p)
  nwidth = ncol(p)
  if(nlength != nwidth) {
    print("Error, your input is NOT a squere matrix!!")
    return
  }
  q <- array(0, c(nlength, nwidth))
  for(i in 1:nlength) {
    for(j in 1:nwidth) {
      if(i == j) {
        q[i,j] <- 2 * ( sum(p[,j]^2) - p[i,j]^2 )
      }
      else {
        q[i,j] <- 2 * (p[i,j]^2 - p[i,j])
      }
    }
  }
  #print("Caculate Q Matrix:")
  #print(q)

  e <- array(1, c(nlength,1))
  w <- t(solve(q,e)) * as.double( 1/(t(e) %*% solve(q, e)) )
  print("W weight:")
  print(w)
}

fuzzy.deal <- function(input_matrix) {
  p <- t(input_matrix)
  print("Original P Matrix: ")
  print(p)
  new_p <- fuzzy.calc(p)
  print("The final P matrix:")
  print(new_p)
  fuzzy.weight(new_p)
}

# 测试方法
'
fuzzy.test <- function() {
  # 测试矩阵p
  p = array(c(0.5,0.1,0.6,0.7, 0.9,0.5,0.8,0.4, 0.4,0.2,0.5,0.9, 0.3,0.6,0.1,0.5), c(4,4))
  #p = array(c(0.5,0.1,0.6,0.7, 0.9,0.5,0.8,0.4, 0.4,0.2,0.5,0.9, 0.3,0.6,0.1,0.5), c(4,4))
  p <- t(p)
  print("------------Test Caculate P Matrix-----------")
  print("Original P Matrix: ")
  print(p)
  p <- fuzzy.calc(p)
  print("The final P matrix:")
  print(p)

  #p <- array(c(0.5,0.1,0.6,0.7, 0.9,0.5,0.8,0.4, 0.4,0.2,0.5,0.9, 0.3,0.6,0.1,0.5), c(4,4))
  #q <- array(c(2.12,-0.18,-0.48,-0.42, -0.18,0.82,-0.32,-0.48, -0.48,-0.32,2.02,-0.18, -0.42,-0.48,-0.18,2.92), c(4,4))
  #p <- t(p)
  #q <- t(q)
  # 若算出的Q与上面输入的q相等则正确
  print("origin p matrix:")
  print(p)
  print("------------Test Caculate Weight-----------")
  fuzzy.weight(p)
}
'
#p <- fuzzy.test()

p <- array(c(0.5,0.3,0.65,0.2,
        0.7,0.5,0.7,0.5,
        0.35,0.3,0.5,0.4,
        0.8,0.5,0.6,0.5), c(4,4))
fuzzy.deal(p)

p <- array(c(0.5,0.3,0.4,
        0.7,0.5,0.6,
        0.6,0.4,0.5), c(3,3))
fuzzy.deal(p)
p <- array(c(0.5,0.8,0.9,0.6,
        0.2,0.5,0.6,0.4,
        0.1,0.4,0.5,0.3,
        0.4,0.6,0.7,0.5), c(4,4))
fuzzy.deal(p)
p = array(c(0.5,0.8,0.8,0.7,0.35,0.6,0.63,
        0.2,0.5,0.75,0.3,0.125,0.4,0.3,
        0.2,0.25,0.5,0.4,0.2,0.375,0.375,
        0.3,0.7,0.6,0.5,0.27,0.5,0.425,
        0.65,0.875,0.8,0.73,0.5,0.6,0.39,
        0.4,0.6,0.625,0.5,0.4,0.5,0.65,
        0.37,0.7,0.625,0.575,0.61,0.35,0.5), c(7,7))
fuzzy.deal(p)
p <- array(c(0.5,0.6,0.7,0.75,
        0.4,0.5,0.68,0.62,
        0.3,0.32,0.5,0.44,
        0.25,0.38,0.56,0.5),c(4,4))
fuzzy.deal(p)
