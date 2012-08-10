random.writeFile <- function(data) {
  write.csv(data, '~/develop/git/Rpkg/random/list.csv')
}

# 根据均值和方差生成数据,number为行数,col为列数,m为均值,s为方差
random.gen <- function(number, col, m, s) {
  res <- array(0, dim=c(number, col))
  for(i in 1:col) {
    l <- as.integer(rnorm(160, mean=m, sd=s))
    print(paste("average:", sum(l/number)))
    res[,i] <- l
  }
  random.writeFile(res)
}

# 均值在3之间,数据差异较大
random.gen(160, 2, 3.5, 1.3)

# 均值在5之间,数据差异小,生成5附近的数
random.gen(160, 2, 5, 0.8)

# todo
# 生成单侧随机数
