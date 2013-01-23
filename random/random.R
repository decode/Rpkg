# 写数据到文件
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
  return(res)
}

# 均值在3之间,数据差异较大
random.gen(160, 2, 3.5, 1.3)

# 均值在5之间,数据差异小,生成5附近的数
random.gen(160, 2, 5, 0.8)

# 根据概率生成随机数, l:要生成的数据选项, p:对应生成的概率
random.gen2 <- function(number, col, l, p) {
  res <- array(0, dim=c(number, col))
  for(i in 1:col) {
    s <- sample(l, number, re=T, prob=p)
    res[,i] <- s
    print(paste("Average:", sum(s)/number))
    print(paste("SD:", sd(res[,i])))
  }
  random.writeFile(res)
  return(res)
}

l <- c(1,2,3,4,5)
# 5
p5 <- c(0, 0.05, 0.05, 0.3, 0.6)
p4 <- c(0, 0.1, 0.2, 0.6, 0.1)
p3 <- c(0.05, 0.1, 0.7, 0.1, 0.05)
p2 <- c(0.2, 0.6, 0.1, 0.1, 0)
p1 <- c(0.6, 0.3, 0.05, 0.05, 0)

res <- random.gen2(205, 13, l, p5)
res <- random.gen2(205, 11, l, p4)
res <- random.gen2(205, 16, l, p3)
res <- random.gen2(205, 8, l, p2)
res <- random.gen2(205, 5, l, p1)

la <- c(1,5)
pa <- c(0.2,0.8)
res <- random.gen2(205, 1, la, pa)

p3 <- c(0.05, 0.6, 0.1, 0.1, 0.15)
res <- random.gen2(205, 1, l, p3)

p2 <- c(0.05, 0.6, 0.15, 0.2, 0)
res <- random.gen2(205, 1, l, p2)
