# 二项分布 100次, 0/1分布, 发生概率为0.5
#rbinom(100, 1, 0.5)

# 泊松分布 100批次, 单位时间(或单位面积)内随机事件的平均发生率0.2
#rpois(100, 0.2)
library('kohonen')

# 设置时间节点数
time_length <- 1000
# 设置条数
c_type <- 100

random.credit_type <- seq(0, 0, length=c_type)

# 概率数据集合
random.p <- rbind(
           # 大范围提高
           c(0.01, 0.1, 0.7, 0.6, 0.5, 0.3, 0.3, 0.3, 0.3, 0.3),
           c(0.01, 0.1, 0.7, 0.6, 0.8, 0.7, 0.5, 0.3, 0.5, 0.4),
           # 平缓增长
           c(0.1, 0.1, 0.2, 0.2, 0.4, 0.3, 0.4, 0.4, 0.41, 0.45),
           c(0.01, 0.05, 0.1, 0.1, 0.3, 0.35, 0.4, 0.5, 0.5, 0.45),
           # 平淡
           c(0.1, 0.01, 0.1, 0.2, 0.2, 0.1, 0.2, 0.25, 0.22, 0.21),
           c(0.1, 0.01, 0.1, 0.2, 0.1, 0.3, 0.1, 0.18, 0.14, 0.15),
           # 高位平淡
           c(0.1, 0.3, 0.4, 0.3, 0.3, 0.4, 0.4, 0.5, 0.4, 0.5),
           c(0.01, 0.3, 0.2, 0.2, 0.4, 0.2, 0.3, 0.4, 0.3, 0.28),
           # 先高后低
           c(0.1, 0.4, 0.5, 0.6, 0.5, 0.1, 0.1, 0.2, 0.1, 0.2),
           c(0.3, 0.3, 0.6, 0.6, 0.6, 0.1, 0.1, 0.11, 0.2, 0.15)
           )

# 生成概率类型序列
random.generate_prob <- function(number) {
  p_type <- round(runif(1, 1, number))
  return(p_type)
}

# 初始化数据
# number - 数据条数
# timelength - 时间节点数
random.init <- function(plist, number=100, timelength=1000) {
  height = nrow(plist)
  length = ncol(plist)
  step = ceiling(timelength/length)

  credit <- array(0, dim=c(number, timelength))
  random.credit_type <<- seq(0, 0, length=number)
  
  for(s_type in 1:number) {
    p = random.generate_prob(height)
    random.credit_type[s_type] <<- p
    prob = plist[p,]

    credit_sum = 0
    for(f in 1:timelength) {
      jump = ceiling(f/step)
      credit_sum <- credit_sum + rpois(1, 10*prob[jump])
      credit[s_type, f] <- credit_sum 
    }
  }
  print(random.credit_type)
  return(credit)
}

credit <- random.init(random.p, c_type, time_length)
print(random.credit_type)
result <- som(data = credit, grid = somgrid(5, 5, "hexagonal"))
write.csv(cbind(result$unit.classif, random.credit_type, credit), 'random.csv')

# 构造一条新数据
n <- random.init(random.p, number=1, timelength=time_length)
#p <- predict(result, n)
