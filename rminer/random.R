# 二项分布 100次, 0/1分布, 发生概率为0.5
#rbinom(100, 1, 0.5)

# 泊松分布 100批次, 单位时间(或单位面积)内随机事件的平均发生率0.2
#rpois(100, 0.2)

# 设置时间节点数
time_length <- 1000
# 设置条数
c_type <- 100

random.credit <- array(0, dim=c(c_type, time_length+1))
random.credit_type <- seq(0, 0, length=c_type)

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
random.length = ncol(random.p)
random.height = nrow(random.p)

# 生成概率类型序列
random.generate_prob <- function(number) {
  #p_type <- rbinom(1, number, 0.5)
  p_type <- round(runif(1, 1, number))
  #prob = random.p[p_type, ]
  return(p_type)
}

# 初始化数据
random.init <- function() {
  step = ceiling(time_length/random.length)

  for(s_type in 1:c_type) {
    p = random.generate_prob(random.height)
    random.credit_type[s_type] <<- p
    random.credit[s_type, 1] <<- p
    prob = random.p[p,]

    credit_sum = 0
    for(f in 1:time_length) {
      jump = ceiling(f/step)
      credit_sum <- credit_sum + rpois(1, 20*prob[jump])
      random.credit[s_type, f+1] <<- credit_sum 
    }
  }
  return(random.credit)
}

credit <- random.init()

print(random.credit_type)
write.csv(credit, 'random.csv')

