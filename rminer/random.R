# 二项分布 100次, 0/1分布, 发生概率为0.5
#rbinom(100, 1, 0.5)

# 泊松分布 100批次, 单位时间(或单位面积)内随机事件的平均发生率0.2
#rpois(100, 0.2)
library('kohonen')

# 设置时间节点数
time_length <- 1000
# 设置条数
c_type <- 500

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

testp <- rbind(c(0, 0.5, 0.8, 0.1, 0.1, 0.4, 0.1, 0, 0, 0, 0.01, 0.02, 0.03, 0.04, 0.1, 0.11, 0.19, 0.018, 0.33, 0.3))

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

random.prepare <- function() {
  credit <<- random.init(random.p, c_type, time_length)
  credit_type <<- random.credit_type

  print(random.credit_type)
  #result <- som(data = credit, grid = somgrid(5, 5, "hexagonal"))
  result <<- xyf(scale(credit), classvec2classmat(credit_type),
                grid = somgrid(5, 5, "hexagonal"), rlen=100)
  write.csv(cbind(result$unit.classif, credit_type, credit), 'random.csv')
  par(mfrow=c(2,2))
  plot(result, type="mapping", labels=credit_type, col=credit_type+1, main="mapping plot")
  plot(result, type="count", main="counts")
  #plot(result, type="mapping", col = credit_type+1, pchs = credit_type, bgcol = bgcols[as.integer(xyfpredictions)], main = "another mapping plot")
  plot(result, type="codes", main = c("Codes X", "Codes Y"))
}


random.prepare()

# 测试预测效果
random.test <- function(ram=random.p, number=50, base_type=random.credit_type) {
  # 构造一条新数据
  n <<- random.init(ram, number, timelength=time_length)
  #p <- predict(result, n)
  Xtest <- scale(n)
  predict_result <<- predict(result, newdata = Xtest)
  pre <<- factor((predict_result$prediction), levels=sort(unique(base_type)))
  confus <<- table(base_type, pre)

  # 输出
  print(random.credit_type)
  print(pre)
  print('--------------------------------------------------')
  print(confus)
  correct <- sum(diag(confus))
  print(correct)
  print(correct/number)

  if(number <= 4) {
    par(mfrow=c(2, number))
  }
  else {
    height <- ceiling(number/4)
    par(mfrow=c(height*2, 4))
  }
  if(number <= 20) {
    for(num in 1:number) {
      plot(n[num,], main=predict_result$predict_result[num])
      for(j in 1:length(credit_type)) {
        if(credit_type[j] == as.integer(predict_result$prediction[num])) {
          plot(credit[j,], main=as.character(credit_type[j]))
          break
        }
      }
    }
  }
}

random.test()
random.test(testp, 30)
#n <- random.init(testp, 10, timelength=time_length)
