library(RMySQL)

rminer.connect <- function(database, username, passwd) {
  #rminer.dbcon <- dbConnect(MySQL(), dbname = database, user = username, password = passwd) 
  #dbListTables(rminer.dbcon)
  #return(rminer.dbcon)
  return(dbConnect(MySQL(), dbname = database, user = username, password = passwd))
}

rminer.disconnect <- function(dbcon) {
  dbDisconnect(dbcon)
}

# 数据库查询
rminer.query <- function(dbcon, sql, limit=-1) {
  rs <- dbSendQuery(dbcon, sql)
  data <- fetch(rs, n = limit)
  return(data)
}

# 预处理查询数据
rminer.pre_data <- function(source, col) {
  result <- source
  for(i in 1:nrow(source)) {
    # 将卖家注册日期变为距今天数
    day <- as.integer(difftime(Sys.Date(), as.Date(source[i,col]), units="days"))
    result[i, col] <- day
  }
  return(result)
}

rminer.test <- function() {
  # 数据库连接
  dbcon = rminer.connect("tbclawer", "root", "jjhome")
  # 查询Account Users表
  #result <- rminer.query(dbcon, "SELECT u.name, u.place, u.shop_url,
  #a.buyer_rate, a.seller_rate, a.sim_trade, a.good_trade, 
  #a.s1, a.s2, a.s3, a.s1_count, a.s2_count, a.s3_count,
  #a.r1, a.r2, a.r3, a.r4, a.r5, a.r6, a.r7, a.r8, 
  #a.r9, a.r10, a.r11, a.r12, 
  #a.c1, a.c2, a.c3,
  #a.favourate, a.amount, a.register_time
  #FROM users u RIGHT OUTER JOIN accounts a 
  #ON u.id = a.user_id where register_time != 0 and amount > 0", 60)

  result <- rminer.query(dbcon, "SELECT u.*, a.* 
      FROM users u RIGHT OUTER JOIN accounts a 
      ON u.id = a.user_id where register_time != 0 and amount > 0 and s1 > 0", 100)

      #a.r13, a.r14, a.r15, a.r16, a.r17, a.r18, a.r19, a.r20, a.r21, a.r22, a.r23, a.r24, 
      #print(result)
      result <- rminer.pre_data(result, 12)
      result <- rminer.pre_data(result, 46)
      result <- rminer.pre_data(result, 47)
      result <- rminer.pre_data(result, 48)
      write.csv(result, file="result.csv")

      print(result)
      #print(ncol(result))
      print(names(result))
      #print(format(Sys.Date(), "%d"))
      #rminer.disconnect(dbcon)
    }
    rminer.test()
