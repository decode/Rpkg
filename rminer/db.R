library(RMySQL)

rminer.connect <- function(database, username, passwd) {
  rminer.dbcon <- dbConnect(MySQL(), dbname = database, user = username, password = passwd) 
  dbListTables(rminer.dbcon)
  return(rminer.dbcon)
}

rminer.disconnect <- function() {
  dbDisconnect(rminer.dbcon)
}

# 数据库查询
rminer.query <- function(sql, limit=-1) {
  rs <- dbSendQuery(rminer.dbcon, sql)
  data <- fetch(rs, n = limit)
  return(data)
}

# 预处理查询数据
rminer.pre_data <- function(source) {
  result <- source
  for(i in 1:nrow(source)) {
    # 将卖家注册日期变为距今天数
    print(source[i,26])
    day <- as.integer(difftime(Sys.Date(), as.Date(source[i,26]), units="days"))
    result[i, 26] <- day
  }
  return(result)
}

# 数据库连接
rminer.dbcon = rminer.connect("tbclawer", "root", "123654")

# 查询Account Users表
result <- rminer.query("SELECT u.name, u.place,
    a.buyer_rate, a.seller_rate, a.sim_trade, a.good_trade, 
    a.s1, a.s2, a.s3, a.s1_count, a.s2_count, a.s3_count,
    a.s_6month_good, a.s_6month_normal, a.s_6month_bad, 
    a.b_6month_good, a.b_6month_normal, a.b_6month_bad, 
    a.s_month6_good, a.s_month6_normal, a.s_month6_bad, 
    a.b_month6_good, a.b_month6_normal, a.b_month6_bad,
    favourate, register_time 
    FROM users u RIGHT OUTER JOIN accounts a 
    ON u.id = a.user_id", 10)

result <- rminer.pre_data(result)
write.csv(result, file="result.csv")

print(result)
print(ncol(result))
print(format(Sys.Date(), "%d"))
