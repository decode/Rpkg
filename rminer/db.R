library(RMySQL)

rminer.connect <- function(database, username, passwd) {
  rminer.dbcon <- dbConnect(MySQL(), dbname = database, user = username, password = passwd) 
  dbListTables(rminer.dbcon)
  return(rminer.dbcon)
}

rminer.disconnect <- function() {
  dbDisconnect(rminer.dbcon)
}

rminer.query <- function(sql, limit=-1) {
  rs <- dbSendQuery(rminer.dbcon, sql)
  data <- fetch(rs, n = limit)
  return(data)
}

rminer.dbcon = rminer.connect("tbclawer", "root", "123654")
#Account Data
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

write.csv(result, file="result.csv")

#age <- as.integer(format(Sys.Date(), "%d")) - as.integer(substr(id, 7, 8))
print(result)
print(ncol(result))
print(format(Sys.Date(), "%d"))
