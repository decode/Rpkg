library(RMySQL)
rminer.dbcon

rminer.connect <- function(database, username, passwd) {
  rminer.dbcon <- dbConnect(MySQL(), dbname = database, user = username, password = passwd) 
  dbListTables(rminer.dbcon)
  return(rminer.dbcon)
}

rminer.disconnect <- function() {
  dbDisconnect(rminer.dbcon)
}

rminer.query(sql) <- function() {
  conn <- rminer.dbcon
  rs <- dbSendQuery(conn, sql)
  data <- fetch(rs, n = -1)
}
