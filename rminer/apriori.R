source("db.R")

#apriori.init <- function() {
  rminer.dbcon = rminer.connect("tbclawer", "root", "123654")
  result <- rminer.query("SELECT id, name FROM items order by id desc", 40)
  print(result)
  rminer.disconnect
  return(result)
  #}

apriori.prepare <- function(data) {

}

#apriori.init()
