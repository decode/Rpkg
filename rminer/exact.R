# An util get information form id, like birthday, gender, area
rminer.exact_form_id <- function(id) {
  if(nchar(id) != 18 && nchar(id) != 15)
    return
  district <- substr(id, 0, 6)
  # ID digital is 18 like: xxxxxx    xxxxxxxx   xxx    x
  #                       (district)(birthday)(gender)(crc code)
  if(nchar(id) == 18) {
    age <- as.integer(format(Sys.Date(), "%Y")) - as.integer(substr(id, 7, 10))
    #birthday <- as.Date(substr(id, 7, 14), "%Y%m%d")
    birthday <- substr(id, 11, 14)
    gender <- as.integer(substr(id, 15, 17)) %% 2
  }
  # ID digital is 15 like: xxxxxx    xxxxxx    xxx
  #                       (district)(birthday)(gender)
  else {
    age <- as.integer(format(Sys.Date(), "%y")) - as.integer(substr(id, 7, 8))
    if(!is.na(age) && age < 0) {
      age <- age + 100
    }
    #birthday <- as.Date(substr(id, 7, 12), "%Y%m%d")
    birthday <- substr(id, 9, 12)
    gender <- as.integer(substr(id, 13, 15)) %% 2
  }
  lst <- list("district" = district, "age" = age, "birthday" = birthday, "gender" = gender)
  return(lst)
}

rminer.load_csv <- function(filename) {
  data <- read.csv(filename)
  return(data)
}

rminer.exact <- function(data, column_name) {
  for(i in 1:nrow(data)) {
    if(is.na(data[i, column_name]))
      next
    field = as.character(data[i, column_name])
    #info <- rminer.exact_form_id(field)
    info <- rminer.exact_from_date(field)
    # Add extracted information to origin dataset
    for(j in 1:length(names(info))) {
      data[i, names(info)[j]] <- info[names(info)[j]]
    }
    print(data[i,])
  }
  return(data)
}

data <- rminer.load_csv('~/Downloads/XJG.csv')
result <- rminer.exact(data, 4)
write.csv(result, file="new.csv")

# Fetch the time of user who become customer
rminer.exact_from_date <- function(date_time) {
  if(nchar(date_time) < 10)
    return
  purchase_time <- as.Date(substr(date_time, 0, 9), "%Y-%m-%d")
  old_days <-  as.integer(difftime (Sys.Date(), a, units="days"))
  old_months <- (as.integer(format(Sys.Date(), "%Y")) - as.integer(format(purchase_time, "%Y"))) * 12 + as.integer(format(Sys.Date(), "%m")) - as.integer(format(purchase_time, "%m"))
  lst <- list("days" = old_days, "months" = old_months)
  return(lst)
}
