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
    if(age < 0) {
      age <- as.integer(format(Sys.Date(), "%Y")) - as.integer(substr(id, 6, 9))
      birthday <- substr(id, 10, 13)
    }
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
  if(is.na(age) || age < 0) age <- 0
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
    field1 = as.character(data[i, column_name+1])

    ## Process datas
    info <- rminer.exact_form_id(field)
    info <- c(info, rminer.exact_from_date(field1))
    ## End Process

    # Add extracted information to origin dataset
    for(j in 1:length(names(info))) {
      data[i, names(info)[j]] <- info[names(info)[j]]
    }
    #print(data[i,])
    print(i)
  }
  return(data)
}

# Fetch the time of user who become customer
rminer.exact_from_date <- function(date_time) {
  if(nchar(date_time) < 10)
    return
  purchase_time <- as.Date(substr(date_time, 0, 10), "%Y-%m-%d")
  old_days <-  as.integer(difftime (Sys.Date(), purchase_time, units="days"))
  old_months <- (as.integer(format(Sys.Date(), "%Y")) - as.integer(format(purchase_time, "%Y"))) * 12 + as.integer(format(Sys.Date(), "%m")) - as.integer(format(purchase_time, "%m"))
  lst <- list("days" = old_days, "months" = old_months)
  #print(old_days)
  #print(old_months)
  return(lst)
}

rminer.clean_invalid_num <- function(data, column_name) {
  for(i in 1:nrow(data)) {
    if(is.na(data[i, column_name]) || as.integer(data[i, column_name]) < 0)
      data[i, column_name] <- 0
    print(i)
  }
  return(data)
}

rminer.clean_invalid_str <- function(data, column_name) {
  for(i in 1:nrow(data)) {
    if(is.na(data[i, column_name]))
      data[i, column_name] <- ""
    print(i)
  }
  return(data)
}

data <- rminer.load_csv('new.csv')
result <- rminer.exact(data, 5)
#result <- rminer.clean_invalid_num(data, 45)
#result <- rminer.clean_invalid_str(data, 46)
write.csv(result, file="new1.csv")

