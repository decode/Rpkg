# An util get information form id, like birthday, gender, area
rminer.exact_form_id <- function(id) {

}

rminer.load_csv  <- function(filename) {
  data <- read.csv(filename)
  return(data)
}

rminer.exact <- function(data, column_name) {
  for(i in 1:nrow(data)) {
    field = data[i, column_name]
    rminer.exact_form_id(field)
  }
}

data <- rminer.load_csv('~/Downloads/XJG.csv')
a <- as.character(data[1,4])
