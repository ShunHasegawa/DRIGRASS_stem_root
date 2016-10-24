# compute SE
se <- function(...) ci(...)[4]




# compute number of observations
get_n <- function(x) sum(!is.na(x))

