
#' get_dt
#'
#' @param C database connection
#' @param query character, SQL query
#'
#' @return data.table, result of query
#' @export

get_dt <- function(C, query) {
  dt <- odbc::dbGetQuery(C, query)
  dt <- as.data.table(dt)
  return(dt)
}
