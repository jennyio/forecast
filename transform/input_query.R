#' input_query
#'
#' @param begin.curr.week character, Monday of current forecast week, format YYYY-mm-dd
#' @param input.view character, name of database table/view containing training input data
#'
#' @return character
#' @export

input_query <- function(begin.curr.week, input.view) {
  
  query <- paste0("SELECT i.*
                  
                  FROM ", input.view, " i
                  
                  WHERE 
                  DAYS_BETWEEN(CAST('", begin.curr.week, "' AS DATE), i.LAST_STOCK_WEEK_START) < 370)")
  return(query)
}