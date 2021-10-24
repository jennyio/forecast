#' create_table
#'
#' @param C DB connection
#' @param table character, table name (schema.tablename)
#' @param cols vector, table column names (should be present in data.table you want to write to)
#' @param types vector, Column types of table
#' @export

create_table <- function(C, table, cols, types) {
  
  if(length(cols) == length(types)) {
    # Construct data types and column names
    col.names <- paste(paste(cols, types, sep = ' '), collapse = ',')
  } 
  # Create table
  query <- paste0("CREATE TABLE IF NOT EXISTS ", table, " (", col.names, ")")
  dbSendQuery(C, query)
}

#' write_table
#'
#' @param C 
#' @param write.df data.table to write 
#' @param table character, target table name
#' @param cols vector, column names to write. Must be contained in write.df
#' @param types vector, column types as used in SQL CREATE statement.
#' @param overwrite logical, parameter of dbWriteTable. Delete content of table or not.
#' @description Create table with given columns and column types if it does not exist yet.
#' @return NULL
#' @export

write_table <- function(C, write.df, table, cols, types, overwrite = FALSE) {
  
  if(length(cols) != length(types)) {
    stop(paste0("Number of column names and types do not match."))
  } 
  # Truncates table and inserts new data
  writer <- try(odbc::dbWriteTable(C, name = table, 
                                   value = write.df[, cols, with = FALSE], 
                                   overwrite = overwrite, 
                                   writeCols = cols))
  
  # Creates table if not exists. 
  if ("try-error" %in% class(writer)) {
    if(length(cols) == length(types)) {
      # Construct data types and column names
      col.names <- paste(paste(cols, types, sep = ' '), collapse = ',')
    } 
    
    # Create table
    query <- paste0("CREATE TABLE IF NOT EXISTS ", table, " (", col.names, ")")
    odbc::dbSendQuery(C, query)
    # Try again writing
    odbc::dbWriteTable(C, name = table, 
                       value = write.df[, cols, with = FALSE], 
                       overwrite = overwrite, writeCols = cols)
  } 
  print(paste0("Updated or created table ", table))
  n.rows <- odbc::sqlQuery(C, paste0("SELECT COUNT(*) FROM  ", table, ";"))
  print(paste0('Rows written: ', n.rows))
}