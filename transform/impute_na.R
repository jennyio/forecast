#' impute_na
#'
#' @param x numeric vector, with NA values
#' @description Impute missing values by 'next observation carried backwards' 
#' @return
#' @export

impute_na <- function(x) {
  
  # nobc = next observation carried backwards
  ret <- try(imputeTS::na_locf(x, option = 'nocb'), silent = T)
  if('try-error' %in% class(ret)) {
    ret <- rep(NA_real_, length(x))
  }
  return(ret)
}
