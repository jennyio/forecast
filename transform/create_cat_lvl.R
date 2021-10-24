#' create_cat_lvl
#'
#' @param df data.table including columns cat.lvl1, cat.lvl2
#' @param cat.lvl1 character, column name for categorial variable 1
#' @param cat.lvl2 character, column name for categorial variable 2
#' @description Create column for fore casting clusters based on categorial variables 1 and 2
#' @return data.table including column CATEGORY_LEVEL
#' @export

create_cat_lvl <- function(df, cat.lvl1, cat.lvl2) {
  df[, CATEGORY_LEVEL := paste0(get(cat.lvl1), '_', get(cat.lvl2))]
  return(df)
}