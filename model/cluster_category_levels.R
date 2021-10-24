
#' cluster_category_levels
#'
#' @param df data.table, one row per week, category_1/category_2. 
#' Contains columns QTY, NUM_INSTOCK_ITEMS, BEGIN_OF_WEEK
#' @param DO.RECLUSTER logical, compute new clustering
#' @param cat.lvl character, column name of categorical variable 
#' @param n.cl int, number seasonal clusters
#'
#' @return list with cluster names and which category levels belong to the resp. cluster
#' @export

cluster_category_levels <- function(df, 
                                    DO.RECLUSTER, 
                                    cat.lvl = "CATEGORY_LEVEL", 
                                    n.cl = 8) {
  
  # Group data by week and category level 
  df.agg <- df[, list(QTY_CAPPED = sum(QTY_CAPPED), 
                      NUM_INSTOCK_ITEMS = sum(NUM_INSTOCK_ITEMS)), 
               by = c("BEGIN_OF_WEEK", cat.lvl)]
  
  # Add dates
  unique.dates <- unique(df.agg[, BEGIN_OF_WEEK])
  df.dates <- data.frame(BEGIN_OF_WEEK = unique.dates, stringsAsFactors = F)
  
  # Cross product of dates and categories
  df.agg <- merge(df.dates, df.agg, 
                  all.x = T, all.y = F, by = c(cat.lvl, 'BEGIN_OF_WEEK'))
  df.agg[is.na(df.agg)] <- 0
  
  df.agg <- df.agg[order(get(cat.lvl), BEGIN_OF_WEEK), ]
  
  # Start clustering or use last clustering results
  if (DO.RECLUSTER) {
    
    # Clustering based on log(avg qty sold per in-stock item).
    df.agg[, LOG_TARGET_NORM := log((QTY /  NUM_INSTOCK_ITEMS) + 1)]
    df.agg[is.na(LOG_TARGET_NORM), LOG_TARGET_NORM := 0]
    
    # Reshape data to contain category levels in columns and weeks in rows
    df.shaped <- reshape(df.agg[, c('BEGIN_OF_WEEK', 'LOG_TARGET_NORM', cat.lvl), with = FALSE],
                         idvar = 'BEGIN_OF_WEEK',
                         timevar = cat.lvl,
                         direction = 'wide') 
    df.shaped <- df.shaped[order(BEGIN_OF_WEEK),]
    df.shaped[is.na(df.shaped)] <- 0
    
    # Standardize columns 
    dt_scaled <- df.shaped[, !"BEGIN_OF_WEEK", with = F]
    cols <- names(dt_scaled)
    dt_scaled[, (cols) := lapply(.SD, scale), .SDcols = cols]
    
    # Dissimilarity between category levels besed on correlation of the time series
    dissimilarity <- TSclust::diss(dt_scaled, 'COR')
    dissimilarity[is.na(dissimilarity)] <- 0
    
    # Hierarchical clustering
    h_cl <- hclust(dissimilarity)
    plot(h_cl)
    
    # Cut of n.cl clusters
    cut_hcl <- cutree(h_cl, k=n.cl)
    # table(cut_hcl)
    
    seas.groups <- list()
    
    # Get category levels which belong to each cluster
    for (i in 1:n.cl) {
      
      # Names of each category level cluster
      cluster_id <- names(cut_hcl[cut_hcl==i])
      splt <- strsplit(cluster_id, '\\.')
      ids <- unlist(splt)[2 * 1:length(cluster_id)]
      
      # Cluster name
      n <- paste0('SEAS_CLUSTER', i)
      seas.groups[[n]] <- ids
      
      # Print categories in cluster
      print(sort(ids))
    }
  } else {seas.groups <- NULL}
  return(seas.groups)
}