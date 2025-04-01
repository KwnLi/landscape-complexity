makeforcedata <- function(shaptab, order.col){
  shaptab <- as.data.frame(shaptab)
  features <- names(shaptab)
  shaptab <- shaptab |> 
    tibble::rownames_to_column("rowid") |>
    tidyr::pivot_longer(cols=features, names_to = "feature", values_to = "value") |>
    dplyr::group_by(rowid) |>
    group_modify(~ .x %>% cbind(ordercol = .x[.x$feature==order.col,]$value)) |>
    ungroup() |>
    mutate(rowid = fct_reorder(as.factor(rowid), ordercol))
}

makeforcedata.hclust <- function(shaptab, ...){
  shaptab <- as.data.frame(shaptab)
  features <- names(shaptab)
  
  shap.hclust <- hclust(dist(shaptab), ...)
  
  shaptab <- shaptab |> 
    tibble::rownames_to_column("rowid") |>
    dplyr::bind_cols(ordercol = shap.hclust$order) |>
    tidyr::pivot_longer(cols=features, names_to = "feature", values_to = "value") |>
    mutate(rowid = fct_reorder(as.factor(rowid), ordercol))
}

makeforcedata.predict <- function(shaptab, predictions, ...){
  shaptab <- as.data.frame(shaptab)
  features <- names(shaptab)
  
  shaptab <- shaptab |> 
    tibble::rownames_to_column("rowid") |>
    dplyr::bind_cols(ordercol = predictions) |>
    tidyr::pivot_longer(cols=features, names_to = "feature", values_to = "value") |>
    mutate(rowid = fct_reorder(as.factor(rowid), ordercol))
}

makeforcedata.orderby <- function(shaptab, orderby, rename.fn, other.var){
  shaptab <- as.data.frame(shaptab)
  features <- names(shaptab)
  
  shaptab <- shaptab |> 
    tibble::rownames_to_column("rowid") |>
    dplyr::bind_cols(ordercol = orderby) |>
    tidyr::pivot_longer(cols=features, names_to = "feature", values_to = "value") |>
    mutate(rowid = fct_reorder(as.factor(rowid), ordercol))
  
  if(!missing(rename.fn)){
    if(is.function(rename.fn)){
      shaptab <- shaptab |>
        mutate(feature2 = rename.fn(feature)) |>
        # order feature2 by absolute magnitude of total shap values
        group_by(feature2) |>
        group_modify(~ .x %>% bind_cols(totalshap = sum(abs(.x$value)))) |>
        ungroup() |>
        mutate(feature2 = fct_reorder(as.factor(feature2), totalshap))
      
      if(!missing(other.var)){
        shaptab <- shaptab |>
          mutate(feature2 = fct_relevel(feature2, other.var))
      }
        
    }else{
      stop("rename.fn is not a function")
    }
  }
  return(shaptab)
}
