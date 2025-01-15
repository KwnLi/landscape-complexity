plot_fun <- function(mod, pred.vars, mod.data, color_by = "margin_dens", return_data = TRUE, ...){
  predict.list <- lapply(pred.vars,\(x){ggeffects::ggpredict(mod, x, ...)})
  names(predict.list) <- pred.vars
  
  pred.trends <- dplyr::bind_rows(predict.list, .id = "pred.var") %>%
    dplyr::rename_with(~as.character(mod$terms[[2]])[length(as.character(mod$terms[[2]]))],
                       predicted)
  
  if(return_data){
    pred.data <- mod.data %>%
      select(all_of(c(as.character(mod$terms[[2]])[length(as.character(mod$terms[[2]]))], pred.vars))) %>%
      mutate(color_by = get(color_by)) %>%
      pivot_longer(all_of(pred.vars), names_to = "pred.var", values_to = "x") %>%
      rename(!!color_by := color_by)
  }
  
  if(return_data){
    list(pred.trends, pred.data)
  }else{
    pred.trends
  }
  
}