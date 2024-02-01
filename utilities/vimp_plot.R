vimp_plot <- function(object) {
  # vimp_plot takes as input the output of the function ranger::ranger()
  
  if(class(object) != "ranger") {
    stop("Argument 'object' must be of class 'ranger'.")
  }
  
  if(is.null(object$variable.importance)) {
    stop("Element 'variable.importance' not found.")
  }
  
  variable_imp_df <- data.frame(
    var = names(object$variable.importance),
    importance = as.numeric(object$variable.importance)
  )
  
  variable_imp_df %>% 
    ggplot() +
    geom_col(aes(reorder(var, importance), importance, fill = var)) + 
    coord_flip() +
    theme(
      legend.position = "none"
    )
}