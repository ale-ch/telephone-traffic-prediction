library(data.table)
library(stringr)

compress_load_data <- function(path, new_path) {
  # Read a dataset and output it as a '.rda' or a '.rds' file.
  
  if(!(str_ends(new_path, ".rda") | str_ends(new_path, ".rds"))) {
    stop("Extension of new file must be either '.rda' or '.rds'.")
  }
  
  df <- fread(input = path)
  saveRDS(df, new_path)
  readRDS(new_path)
}
