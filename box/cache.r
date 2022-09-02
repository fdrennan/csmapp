#' @export
check <- function() {
  box::use(readr)
  cache_path <- getOption('cache_path')
  if (getOption('cache')) {
    data <- readr$read_rds(cache_path)
  } else {
    box::use(./meta)
    data <- meta$get_data()
    readr$write_rds(data, cache_path)
    options(cache=TRUE)
    data
  }
}