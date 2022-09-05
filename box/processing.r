#' @export
make_data_to_analyze <- function(filteredData) {
  box::use(
    shiny, dplyr, fs, cli, glue, purrr, readr, haven,
    htmlTable
  )
  data <- filteredData |>
    dplyr$arrange(dplyr$desc(date))
  if (getOption("development")) {
    data$path <- paste0(getOption("datamisc_cache_path"), data$path)
  }


  n_files <- nrow(data)
  data <- shiny$withProgress(
    message = "Reading In Data",
    detail = "This may take a while...",
    value = 0,
    {
      purrr$imap(
        split(data, 1:n_files),
        function(data, y) {
          y <- as.numeric(y)
          with(
            data,
            {
              path_ext_type <- fs$path_ext(path)
              study_data <- switch(path_ext_type,
                "csv" = readr$read_csv(path),
                "sas7bdat" = haven$read_sas(path)
              )
              shiny$incProgress(1 / n_files)
              dplyr$bind_cols(data, study_data)
            }
          )
        }
      )
    }
  )
  purrr$map(data, function(x) {
    list(
      study = unique(x$study),
      date = unique(x$date),
      analysis = unique(x$analysis),
      column_names = colnames(x),
      PARAMCD = unique(x$PARAMCD),
      data = x
    )
  })
}
