
server_devop <- function(id='devop') {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      shiny$observeEvent(input$deleteDevelopmentFolder, {
        fs$dir_delete(getOption("datamisc_cache_path"))
        shiny$showNotification("Development Data Deleted")
      })
      shiny$observeEvent(input$createDevelopmentFolder, {
        shiny$req(filteredData())
        data <- filteredData()
        datamisc_cache_path <- getOption("datamisc_cache_path")
        
        data <- data |>
          dplyr$rowwise() |>
          dplyr$mutate(
            local_path = fs$path_join(c(datamisc_cache_path, path))
          )
        data <- data |>
          dplyr$filter(!file.exists(local_path))
        
        
        if (nrow(data)) {
          n_files <- length(data$local_path)
          shiny$showNotification(glue$glue("Copying {n_files} to local project"))
          data <- data |>
            dplyr$mutate(
              create_dir = fs$dir_create(fs$path_dir(local_path), recurse = T),
              copied = file.copy(path, local_path, overwrite = FALSE)
            ) |>
            dplyr$ungroup()
        }
        
        shiny$showNotification("Data moved to local storage")
      })
    }
  )
}


#' @export
server <- function(input, output, session) {
  box::use(
    ../metadata/server_metadata, shiny, dplyr, fs, cli,
    glue, purrr, readr, haven, htmlTable
  )
 
  server_devop() 
  filteredData <- server_metadata$server()
  
 
  
  output$outline <- shiny$renderTable({
    shiny$req(filteredData())
    data <- filteredData()
    data |>
      dplyr$transmute(study, year, monthName, analysis, filename,
                      size_hr = as.character(size_hr)
      )
  })
  
  dataToAnalyze <- shiny$reactive({
    box::use(../processing)
    shiny$req(filteredData())
    processing$make_data_to_analyze(filteredData())
  })
  
  output$previewData <- shiny$renderUI({
    shiny$req(dataToAnalyze)
    dataNames <- dataToAnalyze()
    
    purrr$map(
      dataNames,
      function(x) {
        data <- x$data
        nrowData <- nrow(data)
        column_names <- x$column_names
        column_names <- column_names[column_names == toupper(column_names)]
        nMissing <- data |>
          dplyr$summarise_all(function(x) {
            sum(is.na(x)) / nrowData
          })
        
        nMissing <-
          nMissing |>
          dplyr$select_if(
            function(x) {
              x > 0
            }
          )
        if (ncol(nMissing)) {
          nMissing <- nMissing |>
            dplyr$mutate_all(function(x) {
              paste0(
                round(x, 2), "%"
              )
            })
          colnames(nMissing) <- paste0(" - ", colnames(nMissing), " - ")
          nMissing <- shiny$div(
            shiny$h4("Missing Data"),
            shiny$div(
              htmlTable$htmlTable(nMissing)
            )
          )
        } else {
          nMissing <- shiny$div()
        }
        if ("PARAMCD" %in% colnames(x$data)) {
          paramcd <- unique(x$data[, "PARAMCD"]$PARAMCD)
          paramcd <- shiny$div(
            shiny$h3("PARAMCD"),
            purrr$map(paramcd, function(x) {
              shiny$tags$em(paste0(x, " * "))
            })
          )
        } else {
          paramcd <- shiny$div()
        }
        shiny$wellPanel(
          shiny$h4(glue$glue("{toupper(x$analysis)}")),
          shiny$h4("Columns"),
          shiny$tags$ul(
            purrr$map(column_names, function(x) {
              shiny$tags$li(x)
            })
          ),
          paramcd,
          nMissing
        )
      }
    )
  })
}
