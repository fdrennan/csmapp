
ui <- function() {
   box::use(shiny)
   box::use(./box/ui/meta)
   shiny$fluidPage(
     shiny$titlePanel('CSM Management System'),
     shiny$sidebarLayout(shiny$sidebarPanel(width=3,
       shiny$fluidRow(
         meta$ui_metadata(),
         {
           
           if (getOption('development')) {
             NULL
           } else {
             shiny$inputPanel(
               shiny$actionButton('deleteDevelopmentFolder', 'Delete Develoment Folder'),
               shiny$actionButton('createDevelopmentFolder', 'Create Develoment Folder')
             )
           }
         }
       )
     ),
     shiny$mainPanel(
       shiny$tabsetPanel(
         shiny$tabPanel(
           'Raw Meta Data',
           shiny$tableOutput('outline')
         ),
         shiny$tabPanel(
           'Preview Data',
           shiny$tableOutput('previewData')
         )
       )
     ))
   )
}

server <- function(input, output, session) {
   box::use(./box/ui/meta, shiny, dplyr, fs, cli, glue, purrr, readr, haven)
   filteredData <- meta$server_metadata()
   
   shiny$observeEvent(input$deleteDevelopmentFolder, {
      fs$dir_delete(getOption('datamisc_cache_path'))
      shiny$showNotification('Development Data Deleted')
   })
   
   shiny$observeEvent(input$createDevelopmentFolder, {
      shiny$req(filteredData())
      data <- filteredData()
      datamisc_cache_path <- getOption('datamisc_cache_path')
      
      data <- data |> 
         dplyr$rowwise() |> 
         dplyr$mutate(
            local_path = fs$path_join(c(datamisc_cache_path, path))
         ) 
      data <- data |> 
         dplyr$filter(!file.exists(local_path))
      
      
      if (nrow(data)) {
         n_files <- length(data$local_path)
         shiny$showNotification(glue$glue('Copying {n_files} to local project'))
         data <- data |> 
            dplyr$mutate(
               create_dir = fs$dir_create(fs$path_dir(local_path), recurse = T),
               copied = file.copy(path, local_path, overwrite = FALSE)
            ) |> 
            dplyr$ungroup()
      }
      
      shiny$showNotification('Data moved to local storage')
   })
   
   output$outline <- shiny$renderTable({
      shiny$req(filteredData())
      data <- filteredData()
      data |> 
         dplyr$transmute(study, year, monthName, analysis, filename, 
                         size_hr = as.character(size_hr)) 
   })
   
   dataToAnalyze <- reactive({
     shiny$req(filteredData())
     data <- filteredData()
     if (getOption('development')) {
       data$path <- paste0(getOption('datamisc_cache_path'), data$path)
     } 
     n_files <- nrow(data)
     data <- purrr$imap_dfr(
       split(data, 1:n_files),
       function(data, y) {
         y <- as.numeric(y)
         with(
           data, {
             path_ext_type <- fs$path_ext(path)
             shiny$showNotification(shiny$div(
               shiny$h6(glue$glue(
                 'Study {study}'
               )),
               shiny$h6(
                 glue$glue(
                   'Filename {filename}'
                 )
               ),
               shiny$h6(
                 glue$glue(
                   'Files Remaining: {n_files-y}'
                 )
               )
             ), duration=2, closeButton = FALSE)
             study_data <- switch(
               path_ext_type,
               'csv' = readr$read_csv(path),
               'sas7bdat' = haven$read_sas(path)
             )
             
             dplyr$bind_cols(data, study_data)
             
           }
         )
       }
     )
     
     data
   })
   
   output$previewData <- shiny$renderTable({
     shiny$req(dataToAnalyze)
     data <- dataToAnalyze()
     
     data |> 
       head(100)
      
   })
}

box::use(shiny)
shiny$shinyApp(ui, server)
