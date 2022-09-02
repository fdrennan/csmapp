
ui <- function() {
   box::use(shiny)
   box::use(./box/ui/meta)
   shiny$fluidPage(
      shiny$fluidRow(
         shiny$column(
            4,
            meta$ui_metadata()
         ),
         shiny$column(
            8,
            shiny$inputPanel(
               shiny$actionButton('createDevelopmentFolder', 'Create Develoment Folder')
            ),
            shiny$tableOutput('outline')
         )
      )
   )
}

server <- function(input, output, session) {
   box::use(./box/ui/meta, shiny, dplyr, fs)
   filteredData <- meta$server_metadata()
   
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
         dplyr$filter(!fs$file_exists(path))
         
      if (nrow(data)) {
         data <- data |> 
            dplyr$mutate(
               create_dir = fs$dir_create(fs$path_dir(local_path), recurse = T),
               copied = fs$file_copy(path, local_path, overwrite = FALSE)
            ) |> 
            dplyr$ungroup()
      }
      
      shiny$showNotification('Data moved to local storage')
   })
   
   output$outline <- shiny$renderTable({
      shiny$req(filteredData())
      data <- filteredData()
      data |> 
         dplyr$transmute(study, year, monthName, analysis, filename, size_hr = as.character(size_hr)) 
   })
}

box::use(shiny)
shiny$shinyApp(ui, server)
