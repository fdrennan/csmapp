


box::use(shiny, ./box/home/ui_home)
box::use(shiny, ./box/home/server_home)
shiny$shinyApp(ui_home$ui, server_home$server)

