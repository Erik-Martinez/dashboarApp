library(shinydashboard)

# Llama a los archivos ui.R y server.R
source("ui.R")
source("server.R")

# Define la aplicación Shiny 
shinyApp(ui = ui, server = server)
