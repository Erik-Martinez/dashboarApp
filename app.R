#librerias

library(shiny)

# Llama a los archivos ui.R y server.R
source("server.R")
source("ui.R")

# Define la aplicación Shiny 
shinyApp(ui = ui, server = server)
