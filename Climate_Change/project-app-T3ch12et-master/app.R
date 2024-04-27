# Final project
library("shiny")
library("plotly")

# Executes the respective files
source("app_server.R")
source("app_ui.R")


# Creates a new shinyApp using the loaded ui and server variables
shinyApp(ui = ui, server = server)

