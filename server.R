#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
  #######International fund
    output$table <- renderTable({
        head(cars, 4)
    })
    
    output$inf <- DT::renderDataTable({
      data <- Data
}
)})
