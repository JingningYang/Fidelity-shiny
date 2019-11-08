#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.min.css", 
                  navbarPage("Fidelity", theme = "bootstrap.min.css",
                             tabPanel("Home",
                                      sidebarPanel(
                                          fileInput("file", "File input:"),
                                          textInput("txt", "Text input:", "general"),
                                          sliderInput("slider", "Slider input:", 1, 100, 30),
                                          tags$h5("Deafult actionButton:"),
                                          actionButton("action", "Search"),
                                          
                                          tags$h5("actionButton with CSS class:"),
                                          actionButton("action2", "Action button", class = "btn-primary")
                                      ),
                                      
                             ),
                             tabPanel("Sector Fund", 
                                      mainPanel(
                                          tabsetPanel(
                                              tabPanel("Tab 1",
                                                       h4("Table"),
                                                       tableOutput("table"),
                                                       h4("Verbatim text output"),
                                                       verbatimTextOutput("txtout"),
                                                       h1("Header 1"),
                                                       h2("Header 2"),
                                                       h3("Header 3"),
                                                       h4("Header 4"),
                                                       h5("Header 5")
                                              ),
                                              tabPanel("Tab 2", "This panel is intentionally left blank"),
                                              tabPanel("Tab 3", "This panel is intentionally left blank")
                                          )
                                      )),
                             tabPanel("International Fund", "This panel is intentionally left blank"),
                             tabPanel("Bond Fund")
                  )
)))
