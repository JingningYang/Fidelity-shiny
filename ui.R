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
shinyUI(tagList(fluidPage(#tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")), #First way to style CSS file
                          theme = "bootstrap.css",   #Second way(easiest way) to style CSS file
                          #tags$head(tags$style(HTML("body{ background: green; }"))), #Change background color manually, but not beautiful. 
                          
#Should we only use green background or still try to figure out how to use boostrap.min.css and make the position become horizontal rather than vertial? 
     
                  navbarPage("Fidelity", #position = "fixed-top", inverse = T, 
                        tabPanel("Home",
                                 tags$br(),
                                 tags$br(),
                              tags$h2("Two-semester Fidelity Project for Mutual Funds: Sector, Bond, and International"),
                              tags$br(),
                              tags$h3("Our Group:")       
                             ),
                        
                        
                         tabPanel("Sector Fund", 
                                      mainPanel(
                                          tabsetPanel(
                                              tabPanel("Tab 1",
                                                       h4("Table"),
                                                       tableOutput("table")))
                                      )),
                        
                        
                          tabPanel("International Fund", 
                                   tags$br(),
                                   tags$h2("Explanation"),
                                   sidebarPanel(
                                     selectInput("funds", "Selected International Funds:", c("FWWFX","FHKCX","FIVLX","FIVFX")),
                                     sliderInput("nav", "NAV(net asset value per share)",0,10000,0),
                                     selectInput("index", "Indices", c("MAKE A DATA FRAME?")),
                               
                               tags$h5(),
                               actionButton("action", "Search"),
                               
                               tags$h5("actionButton:"),
                               actionButton("action2", "Action button", class = "btn-primary")
                             )),
                        
                        
                          tabPanel("Bond Fund")
                  )
)))
