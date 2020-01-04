library(shiny)
library(DT)
library(httr)
library(shinydashboard)
library(ggthemes)
library(plotly)



signals <- c("Comparison" = "sigComparison",
             "Crossover" = "sigCrossover")
relations <- c("Less than" = "lt", "Greater Than" = "gt")

title <- tags$a(href="https://www.fidelity.com", 
                tags$img(src="Fid_Logo_Rev_green.jpg", height="55", width="130"))

footer <- tags$a(href="http://www.bu.edu/mssp/",
                 tags$img(src="Boston_University_Terriers_logo.jpg", height="55", width="80"))

##############
shinyUI(tagList(fluidPage(theme = "bootstrap.css",   
                          navbarPage(title = title, position = "fixed-top",
                                  
                           tabPanel("Home",
                                              tags$br(),
                                              tags$br(),
                                              titlePanel("Two-semester Fidelity Project for Mutual Funds: Sector, Bond, and International"),
                                              tags$br(),
                                              tags$h3("Our Group:"),
                                    mainPanel(img(src="IMG_1415.JPG",width=400, height=300),
                                              img(src="sector.JPG",width=400, height=300),
                                              img(src="IMG_0816.JPG",width=400, height=300),
                                              tags$footer(footer, align="center", style="position:absolute; bottom:0"))
                                    
                                    ),
                                     
                           ######################Start of Sector Fund####################
                           tabPanel("Sector Fund",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("choose",h3("Choose Fund"),
                                                    c("FRESX","FSMEX","FSPCX","FSPTX","FSCPX","Group")),width = 3,
                                        sliderInput("slidea", "alpha:",
                                                    min = 0, max = 1, value = 500),
                                        sliderInput("slideb", "Lambda:",
                                                    min = -15, max = 2, value = 500)
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel(h4("Introduction"),  htmlOutput("html1")),
                                          tabPanel(h4("Result"), plotlyOutput("Result1")),
                                          tabPanel(h4("Plot"), plotOutput("Plot1"))
                                        )
                                      )
                                    )
                           ),
                           ######################End of Sector Fund####################
                                     
  tabPanel("International Fund", 
           
           sidebarLayout(
             sidebarPanel(
               selectInput("choose1", h3("Choose One Fund"),
                           c("FWWFX", "FHKCX", "FIVLX", "FIVFX", "FIGRX")), width = 3
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Introduction", htmlOutput("text")),
                 tabPanel("Result", plotlyOutput("model",width ='100%',height = '700px')),
                 tabPanel("Plot", plotlyOutput("graph",width ='100%',height = '700px'))
               )
             )
           )),
                                     ###################END of INTERNATIONAL FUND##########################################
  ###################Bond Fund#################################################
  tabPanel(title="Bond Fund",value = "Prices",
           tabsetPanel(
             type = "tabs",
             
             
             tabPanel(title = "FMNDX",
                      value = "fmndx",
                      
                      navlistPanel(
                        tabPanel("Introduction",
                                 fluidRow( h4(textOutput('FMNDX_Harry')))
                        ),
                        tabPanel("Plot",plotlyOutput("Graph.fmn", width = '1350px', height = '500px')),
                        tabPanel("Model",plotlyOutput("modelfmndx", width = '1350px', height = '500px'))
                      )
                      
                      
             ),
             tabPanel(title = "FBNDX",
                      value = "fbndx",
                      navlistPanel(
                        tabPanel("Introduction",
                                 fluidRow( h4(textOutput('FBNDX_Yuanyuan')))
                        ),
                        tabPanel("Plot",plotlyOutput("Graph.fbn", width = '1350px', height = '500px')),
                        tabPanel("Model",plotlyOutput("modelfbndx", width = '1350px', height = '500px'))
                      )
                      
             )#tabFB
             ,tabPanel(title = "FCSTX",
                       value = "fcstx",
                       navlistPanel(
                         tabPanel("Introduction",
                                  fluidRow( h4(textOutput('FCSTX_Teresa')))
                         ),
                         tabPanel("Plot",plotlyOutput("Graph.fcs", width = '1350px', height = '500px')),
                         tabPanel("Model",plotlyOutput("plot_fcsPlot", width = '1350px', height = '500px'))
                       )
                       
             )#tabfcs
             ,tabPanel(title = "SPHIX",
                       value = "sphix",
                       navlistPanel(
                         tabPanel("Introduction",
                                  fluidRow( h4(textOutput('SPHIX_Fiona')))
                         ),
                         tabPanel("Plot",plotlyOutput("Graph.shp", width = '1350px', height = '500px'))
                                                  
                                                  ,
                         tabPanel("Model",plotlyOutput("modelsphix", width = '1350px', height = '500px'))
                       )
                       
             )#tabSPH
             ,tabPanel(title = "FMSFX",
                       value = "fmsfx",
                       navlistPanel(
                         tabPanel("Introduction",
                                  fluidRow( h4(textOutput('FMSFX_Yiping')))
                         ),
                         tabPanel("Plot",plotlyOutput("Graph.fms", width = '1350px', height = '500px')),
                         
                         tabPanel("Model",plotlyOutput("modelfmsfx"),width = '2350px', height = '1000px')
                       )
                       )#tabFMS
           )
           
  )
                                     
                          ))))
