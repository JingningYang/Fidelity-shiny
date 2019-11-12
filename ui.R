library(shiny)

title <- tags$a(href="https://www.fidelity.com", 
                tags$img(src="Boston_University_Terriers_logo.jpg", height="55", width="80"),
                tags$img(src="Fid_Logo_Rev_green.jpg", height="55", width="130"))

shinyUI(tagList(fluidPage(theme = "bootstrap.css",   
                  navbarPage(title = title, #position = "fixed-top", inverse = T, 
                        tabPanel("Home",
                                 tags$br(),
                                 tags$br(),
                              tags$h2("Two-semester Fidelity Project for Mutual Funds: Sector, Bond, and International"),
                              tags$br(),
                              tags$h3("Our Group:")       
                             ),
                        
                         tabPanel("Sector Fund",
                                  titlePanel("Sector Fund"),
                                  sidebarPanel(
                                    selectInput("sector","Sector Funds:",c("FSAIX","FRESX","FREMEX","FSDCX","FSPCX")),
                                    dateRangeInput("date", strong("Date range"), start = "2014-01-01", end = "2018-12-31",
                                                   min = "2014-01-01", max = "2018-12-31"),
                                  )),

                          tabPanel("International Fund", 
                                   tags$br(),
                                   tags$h2("Select 5 international mutual funds:"),
                                   tags$br(),
                                   tags$h3("FWWFX(Fidelity World Wide Fund), FHKCX(Fidelity China Region Fund), FIVLX(Fidelity International Value Fund), FIVFX(Fidelity International Capital Appreciation Fund)"),
                                   tags$br(),
                                   tags$h2("For each mutual fund, we used at least 5 indices to compare their NAV from 2014 to 2018"),
                                   sidebarPanel(
                                     selectInput("funds", "Selected International Funds:", c("FWWFX","FHKCX","FIVLX","FIVFX")),
                              #Show the graph of the NAV of the fund from 2014-2018
                                     sliderInput("nav", "NAV(net asset value per share)",0,10000,0),
                              #?
                                     selectInput("index", "Indices", c("MAKE A DATA FRAME?")),
                              
                        #Show every indices on the same graph from 2014-2018 when we choose choice.
                                     h5("Now, we try to make a model fit our selected fund"),
                                     selectInput("mo", "Models we can try:", c("Linear regression")),
                        #show the summary of models on the right hand side.
                                     
                               tags$h5(),
                               actionButton("action", "Search"),
                               tags$h5("actionButton:"),
                               actionButton("action2", "Action button", class = "btn-primary")
                             )
                             
            ),
                        
                        
                        
                          tabPanel("Bond Fund")
                  )
)))
