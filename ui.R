library(shiny)

prices_choices <-
  c("FBNDX" = "FBNDX",
    "Qundl" = "qundl",
    "Alpha Vantage" = "alphavantage")
prices_chart_types <- c(
  "Bar Chart" = "bars",
  "Line Chart" = "line",
  "Match Sticks" = "matchsticks",
  "Candlestick Chart" = "candlesticks"
)
prices_chart_themes <- c("White" = "white",
                         "Black" = "black")
indicators <- c(
  "Moving Average" = "addSMA",
  "Welles Wilder's Directional Movement Indicator" = "addADX",
  "Average True Range" = "addATR",
  "Bollinger Bands" = "addBBands",
  "Bollinger Band Width" = "addBBands2",
  "Bollinger %b" = "addBBands3",
  "Commodity Channel Index" = "addCCI",
  "Chaiken Money Flow" = "addCMF",
  "Chande Momentum Oscillator" = "addCMO",
  "Double Exponential Moving Average" = "addDEMA",
  "Detrended Price Oscillator" = "addDPO",
  "Exponential Moving Average" = "addEMA",
  "Price Envelope" = "addEnvelope",
  "Exponential Volume Weigthed Moving Average" = "addEVWMA",
  "Options and Futures Expiration" = "addExpiry",
  "Moving Average Convergence Divergence" = "addMACD",
  "Momentum" = "addMomentum",
  "Rate of Change" = "addROC",
  "Relative Strength Indicator" = "addRSI",
  "Parabolic Stop and Reverse" = "addSAR",
  "Stocastic Momentum Index" = "addSMI",
  "Triple Smoothed Exponential Oscillator" = "addTRIX",
  "Weighted Moving Average" = "addWMA",
  "Williams %R" = "addWPR",
  "ZLEMA" = "addZLEMA"
)
return_features <- c(
  "Open" = "open",
  "Close" = "close",
  "High" = "high",
  "Low" = "low",
  "Adjusted" = "adjusted",
  "Volume" = "volume"
)
return_options <- c("Log" = "log",
                    "Arithmetic" = "arithmetic")
return_period_options <- c(
  "Yearly" = "yearly",
  "Quarterly" = "quarterly",
  "Monthly" = "monthly",
  "Weekly" = "weekly",
  "Daily" = "daily"
)
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
                              tags$h2("Two-semester Fidelity Project for Mutual Funds: Sector, Bond, and International"),
                              tags$br(),
                              tags$h3("Our Group:"),
                              
                              tags$footer(footer, align="center", style="position:absolute; bottom:0")
                             ),
                        
                         tabPanel("Sector Fund",
                                  titlePanel("Sector Fund"),
                                  sidebarPanel(
                                    selectInput("sector","Choose Funds:",c("FSAIX","FRESX","FREMEX","FSDCX","FSPCX")),
                                    dateRangeInput("date", strong("Date range"), start = "2014-01-01", end = "2018-12-31",
                                                   min = "2014-01-01", max = "2018-12-31")
                                  )),

                          tabPanel("International Fund", 
                                   tags$br(),
                                   tags$h2("Select 5 international mutual funds:"),
                                   tags$br(),
                                   tags$h3("FWWFX(Fidelity World Wide Fund), FHKCX(Fidelity China Region Fund), FIVLX(Fidelity International Value Fund), FIVFX(Fidelity International Capital Appreciation Fund)"),
                                   tags$br(),
                                   tags$h2("For each mutual fund, we used at least 5 indices to compare their NAV from 2014 to 2018"),
                                   sidebarPanel(
                                     tags$h4("Selected funds:"),
                                     checkboxInput("fwwfx", "FWWFX"),
                                     checkboxInput("fhkcx", "FHKCX"),
                                     checkboxInput("fivlx", "FIVLX"),
                                     checkboxInput("fivfx", "FIVFX"),
                                     checkboxInput("figrx", "FIGRX"),
                                     
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
                        
                          tabPanel(title="Bond Fund",value = "Prices",
            tabsetPanel(
              type = "tabs",
              tabPanel(
                title = "Buy & Sell Simulator",
                value = "indi_sig",
                wellPanel(
                  fluidRow(
                    column(
                      width = 2,
                      radioButtons(
                        "pricesSource",
                        label = h3("Bond Sticker"),
                        choices = prices_choices
                      )
                    ),
                    column(
                      width = 2,
                      radioButtons(
                        "pricesChartType",
                        label = h3("Chart Type"),
                        choices = prices_chart_types,
                        selected = "candlesticks"
                      )
                    ),
                    column(
                      width = 2,
                      radioButtons(
                        "pricesChartTheme",
                        label = h3("Chart Theme"),
                        choices = prices_chart_themes
                      )
                    ),
                    column(
                      width = 2,
                      h3("Miscellaneous"),
                      checkboxInput("priceChartGrid",
                                    "Show Grid",
                                    value = TRUE),
                      checkboxInput("priceLogScale",
                                    "Log Scale",
                                    value = FALSE)
                    ),
                    column(
                      width = 4,
                      h3("Simulation Setup"),
                      selectInput("indicators", "Indicators",
                                  indicators , multiple = TRUE),
                      dateRangeInput(
                        inputId = "daterange",
                        label = "Date range",
                        start = "2014-01-01",
                        end = "2018-12-31"
                      ),
                      actionButton("predict_btn",
                                   "Simulate")
                    )
                  )),
                fluidRow(column(width = 12,
                                plotOutput("prices_plot"))),
                conditionalPanel("input.predict_btn",
                                 hr(),
                                 fluidRow(column(width = 12,
                                                 verbatimTextOutput("simulator_summary"))),
                                 hr(),
                                 fluidRow(column(width = 12,
                                                 h4("Trading Evaluation"),
                                                 verbatimTextOutput("trading_evaluation"))),
                                 hr(),
                                 
                                 fluidRow(column(width = 12,
                                                 h4("Downside Risk"),
                                                 verbatimTextOutput("downside_risk"))),
                                 hr(),
                                 fluidRow(column(width = 12,
                                                 plotOutput("buy_sell_plot"))),
                                 hr(),
                                 fluidRow(column(width = 12,
                                                 plotOutput("CumReturns"))),
                                 hr(),
                                 fluidRow(column(width = 12,
                                                 plotOutput("yearlyReturn"))),
                                 hr(),
                                 fluidRow(column(width = 12,
                                                 plotOutput("rets"))))
              ),
              tabPanel(
                title = "Periodic Returns",
                value = "perdioic_returns",
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput(
                      inputId = "daterange_return",
                      label = "Date range",
                      start = "2014-01-01",
                      end = "2018-12-31"
                    ),
                    selectInput("return_features", "Apply return to",
                                return_features),
                    selectInput("return_function", "Transformation Function",
                                return_options),
                    selectInput(
                      "return_period",
                      "Transformation Priod",
                      return_period_options,
                      selected = "yearly"
                    )
                  ),
                  mainPanel(plotOutput("return_plot"))
                )
              ),
              tabPanel(
                title = "Simulation",
                value = "simulation",
                sidebarLayout(
                  sidebarPanel(
                    numericInput("days_num", "Number of Bond Price Simulations", value = 252),
                    numericInput("sim_num", "Number of Monte Carlo Simulations", value = 250)
                  ),
                  mainPanel(
                    plotOutput("daily_return_plot"),
                    plotOutput("simulation_plot")
                  )
                )
              ),
              tabPanel(title = "Dataset download", icon =  icon("download"), value = 8,
                       p(class = "lead","Download complete dataset"),
                       br(),
                       downloadButton(outputId = "downloadExpressionData", label = "Download expression data", class= "btn-primary"),
                       p(""),
                       downloadButton(outputId = "downloadpData", label = "Download pheno data", class= "btn-primary")
              )
            )
                  )
)
)
                  )
)
