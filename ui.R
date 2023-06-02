options(shiny.sanitize.errors = FALSE)

library(shinydashboard)
library(highcharter)
library(plotly)
library('ggplot2')
library(quantmod)
library('forecast')
library('tseries')
library(DT)
library(PerformanceAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
library(dygraphs)
library(magrittr)
library(pracma)
require(xts)
require(jsonlite)
library(TTR)
library(zoo)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(rvest)
library(stringr)
library(magrittr)
library("data.table")
library(shinyjs)
library(rintrojs)
library(shinyalert)
library(ggfortify)
library("lubridate")
library(tidyquant)
library("RJSONIO")
library(httr)
library(taRifx)
library("prophet")

library("dplyr")
library("tidyr")
library(twitteR)
library(magrittr)
library(SentimentAnalysis)
require(gridExtra)
require(devtools)
require(quantmod)
require("usethis")
# require(IKTrading)
require("sqldf")
library("sqldf")
library(caret)

library(taRifx)

# library(RcppArmadillo)
library(SnowballC)
library("shinybusy")
library("shinycssloaders")
library("shinycustomloader")
library("nse2r")
library(slickR)
# library(smartapi)
library("IBrokers")
library(ggcorrplot)
require(quantmod)
require(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tibble)

# library(smartapi)
library(optionstrat)
library("derivmkts")
library(anytime)

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;
}"'

ui <- tagList(
  # dashboardPage(
  # dashboardHeader(title = "Stocks Analysis Tool"), skin = "red",
  # dashboardSidebar(disable = TRUE),
  # dashboardBody(
  
  navbarPage("Stocks Analysis", selected = "Home", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cosmo"),
    useShinyjs(),
    introjsUI(),
    useShinyalert(),
    header = tagList(
      useShinydashboard()
    ),
tags$style(js),
# tags$head(tags$style(HTML(" .nav-tabs-custom>.tab-content { overflow-y: auto; height: 750px; }"))),
    tabBox(id = "tabs",
      side = "right", width = 12,
      height = "45px",
      selected = "Home",
      tabPanel("About us",
               fluidRow(
                 column(width = 6,
               tags$div(class = "box box-solid",
                        tags$div(class = "box-header with-border collapse",
                                 tags$i(class = "fas fa-book-open"),
                                 tags$h3(class = "box-title custom-box-header","The Short")
                        ),
                        tags$div(class = "box-body",
                                 tags$p("Stocks Analysis is an innovative trading idea to make trading technology accessible to everyone. This is a one stop application to having a full trading platform in a web browser."),
                                 tags$p("Short"),
                                 tags$p("Sai Teja Reddy is currently working as a Data Scientist at one of the most reputed technology organization. Prior to that, worked as Sales Operations Analyst and Business Analyst at Uber with almost 2 years of experience. He began his carrer as a Developer and has good hands on experience with microsoft technologies. He is an Electrical graduate from National Insititute of Technology Durgapur."),
                                 tags$p("Long"),
                                 tags$p("I am a technology lover and good writer. The purpose was building programmed software which allows to automate the trading of Indian stock trades by acting upon high volumes, creating strategies and doing backtest to generate more profits."),
                                 br()
                                 
                        )
               )
               )
               )
               
      ),
      tabPanel("Tips & Tricks"
               
      ),
      tabPanel("Portfolio Optimisation",icon = icon("book-reader"), theme = shinytheme("cosmo"),
               
               box(width=12,status="primary",title = "Portfolio Selections",solidHeader=TRUE,height = 1200,
                   column(8,
                          
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock1", label = "Company 1:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "RELIANCE.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size1", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size1", label=h6("Portfolio weight %:"), value=0.10, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock2", label = "Company 2:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "DRREDDY.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size2", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size2", label=h6("Portfolio weight %:"), value=0.10, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock3", label = "Company 3:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","VOLTAS.NS"), selected = "VOLTAS.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size3", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size3", label=h6("Portfolio weight %:"), value=0.1, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock4", label = "Company 4:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","ITC.NS"), selected = "ITC.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size4", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size4", label=h6("Portfolio weight %:"), value=0.1, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock5", label = "Company 5:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","WIPRO.NS"), selected = "WIPRO.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size5", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size5", label=h6("Portfolio weight %:"), value=0.1, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock6", label = "Company 6:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","HCLTECH.NS"), selected = "HCLTECH.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size6", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size6", label=h6("Portfolio weight %:"), value=0.1, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock7", label = "Company 7:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","TATAMOTORS.NS"), selected = "TATAMOTORS.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size7", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size7", label=h6("Portfolio weight %:"), value=0.1, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock8", label = "Company 8:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","JSWSTEEL.NS"), selected = "JSWSTEEL.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size8", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size8", label=h6("Portfolio weight %:"), value=0.1, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock9", label = "Company 9:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","TITAN.NS"), selected = "TITAN.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size9", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size9", label=h6("Portfolio weight %:"),value=0.1, min =0,max = 1)
                            )
                          ),
                          fluidRow(
                            column(4,
                                   selectizeInput("portfolio_stock10", label = "Company 10:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "SBIN.NS", multiple = FALSE,
                                                  options = list(create = TRUE,maxOptions = 10))
                            ),
                            column(2,
                                   numericInput("portfolio_1_size10", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                            ),
                            column(2,
                                   numericInput("portfolio_2_size10", label=h6("Portfolio weight %:"), value=0.1, min =0,max = 1)
                            )
                          )
                          
                          ),
                   column(4,
                          
                          # textInput("corr_input", "Company : ", value = "UBER"),
                          # actionButton("corr_btn", "click me to add tickers"),
                          dateRangeInput("corr_daterange", "Date range:", start = "2019-05-10", end = Sys.Date(),format = "yyyy-mm-dd"),
                          selectizeInput("port_selection", label = "Optimisation Method :", choices=c("minimum-variance","mean-variance"), selected = "minimum-variance", multiple = FALSE,
                                         options = list(create = TRUE,maxOptions = 2)),
                          numericInput("initial_cap", label=h6("Initial Capital:"), value=100000, min =1000,max = 10000000000),
                          selectizeInput("port_bench_mark", label = "Bench Mark :", choices=c("SENSEX"="%5EBSESN","NIFTY50"="%5ENSEI","NIFTY BANK"="%5ENSEBANK"), selected = "SENSEX", multiple = FALSE,
                                         options = list(create = TRUE,maxOptions = 3)),
                          actionButton(inputId = "corr_action",label = "Analyse Portfolio",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          
                   )
                   
                   
                   
               ),
               
               
               
               
               
               # box(width=12,status="primary",solidHeader=TRUE,height = 400,
               #     fluidRow(
               #       
               #       column(4,
               #              # textInput("corr_input", "Company : ", value = "UBER"),
               #              # actionButton("corr_btn", "click me to add tickers"),
               #              uiOutput("newInputs")
               #              # selectizeInput("corr_input", label = "Company :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "UBER", multiple = FALSE,
               #              #                options = list(create = TRUE,maxOptions = 10))
               #       ),
               #       column(4,
               #              dateRangeInput("corr_daterange", "Date range:", start = "2019-05-10", end = Sys.Date(),format = "yyyy-mm-dd")
               #       ),
               #       column(2,
               #              selectizeInput("port_selection", label = "Optimisation Method :", choices=c("minimum-variance","mean-variance"), selected = "minimum-variance", multiple = FALSE,
               #                             options = list(create = TRUE,maxOptions = 2))
               #       ),
               #       column(2,
               #              numericInput("initial_cap", label=h6("Initial Capital:"), value=100000, min =1000,max = 10000000000),
               #       ),
               #       column(2,
               #              selectizeInput("port_bench_mark", label = "Bench Mark :", choices=c("SENSEX"="%5EBSESN","NIFTY50"="%5ENSEI","NIFTY BANK"="%5ENSEBANK"), selected = "SENSEX", multiple = FALSE,
               #                             options = list(create = TRUE,maxOptions = 3))
               #       ),
               #       column(2,
               #              actionButton(inputId = "corr_action",label = "Run Correlation",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               #       )
               #     )
               #     
               # ),
               # box(width=12,title= "Portfolio Selection",status="primary",solidHeader=TRUE ,
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock1", label = "Company 1:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "RELIANCE.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size1", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size1", label=h6("Portfolio weight %:"), value=0.50, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock2", label = "Company 2:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "DRREDDY.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size2", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size2", label=h6("Portfolio weight %:"), value=0.50, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock3", label = "Company 3:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","VOLTAS.NS"), selected = "VOLTAS.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size3", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size3", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock4", label = "Company 4:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","ITC.NS"), selected = "ITC.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size4", label=h6("Portfolio weight %:"), value=0.25, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size4", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock5", label = "Company 5:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","WIPRO.NS"), selected = "WIPRO.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size5", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size5", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock6", label = "Company 6:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","HCLTECH.NS"), selected = "HCLTECH.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size6", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size6", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock7", label = "Company 7:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","TATAMOTORS.NS"), selected = "TATAMOTORS.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size7", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size7", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock8", label = "Company 8:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","JSWSTEEL.NS"), selected = "JSWSTEEL.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size8", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size8", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock9", label = "Company 9:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","TITAN.NS"), selected = "TITAN.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size9", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size9", label=h6("Portfolio weight %:"),value=0, min =0,max = 1)
                   #   )
                   # ),
                   # fluidRow(
                   #   column(4,
                   #          selectizeInput("portfolio_stock10", label = "Company 10:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "SBIN.NS", multiple = FALSE,
                   #                         options = list(create = TRUE,maxOptions = 10))
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_1_size10", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   ),
                   #   column(2,
                   #          numericInput("portfolio_2_size10", label=h6("Portfolio weight %:"), value=0, min =0,max = 1)
                   #   )
                   # )
                   
                   
                   
                   # column(4,
                   #        selectizeInput("portfolio_stock1", label = "Company 1:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "RELIANCE.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock2", label = "Company 2:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "DRREDDY.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock3", label = "Company 3:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","ITC.NS"), selected = "ITC.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock4", label = "Company 4:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","WIPRO.NS"), selected = "WIPRO.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock5", label = "Company 5:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","VOLTAS.NS"), selected = "VOLTAS.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock6", label = "Company 6:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","TATAMOTORS.NS"), selected = "TATAMOTORS.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock7", label = "Company 7:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","HCLTECH.NS"), selected = "HCLTECH.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock8", label = "Company 8:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","SBIN.NS"), selected = "SBIN.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock9", label = "Company 9:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","HDFCBANK.NS"), selected = "HDFCBANK.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10)),
                   #        selectizeInput("portfolio_stock10", label = "Company 10:", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","JSWSTEEL.NS"), selected = "JSWSTEEL.NS", multiple = FALSE,
                   #                       options = list(create = TRUE,maxOptions = 10))
                   # ),
                   # column(2,
                   #        numericInput("portfolio_1_size1", label=h6("Portfolio weight %:"), value=50, min =1,max = 100),
                   #        numericInput("portfolio_1_size2", label=h6("Portfolio weight %:"), value=50, min =1,max = 100),
                   #        numericInput("portfolio_1_size3", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_1_size4", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_1_size5", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_1_size6", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_1_size7", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_1_size8", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_1_size9", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_1_size10", label=h6("Portfolio weight %:"), value=0, min =1,max = 100)
                   # ),
                   # column(2,
                   #        numericInput("portfolio_2_size1", label=h6("Portfolio weight %:"), value=50, min =1,max = 100),
                   #        numericInput("portfolio_2_size2", label=h6("Portfolio weight %:"), value=50, min =1,max = 100),
                   #        numericInput("portfolio_2_size3", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_2_size4", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_2_size5", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_2_size6", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_2_size7", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_2_size8", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_2_size9", label=h6("Portfolio weight %:"), value=0, min =1,max = 100),
                   #        numericInput("portfolio_2_size10", label=h6("Portfolio weight %:"), value=0, min =1,max = 100)
                   # )
                   
               # ),
               
               tabBox(id ="portfolio_tabs",
                      side = "left", width = 12,
                      height = "600px",
                      selected = "Portfolio Performance",
                      tabPanel("Portfolio Performance",
                               box(width=12,title= "MOM Performance",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   column(6,
                                          plotlyOutput("port_distribution")
                                   ),
                                   column(6,
                                          plotlyOutput("port_mom_returns_plot")
                                   )
                                   
                               ),
                               box(width=12,title= "Portfolio Plot",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   plotlyOutput("port_returns_plot")
                               )
                               
                      ),
                      tabPanel("Correlation Analysis",
                               box(width=12,title= "Correlation Plot",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   plotOutput("correlation_plot", width = "100%",height = "900px")
                               )
                               ),
                      tabPanel("Portfolio Metrics",
                               box(width=12,title= "Optimised Metrics",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   div(
                                     withSpinner( DT::dataTableOutput("port_optimised_metrics"))
                                   )
                               )
                      ),
                      tabPanel("Optimised Weights",
                               box(width=12,title= "Optimised Returns & Weights",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   column(4,
                                          verbatimTextOutput("port_optimised_returns")
                                   ),
                                   column(8,
                                          verbatimTextOutput("port_optimised_weights")
                                   ),
                               )
                               
                      ),
                      tabPanel("Risk Vs Returns",
                               box(width=12,title= "Efficient Frontier & Weights",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   column(6,
                                          plotlyOutput("port_efficient_frontier")
                                   ),
                                   column(6,
                                          plotlyOutput("port_weights_across_frontier")
                                   )
                               )
                      ),
                      tabPanel("Definitions",
                               
                               tags$div(class = "box box-solid",
                                        tags$div(class = "box-header with-border collapse",
                                                 tags$i(class = "fas fa-book-open"),
                                                 tags$h3(class = "box-title custom-box-header","The Short")
                                        ),
                                        tags$div(class = "box-body",
                                                 tags$p("* Active Premium = Investment's annualized return - Benchmark's annualized return"),
                                                 tags$p("* Alpha shows how well (or badly) a stock has performed in comparison to a benchmark index."),
                                                 tags$p("* Beta indicates how volatile a stock's price has been in comparison to the market as a whole."),
                                                 tags$p("* Correlation explains the relationship between the portfolio and the benchmark."),
                                                 tags$p("* The arithmetic mean is the simple average, or sum of a series of numbers divided by the count of that series of numbers based on the monthly returns."),
                                                 tags$p("* Geometric means takes into account the compounding effect during the calculation period based on the monthly returns."),
                                                 tags$p("* Kurtosis is a measure of the combined weight of a distribution's tails relative to the center of the distribution."),
                                                 tags$p("* UCL represents upper control limit on a control chart, and LCL represents lower control limit. It is generally +/- 3 sigma away from the mean/meadian."),
                                                 tags$p("* UCL represents upper control limit on a control chart, and LCL represents lower control limit. It is generally +/- 3 sigma away from the mean/meadian."),
                                                 tags$p("* Maximum and Minimum indicates the max and min monthly returns of the portfolio."),
                                                 tags$p("* Skewness is the measure of asymmetry in the distribution of market returns."),
                                                 tags$p("* Stdev is the rate of return on an investment portfolio and is used to measure the inherent volatility of an investment."),
                                                 tags$p("* Portfolio variance is a measure of the dispersion of returns of a portfolio and is essentially a measurement of risk."),
                                                 tags$p("* The annualized return is the geometric average of annual returns of each year over the investment period."),
                                                 tags$p("* The annualized Sharpe ratio is computed by dividing the annualized mean monthly excess return by the annualized monthly standard deviation of excess return."),
                                                 tags$p("* The Annualized Standard Deviation is the standard deviation multiplied by the square root of the number of periods in one year i.e., 12."),
                                                 tags$p("* Maximum drawdown (MDD) is a measure of an asset's largest price drop from a peak to a trough. Maximum drawdown is an indicator of downside risk over a specified time period."),
                                                 tags$p("* Gain Deviation measures the deviation of positive returns and Loss Deviation measure the deviation of negative returns."),
                                                 tags$p("* Downside deviation is a measure of downside risk that focuses on returns that fall below a minimum threshold or minimum acceptable return (MAR)."),
                                                 tags$p("* The Sortino ratio is a variation of the Sharpe ratio that only factors in downside risk."),
                                                 tags$p("* The information ratio (IR) is a measurement of portfolio returns above the returns of a benchmark, usually an index to the volatility of those returns"),
                                                 tags$p("* Tracking error is the difference in actual performance between a position (usually an entire portfolio) and its corresponding benchmark.")
                                                 # tags$p("Short"),
                                                 # tags$p("Sai Teja Reddy is currently working as a Data Scientist at one of the most reputed technology organization. Prior to that, worked as Sales Operations Analyst and Business Analyst at Uber with almost 2 years of experience. He began his carrer as a Developer and has good hands on experience with microsoft technologies. He is an Electrical graduate from National Insititute of Technology Durgapur."),
                                                 # tags$p("Long"),
                                                 # tags$p("I am a technology lover and good writer. The purpose was building programmed software which allows to automate the trading of Indian stock trades by acting upon high volumes, creating strategies and doing backtest to generate more profits."),
                                                 # br()
                                                 
                                        )
                               )
                               
                               
                                   
                        
                        
                      )
                      
                      
                      
                      
                      )
               # box(width=12,title= "Optimised Weights",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
               #     column(4,
               #            div(
               #              withSpinner( DT::dataTableOutput("port_optimised_metrics")))
               #     ),
               # ),
               # box(width=12,title= "Optimised Weights",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
               #     column(4,
               #            verbatimTextOutput("port_optimised_returns")
               #     ),
               #     column(8,
               #            verbatimTextOutput("port_optimised_weights")
               #     ),
               # ),
               # box(width=12,title= "Correlation plot",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
               #    plotOutput("correlation_plot", width = "100%",height = "900px")
               # ),
               # box(width=12,title= "Portfolio Value",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
               #    plotOutput("port_returns_plot")
               # )
      ),
      tabPanel("Real Time NSE Market",
               tabBox(id ="company_tab_real_time",
                      side = "left", width = 12,
                      height = "600px",
                      selected = "Pre Open Session Data",
                      tabPanel("Pre Open Session Data",
                               fluidRow(
                                 box(status = "primary",
                                     solidHeader = T,
                                     width = 12,

                                     tabBox(
                                       side = "left", height = "250px", width = 12,
                                       selected = "Nifty",
                                       tabPanel("Nifty",
                                                box(width=12,title= "Nifty Pre open market",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    # withLoader(DT::dataTableOutput('nifty_pre_open'),type='html',loader ='pacman')
                                                    DT::dataTableOutput('nifty_pre_open')
                                                )
                                       ),
                                       tabPanel("Nifty Bank",
                                                box(width=12,title= "Nifty Bank Pre open market",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    # withLoader(DT::dataTableOutput('nifty_bank_pre_open'),type='html',loader ='pacman')
                                                    DT::dataTableOutput('nifty_bank_pre_open')
                                                )
                                       )
                                     )
                                 ))
                      ),
                      tabPanel("Stocks Action",
                               fluidRow(
                                 box(status = "primary",
                                     solidHeader = T,
                                     width = 12,

                                     tabBox(
                                       side = "left", height = "250px", width = 12,
                                       selected = "Most Traded",
                                       tabPanel("Most Traded",
                                                box(width=12,title= "Top 10 Most traded",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    withLoader(DT::dataTableOutput('nifty_most_traded'),type='html',loader ='pacman')
                                                )
                                       ),
                                       tabPanel("Top Gainers",
                                                box(width=12,title= "Top 10 Gainers",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    withLoader(DT::dataTableOutput('nifty_top_gainers'),type='html',loader ='pacman')
                                                )
                                       ),
                                       tabPanel("Top Losers",
                                                box(width=12,title= "Top 10 Losers",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    withLoader(DT::dataTableOutput('nifty_top_losers'),type='html',loader ='pacman')
                                                )
                                       ),
                                       tabPanel("52 Week High",
                                                box(width=12,title= "New 52 Week Highs",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    withLoader(DT::dataTableOutput('nifty_52_week_high'),type='html',loader ='pacman')
                                                )
                                       ),
                                       tabPanel("52 Week Low",
                                                box(width=12,title= "New 52 Week Lows",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    withLoader(DT::dataTableOutput('nifty_52_week_low'),type='html',loader ='pacman')
                                                )
                                       )

                                     )
                                 ))
                      ),
                      tabPanel("Future Options",
                               fluidRow(
                                 box(status = "primary",
                                     solidHeader = T,
                                     width = 12,

                                     tabBox(
                                       side = "left", height = "250px", width = 12,
                                       selected = "Top Gainers",

                                       tabPanel("Top Gainers",
                                                box(width=12,title= "Futures & options top gainers",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    withLoader(DT::dataTableOutput('future_top_gainers'),type='html',loader ='pacman')
                                                )
                                       ),
                                       tabPanel("Top Losers",
                                                box(width=12,title= "Futures & options top losers",status="primary",solidHeader=TRUE , collapsible = F, collapsed = F ,

                                                    withLoader(DT::dataTableOutput('future_top_losers'),type='html',loader ='pacman')
                                                )
                                       )
                                     )
                                 ))
                      )
               )

      ),
      tabPanel("Place Orders",
               box(width=12,status="primary",solidHeader=TRUE,height = 250,
                   fluidRow(

                     column(2,
                            selectizeInput("angel_stock", label = "Company :", choices=c("BANDHANBNK-EQ","HDFCBNK-EQ"), selected = "BANDHANBNK-EQ", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                     ),
                     column(1,
                            numericInput("angel_qty", label=h6("Qty:"), value=50, min =1,max = 1000)
                     ),
                     column(2,
                            selectizeInput("angel_order_type", label = "Order Type :", choices=c("Market Order"="MARKET","Limit Order"="LIMIT","STOP Loss"="SL","STop Loss Medium"="SL-M"), selected = "Market Order", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                     ),

                     conditionalPanel(
                       condition = "input.angel_order_type == 'LIMIT'",
                       column(2,
                       numericInput("angel_limit_price", label=h6("Limit Price:"), value=0, min =0)
                       )
                     ),

                     column(2,
                            selectizeInput("angel_variety", label = "Variety :", choices=c("MIS","NRML","CNC","BO","CO"), selected = "MIS", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                     ),

                     column(2,
                            selectizeInput("angel_exchange", label = "Exchange :", choices=c("NSE","BSE"), selected = "NSE", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                     ),
                     column(2,
                            selectizeInput("angel_validity", label = "Validity :", choices=c("DAY","IOC"), selected = "DAY", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                     ),
                     column(2,
                            selectizeInput("angel_product", label = "Product :", choices=c("DELIVERY","INTRADAY","MARGIN"), selected = "INTRADAY", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                     ),
                     column(2,
                            selectizeInput("angel_call", label = "Call :", choices=c("BUY","SELL"), selected = "BUY", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                     ),
                     actionButton(inputId = "angel_action",label = "Execute",width = "280px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                     # ,
                     # column(4,
                     #        dateRangeInput("daterange", "Date range:", start = "2019-05-10", end = Sys.Date(),format = "yyyy-mm-dd")
                     # )
                   )
               ),
               # tabBox(id ="angel_broking_orders",
               #        side = "left", width = 12,
               #        height = "600px",
               #
               #
               #
               # )
      ),
      tabPanel("ML Trading",icon = icon("book-reader"), theme = shinytheme("cosmo"),
               
               box(width=12,status="primary",solidHeader=TRUE,height = 100,
                   fluidRow(
                     
                     column(3,
                            selectizeInput("ml_stock_input", label = "Nifty 50 Stocks :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","IOC.NS"), selected = "RELIANCE.NS", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10))
                     ),
                     column(1,
                            # submitButton("Run Trade",width = "100px")
                            actionButton(inputId = "ml_submit",label = "Run Trade",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                     )
                     
                   )
               ),
               hidden(
               box(id = "myTechnicalBox",width=12,status="primary",solidHeader=TRUE,height = 350,collapsible = T, collapsed = T ,
                   fluidRow(
                     column(1,
                       numericInput("ml_Volume", label=h6("Volume:"), value=10000, min =1,max = 100000000)
                     ),
                     column(1,
                            numericInput("ml_rsi", label=h6("RSI:"), value=45, min =1,max = 1000)
                     ),
                     column(1,
                            numericInput("ml_ema10", label=h6("EMA 10:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_sma10", label=h6("SMA 10:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_ema20", label=h6("EMA 20:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_sma20", label=h6("SMA 20:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_ema30", label=h6("EMA 30:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_sma30", label=h6("SMA 30:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                             numericInput("ml_ema50", label=h6("EMA 50:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_sma50", label=h6("SMA 50:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_ema100", label=h6("EMA 100:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_sma100", label=h6("SMA 100:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_ema200", label=h6("EMA 200:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_sma200", label=h6("SMA 200:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_vwma20", label=h6("VWMA 20:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_HMA9", label=h6("HMA 9:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_ATR", label=h6("ATR:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_SMI", label=h6("SMI:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_ADX", label=h6("ADX:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_Aroon", label=h6("Aroon:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_BB", label=h6("BB:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_ChaikinVol", label=h6("ChaikinVol:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_CLV", label=h6("CLV:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_MACD", label=h6("MACD:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_MFI", label=h6("MFI:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_SAR", label=h6("SAR:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_Volat", label=h6("Volat:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_turningLine", label=h6("turningLine:"), value=2000, min =1,max = 100000)
                     ),
                     column(1,
                            numericInput("ml_baseLine", label=h6("baseLine:"), value=2000, min =1,max = 100000)
                     )
                     
                     
                   )
                   
               )
               ),
               box(width=12,status="primary",solidHeader=TRUE,height = 100,
                   fluidRow(
                     column(4,
                            htmlOutput("linear_output")
                            # div(withSpinner(span(textOutput("linear_output"), style="color:red")))
                            
                            
                     ),
                     column(4,
                            div(withSpinner(htmlOutput("logistic_output")))
                            
                     )
                   )
               ),
               box(width=12,title= "Summary",status="primary", solidHeader=TRUE ,  collapsible = T, collapsed = T ,
                   column(6,
                          div(
                              withSpinner(textOutput("linear_equation")))
                          ),
                  
                   column(6,
                              withSpinner(verbatimTextOutput("linear_summary"))
                          )
               ),
               box(width=12,title= "Stocks Performance",status="primary", solidHeader=TRUE ,  collapsible = T, collapsed = T ,
                   column(8,
                          div(
                            withSpinner( DT::dataTableOutput("ml_linear_table")))
                   )
               )
               
      ),
      tabPanel("Options Chain",icon = icon("book-reader"), theme = shinytheme("cosmo"),
               box(width=12,status="primary",solidHeader=TRUE,height = 200,
                   fluidRow(
                         column(3,
                                selectizeInput("options_input", label = "Select Option :", choices=c("BANKNIFTY","NIFTY"), selected = "BANKNIFTY", multiple = FALSE,
                                               options = list(create = TRUE,maxOptions = 10))
                         ),
                         column(3,
                                selectizeInput("options_expiry", label = "Select Expiry :", choices=c("03-Feb-2022","10-Feb-2022"), selected = "03-Feb-2022", multiple = FALSE,
                                               options = list(create = TRUE,maxOptions = 10))
                         )
                   )
               ),
                   box(width=12,status="primary",solidHeader=TRUE,height = 200,
                     fluidRow(
                       
                              valueBoxOutput("options_market_price",width = 2),
                              valueBoxOutput("options_oi_pcr",width = 2),
                              valueBoxOutput("options_volume_pcr",width = 2),
                              valueBoxOutput("options_intraday_sign",width = 2)
                       
                     )
                   ),
               
                   box(width=12,title= "Options Chain",status="primary", solidHeader=TRUE ,  collapsible = T, collapsed = F ,
                       div(style = 'overflow-y:scroll;height:500px;',
                           withSpinner( DT::dataTableOutput("options_chain_data")))
                   ),
               box(width=12,title= "Options Chain Day Performance",status="primary", solidHeader=TRUE ,  collapsible = T, collapsed = T ,
                   div(style = 'overflow-y:scroll;height:500px;',
                       withSpinner( DT::dataTableOutput("options_day_history")))
               ),
               box(width=12,title= "Multi Strike Data",status="primary",solidHeader=TRUE,  collapsible = T, collapsed = T ,
                   htmltools::div(style = "display:inline-block", highchartOutput("multi_strike_data", width = 1300, height = 1000))
               ),
               box(width=12,title= "Option Greeks Calculator",status="primary",solidHeader=TRUE,  collapsible = T, collapsed = T ,
                   fluidRow(
                     column(1,
                            numericInput("options_spot_price", label=h6("SPOT:"), value=34584, min =1, step = 1,max = 100000)
                            ),
                     column(1,
                            numericInput("options_strike_price", label=h6("STRIKE:"), value=34600, min =1, step = 1,max = 100000)
                     ),
                     column(1,
                            numericInput("options_expiry_date", label=h6("Days to Expiry:"), value=4, min =1, step = 1,max = 365)
                            ),
                     column(1,
                            numericInput("options_volatility", label=h6("VOLATILITY:"), value=0.1595, min =1, step = 0.0001,max = 1)
                            ),
                     column(1,
                            numericInput("options_interest", label=h6("INTEREST%:"), value=0.03388, min =1, step = 0.0001,max = 1)
                            ),
                     column(1,
                            numericInput("options_dividend", label=h6("DIVIDEND:"), value=0, min =1, step = 0.0001,max = 1)
                     )
                     
                   ),
                   
                   box(width=12,title= "Option Greeks Value",status="primary", solidHeader=TRUE ,  collapsible = T, collapsed = T ,
                       div(style = 'overflow-y:scroll;height:500px;',
                           withSpinner( DT::dataTableOutput("options_greek_value")))
                   ),
                  
                   
               ),
               box(width=12,title= "PayOff Chart",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
                   
                   fluidRow(
                     column(2,
                     selectizeInput("bot_first_strike_price", label = "Select Strike Price :", choices=c("14800","14900","15000","15050","15100","15750","15800"), selected = "15750", multiple = FALSE,
                                    options = list(create = TRUE,maxOptions = 10)),
                     selectizeInput("bot_first_strike_side", label = "Select Side :", choices=c("CE","PE"), selected = "CE", multiple = FALSE,
                                    options = list(create = TRUE,maxOptions = 10)),
                     selectizeInput("bot_first_transaction", label = "Select Action :", choices=c("Buy","Sell"), selected = "Buy", multiple = FALSE,
                                    options = list(create = TRUE,maxOptions = 10))
                     ),
                     column(2,
                            selectizeInput("bot_second_strike_price", label = "Select Strike Price :", choices=c("14800","14900","15000","15050","15100","15750","15800","15900"), selected = "15900", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                            selectizeInput("bot_second_strike_side", label = "Select Side :", choices=c("CE","PE"), selected = "CE", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10)),
                            selectizeInput("bot_second_transaction", label = "Select Action :", choices=c("Buy","Sell"), selected = "Sell", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10))
                     ),
                     column(2,
                            
                            numericInput("bot_first_premium", label=h6("Select First Premium:"), value=87.35, min =1, step = 0.01,max = 1000),
                            numericInput("bot_scond_premium", label=h6("Select Second Premium:"), value=25.05, min =1, step = 0.01,max = 1000),
                            numericInput("bot_lot_size", label=h6("Select Lot Size:"), value=25, min =1, step = 25,max = 50000)
                     )
                   ),
                   
                   column(6,
                          plotlyOutput("options_payoff_chart")
                   ),
                   column(6,
                          DT::dataTableOutput("options_breakeven_point")
                   )
               )
               
               
               
      ),
      tabPanel("BOT Trading",icon = icon("book-reader"), theme = shinytheme("cosmo"),
               
               box(width=12,status="primary",solidHeader=TRUE,height = 200,
                   fluidRow(
                     
                     column(3,
                            selectizeInput("bot_stock_input", label = "Nifty 50 Stocks :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "UBER", multiple = TRUE,
                                           options = list(create = TRUE,maxOptions = 10)),
                            radioButtons("nifty_selection", "Select the Bucket:",
                                         c("Nifty 50" = "nifty_50",
                                           "Nifty 500" = "nifty_500",
                                           "US 30" = "us_30"),
                                         selected = "nifty_50")
                     ),
                     # column(4,
                     #        dateRangeInput("bot_daterange", "Date range:", start = "2019-05-10", end = Sys.Date(),format = "yyyy-mm-dd")
                     # ),
                     column(1,
                            selectizeInput("bot_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","4 hour"="4h","1 Day"="1d"), selected = "5 min", multiple = FALSE,
                                           options = list(maxOptions = 10))
                     ),
                     column(2,
                            # selectizeInput("bot_strategy", label = "Strategy :", choices=c("Moving Average and MACD","MACD","Super Trend","Harmonics","Supply and Demand","Brahmastra"), selected = "MA", multiple = FALSE,
                            #                options = list(create = TRUE,maxOptions = 10))
                            prettyCheckboxGroup("bot_strategy", "Strategy:",
                                              c("Cowboy" = "cowboy",
                                                "Sweths Violation" = "sweths_violation",
                                                "Reds Rocket"="reds_rocket",
                                                "Reds Brahmos"="reds_brahmos",
                                                "Blackout" = "blackout",
                                                "Gap Up"="gap_up",
                                                "Gap Down"= "gap_down",
                                                "5 Candle ABC"="abc_5_cand",
                                                "3 Candle ABC"= "abc_3_cand",
                                                "Volume Breakout"="volume_breakout",
                                                "Inside Candle"="inside_candle",
                                                "Demand And Supply Zone" = "demand_and_supply"),
                                              selected = "reds_rocket")
                     ),
                     column(2,
                            numericInput("bot_capital", label=h6("Capital:"), value=10000, min =1, step = 100,max = 100000)
                     ),
                     column(1,
                            numericInput("bot_profit", label=h6("Profit %:"), value=1, min =1, step = 0.1,max = 10)
                     ),
                     column(1,
                            numericInput("bot_loss", label=h6("Loss %:"), value=1, min =1, step = 0.1,max = 10)
                     ),
                     column(1,
                            # submitButton("Run Trade",width = "100px")
                            actionButton(inputId = "bot_submit",label = "Run Trade",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                     )
                   )
               ),
               box(
                 width=12,status="primary",solidHeader=TRUE,height = 200,
                 fluidRow(
                 column(3,
                        actionButton("report", "Generate report", icon("html5"), 
                                     style="color: black; background-color: yellow; border-color: green")
                 ),
                 column(3,
                        radioButtons("ip_technical", "Dynamic SR Levels:",
                                     c("Classical Support and Resistance" = "classical_sr",
                                       "Fibonacci Support and Resistance" = "fib_sr",
                                       "ML Support and Resistance" = "ml_sr")
                 )
                 ),
                 column(3,
                        selectizeInput("bot_stock_default", label = "Dynamic Support and Resistance Stock :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "RELIANCE.NS", multiple = FALSE,
                                       options = list(create = TRUE,maxOptions = 10))
                 )
                 )
                 
               
               ),
               box(width=12,title= "Trade Signals",status="primary", solidHeader=TRUE ,  collapsible = T, collapsed = T ,
                   div(style = 'overflow-y:scroll;height:500px;',
                       withSpinner( DT::dataTableOutput("bot_buy_and_sell")))
               ),
               box(width=12,title= "Realtime Market Charts",status="primary",solidHeader=TRUE,  collapsible = T, collapsed = T ,
                   # div(style = 'overflow-y:scroll;height:1500px;',
                   #      plotlyOutput("stocks_candle", height = 'auto', width = 'auto')
                   # )
                   htmltools::div(style = "display:inline-block", plotlyOutput("stocks_candle", width = 1300, height = 1000))
               ),
               box(width=12,title= "Backtesting Strategy",status="primary",solidHeader=TRUE ,  collapsible = T, collapsed = T ,
                   column(5,
                          div(style = 'overflow-y:scroll;',
                              withSpinner( DT::dataTableOutput("bot_backtest")))
                   ),
                   column(7,
                          actionButton(inputId = "bot_refresh",label = "Refresh Chart",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          div(style = 'overflow-y:scroll;',
                              withSpinner(
                                htmltools::div(style = "display:inline-block", plotOutput("perfPlot", width = 1300, height = 800))
                                # plotOutput("perfPlot")
                                )
                          )
                   )
               ),
               box(width=12,title= "Trade Performance",status="primary",solidHeader=TRUE ,  collapsible = T, collapsed = T ,
                   column(8,
                          div(style = 'overflow-y:scroll;',
                              withSpinner( DT::dataTableOutput("bot_performance")))
                   ),
                   column(4,
                          div(style = 'overflow-y:scroll;',
                              withSpinner( DT::dataTableOutput("bot_stats")))
                   )
               )
               
               # box(width=12,status="primary",solidHeader=TRUE,height = 800,
               #     fluidRow(
               #       column(6,
               #            DT::dataTableOutput("bot_buy_and_sell")
               #       )
               #       # ,
               #       # column(3,
               #       #        htmlOutput("bot_strategy_output")
               #       # )
               #     )
               # )
               
      ),
      tabPanel("Company Analysis",icon = icon("book-reader"), theme = shinytheme("cosmo"),
               
               box(width=12,status="primary",solidHeader=TRUE,height = 100,
                   fluidRow(
                     
                     column(4,
                            selectizeInput("stock_input", label = "Company :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "UBER", multiple = FALSE,
                                           options = list(create = TRUE,maxOptions = 10))
                     ),
                     column(4,
                            dateRangeInput("daterange", "Date range:", start = "2019-05-10", end = Sys.Date(),format = "yyyy-mm-dd")
                     )
                   )
               ),
               tabBox(id ="company_tab",
                      side = "left", width = 12,
                      height = "600px",
                      selected = "Overview",
                      tabPanel("Overview",
                               box(width=12,title= "Current Value",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   # div(style = 'overflow-x: scroll', tableOutput("current_status"))
                                   withLoader(dataTableOutput('current_status'),type='html',loader ='pacman')
                               )
                               
                      ),
                      tabPanel("Valuation",
                               
                               infoBoxOutput("market_cap"),
                               infoBoxOutput("current_PE"),
                               infoBoxOutput("industry_PE"),
                               infoBoxOutput("p_b"),
                               infoBoxOutput("SC_TTM"),
                               infoBoxOutput("FV"),
                               infoBoxOutput("DIVPR"),
                               infoBoxOutput("P_C"),
                               infoBoxOutput("DELV"),
                               infoBoxOutput("AvgDelVolPer_3day"),
                               infoBoxOutput("AvgDelVolPer_5day"),
                               infoBoxOutput("AvgDelVolPer_8day")
                               
                      ),
                      tabPanel("Price & Returns",
                               
                               box(width=12,title= "Price Chart",status="primary",height = 400, solidHeader=TRUE ,  collapsible = T, collapsed = F ,
                                   div(style = 'overflow-y:scroll;height:400px;',
                                       withSpinner(highchartOutput("stocks_plot")))
                               ),
                               box(width=12,title= "Returns",status="primary", height = 400 ,solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   div(style = 'overflow-y:scroll;height:400px;',
                                       withSpinner(highchartOutput("stocks_returns")))
                               ),
                               
                      ),
                      tabPanel("Historic Data",
                               
                               box(width=12,title= "Price Data",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   withLoader( dataTableOutput('stocks_data'),type='html',loader ='pacman')
                               )
                               
                      ),
                      
                      tabPanel("Fundamental Analysis",
                               
                               fluidRow(
                                 
                                 column(4,
                                        selectizeInput("time_period", label = "Time Period :", choices=c("Quarterly","Half Yearly","Nine Months","Yearly"), selected = "Quarterly", multiple = FALSE)
                                 ),
                                 column(4,
                                        selectizeInput("stock_category", label = "Category :", choices=c("Consolidated","Standalone"), selected = "Consolidated", multiple = FALSE)
                                 )
                               ),
                               
                               fluidRow(
                                 box(width=12,title= "Income Statements",status="primary", height = 500 ,solidHeader=TRUE , collapsible = T, collapsed = F ,
                                     withSpinner(highchartOutput("income_statements_plot"))
                                 ),
                                 box(
                                   width = 12,title = "Income Statements", status="primary",DT::dataTableOutput('key_stats_income'),solidHeader = TRUE, collapsible = T, collapsed = F
                                 )
                                 ,
                                 box(
                                   width = 12,title = "Balance Sheets", status="primary",DT::dataTableOutput('key_stats_balance'),solidHeader = TRUE, collapsible = T, collapsed = T
                                 )
                                 ,
                                 box(
                                   width = 12,title = "Cash Flows", status="primary",DT::dataTableOutput('key_stats_cash_flows'),solidHeader = TRUE, collapsible = T, collapsed = T
                                 )
                               )
                               
                      ),
                      tabPanel("Technical",
                               
                               box(width=12,status="primary",titile= "Bollinger Bands, Moving Average and MACD", solidHeader=TRUE,height = 850, collapsible = T, collapsed = F ,
                                   fluidRow(
                                     withSpinner(plotlyOutput("candle_1"))
                                   )
                               )
                               
                      ),
                      tabPanel("SWOT Analysis",
                               # fluidRow(
                               #   box(
                               #     width = 6,title = "valuation", status="primary",DT::dataTableOutput('input_valuation'),solidHeader = TRUE, collapsible = T, collapsed = T
                               #   )
                               # ),
                               fluidRow(
                                 box(
                                   width = 6,title = "Strengths", status="primary",DT::dataTableOutput('input_strength'),solidHeader = TRUE, collapsible = T, collapsed = F
                                 ),
                                 box(
                                   width = 6,title = "Weakness", status="primary",DT::dataTableOutput('input_weakness'),solidHeader = TRUE, collapsible = T, collapsed = F
                                 )
                               ), 
                               fluidRow(
                                 box(
                                   width = 6,title = "Opportunities", status="primary",DT::dataTableOutput('input_opportunities'),solidHeader = TRUE, collapsible = T, collapsed = T
                                 ),
                                 box(
                                   width = 6,title = "Threats", status="primary",DT::dataTableOutput('input_threats'),solidHeader = TRUE, collapsible = T, collapsed = T
                                 )
                               )
                      ),
                      tabPanel("News",
                               fluidRow(
                                 box(
                                   width = 12,title = "Economic Times Latest News", status="primary",DT::dataTableOutput('economic_latest'),solidHeader = TRUE, collapsible = T, collapsed = T
                                   
                                 )
                               ),
                               fluidRow(
                                 box(
                                   width = 12,title = "Economic Times News", status="primary",DT::dataTableOutput('economic_news'),solidHeader = TRUE, collapsible = T, collapsed = F
                                 )
                               ),
                               fluidRow(
                                 box(
                                   width = 12,title = "Money Control News", status="primary",DT::dataTableOutput('money_control_news'),solidHeader = TRUE, collapsible = T, collapsed = T
                                 )
                               ),
                               fluidRow(
                                 box(
                                   width = 12,title = "Livemint News", status="primary",DT::dataTableOutput('live_mint_news'),solidHeader = TRUE, collapsible = T, collapsed = T
                                 )
                               )
                               
                      ),
                      tabPanel("Peers",
                               box(width=12,title= "Peers Data",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   div(style = 'overflow-x: scroll', DT::dataTableOutput('peers_data'))
                               ),
                               box(width=12,title= "Returns",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   plotlyOutput("comparision_plot")
                               ),
                               numericInput(inputId = "rfrInput", label = "Risk-free rate, %", value = 6.5, 
                                            min = 0.0, max = 100.0, step = 0.25, width = "25%"),
                               box(width=12,title= "Returns",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                   
                                   DT::dataTableOutput(outputId = "stocksTable", width = "100%")
                               )
                      ),
                      tabPanel("Future Prediction",
                               
                               fluidRow(
                                 
                                 box(
                                   title = "Forecast with ARIMA model Chart",
                                   status = "primary",
                                   width = 8,
                                   highchartOutput("forecasting_plot"),
                                   # plotOutput("auto.arima", height = 350),
                                   # highchartOutput("forecasting_plot"),
                                   height = 500
                                 ),
                                 box(
                                   title = "Forecast with ARIMA model Data",
                                   
                                   width = 4,
                                   tableOutput("auto.arima1"),
                                   height = 500
                                 )
                                 
                               )
                               
                      ),
                      tabPanel("Twitter sentiment",
                               fluidRow(box(status = "primary",
                                            solidHeader = T,
                                            width = 12,
                                            
                                            fluidRow(
                                              
                                              # column(3,
                                              #        selectInput("input_stock_news", 
                                              #                    label = "Company :",
                                              #                    choices = list("RELIANCE","IDEA","HDFC","IDBI", "SINTEX","SUZLON","YESBANK","ZeeEntertainment"),
                                              #                    selected = "RELIANCE")
                                              #        
                                              # )
                                              column(3,
                                                     textInput("twitter_text1", "Enter search word 1", "@relianceindltd")
                                              ),
                                              column(3,
                                                     textInput("twitter_text2", "Enter search word 2", "#reliance")
                                              ),
                                              column(3,
                                                     numericInput("twitter_count", label=h6("Number of tweets:"), value=1750, min =1, step = 10,max = 1000),
                                              )
                                            ),
                                            fluidRow(
                                              column(6, DT::dataTableOutput('twitter_sentiment')),
                                              column(6, plotly::plotlyOutput(outputId = "twitter_sentiment_plot")  )
                                            )
                                            
                               ))
                               
                      ),
                      tabPanel("Back Strategy",
                               
                               fluidRow(
                                 plotOutput('strategy_back_test_plot')
                               ),
                               fluidRow(
                                 column(4,
                                        h4("Dataset"),
                                        helpText("Specify the symbol, date range and the basic plot style."),
                                        br(),
                                        checkboxInput("strategy_back_test_addVo", "Plot Volume",
                                                      value = FALSE),
                                        selectInput("strategy_back_test_plot_select", label = h5("Plot Style"),
                                                    choices = list("Lin" = 1, "Candlestick" = 2), selected = 1),
                                        br()
                                        # ,
                                        # submitButton('Submit')
                                        # actionButton(inputId = "strategy_back_test_submit",label = "Submit",width = "280px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                 ),
                                 column(4,
                                        h4("Technical Indicator"),
                                        helpText("Visualize Bollinger bands and the orders generated by it. The order are generated
                    based on the assumption that the price bouncing back when it touches the edge of the
                    Bollinger band."),
                                        checkboxInput("strategy_back_test_addBB", "Add Bollinger Bands", value = FALSE),
                                        sliderInput("strategy_back_test_BB_win", label=h6("Bollinger's Window Size:"), min = 14, max = 60, value = 20, step=1),
                                        sliderInput("strategy_back_test_sd", label=h6("Multiple of Standard Deviation:"), min = 1.0, max = 3.2, value = 2.0, step=0.1),
                                        br(),
                                        checkboxInput("strategy_back_test_processed", "Generate order", value = FALSE),
                                        checkboxInput("strategy_back_test_stop_profit", "Stop profit?", value = TRUE),
                                        numericInput("strategy_back_test_stop_day", label=h6("Max Days of Holding:"), value=1000, min =1, step = 1),
                                        sliderInput("strategy_back_test_stop_trig", label=h6("Stop Sell Trigger:"), min = 0, max = 0.15, value = 0.0, step=0.001)
                                 ),
                                 column(4,
                                        h4('Modifying with MACD'),
                                        helpText('Our assumption of bouncing price may be invalid when trend is strong. MACD helps to
                    identify such trend and to adjust strategy accordingly. Tuning the parameter below
                    to catch trends with different period.'),
                                        checkboxInput("strategy_back_test_addMACD", "Plot MACD", value = FALSE),
                                        sliderInput("strategy_back_test_macd_fast", label=h6("Fast Indicator:"), min = 7, max = 60,value = 12, step=1),
                                        sliderInput("strategy_back_test_macd_slow", label=h6("Slow Indicator:"), min = 20, max = 150,value = 26, step=1),
                                        sliderInput("strategy_back_test_macd_signal", label=h6("Signal:"), min = 5, max = 60, value = 9, step=1),
                                        checkboxInput("strategy_back_test_modi_macd", "Modify the order by MACD (Stop sell trigger in the left column is necessary when modifying with MACD).", value = FALSE)
                                 )
                               )
                               
                      ),
                      tabPanel("Intraday",
                               fluidRow(box(status = "primary",
                                            solidHeader = T,
                                            width = 12,
                                            box(width=12,status="primary",solidHeader=TRUE,height = 75,
                                                fluidRow(
                                                  
                                                  # column(4,
                                                  #        selectizeInput("candle_stick_stock_input", label = "Company :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL"), selected = "UBER", multiple = FALSE,
                                                  #                       options = list(create = TRUE,maxOptions = 10))
                                                  # ),
                                                  column(4,
                                                         selectizeInput("candle_stick_range", label = "Range :", choices=c("1 Day"="1d","5 Days"="5d","1 Month"="1mo","6 Months"="6mo","YTD"="ytd","1 Year"="1y","5 Years"="5y","Max"="max"), selected = "5 Days", multiple = FALSE,
                                                                        options = list(maxOptions = 10))
                                                  )
                                                )
                                            )
                                            ,
                                            # box(width=12,status="primary",solidHeader=TRUE,height = 500,
                                            #     fluidRow(
                                            #       plotlyOutput("stocks_candle")
                                            #     )
                                            # ),
                                            box(width=12,status="primary",solidHeader=TRUE,height = 600,
                                                fluidRow(
                                                  withSpinner(highchartOutput("stocks_candle_realtime"))
                                                )
                                            ),
                                            box(width=12,status="primary",solidHeader=TRUE,height = 600,
                                                fluidRow(
                                                  withSpinner(plotOutput("intraday_support_and_resistance", height=300))
                                                )
                                            )
                                            ,
                                            box(width=12,title= "Real time table",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
                                                fluidRow(
                                                  withLoader( DT::dataTableOutput('stocks_candle_realtime_data'),type='html',loader ='pacman')
                                                )
                                            ),
                                            box(width=12,title= "Buy Calls",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                                fluidRow(
                                                  column(6,
                                                        withLoader( DT::dataTableOutput('stocks_buy_call'),type='html',loader ='pacman')
                                                  ),
                                                  column(6,
                                                        withLoader( DT::dataTableOutput('stocks_sell_call'),type='html',loader ='pacman')  
                                                  )
                                                )
                                            )
                               ))
                               
                      )
                      # tabPanel("Risk & Reward Calculator",
                      #          fluidRow(box(status = "primary",
                      #                       solidHeader = T,
                      #                       width = 12,
                      #                       box(width=12,status="primary",solidHeader=TRUE,height = 100,
                      #                           fluidRow(
                      #                             column(3,
                      #                                    selectizeInput("market_direction", label = "Market Direction :", choices=c("Upside" = "1","Downwards"= "-1"), selected = "Upside", multiple = FALSE)
                      #                             ),
                      #                             column(2,
                      #                                    numericInput("stop_loss", label = "Stop Loss :",820.45)
                      #                             ),
                      #                             column(2,
                      #                                    numericInput("entry_point", label = "Entry Point :",821.55)
                      #                             ),
                      #                             column(2,
                      #                                    numericInput("target", label = "Target :",826.81)
                      #                             ),
                      #                             column(2,
                      #                                    numericInput("capital", label = "Capital :",10000)
                      #                             )
                      #                           )
                      #                       )
                      #                       ,
                      #                       box(width=12,status="primary",solidHeader=TRUE,height = 500,
                      #                           fluidRow(
                      #                             column(3,
                      #                                 textOutput("Risk_reward_ratio")
                      #                             ),
                      #                             column(3,
                      #                                 textOutput("Quantity")
                      #                             ),
                      #                             column(3,
                      #                                    textOutput("Profit_book")
                      #                             ),
                      #                             column(3,
                      #                                    textOutput("Loss_book")
                      #                             )
                      #                           )
                      #                       )
                      #          ))
                      #          
                      # )
                      
               )
      ),
      tabPanel("Home",icon = icon("home"),
               slickROutput("slickr",width = "100%", height = "500px"),
               hr(),
               hr(),
               box(width=12,status="primary",solidHeader=TRUE,height = 100,
                   column(12, align="center",
                   uiOutput(outputId = "Disclaimer")
                   ),
                   column(12, align="center",
                   uiOutput(outputId = 'follow')
                   ),
               tags$head(tags$style("#info{color: white;
                                 font-size: 15px;
                         font-style: italic;
                         }"
               )
               ),
               fluidRow(
                 column(6, align = "right",
                        tags$a(href="https://www.linkedin.com/in/sai-teja-reddy-8b8016139/", "LinkedIn", target="_blank")
                 ),
                 column(6, align = "left",
                        tags$a(href="https://www.instagram.com/sai_teja_reddy/?hl=en", "Instagram", target="_blank")
                 )
               )
               )
      )



      

      
      
      
      
    )
)
# )
# ),
# tags$footer(
#   textOutput('Disclaimer'),
#   textOutput('follow'),
#   tags$head(tags$style("#info{color: white;
#                                  font-size: 15px;
#                          font-style: italic;
#                          }"
#   )
#   ),
#   fluidRow(
#   column(6, align = "right",
#   tags$a(href="https://www.linkedin.com/in/sai-teja-reddy-8b8016139/", "LinkedIn", target="_blank")
#   ),
#   column(6, align = "left",
#          tags$a(href="https://www.instagram.com/sai_teja_reddy/?hl=en", "Instagram", target="_blank")
#          )
#   ),
#   
# 
# align = "center", style = "
#               position:absolute;
#               bottom:0;
#               width:100%;
#               height:75px;   /* Height of the footer */
#               color: white;
#               padding: 10px;
#               background-color: black;
#               z-index: 1000;"
# )
)