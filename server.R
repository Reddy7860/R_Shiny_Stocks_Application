
source(paste(getwd(),"/rscriptfortest.R" ,sep = ""))
source(paste(getwd(),"/tradedynamic.R", sep = ""))
source(paste(getwd(),"/helpers.R", sep = ""))

server <- function(input, output,session) {
  
  login_params = list(api_key = 'LPUVlRxd')
  
  login_object = smartapi::create_connection_object(login_params)
  
  session_data <- smartapi::generate_session(login_object,"J95213","start@123")
  
  
  ####################################### Functions   #######################################################
  
  getYahooSymbols <- function(){
    NSE_List <- read.csv(paste0(getwd(),"/data/NSE_Stocks_List.csv", sep = ""))
    NSE_List <- as.data.frame(NSE_List)
    
    NSE_Tickers <- NSE_List$Yahoo.Symbol
    
    return(NSE_Tickers)
  }
  
  get_twitter_raw <- function(){
    # ## store api keys (these are fake example values; replace with your own keys)
    api_key <- "EcwgntjhzjOrbQPoEP7Fb3CJ0"
    api_secret_key <- "6QQeg4DbODU4HwlfgcmLR9ClH1QqTUqbyZ909CxidRcl25eSth"
    access_token <- "2551793106-DLgebg9ti6dIVaMp4Nir8wN73pVeXyBA8ayKfMB"
    access_token_secret <- "wQiRRT9hy503EDH60W0M9llIJs95rCt2CCfuU3QiZkvdK"
    
    #Setup Twitter
    setup_twitter_oauth(consumer_key = api_key,
                        consumer_secret = api_secret_key,
                        access_token = access_token,
                        access_secret = access_token_secret)
    
    numberOfTweets <- as.numeric(input$twitter_count)
    
    tweets <- searchTwitter(searchString=as.character(input$twitter_text1), n = numberOfTweets, lang="en")
    tweets2 <- searchTwitter(searchString=as.character(input$twitter_text2), n = numberOfTweets, lang="en")
    
    tweetsDF <- twListToDF(tweets)
    tweetsDF2 <- twListToDF(tweets2)
    tweetsFullDF <- rbind(tweetsDF, tweetsDF2)
    
    return(tweetsFullDF)
  }
  
  
  get_twitter_data <- function(){
    
    tweetsFullDF <- get_twitter_raw()
    
    #Convert to dataframe and encode to native
    x <- tweetsFullDF
    x$text <- enc2native(x$text)
    
    #Clean text
    x$text <- gsub("^[[:space:]]*","",x$text) # Remove leading whitespaces
    x$text <- gsub("[[:space:]]*$","",x$text) # Remove trailing whitespaces
    x$text <- gsub(" +"," ",x$text) #Remove extra whitespaces
    x$text <- gsub("'", "%%", x$text) #Replace apostrophes with %%
    x$text <- iconv(x$text, "latin1", "ASCII", sub="") # Remove emojis
    x$text <- gsub("<(.*)>", "", x$text) #Remove Unicodes like <U+A>
    x$text <- gsub("\\ \\. ", " ", x$text) #Replace orphaned fullstops with space
    x$text <- gsub("  ", " ", x$text) #Replace double space with single space
    x$text <- gsub("%%", "\'", x$text) #Change %% back to apostrophes
    x$text <- gsub("https(.*)*$", "", x$text) #Remove tweet URL
    x$text <- gsub("\\n", "-", x$text) #Replace line breaks with "-"
    x$text <- gsub("--", "-", x$text) #Remove double "-" from double line breaks
    x$text <- gsub("&amp;", "&", x$text) #Fix ampersand &
    x$text[x$text == " "] <- "<no text>"
    
    for (i in 1:nrow(x)) {
      if (x$truncated[i] == TRUE) {
        x$text[i] <- gsub("[[:space:]]*$","...",x$text[i])
      }
    }
    
    #Select desired column
    cleanTweets <- x %>% 
      select("text")
    
    #Analyze sentiment
    sentiment <- analyzeSentiment(cleanTweets)
    #Extract dictionary-based sentiment according to the QDAP dictionary
    sentiment2 <- sentiment$SentimentQDAP
    #View sentiment direction (i.e. positive, neutral and negative)
    sentiment3 <- convertToDirection(sentiment$SentimentQDAP)
    
    #Extract and convert 'date' column
    date <- x$created
    date <- str_extract(date, "\\d{4}-\\d{2}-\\d{2}")
    date <- as.Date(date)
    date <- as.Date(date, format = "%m/%d/%y")
    
    #Create new dataframe with desired columns
    df <- cbind(cleanTweets, sentiment2, sentiment3, date)
    #Remove rows with NA
    df <- df[complete.cases(df), ]
    
    
    #Calculate the average of daily sentiment score
    df2 <- df %>% 
      group_by(date) %>%
      summarize(meanSentiment = mean(sentiment2, na.rm=TRUE))
    
    
    DT::datatable(df2, editable = TRUE)
    
    #Get frquency of each sentiment i.e. positive, neutral, and negative  
    freq <- df %>% 
      group_by(date,sentiment3) %>% 
      summarise(Freq=n())
    
    return(freq)
  }
  
  get_income_statement <- function(stock){
    for (stocknum in 1:length(stock)) {
      tryCatch(
        {
          # Income Statement
          url <- "https://finance.yahoo.com/quote/"
          url <- paste0(url,stock[stocknum],"/financials?p=",stock[stocknum])
          wahis.session <- html_session(url)
          
          nodes <- wahis.session %>%
            html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[4]//span')
          
          yh_data <- nodes %>%
            xml_text() %>%
            gsub(pattern = ',', replacement = '')
          colnums <- 1:6
          col_nms <- yh_data[colnums]
          yh_data <- yh_data[-colnums]
          
          lab_inds <- nodes %>%
            html_attr(name = 'class') == "Va(m)"
          lab_inds[is.na(lab_inds)] <- FALSE
          
          lab_inds <- lab_inds[-colnums]
          data <- matrix(NA, nrow = sum(lab_inds), ncol = 5, dimnames = list(yh_data[lab_inds], col_nms[-1]))
          row_num <- 1
          for (i in 2:(length(lab_inds)-4)) {
            t_ind <- !lab_inds[i:(i+4)]
            if (sum(t_ind) == 5) {
              data[row_num, 1:5] <- as.numeric(yh_data[i:(i+4)])
            }
            if (lab_inds[i]) {
              row_num <- row_num+1
            }
          }
          
          temp1 <- as.data.frame(data)
          return(temp1)
        }
      )
    }
    
  }
  
  get_balance_sheet <- function(stock){
    for (stocknum in 1:length(stock)) {
      tryCatch(
        {
          # Balance Sheet
          url <- "https://finance.yahoo.com/quote/"
          url <- paste0(url,stock[stocknum],"/balance-sheet?p=",stock[stocknum])
          wahis.session <- html_session(url)
          
          nodes <- wahis.session %>%
            html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[4]/div[1]/div[1]//span')
          
          yh_data <- nodes %>%
            xml_text() %>%
            gsub(pattern = ',', replacement = '')
          
          colnums <- 1:5
          col_nms <- yh_data[colnums]
          yh_data <- yh_data[-colnums]
          
          
          
          lab_inds <- nodes %>%
            html_attr(name = 'class') == "Va(m)"
          
          lab_inds[is.na(lab_inds)] <- FALSE
          
          lab_inds <- lab_inds[-colnums]
          data <- matrix(NA, nrow = sum(lab_inds), ncol = 4, dimnames = list(yh_data[lab_inds], col_nms[-1]))
          row_num <- 1
          for (i in 2:(length(lab_inds)-3)) {
            t_ind <- !lab_inds[i:(i+3)]
            if (sum(t_ind) == 4) {
              data[row_num, 1:4] <- as.numeric(yh_data[i:(i+3)])
            }
            if (lab_inds[i]) {
              row_num <- row_num+1
            }
          }
          
          temp2 <- as.data.frame(data)
          return(temp2)
        }
      )
    }
    
  }
  
  get_cash_flows <- function(stock){
    # browser()
    for (stocknum in 1:length(stock)) {
      tryCatch(
        {
          # Cash Flow
          url <- "https://finance.yahoo.com/quote/"
          url <- paste0(url,stock[stocknum],"/cash-flow?p=",stock[stocknum])
          # print(url)
          wahis.session <- html_session(url)
          nodes <- wahis.session %>%
            html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/div[1]/div[1]//span')
          
          yh_data <- nodes %>%
            xml_text() %>%
            gsub(pattern = ',', replacement = '')
          colnums <- 1:6
          col_nms <- yh_data[colnums]
          yh_data <- yh_data[-colnums]
          
          
          lab_inds <- nodes %>%
            html_attr(name = 'class') == "Va(m)"
          lab_inds[is.na(lab_inds)] <- FALSE
          
          lab_inds <- lab_inds[-colnums]
          data <- matrix(NA, nrow = sum(lab_inds), ncol = 5, dimnames = list(yh_data[lab_inds], col_nms[-1]))
          row_num <- 1
          for (i in 2:(length(lab_inds)-4)) {
            t_ind <- !lab_inds[i:(i+4)]
            if (sum(t_ind) == 5) {
              data[row_num, 1:5] <- as.numeric(yh_data[i:(i+4)])
            }
            if (lab_inds[i]) {
              row_num <- row_num+1
            }
          }
          
          temp3 <- as.data.frame(data)
          return(temp3)
        }
      )
    }
    
  }
  
  ######################################## GLOBAL VARIABLES   #############################################
  
  stocks_info <- list()
  
  stocks_valuation <- list()
  
  
  
  ######################################## Observe Events #################################################
  
  observe({
    # browser()
    NSE_Tickers <- getYahooSymbols()
    
    NSE_Tickers <- sample(NSE_Tickers)
    
    updateSelectizeInput(
      session,
      "stock_input",
      label = "Company :",
      choices = NSE_Tickers,
      selected = "RELIANCE.NS",
      options = list(),
      server = FALSE
    )
    
  })
  
  observe({
    # browser()
    NSE_Tickers <- getYahooSymbols()
    
    NSE_Tickers <- c("All",NSE_Tickers)
    
    # NSE_Tickers <- sample(NSE_Tickers)
    
    updateSelectizeInput(
      session,
      "bot_stock_input",
      label = "Company :",
      choices = NSE_Tickers,
      selected = "All",
      options = list(),
      server = FALSE
    )
    
  })
  
  observe({
    # browser()
    if(input$tabs == "Tips & Tricks"){
      tips_data <- read.csv(paste0(getwd(),"/data/Stock_Market_Tips.csv", sep = ""))
      # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
      
      rand_row <- sample(1:nrow(tips_data), 1)
      
      
      tip <- tips_data[as.numeric(rand_row),2]
      
      introjs(
        session,
        options = list(
          steps = list(
            list(
              element = "#tour_1",
              intro = tip
            )
          )
        )
      )
    }
    
  
    
  })
  
  observe({
    
    # browser()
    
    if(input$company_tab == "Valuation"){
      
      if(input$stock_input != ""){
       
        
        money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
        # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
        
        row_number <- which(grepl(input$stock_input, money_control_data$Company))
        
        
        money_control_url <- paste("https://priceapi.moneycontrol.com/pricefeed/nse/equitycash/",money_control_data[row_number,5],sep="")
        
        # money_control_url <- "https://priceapi.moneycontrol.com/pricefeed/nse/equitycash/RI"
        
        stocks_valuation <<- fromJSON(money_control_url)
      }
      
      
      
    }
  })
  
  observe({
    
    # browser()
    
    if(input$company_tab == "Twitter sentiment"){
      
      if(input$stock_input != ""){
        
        
        twitter_data <- read.csv(paste0(getwd(),"/data/Twitter_Data.csv", sep = ""))
        
        row_number <- which(grepl(input$stock_input, twitter_data$Company))
        
        hashtag_1 <- twitter_data[row_number,2]
        
        updateTextInput(session, "twitter_text1", value = paste(hashtag_1))
      
        }
      
      
      
    }
  })
  
  observe({
    
    # browser()
    
    if(input$company_tab == "Twitter sentiment"){
      
      if(input$stock_input != ""){
        
        
        twitter_data <- read.csv(paste0(getwd(),"/data/Twitter_Data.csv", sep = ""))
        
        row_number <- which(grepl(input$stock_input, twitter_data$Company))
        
        hashtag_2 <- twitter_data[row_number,3]
        
        updateTextInput(session, "twitter_text2", value = paste(hashtag_2))
        
      }
      
      
      
    }
  })
  
  
  
  observeEvent(input$angel_action,{
    
    # browser()
    
    
    
    login_params = list(api_key = 'LPUVlRxd')
    
    login_object = create_connection_object(login_params)
    
    session_data <- generate_session(login_object,"J95213","start@123")
    
    limit_price = 0
    
    if(input$angel_order_type == "LIMIT"){
      limit_price = as.numeric(input$angel_limit_price)
    }
    
    order_place <- place_order(object = session_data,
                               variety= "NORMAL",
                               tradingsymbol= as.character(input$angel_stock),
                               symboltoken= "2263",
                               transactiontype= as.character(input$angel_call),
                               exchange= as.character(input$angel_exchange),
                               ordertype= as.character(input$angel_order_type),
                               producttype= as.character(input$angel_product),
                               duration= as.character(input$angel_validity),
                               price= limit_price,
                               squareoff= "0",
                               stoploss= "0",
                               quantity= as.numeric(input$angel_qty)
    )
    
    
    
    
  })
  
  ######################################## Reactive functions #############################################
  
  
  StocksData <- reactive({
    
    stocks_data <- getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE)
    
    stocks_data_mm <- subset(stocks_data, index(stocks_data) >= as.Date(input$daterange[1]))
    
    
    stocks_data_mm10 <- rollmean(stocks_data_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
    stocks_data_mm30 <- rollmean(stocks_data_mm[,6], 30, fill = list(NA, NULL, NA), align = "right")
    
    stocks_data_mm$mm10 <- coredata(stocks_data_mm10)
    stocks_data_mm$mm30 <- coredata(stocks_data_mm30)
    
    stocks_data_mm
    
  })
  
  GetCurrentValue <- reactive({
    # browser()
    what_metrics <- yahooQF(c("Trade Time",
                              "Previous Close",
                              "Last",
                              "Change",
                              "% Change",
                              "High",
                              "Low",
                              "Open",
                              "Bid",
                              "Ask",
                              "Days Range",
                              "Days Range (Real-time)",
                              "52-week Low",
                              "52-week High",
                              "Volume",
                              "Average Daily Volume",
                              "Market Capitalization",
                              "P/E Ratio",
                              "P/E Ratio (Real-time)",
                              "EPS Estimate Next Year",
                              "EPS Estimate Next Quarter",
                              "Price/EPS Estimate Current Year",
                              "Price/EPS Estimate Next Year",
                              "Price/Sales",
                              "PEG Ratio",
                              "Dividend Yield",
                              "1 yr Target Volume",
                              "Earnings/Sh Error Indication (returned for symbol changed / invalid)"
    ))
    
    
    
    tickers <- input$stock_input
    # Not all the metrics are returned by Yahoo.
    metrics <- getQuote(paste(tickers, sep="", collapse=";"), what=what_metrics)
    
    #Add tickers as the first column and remove the first column which had date stamps
    metrics <- data.frame(Symbol=tickers, metrics[,2:length(metrics)])
    # browser()
    colnames(metrics) <- c("Symbol","Previous Close","Change","Open","Bid","Ask","52 Weeks Low","52 Weeks High","Volume","Average Volume","Market Capetilization","P/E Ratio","Price EPS Estimate Next Year","Dividend Yield")
    
    metrics$`Previous Close` = round(metrics$`Previous Close`,2)
    metrics$`Change` = round(metrics$`Change`,2)
    metrics$`Open` = round(metrics$`Open`,2)
    metrics$`Bid` = round(metrics$`Bid`,2)
    metrics$`Ask` = round(metrics$`Ask`,2)
    metrics$`52 Weeks Low` = round(metrics$`52 Weeks Low`,2)
    
    metrics$`52 Weeks High` = round(metrics$`52 Weeks High`,2)
    metrics$`Volume` = round(metrics$`Volume`,2)
    metrics$`Average Volume` = round(metrics$`Average Volume`,2)
    metrics$`Market Capetilization` = round(metrics$`Market Capetilization`,2)
    metrics$`P/E Ratio` = round(metrics$`P/E Ratio`,2)
    metrics$`Price EPS Estimate Next Year` = round(metrics$`Price EPS Estimate Next Year`,2)
    metrics$`Dividend Yield` = round(metrics$`Dividend Yield`,2)
    
    metrics <- t(metrics)
    
    df <- cbind(Name = rownames(metrics), metrics)
    rownames(df) <- 1:nrow(df)
    colnames(df) <- c("Name","Value")
    
    definitions <- c("The ticker symbol as per Yahoo Finance",
                     "The last price reported as traded during a given time period",
                     "The difference between the current price and previous day settlement price",
                     "The price at which the trade opens in the market when trading begins",
                     "The highest price an investor is willing to pay for a share",
                     "The lowest price at which a shareholder is willing to part with shares",
                     "The 52 weeks low is the lowest price at which a security has traded during the time period",
                     "The 52 weeks high is the highest price at which a security has traded during the time period",
                     "The number of shares of a security traded during a given period of time",
                     "Average daily trading volume (ADTV) is the average number of shares traded within a day in a given stock",
                     "The aggregate valuation of the company based on its current share price and the total number of outstanding stocks",
                     "The price-earnings ratio (P/E ratio) relates a company's share price to its earnings per share",
                     "The forward P/E estimates the relative value of the earnings",
                     "This is a financial ratio (dividend/price) that shows how much a company pays out in dividends each year relative to its stock price"
    )
    df <- cbind(df, Definitions = definitions)
    df
  })
  
  read_html_file <- reactive({
    # browser()
    money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
    # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
    
    row_number <- which(grepl(input$stock_input, money_control_data$Company))
    
    money_control_url = paste("https://www.moneycontrol.com/swot-analysis",money_control_data[row_number,2],money_control_data[row_number,3],"strength", sep="/")
    
    print(money_control_url)
    
    wp = xml2::read_html(money_control_url)
    
    total_str <- rvest::html_text(rvest::html_nodes(wp, '.tab-content'))
    modified_str <- str_replace_all(total_str[3], "[\r\t]" , "")
    modified_str
  })
  
  economic_times_latest <- reactive({
    # browser()
    
    # read.csv(paste(getwd(),"/date/Economic_Times_Tickers.csv", sep = ""))
    economic_data <- read.csv(paste0(getwd(),"/data/Economic_Times_Tickers.csv", sep = ""))
    row_number <- which(grepl(input$stock_input, economic_data$Company))
    economic_url = paste("https://economictimes.indiatimes.com",economic_data[row_number,2],"stocks",economic_data[row_number,3], sep="/")
    economic_url = paste(economic_url,".cms",sep="")
    #url = "https://economictimes.indiatimes.com/yes-bank-ltd/stocks/companyid-16552.cms"
    
    news_data <- read_html(economic_url)
    
    # total_str <- rvest::html_text(rvest::html_nodes(news_data, '.flr'))
    # 
    # modified_str <- str_replace_all(total_str[grep("News & Updates", total_str)], "[\r\t\n]" , "")
    # 
    # final_news <- substring(modified_str,1,str_locate_all(pattern ='IST', modified_str)[[1]][40,2])
    # 
    # final_news <- str_replace_all(final_news, "IST" , "\n")
    
    total_str <- rvest::html_text(rvest::html_nodes(news_data, '.s_container'))
    
    modified_str <- total_str[5]
    
    final_news <- gsub(" News ", " news ", modified_str)
    
    final_news <- gsub("News", "\n", final_news)
    
    # final_news <- paste(" ",final_news, sep = "\n")
    # 
    # final_news <- paste(final_news, " ", sep = "\n")
    
    # final_news <- read.table(text=final_news, sep='\n',skip = 1, col.names=c('content'))
    news_list <- strsplit(final_news, "\n")
    
    news_data <- as.data.frame(news_list)
    
    colnames(news_data) <- "Headlines"
    
    final_news <- news_data[1:3,]
    
    # final_news <- news_list[[1]][1:3]
    
    final_news
    
  })
  
  
  economic_times_news <- reactive({
    # browser()
    
    # read.csv(paste(getwd(),"/date/Economic_Times_Tickers.csv", sep = ""))
    economic_data <- read.csv(paste0(getwd(),"/data/Economic_Times_Tickers.csv", sep = ""))
    row_number <- which(grepl(input$stock_input, economic_data$Company))
    economic_url = paste("https://economictimes.indiatimes.com",economic_data[row_number,2],"stocks",economic_data[row_number,2],"stocksupdate",economic_data[row_number,3], sep="/")
    economic_url = paste(economic_url,".cms",sep="")
    #url = "https://economictimes.indiatimes.com/yes-bank-ltd/stocks/companyid-16552.cms"
    
    news_data <- read_html(economic_url)
    
    total_str <- rvest::html_text(rvest::html_nodes(news_data, '.updates-container'))
    
    modified_str <- gsub("            \t\t\t\t\t\t\t\t\t           ","",total_str)
    
    modified_str <- gsub("            \t\t\t\t\t\t\t\t\t        ","",modified_str)
    
    modified_str <- gsub("\n",";",modified_str)
    
    modified_str <- substr(modified_str,129,nchar(modified_str))
    
    final_news <- str_replace_all(modified_str, "IST" , "\n")
    
    final_news <- read.table(text=final_news, sep='\n',skip = 1, col.names=c('content'))
    
    # # total_str <- rvest::html_text(rvest::html_nodes(news_data, '.flr'))
    # # 
    # # modified_str <- str_replace_all(total_str[grep("News & Updates", total_str)], "[\r\t\n]" , "")
    # # 
    # # final_news <- substring(modified_str,1,str_locate_all(pattern ='IST', modified_str)[[1]][40,2])
    # # 
    # # final_news <- str_replace_all(final_news, "IST" , "\n")
    # 
    # total_str <- rvest::html_text(rvest::html_nodes(news_data, '.s_container'))
    # 
    # modified_str <- total_str[5]
    # 
    # final_news <- gsub(" News ", " news ", modified_str)
    # 
    # final_news <- gsub("News", "\n", final_news)
    # 
    # # final_news <- paste(" ",final_news, sep = "\n")
    # # 
    # # final_news <- paste(final_news, " ", sep = "\n")
    # 
    # # final_news <- read.table(text=final_news, sep='\n',skip = 1, col.names=c('content'))
    # news_list <- strsplit(final_news, "\n")
    # 
    # news_data <- as.data.frame(news_list)
    # 
    # colnames(news_data) <- "Headlines"
    # 
    # final_news <- news_data[1:3,]
    # 
    # # final_news <- news_list[[1]][1:3]
    
    final_news
    
  })
  
  
  strategy_back_test_dataInput <- reactive({

    result=list()
    data <- na.omit(getSymbols(input$stock_input,
                              src = "yahoo", 
                              from = as.Date(input$daterange[1]), 
                              to = as.Date(input$daterange[2]), 
                              auto.assign = FALSE))
    
    result$data = data
    return(result)
  })  
  
  
  generalData <- reactive({
    # browser()
  if(input$stock_input != ""){
    NSE_List <- read.csv(paste0(getwd(),"/data/NSE_Stocks_List.csv", sep = ""))
    row_number <- which(grepl(input$stock_input, NSE_List$Yahoo.Symbol))
    
    stock_industry <- NSE_List[row_number,2]
    
    default_stock <- getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE)
    
    stocks <- NSE_List %>% filter(Industry == as.character(stock_industry))
    
    # stocks <- filter(stocks, !Yahoo.Symbol == input$stock_input)
    
    stocks <- stocks %>% select(c("Yahoo.Symbol"))
    
    # stocks <- read.csv(paste(getwd(),"/data/rtsi_tickers.csv", sep = ""),stringsAsFactors = F)
    # stocks <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'rtsi_tickers.csv'), stringsAsFactors = F)
    days.ago <- difftime(input$daterange[1], input$daterange[2], units = c("days"))
    rf.rate = as.numeric(input$rfrInput / 365 * days.ago)
    
    market.prices <- as.numeric(na.omit(getSymbols(input$stock_input, from = input$daterange[1], to = input$daterange[2], src = "yahoo", auto.assign = FALSE))[, 4])
    
    market.returns <- na.omit(diff(market.prices) / market.prices[1 : (length(market.prices) - 1)])
    m.return <- (market.prices[length(market.prices)] - market.prices[1]) / market.prices[1] * 100
    
    withProgress(message = "Loading stocks data", value = 0, {
      asset.prices <- sapply(t(stocks),
                            
                             function(x) {
                               incProgress(1 / nrow(stocks), detail = x)
                               as.numeric(na.omit(getSymbols(as.character(x), from = input$daterange[1], to = input$daterange[2],
                                                             src = "yahoo", auto.assign = F))[, 4])
                             },
                             simplify=FALSE, USE.NAMES=TRUE)
      # browser()
      stocks.df <- data.frame(ticker = names(asset.prices), beta = rep(NA, nrow(stocks)), expected.return = rep(NA, nrow(stocks)),
                              return = rep(NA, nrow(stocks)), alpha = rep(NA, nrow(stocks)), r2 = rep(NA, nrow(stocks)),
                              sortino = rep(NA, nrow(stocks)))
    })
    
    # browser()
    
    withProgress(message = "Analyzing price data", value = 0, {
      stocks.df[, c("beta", "expected.return", "return",
                    "alpha", "r2", "sortino")] <- t(as.data.frame(
                      lapply(asset.prices,
                             function(x){
                               # browser()
                               incProgress(1 / nrow(stocks))
                               asset.returns <- na.omit(diff(x) / x[1 : (length(x) - 1)])
                               beta = cov(asset.returns, market.returns) / var(market.returns)
                               
                               lm.df = data.frame(market = market.prices, asset = x)
                               lm.fit = lm(formula = market~asset, data = lm.df)
                               r2 = summary(lm.fit)$adj.r.squared
                               
                               expected.return = rf.rate + beta * (m.return - rf.rate)
                               stock.return = (x[length(x)] - x[1]) / x[1] * 100
                               alpha = stock.return - expected.return
                               sortino = SortinoRatio(R = asset.returns)[, 1]
                               round(c(beta, expected.return, stock.return, alpha, r2, sortino), 4)
                             }
                      )
                    ))
    })
    
    step = 0.1
    sml <- rf.rate + seq(0, 2.5, by = step) * (m.return - rf.rate)
    slope = (sml[2] - sml[1]) / step
    list(stocks.df, rf.rate, slope, market.prices, asset.prices, stocks)
  }
   
  })
  
  ################################  Rendering Part ##################################################
  
  output$slickr <- renderSlickR({
    folder <- paste(getwd(),"/images" ,sep = "")
    
    imgs <- list.files(folder, pattern=".jpg", full.names = TRUE)
    slickR(imgs)
  })
  
  
  output$current_status <-  DT::renderDataTable({
    
    
    DT::datatable(GetCurrentValue(),
                  extensions = 'Buttons',
                  options=list(dom = 'Bfrtip',
                               buttons = c('excel','csv','print'),
                               rowCallback=JS(
                                 'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1) 
            $(row).css("background","#D7DBDD")
   }'))
                  
                  )
    
  })
    
    
  
  
  output$stocks_plot <- renderHighchart({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.1, detail = paste("Doing part"))
      Sys.sleep(0.1)
      # browser()
      
      stocks_data_mm <- StocksData()
      
      stocks_data_mm <- data.frame(Date = index(stocks_data_mm), coredata(stocks_data_mm) )
      
      colnames(stocks_data_mm)[6] <- "Volume"
      colnames(stocks_data_mm)[7] <- "Adjusted"
      
      stocks_data_mm[,-c(1,6)] <-round(stocks_data_mm[,-c(1,6)],2)
      
      # if(input$chart == "line"){
        highchart(type = "stock") %>%
          hc_rangeSelector(selected = 2) %>%
          hc_yAxis_multiples(
            list(title = list(text = "VOLUME"),opposite=FALSE),
            list( title = list(text = "OHLC"),opposite = TRUE)
          ) %>%
          hc_legend(enabled = TRUE,verticalAlign = "top") %>%
          hc_add_series(stocks_data_mm, "column", hcaes(Date,
                                                       Volume), color = "#b3b3cc", yAxis = 0,  zIndex = 0,  name = "Volume") %>%
          hc_add_series(stocks_data_mm,
                        "line", hcaes(Date, Adjusted),  zIndex = 2, yAxis = 1, name = "Price") %>%
          hc_add_series(stocks_data_mm, "line", hcaes(Date,
                                                     mm10),  zIndex = 2, yAxis = 1, name = "Weekly Moving Average") %>%
          hc_add_series(stocks_data_mm, "line", hcaes(Date,
                                                     mm30),  zIndex = 2, yAxis = 1, name = "Monthly Moving Average") %>%
          
          
          hc_plotOptions(series = list(groupPadding = 0,borderWidth=1))  %>%
          hc_exporting(enabled = TRUE, filename = "custom-file-name")
        
      # }
      # else{
      #   highchart(type = "stock") %>%
      #     hc_add_series(StocksData(), "candlestick" , name = "Counts") %>%
      #     hc_exporting(enabled = TRUE,filename = "custom-file-name")
      # }
      
    })
  })
  
  output$stocks_returns <- renderHighchart({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.1, detail = paste("Doing part"))
      Sys.sleep(0.1)
      
      
      uber_stock <- getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE)
      
      uber_stock_ret <- diff(log(uber_stock[,6]))
      uber_stock_ret <- uber_stock_ret[-1,]
      
      uber_stock_ret <- data.frame(Date = index(uber_stock_ret), coredata(uber_stock_ret) )
      # browser()
      colnames(uber_stock_ret)[2] = "Adjusted"
      
      highchart(type = "stock") %>%
        hc_add_series(uber_stock_ret, "line", hcaes(Date, Adjusted), name = "Counts") %>%
        hc_exporting(enabled = TRUE,filename = "custom-file-name")
      
      
    })
  })
  
  output$input_valuation <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      # browser()
      money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
      # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
      row_number <- which(grepl(input$stock_input, money_control_data$Company))
      money_control_url = paste("https://www.moneycontrol.com/india/stockpricequote",money_control_data[row_number,4],money_control_data[row_number,2],money_control_data[row_number,3], sep="/")
      
      #url = "https://www.moneycontrol.com/india/stockpricequote/refineries/relianceindustries/RI"
      
      stock_data <- read_html(money_control_url)
      
      total_str <- rvest::html_text(rvest::html_nodes(stock_data, '.main_wrapper_res'))
      
      modified_str <- str_replace_all(total_str[7], "[\r\t]" , "")
      
      strengths <- substring(modified_str,str_locate(modified_str, 'Market')[1,1],(str_locate(modified_str, 'Note')[1,1] - 3))
      
      strengths_table <- read.table(text=strengths, sep='\n', col.names=c('Content'))
      df <- strengths_table[-which((strengths_table$Content == "                                                    ") | (strengths_table$Content == "                                                                                                ") | (strengths_table$Content == "                                                                    ") | (strengths_table$Content == "                    ")), ]
      
      df <- as.data.frame(df)
      
      df[24,1] = df[43,1]
      df[25,1] = "Delivery Average% (3 Day)"
      df[26,1] = df[31,1]
      df[27,1] = "Delivery Average% (5 Day)"
      df[28,1] = df[34,1]
      df[29,1] = "Delivery Average% (8 Day)"
      df[30,1] = df[37,1]
      
      final_df <- df[1:30,]
      
      final_df <- do.call("cbind", split(final_df, rep(c(1, 2), length.out = nrow(final_df))))
      
      
      colnames(final_df) = c("id","value")
      
      
      DT::datatable(final_df,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    ))
      
      
    })
  })
  
  
  output$input_strength <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      # browser()
      
      modified_str <- read_html_file()
      strengths <- substring(modified_str,str_locate(modified_str, 'Strengths')[1,1],(str_locate(modified_str, 'Weakness')[1,1] - 1))
      strengths_table <- read.table(text=strengths, sep='\n', skip=1, col.names=c('Content'))
      
      # weakness <- substring(modified_str,str_locate(modified_str, 'Weakness')[1,2],(str_locate(modified_str, 'Opportunities')[1,1] - 1))
      # weakness_table <- read.table(text=weakness, sep='\n', skip=1, col.names=c('Content'))
      #
      # Opportunities <- substring(modified_str,str_locate(modified_str, 'Opportunities')[1,2],(str_locate(modified_str, 'Threats')[1,1] - 1))
      # Opportunities_table <- read.table(text=Opportunities, sep='\n', skip=1, col.names=c('Content'))
      #
      # Threads <- substring(modified_str,str_locate(modified_str, 'Threats')[1,2],length(modified_str) - 1)
      # Threads_table <- read.table(text=Threads, sep='\n', skip=1, col.names=c('Content'))
      
      
      
      DT::datatable(strengths_table,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    )) %>%
        formatStyle('Content',  color = 'green', backgroundColor = 'white', fontWeight = 'bold')
      
      
    })
  })
  
  
  output$input_weakness <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      
      modified_str <- read_html_file()
      
      weakness <- substring(modified_str,str_locate(modified_str, 'Weakness')[1,2],(str_locate(modified_str, 'Opportunities')[1,1] - 1))
      weakness_table <- read.table(text=weakness, sep='\n', skip=1, col.names=c('Content'))
      
      DT::datatable(weakness_table,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    )) %>%
        formatStyle('Content',  color = 'red', backgroundColor = 'white', fontWeight = 'bold')
      
      
    })
  })
  
  output$input_opportunities <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      
      modified_str <- read_html_file()
      
      
      Opportunities <- substring(modified_str,str_locate(modified_str, 'Opportunities')[1,2],(str_locate(modified_str, 'Threats')[1,1] - 1))
      Opportunities_table <- read.table(text=Opportunities, sep='\n', skip=1, col.names=c('Content'))
      
      
      
      DT::datatable(Opportunities_table,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    )) %>%
        formatStyle('Content',  color = '#87ceeb', backgroundColor = 'white', fontWeight = 'bold')
      
      
    })
  })
  
  output$input_threats <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      modified_str <- read_html_file()
      
      Threats <- substring(modified_str,str_locate(modified_str, 'Threats')[1,2],length(modified_str) - 1)
      Threats_table <- read.table(text=Threats, sep='\n', skip=1, col.names=c('Content'))
      
      
      
      DT::datatable(Threats_table,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    )) %>%
        formatStyle('Content',  color = 'orange', backgroundColor = 'white', fontWeight = 'bold')
      
      
    })
  })
  
  output$economic_latest <- DT::renderDataTable({
    
    # url = "https://economictimes.indiatimes.com/yes-bank-ltd/stocks/companyid-16552.cms"
    # news_data <- read_html(url)
    #
    # total_str <- rvest::html_text(rvest::html_nodes(news_data, '.flr'))
    #
    # modified_str <- str_replace_all(total_str[31], "[\r\t\n]" , "")
    #
    # final_news <- substring(modified_str,1,str_locate_all(pattern ='IST', modified_str)[[1]][40,2])
    #
    # final_news <- str_replace_all(final_news, "IST" , "\n")
    
    final_news <- economic_times_latest()
    
    final_news <- as.data.frame(final_news)
    DT::datatable(final_news,extensions = c('FixedColumns'),
                  options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                  )) 
    
    
    # library("tidyverse")
    # 
    # dat <- map(final_news, function(x) {
    #   tibble(text = unlist(str_split(x, pattern = "\\n"))) %>%
    #     rowid_to_column(var = "line")
    # })
    
    # latest_news <- as.data.frame(dat)

    
    # latest_news <- substring(final_news,1,str_locate_all(pattern ='\n', final_news)[[1]][1,1])
    
    #final_news_data <- read.table(text=final_news, sep='\n',skip = 1, col.names=c('Content'))
    
    # paste(latest_news)
  })
  
  output$economic_news <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      
      # browser()
      
      # url = "https://economictimes.indiatimes.com/yes-bank-ltd/stocks/companyid-16552.cms"
      # news_data <- read_html(url)
      #
      # total_str <- rvest::html_text(rvest::html_nodes(news_data, '.flr'))
      #
      # modified_str <- str_replace_all(total_str[31], "[\r\t\n]" , "")
      #
      # final_news <- substring(modified_str,1,str_locate_all(pattern ='IST', modified_str)[[1]][40,2])
      #
      # final_news <- str_replace_all(final_news, "IST" , "\n")
      
      final_news <- economic_times_news()
      
      DT::datatable(final_news)
      
      # news_Dataf <- fread(final_news)
      # 
      # final_news_data <- as.data.frame(news_Dataf)
      # 
      # colnames(final_news_data)<-c("Content","Date")
      
      # final_news_data <- read.table(text=final_news, sep='\n',skip = 1, col.names=c('Content'))
      
      # DT::datatable(final_news_data,extensions = c('FixedColumns'),
      #               options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
      #               )) %>%
      #   formatStyle('Content',  color = 'orange', backgroundColor = 'white', fontWeight = 'bold')
      
      
    })
  })
  
  output$money_control_news <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      # browser()
      money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
      # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
      
      row_number <- which(grepl(input$stock_input, money_control_data$Company))
      
      money_control_url = paste("https://www.moneycontrol.com/stocks/company_info/stock_news.php?sc_id=",money_control_data[row_number,3],"&durationType=M&duration=1", sep="")
      
      print(money_control_url)
      
      stock_data <- html_session(money_control_url)
      
      nodes <- stock_data %>%
        html_nodes(xpath = '//*[@id="mc_mainWrapper"]/div[2]/div[2]/div[3]/div[2]/div[2]/div/div[3]/div[1]')
      
      print(nodes)
      
      total_str <- rvest::html_text(nodes)
      
      modified_str <- str_replace_all(total_str, "[\r\t]" , "")
      
      modified_total <- str_replace_all(modified_str, "[\n\n]" , "\t")
      
      print(modified_total)
      
      # stri = "googletag.cmd.push(function() { googletag.display('Moneycontrol/MC_Market/MC_Market_StockPrice_Native_1'); });"
      # 
      # str_count(modified_total, "googletag.cmd")
      # 
      # #str_locate(modified_total, 'googletag.cmd')[1,1]
      # 
      # full_modified <- paste0(substr(modified_total, 1, (str_locate(modified_total, 'googletag.cmd')[1,1] - 1)), substr(modified_total,(str_locate(modified_total, 'googletag.cmd')[1,1] + nchar(stri)),nchar(modified_total)))
      
      # test <- str_replace_all(full_modified, "[\n]" , "")
      # new_test <- str_replace_all(test, "\t" , "")
      
      new_test <- str_replace_all(modified_total, "\t" , "")
      
      final_test <- str_replace_all(new_test, "[...]" , "\t")
      
      final_test <- str_replace_all(final_test, "\t\t\t" , "\n")
      
      temp <- fread(final_test)
      
      print(head(temp))
      
      data <- colnames(temp)
      
      first_row <- as.data.frame(t(data))
      
      print(ncol(temp))
      names(temp) <- c("News","Date","Content")
      
      print(colnames(first_row))
      print(colnames(temp))
      
      names(first_row) <- c("News","Date","Content")
      
      
      df <- rbind(first_row, temp)
      
      DT::datatable(df,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    )) %>%
        formatStyle('News',  color = 'orange', backgroundColor = 'white', fontWeight = 'bold')
      
      
    })
  })
  
  
  output$live_mint_news <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      # browser()
      
      live_mint_data <- read.csv(paste0(getwd(),"/data/livemint.csv", sep = ""))
      # live_mint_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'livemint.csv'), stringsAsFactors = F)
      
      row_number <- which(grepl(input$stock_input, live_mint_data$Company))
      
      livemint_url = paste("https://www.livemint.com/Search/Link/Keyword",live_mint_data[row_number,2], sep="/")
      
      #livemint_url = "https://www.livemint.com/Search/Link/Keyword/Yes-Bank"
      
      stock_data <- html_session(livemint_url)
      
      nodes <- stock_data %>%
        html_nodes(xpath = '//*[@id="mySearchView"]')
      
      
      total_str <- rvest::html_text(nodes)
      
      total_str <- str_replace_all(total_str, "[\r\t\n]" , "")
      
      temp <- str_replace_all(total_str,"                  ","\n")
      temp <- str_replace_all(temp,"          ","\n")
      
      
      final_df <- read.table(text=temp, sep='\n', skip=1, col.names=c('Content'))
      
      DT::datatable(final_df,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    )) %>%
        formatStyle('Content',  color = 'orange', backgroundColor = 'white', fontWeight = 'bold')
      
      
    })
  })
  
  
  
  output$peers_data <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      # browser()
      NSE_List <- read.csv(paste0(getwd(),"/data/NSE_Stocks_List.csv", sep = ""))
      row_number <- which(grepl(input$stock_input, NSE_List$Yahoo.Symbol))
      # row_number <- which(grepl("RELIANCE.NS", NSE_List$Yahoo.Symbol))
      stock_industry <- NSE_List[row_number,6]
      
      url <- paste("https://www.screener.in/api/company/",stock_industry,"/peers/",sep="")
      
      peer_data <- read_html(url)
      
      tbls <- html_nodes(peer_data, "table")
      
      tbls_ls <- peer_data %>%
        html_nodes("table") %>%
        html_table(fill = TRUE)
      
      tbls_ls <- as.data.frame(tbls_ls)
      
      tbls_ls <- tbls_ls[,-c(1)]
      
      colnames(tbls_ls) <- c("Name","Current Price","P/E","Market Cap","Divident Yield %","Net Profit Last Qtr","YOY Qtr profit growth","Sales latest Qtr","YOY Qtr sales growth","Return on Capital")
      
      DT::datatable(tbls_ls,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    )) %>% formatStyle(
                      c('YOY Qtr profit growth','YOY Qtr sales growth','Return on Capital'),
                      color  = styleInterval(c(0), c('red', 'green')),
                      fontWeight = 'bold'
                    )
      
      
    })
  })
  
  
  
  output$forecasting_plot<- renderHighchart({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.1, detail = paste("Doing part"))
      browser()
      # Stock_df <- na.omit(getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE))
      # 
      # Stock_df$Open = Stock_df[,1]
      # Stock_df$High = Stock_df[,2]
      # Stock_df$Low = Stock_df[,3]
      # Stock_df$Close = Stock_df[,4]
      # Stock_df$Volume = Stock_df[,5]
      # Stock_df$Adj = Stock_df[,6]
      # Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
      # 
      # Stock_df <- data.frame(Date = index(Stock_df), coredata(Stock_df) )
      # 
      # ts_start_date <- input$daterange[1]
      # ts_end_date <- input$daterange[2]
      # 
      # 
      # temp_ts_start_date <- ts_start_date
      # temp_ts_end_date <- ts_start_date + nrow(Stock_df)
      # 
      # temp_holdout_date <- temp_ts_end_date
      # month(temp_holdout_date) <- month(temp_ts_end_date) - 3
      # day(temp_holdout_date) <- days_in_month(temp_ts_end_date)
      # 
      # values = seq(from = temp_ts_start_date, to = temp_ts_end_date - 1, by = 'day')
      # 
      # Stock_df <- cbind(Stock_df,values)
      # 
      # # TotalTS<-ts(Stock_df$Close,start=c(year(temp_ts_start_date),month(temp_ts_start_date)),end = c(year(temp_ts_end_date),month(temp_ts_end_date)),frequency = 365)
      # # 
      # # InsampleTs<-window(TotalTS,start = c(year(temp_ts_start_date),month(temp_ts_start_date)),end=c(year(temp_holdout_date)+(month(temp_holdout_date)-1)))
      # # 
      # # OutsampleTs<-window(TotalTS,start=c(year(temp_holdout_date)+month(temp_holdout_date)),end = c(year(temp_ts_end_date)+month(temp_ts_end_date)))
      # # 
      # # fit<-auto.arima(InsampleTs)
      # # 
      # # fcast <- forecast(fit,h=(length(OutsampleTs) + 10))
      # 
      # y <- ts(Stock_df$Close, frequency=7)
      # fit <- ets(y)
      # fcast <- forecast(fit)
      # 
      # df <- fortify(fcast)
      # 
      # # Convert ts decimal time to Date class
      # df$Date <- as.Date(date_decimal(df$Index), "%Y-%m-%d")
      # 
      # # # Stock_df$v7_MA = ma(Stock_df$Close, order=7)
      # # # Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
      # # # 
      # # # rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
      # # # decomp_rental <- stl(rental_ma, s.window="periodic")
      # # # #plot(decomp_rental)
      # # # adj_rental <- seasadj(decomp_rental)
      # # 
      # # InsampleTs<-window(adj_rental,start = c(year(ts_start_date),month(ts_start_date)),end=c(year(ts_holdout_date)+(month(ts_holdout_date)-1)/12))
      # # OutsampleTs<-window(adj_rental,start=c(year(ts_holdout_date)+month(ts_holdout_date)/12),end = c(year(ts_end_date)+month(ts_end_date)/12))
      # # 
      # # fit<-auto.arima(InsampleTs)
      # # 
      # # fcast <- forecast(fit,h=(length(OutsampleTs) + 12))
      # # 
      # # 
      # # df <- fortify(fcast) %>% as_tibble()
      # # 
      # # # Convert ts decimal time to Date class
      # # df$Date <- as.Date(date_decimal(df$Index), "%Y-%m-%d")
      # # 
      # # 
      # # if(input$future_seasonal == "NonSeasonal"){
      # #   
      # #   fit <- auto.arima(Stock_df$Close,ic="bic")
      # #   fcast <- forecast(fit)
      # #   
      # # }
      # # else{
      # #   
      # #   fit_s<-auto.arima(adj_rental, seasonal=TRUE)
      # #   fcast <- forecast(fit_s, h=10)
      # #   
      # # }
      # # 
      # # # df <- fortify(fcast) %>% as_tibble()
      # # df <- fortify(as.data.frame(fcast)) 
      # # 
      # # # Convert ts decimal time to Date class
      # # df$Date <- as.Date(date_decimal(df$Index), "%Y-%m-%d")
      
      Stock_df <- na.omit(getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE))
      
      Stock_df$Open = Stock_df[,1]
      Stock_df$High = Stock_df[,2]
      Stock_df$Low = Stock_df[,3]
      Stock_df$Close = Stock_df[,4]
      Stock_df$Volume = Stock_df[,5]
      Stock_df$Adj = Stock_df[,6]
      Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
      
      Stock_df <- data.frame(Date = index(Stock_df), coredata(Stock_df) )
      
      
      # Stock_df$v7_MA = ma(Stock_df$Close, order=7)
      # Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
      
      rental_ma <- ts(na.omit(Stock_df$Close), frequency=30)
      decomp_rental <- stl(rental_ma, s.window="periodic")
      #plot(decomp_rental)
      adj_rental <- seasadj(decomp_rental)
      
      # if(input$future_seasonal == "NonSeasonal"){
      #   
      #   fit <- auto.arima(Stock_df$Close,ic="bic")
      #   fcast <- forecast(fit)
      #   
      # }
      # else{
      #   
      #   fit_s<-auto.arima(adj_rental, seasonal=TRUE)
      #   fcast <- forecast(fit_s, h=10)
      #   
      # }
      
      fit_s<-auto.arima(adj_rental, seasonal=TRUE)
      fcast <- forecast(fit_s, h=10)
      
      df <- fortify(fcast)
      
      # browser()
      
      # Convert ts decimal time to Date class
      # df$Date <- as.Date(date_decimal(df$Index), "%Y-%m-%d")
      
      temp_end_Date = as.Date(input$daterange[2])
      
      temp_prediction_date <- temp_end_Date + 24
      
      date_range <- seq.Date(temp_end_Date, temp_prediction_date, 1)
      
      next_days <- date_range[!weekdays(date_range) %in% (c("Saturday", "Sunday")) ]
      
      next_days <- as.data.frame(next_days)
      
      next_days <- next_days[1:10,]
      
      original_dates <- Stock_df$Date
      
      original_dates <- as.data.frame(original_dates)
      next_days <- as.data.frame(next_days)
      
      names(original_dates) <- c("Date")
      names(next_days) <- c("Date")
      
      final_dates <- rbind(original_dates,next_days)
      
      df$Date <- as.Date(final_dates$Date)
      
      # df$Date <- as.Date(next_days, "%Y-%m-%d")
      
      chart_output <- highchart(type = "stock") %>% 
        hc_add_series(df, "line", hcaes(Date, Data), name = "Original") %>% 
        hc_add_series(df, "line", hcaes(Date, Fitted), name = "Fitted") %>%
        hc_add_series(df, "line", hcaes(Date, `Point Forecast`), name = "Forecast") %>% 
        hc_add_series(df, "arearange", hcaes(Date, low = `Lo 95`, high = `Hi 95`), name = "Interval") %>%
        hc_add_series(df, "arearange", hcaes(Date, low = `Lo 80`, high = `Hi 80`), name = "Interval") %>%
        hc_exporting(enabled = TRUE,filename = "custom-file-name")
      chart_output
      
    })
    
  })
  
  
  #Auto.Arima1 - plot here  Tile#5
  output$auto.arima1 <- renderTable({
    
    Stock_df <- na.omit(getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE))
    
    Stock_df$Open = Stock_df[,1]
    Stock_df$High = Stock_df[,2]
    Stock_df$Low = Stock_df[,3]
    Stock_df$Close = Stock_df[,4]
    Stock_df$Volume = Stock_df[,5]
    Stock_df$Adj = Stock_df[,6]
    Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
    
    Stock_df <- data.frame(Date = index(Stock_df), coredata(Stock_df) )
    
    
    # Stock_df$v7_MA = ma(Stock_df$Close, order=7)
    # Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
    
    rental_ma <- ts(na.omit(Stock_df$Close), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    #plot(decomp_rental)
    adj_rental <- seasadj(decomp_rental)
    
    # if(input$future_seasonal == "NonSeasonal"){
    #   
    #   fit <- auto.arima(Stock_df$Close,ic="bic")
    #   fcast <- forecast(fit)
    #   
    # }
    # else{
    #   
    #   fit_s<-auto.arima(adj_rental, seasonal=TRUE)
    #   fcast <- forecast(fit_s, h=10)
    #   
    # }
    
    fit_s<-auto.arima(adj_rental, seasonal=TRUE)
    fcast <- forecast(fit_s, h=10)
    
    fcast
  })
  
  output$candle_1 <- renderPlotly({
    
    # browser()
    # uber_stock <- getSymbols("UBER", src = "yahoo", from = as.Date(input$candle_stick_daterange[1]), to = as.Date(input$candle_stick_daterange[2]), auto.assign = FALSE)
    #
    # # basic example of ohlc charts
    # df <- data.frame(Date=index(uber_stock),coredata(uber_stock))
    #
    # names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    #
    # # cutom colors
    # i <- list(line = list(color = '#FFD700'))
    # d <- list(line = list(color = '#0000ff'))
    #
    # fig <- df %>% plot_ly(x = ~Date, type="candlestick",
    #                       open = ~Open, close = ~Close,
    #                       high = ~High, low = ~Low,
    #                       increasing = i, decreasing = d)
    #
    # fig
    
    # Get data
    uber_stock <- na.omit(getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE))
    
    # basic example of ohlc charts
    df <- data.frame(Date=index(uber_stock),coredata(uber_stock))
    
    names(df) <- c("dates", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    
    
    # Color or volume bars
    barcols <- c()
    for (i in 1:length(df$dates)) {
      
      if (i == 1) {barcols[i] <- "#F95959"}
      
      if (i > 1) {
        x <- ifelse(df$Close[i] > df$Close[i - 1], "#455D7A", "#F95959")
        barcols[i] <- x
      }
    }
    
    # Moving Avg line
    MA <- runMean(df$Close)
    
    # Range selector
    rangeselectorlist = list(
      x = 0, y = 0.9,
      bgcolor = "#0099cc",
      font = list(color = "white"),
      
      buttons = list(
        list(count = 1, label = "reset", step = "all"),
        list(count = 1, label = "1yr", step = "year", stepmode = "backward"),
        list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
        list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
        list(step = "all")
      )
    )
    
    BB <- as.data.frame(BBands(df$Close))
    
    CKD <- CMF(df[,c("High", "Low", "Close")], volume = df$Volume)
    CKD.pos <- CKD
    CKD.pos[CKD.pos < 0] <- 0
    
    CKD.neg <- CKD
    CKD.neg[CKD.neg > 0] <- 0
    
    macd <- data.frame(TTR::MACD(df$Close, 12, 26, 9))
    macd$diff <- macd$macd - macd$signal
    
    fig <- plot_ly(df, type = "candlestick",
                   x = ~dates,
                   open = ~Open, high = ~High, low = ~Low, close = ~Close,
                   yaxis = "y",
                   increasing = list(line = list(color = "#455D7A")),
                   decreasing = list(line = list(color = "#F95959")),
                   name = "Price",
                   height = 800, width = 1200) %>%
      
      # MA
      add_lines(x = df$dates, y = BB$mavg,
                line = list(width = 3, dash = "5px", color = "#33bbff"),
                inherit = F, name = "Mov Avg") %>%
      
      # Upper and Lower bounds
      add_lines(x = df$dates, y = BB$up,
                line = list(width = 1, dash = "5px", color = "#737373"),
                fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
                inherit = F, name = "Bollinger") %>%
      
      add_lines(x = df$dates, y = BB$dn,
                line = list(width = 1, dash = "5px", color = "#737373"),
                fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
                inherit = F, name = "Bollinger") %>%
      # # CKD
      # add_lines(x = df$dates, y = CKD.pos,
      #           yaxis = "y2",
      #           line = list(width = 1, color = "black"),
      #           fill = "tozeroy", fillcolor = "#47d147",
      #           inherit = FALSE) %>%
      #
      # add_lines(x = df$dates, y = CKD.neg,
      #           yaxis = "y2",
      #           line = list(width = 1, color = "black"),
      #           fill = "tozeroy", fillcolor = "#ff6666",
    #           inherit = FALSE) %>%
    
    # MACD
    add_lines(x = df$dates, y = macd$macd,
              yaxis = "y2",
              line = list(width = 1, color = "#8c8c8c"),
              inherit = FALSE) %>%
      
      add_lines(x = df$dates, y = macd$signal,
                yaxis = "y2",
                line = list(width = 1, color = "#ff6666"),
                inherit = FALSE) %>%
      
      add_bars(x = df$dates, y = macd$diff,
               marker = list(color = "#bfbfbf"),
               yaxis = "y2",
               inherit = FALSE) %>%
      
      layout(
        plot_bgcolor = "rgb(250,250,250)",
        xaxis = list(title = "", domain = c(0,0.95),
                     rangeslider = list(visible = F),
                     rangeselector = rangeselectorlist),
        yaxis = list(domain = c(0.22, 0.9)),
        yaxis2 = list(domain = c(0, 0.18), side = "right"),
        showlegend = F,
        
        annotations = list(
          list(x = 0, y = 1, xanchor = "left", yanchor = "top",
               xref = "paper", yref = "paper",
               text = paste0(input$stock_input),
               font = list(size = 30, family = "serif"),
               showarrow = FALSE),
          
          list(x = 0.8, y = 0.95, xanchor = "left", yanchor = "top",
               xref = "paper", yref = "paper",
               text = paste0("[", paste(range(df$dates),collapse = " / "), "]"),
               font = list(size = 15, family = "serif"),
               showarrow = FALSE),
          
          list(x = 0, y = 0.18, xanchor = "left", yanchor = "top",
               xref = "paper", yref = "paper",
               text = paste0("MACD (12, 26, 9)"),
               font = list(size = 15, family = "serif"),
               showarrow = FALSE)
          
          # list(x = 0, y = 0.18, xanchor = "left", yanchor = "top",
          #      xref = "paper", yref = "paper",
          #      text = paste0("Chaikin Money Flow"),
          #      font = list(size = 15, family = "serif"),
          #      showarrow = FALSE)
        )
      )
    
    # CKD <- CMF(df[,c("High", "Low", "Close")], volume = df$Volume)
    # CKD.pos <- CKD
    # CKD.pos[CKD.pos < 0] <- 0
    #
    # CKD.neg <- CKD
    # CKD.neg[CKD.neg > 0] <- 0
    #
    # fig <- plot_ly(df, type = "candlestick",
    #         x = ~dates,
    #         open = ~Open, high = ~High, low = ~Low, close = ~Close,
    #         yaxis = "y",
    #         increasing = list(line = list(color = "#455D7A")),
    #         decreasing = list(line = list(color = "#F95959")),
    #         name = "Price",
    #         height = 600, width = 1024) %>%
    #
    #   # MA
    #   add_lines(x = df$dates, y = BB$mavg,
    #             line = list(width = 3, dash = "5px", color = "#33bbff"),
    #             inherit = F, name = "Mov Avg") %>%
    #
    #   # Upper and Lower bounds
    #   add_lines(x = df$dates, y = BB$up,
    #             line = list(width = 1, dash = "5px", color = "#737373"),
    #             fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
    #             inherit = F, name = "Bollinger") %>%
    #
    #   add_lines(x = df$dates, y = BB$dn,
    #             line = list(width = 1, dash = "5px", color = "#737373"),
    #             fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
    #             inherit = F, name = "Bollinger") %>%
    #
    #   # CKD
    #   add_lines(x = df$dates, y = CKD.pos,
    #             yaxis = "y2",
    #             line = list(width = 1, color = "black"),
    #             fill = "tozeroy", fillcolor = "#47d147",
    #             inherit = FALSE) %>%
    #
    #   add_lines(x = df$dates, y = CKD.neg,
    #             yaxis = "y2",
    #             line = list(width = 1, color = "black"),
    #             fill = "tozeroy", fillcolor = "#ff6666",
    #             inherit = FALSE) %>%
    #
    #   layout(
    #     plot_bgcolor = "rgb(250,250,250)",
    #     xaxis = list(title = "", domain = c(0,0.95),
    #
    #                  rangeslider = list(visible = F),
    #
    #                  rangeselector = rangeselectorlist),
    #     yaxis = list(domain = c(0.22, 0.9)),
    #     yaxis2 = list(domain = c(0, 0.18), side = "right"),
    #     showlegend = F,
    #
    #     annotations = list(
    #       list(x = 0, y = 1, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("Microsoft"),
    #            font = list(size = 30, family = "serif"),
    #            showarrow = FALSE),
    #
    #       list(x = 0.8, y = 0.95, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("[", paste(range(df$dates),collapse = " / "), "]"),
    #            font = list(size = 15, family = "serif"),
    #            showarrow = FALSE),
    #
    #       list(x = 0, y = 0.18, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("Chaikin Money Flow"),
    #            font = list(size = 15, family = "serif"),
    #            showarrow = FALSE)
    #     )
    #   )
    
    # BB <- as.data.frame(BBands(df$Close))
    # fig <- plot_ly(df, type = "candlestick",
    #         x = ~dates,
    #         open = ~Open, high = ~High, low = ~Low, close = ~Close,
    #         yaxis = "y",
    #         increasing = list(line = list(color = "#455D7A")),
    #         decreasing = list(line = list(color = "#F95959")),
    #         name = "Price",
    #         height = 600, width = 1024) %>%
    #
    #   add_bars(data = df, x = ~dates, y = ~Volume,
    #            marker = list(color = barcols),
    #            yaxis = "y2", inherit = F, name = "Vol") %>%
    #
    #   # MA
    #   add_lines(x = df$dates, y = BB$mavg,
    #             line = list(width = 3, dash = "5px", color = "#33bbff"),
    #             inherit = F, name = "Mov Avg") %>%
    #
    #   # Upper and Lower bounds
    #   add_lines(x = df$dates, y = BB$up,
    #             line = list(width = 1, dash = "5px", color = "#737373"),
    #             fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
    #             inherit = F, name = "Bollinger") %>%
    #
    #   add_lines(x = df$dates, y = BB$dn,
    #             line = list(width = 1, dash = "5px", color = "#737373"),
    #             fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
    #             inherit = F, name = "Bollinger") %>%
    #
    #   layout(
    #     plot_bgcolor = "rgb(250,250,250)",
    #     xaxis = list(title = "", domain = c(0,0.95),
    #
    #                  rangeslider = list(visible = F),
    #
    #                  rangeselector = rangeselectorlist),
    #     yaxis = list(domain = c(0.22, 0.9)),
    #     yaxis2 = list(domain = c(0, 0.18), side = "right"),
    #     showlegend = F,
    #
    #     annotations = list(
    #       list(x = 0, y = 1, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("Microsoft"),
    #            font = list(size = 30, family = "serif"),
    #            showarrow = FALSE),
    #
    #       list(x = 0.8, y = 0.95, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("[", paste(range(df$dates),collapse = " / "), "]"),
    #            font = list(size = 15, family = "serif"),
    #            showarrow = FALSE),
    #
    #       list(x = 0, y = 0.18, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("Volume"),
    #            font = list(size = 15, family = "serif"),
    #            showarrow = FALSE)
    #     )
    #   )
    
    # fig <- plot_ly(df, type = "candlestick",
    #                       x = ~dates,
    #                       open = ~Open, high = ~High, low = ~Low, close = ~Close,
    #                       yaxis = "y",
    #                       increasing = list(line = list(color = "#455D7A")),
    #                       decreasing = list(line = list(color = "#F95959")),
    #                       name = "Price",
    #                       height = 600, width = 1024) %>%
    #
    #   add_bars(data = df, x = ~dates, y = ~Volume,
    #            marker = list(color = barcols),
    #            yaxis = "y2", inherit = F, name = "Vol") %>%
    #
    #   add_lines(x = df$dates, y = MA,
    #             line = list(width = 3, dash = "5px", color = "#33bbff"),
    #             inherit = F, name = "Mov Avg") %>%
    #
    #   layout(
    #     plot_bgcolor = "rgb(250,250,250)",
    #     xaxis = list(title = "", domain = c(0,0.95),
    #
    #                  rangeslider = list(visible = F),
    #
    #                  rangeselector = rangeselectorlist),
    #     yaxis = list(domain = c(0.22, 0.9)),
    #     yaxis2 = list(domain = c(0, 0.18), side = "right"),
    #
    #     showlegend = F,
    #
    #     annotations = list(
    #       list(x = 0, y = 1, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("Microsoft"),
    #            font = list(size = 30, family = "serif"),
    #            showarrow = FALSE),
    #
    #       list(x = 0.8, y = 0.95, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("[", paste(range(df$dates),collapse = " / "), "]"),
    #            font = list(size = 15, family = "serif"),
    #            showarrow = FALSE),
    #
    #       list(x = 0, y = 0.18, xanchor = "left", yanchor = "top",
    #            xref = "paper", yref = "paper",
    #            text = paste0("Volume"),
    #            font = list(size = 15, family = "serif"),
    #            showarrow = FALSE)
    #     )
    #   )
    
    fig
    
    
  })
  
  
  
  output$strategy_back_test_plot <- renderPlot({
    # observeEvent(input$strategy_back_test_submit,{
    # browser()
    data  = strategy_back_test_dataInput()$data
  
    strategy = BB_Strategy_Generate(data, input$strategy_back_test_BB_win,
                                    input$strategy_back_test_sd, day_stop = input$strategy_back_test_stop_day, processed= input$strategy_back_test_processed,
                                    modi_macd= input$strategy_back_test_modi_macd, macd_fast=input$strategy_back_test_macd_fast,
                                    macd_slow=input$strategy_back_test_macd_slow, macd_signal= input$strategy_back_test_macd_signal,
                                    stop_trig = input$strategy_back_test_stop_trig, stop_profit=input$strategy_back_test_stop_profit)
    
    args <-    switch(input$strategy_back_test_plot_select,
                      "1" = ", theme = chartTheme('white'), type = 'line', TA=NULL)",
                      "2" = ", theme = chartTheme('white'), TA=NULL)"
    )
    plot_func <-switch(input$strategy_back_test_plot_select,
                       "1" = "chartSeries(data[,6], name=input$stock_input",
                       
                       "2" = "chartSeries(data, name=input$stock_input"
    )
    
    eval(parse(text = paste0(plot_func, args)))
    #####################################################################################  
    if(input$strategy_back_test_addVo) quantmod::show(addVo())
    if(input$strategy_back_test_addBB){
      quantmod::show(addBBands(n=input$strategy_back_test_BB_win, sd= input$strategy_back_test_sd))
    }
    
    if(input$strategy_back_test_processed){ 
      #################Plot Transaction####################################
      strategy$Buy[which(strategy$Buy==0)]=NA
      if( length(strategy$Buy[which(!is.na(strategy$Buy))] ) !=0 ){
        quantmod::show(
          addPoints(  y=as.vector(strategy$Buy[which(!is.na(strategy$Buy))]),
                      x=which( !is.na(strategy$Buy)), pch='B', col='red'))}
      strategy$Sell[which(strategy$Sell==0)]=NA
      if( length(strategy$Sell[which(!is.na(strategy$Sell))] ) !=0 ){
        quantmod::show(
          addPoints(  y=as.vector(strategy$Sell[which(!is.na(strategy$Sell))]), 
                      x=which( !is.na(strategy$Sell)), pch='S', col='blue'))}
      #####################################################################
      
      #################Compute Cumulative Return###########################
      lll = length( index(strategy) )
      ll  = lll-1
      M =cbind(as.vector(strategy[,1][-lll]), as.vector(strategy$holding[-1]), as.vector(strategy$holding[-lll]))
      M[,2]= M[,1]*M[,2]; M[,3]= M[,1]*M[,3]
      B= M[,2]; B= B[-ll]; B= c(0, B); M[,2]= B
      M[which(M[,2]==0),2]=NA; M[which(M[,3]==0),3]=NA
      M[,1]= (M[,3]-M[,2]) / M[,2]
      B= M[,1]; B[which(is.na(B))]=0; M[,1]=B;d_ret=c(0, M[,1])+1
      c_rst = rep(1, lll); for(i in 2: lll){c_rst[i]= c_rst[i-1]*d_ret[i]}
      strategy$d_ret= xts(d_ret, order.by= index(strategy))
      strategy$result= xts(c_rst, order.by= index(strategy))
      #####################################################################
      
      #####################Plot Cumulative Return##########################
      a= strategy$result[[lll]]
      a= 100*(a-1)
      a= round(a,0)
      legend= paste('Cumulative Return: ', a, '%', sep='')
      quantmod::show(addTA(strategy$result, legend= legend) )
    }
    
    
    if(input$strategy_back_test_addMACD){
      if(input$strategy_back_test_macd_fast < input$strategy_back_test_macd_signal | input$strategy_back_test_macd_signal <  input$strategy_back_test_macd_slow)###???
        addMACD(fast=input$strategy_back_test_macd_fast, slow=input$strategy_back_test_macd_slow, signal= input$strategy_back_test_macd_signal)
      else print('Invalid Parameter')####????
    }
    # })
  })
  
  
  output$comparision_plot <- renderPlotly({
    # browser()
    
    if(input$stock_input != ""){
      
      NSE_List <- read.csv(paste0(getwd(),"/data/NSE_Stocks_List.csv", sep = ""))
      row_number <- which(grepl(input$stock_input, NSE_List$Yahoo.Symbol))
      # row_number <- which(grepl("RELIANCE.NS", NSE_List$Yahoo.Symbol))
      stock_industry <- NSE_List[row_number,2]
      
      default_stock <- getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE)
      
      comparison_stocks <- NSE_List %>% filter(Industry == as.character(stock_industry))
      
      comparison_stocks <- filter(comparison_stocks, !Yahoo.Symbol == input$stock_input)
      
      
      
      # default_stock <- getSymbols(input$risk_ip, src = "yahoo", from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)
      stocks_data <- data.frame("Company" = character(), "Reward" = numeric(),"Risk" = numeric())
      isolate(
        {
          for (i in 1:nrow(comparison_stocks)) {
            
            stocks_info[[i]] <- as.data.frame(na.omit(getSymbols(comparison_stocks[i,4], src = "yahoo",  from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE)))
            
          }
          
          # for (i in seq_along(info_keeper$input_info))
          # {
          #   id <- info_keeper$input_info[[i]][1]
          #   info_keeper$input_info[[i]][3] <- input[[id]]
          #   stocks_info[[i]] <- as.data.frame(na.omit(getSymbols(input[[paste0(id)]], src = "yahoo",  from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)))
          #   # stocks_info[i]
          #   # stocks_info[i] <- getSymbols(input[[paste0(id)]], src = "yahoo",  from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)
          # }
          stocks_info[[length(stocks_info)+1]] <- as.data.frame(na.omit(getSymbols(input$stock_input, src = "yahoo", from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE)))
          
          for(i in 1:length(stocks_info)){
            stock_ret <- diff(log(stocks_info[[i]][,6]))
            # stock_ret <- stock_ret[-1,]
            
            stock_mean <- mean(stock_ret)
            
            stock_sd <- sd(stock_ret)
            
            test <- data.frame("Company"= sub("\\..*", "", names(stocks_info[[i]])[6]) ,"Reward"=stock_mean,"Risk"=stock_sd)
            # final_stocks_risk[[i]] <- as.data.frame(stock_sd)
            # stocks_data <- rbind(test, data.frame("Company" = Company, Reward = SD))
            stocks_data <- rbind(stocks_data,test)
          }
        })
      
      
      # for(i in 1:n){
      #   df <- rbind(df, data.frame(x = i, y = toString(i)))
      # }
      
      # google_stock <- getSymbols("GOOGL", src = "yahoo", from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)
      # fb_stock <- getSymbols("FB", src = "yahoo",  from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)
      # apple_stock <- getSymbols("AAPL", src = "yahoo",  from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)
      # tesla_stock <- getSymbols("TSLA", src = "yahoo",  from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)
      # amazon_stock <- getSymbols("AMZN", src = "yahoo",  from = as.Date(input$risk_daterange[1]), to = as.Date(input$risk_daterange[2]), auto.assign = FALSE)
      # 
      # 
      # google_stock_ret <- diff(log(google_stock[,6]))
      # google_stock_ret <- google_stock_ret[-1,]
      # 
      # fb_stock_ret <- diff(log(fb_stock[,6]))
      # fb_stock_ret <- fb_stock_ret[-1,]
      # 
      # apple_stock_ret <- diff(log(apple_stock[,6]))
      # apple_stock_ret <- apple_stock_ret[-1,]
      # 
      # tesla_stock_ret <- diff(log(tesla_stock[,6]))
      # tesla_stock_ret <- tesla_stock_ret[-1,]
      # 
      # amazon_stock_ret <- diff(log(amazon_stock[,6]))
      # amazon_stock_ret <- amazon_stock_ret[-1,]
      # 
      # 
      # 
      # google_mean <- mean(google_stock_ret)
      # fb_mean <- mean(fb_stock_ret)
      # apple_mean <- mean(apple_stock_ret)
      # tesla_mean <- mean(tesla_stock_ret)
      # amazon_mean <- mean(amazon_stock_ret)
      # 
      # 
      # google_sd <- sd(google_stock_ret)
      # fb_sd <- sd(fb_stock_ret)
      # apple_sd <- sd(apple_stock_ret)
      # tesla_sd <- sd(tesla_stock_ret)
      # amazon_sd <- sd(amazon_stock_ret)
      # 
      # #I took the mean of log return and standard deviation of log return. The mean is assumed as the consistent rate of return while standard deviation is the risk that comes with purchasing the stock.
      # 
      # 
      # stocks_df <- data.frame("Company" = c("Google","Facebook","Apple","Tesla","Amazon") , "Reward" = c(google_mean,fb_mean,apple_mean,tesla_mean,amazon_mean) , "Risk" = c(google_sd,fb_sd,apple_sd,tesla_sd,amazon_sd))
      
      
      
      plot_ly(stocks_data, x = stocks_data$Reward, y = stocks_data$Risk, text = paste("Company: ", stocks_data$Company , "Reward : ",stocks_data$Reward ,"Risk : ",stocks_data$Risk),
              mode = "markers", color = stocks_data$Reward, size = stocks_data$Risk) %>%
        layout(xaxis = list(title = "Reward"),
               yaxis = list(title = "Risk"),
               margin = list(b = 140),
               title = "Risk vs Reward")
      
    }
    
  })
  
  output$stocksTable <- DT::renderDataTable({
    stocks.df <- generalData()[[1]]
    stocks.df#[, c("ticker", "beta", "alpha", "r2")]
  }, server = TRUE, selection = "single",
  options = list(rowCallback = JS(
    'function(row, data) {
    if (data[5] > 0)
    $("td", row).css("color", "green");
    else if (data[5] < 0)
    $("td", row).css("color", "red");
}'
  ), paging = FALSE, searching = FALSE, processing = FALSE))
  
  
  output$stocks_data <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      # browser()
      # uber_stock_mm <- StocksData()
      #
      # uber_stock_mm <- data.frame(Date = index(uber_stock_mm), coredata(uber_stock_mm) )
      
      # uber_stock <- StocksData()
      
      uber_stock <- na.omit(getSymbols(input$stock_input, src = "yahoo",  from = as.Date(input$daterange[1]), to = as.Date(input$daterange[2]), auto.assign = FALSE))
      
      #uber_stock <- na.omit(getSymbols("RUCHISOYA.NS", src = "yahoo", from = "2019-01-01", to = Sys.Date(), auto.assign = FALSE,periodicity = 'weekly'))
      
      # df <- data.frame(Date=index(uber_stock),coredata(uber_stock))
      #
      # names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted","mm10","mm30")
      
      df <- data.frame(Date=index(uber_stock),coredata(uber_stock))
      
      names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      
      pivot_table <- df
      
      df$category <- ifelse(df$Close>= df$Open, "Bullish", "Bearish")
      
      df$Percentage_change <- NA
      df$M_Upper <- NA
      df$L_Upper <- NA
      df$pattern <- NA
      df$body_percentage <- NA
      df$upper_shadow <- NA
      df$lower_shadow <- NA
      df$spinnig_top <- NA
      df$paper_umbrella <- NA
      df$shooting_star <- NA
      df$classic_high <- NA
      df$classic_low <- NA
      
      
      for(i in 1:nrow(df)){
        if(df[i,8] == "Bullish"){
          #(high-close)/close For Upper Shadow
          df[i,10] = round(((df[i,3] - df[i,5])/ df[i,5]) * 100,3)
          #(low-open)/open For Lower Shadow
          df[i,11] = round(((df[i,4] - df[i,2])/ df[i,2]) * 100,3)
          #(closing - opening)/opening
          df[i,13] = round(((df[i,5] - df[i,2])/df[i,2]) * 100 ,3)
          #(high - close)
          df[i,14] = df[i,3] - df[i,5]
          #(open - low)
          df[i,15] = df[i,2] - df[i,4]
        }
        else{
          #(low-close)/close For Lower Shadow
          df[i,10] = round(((df[i,4] - df[i,5])/ df[i,5]) * 100,3)
          #(high-open)/open For Upper shadow
          df[i,11] = round(((df[i,3] - df[i,2])/ df[i,2]) * 100,3)
          #(opening - closing)/closing
          df[i,13] = round(((df[i,2] - df[i,5])/df[i,5]) * 100 ,3)
          #(high - open)
          df[i,14] = df[i,3] - df[i,2]
          #(close - low)
          df[i,15] = df[i,5] - df[i,4]
        }
        
        if(abs(df[i,10]) <= 0.3 && abs(df[i,11]) <= 0.3){
          df[i,12] ="Marubuzo"
        }
        else{
          df[i,12] ="Non Marubozo"
        }
        if((df[i,13] <= 1.5) && (abs(df[i,14] - df[i,15]) <= 0.05) ){
          if(df[i-1,8] == df[i+1,8]){
            df[i,16] = "Spinning Top"
          }
          else{
            df[i,16] = "Opposite Spininning Top"
          }
        }
        else{
          df[i,16] = "Non Spinning Top"
        }
        if(i <= 6){
          df[i,17] = "Non Paper Umbrella"
          
        }
        else if((df[i,13] <= 2) &&  (df[i,15] >= 2 * abs(df[i,2] - df[i,5])) && round((df[i,14]/max(df[i,2],df[i,5]))*100,3) <= 2 ){
          if(length(which(df[as.numeric(i-7):as.numeric(i-1),8]=="Bullish")) >= 5){
            df[i,17] = "Hammer Paper Umbrella"
          }
          else if(length(which(df[as.numeric(i-7):as.numeric(i-1),8]=="Bearish")) >= 5){
            df[i,17] = "Hanging Paper Umbrella"
          }
          else{
            df[i,17] = "Opposite Paper Umbrella"
          }
          
        }
        else{
          df[i,17] = "Non Paper Umbrella"
        }
        
        if(i <= 6){
          df[i,18] = "Non Shooting Star"
        }
        else if((df[i,13] <= 2) &&  (df[i,14] >= 2 * abs(df[i,2] - df[i,5])) && round((df[i,15]/max(df[i,2],df[i,5]))*100,3) <= 2 ){
          if(length(which(df[as.numeric(i-7):as.numeric(i-1),8]=="Bullish")) >= 5){
            df[i,18] = "Bullish Shooting Star"
          }
          else if(length(which(df[as.numeric(i-7):as.numeric(i-1),8]=="Bearish")) >= 5){
            df[i,18] = "Bearish Shooting Star"
          }
          else{
            df[i,18] = "Opposite Shooting Star"
          }
          
        }
        else{
          df[i,18] = "Non Shooting Star"
        }
      }
      
      # browser()
      
      test <- mutate(pivot_table, last_day_close = lag(Close), last_day_high = lag(High), last_day_low = lag(Low), last_day_adjusted <- lag(Adjusted))
      
      for(i in 1:nrow(test)){
        
        if(i>1){
          #Close + High + Low /3
          pivot_point = (as.numeric(test[i,8]) + as.numeric(test[i,9]) + as.numeric(test[i,10]))/3
          
          support_1 = 2*pivot_point - test[i,9]
          resistance_1 = 2*pivot_point - test[i,10]
          
          support_2 = pivot_point - (resistance_1 - support_1)
          resistance_2 = (pivot_point - support_1) + resistance_1
          
          support_3 = pivot_point - (resistance_2 - support_2)
          resistance_3 = (pivot_point -support_2) +resistance_2
          
          str1 = ""
          str2 = ""
          if(test[i,3] >= resistance_3){
            str1 =  paste("High :", ">=R3", sep=" ")
          }
          else if(test[i,3] >= resistance_2 && test[i,3] < resistance_3){
            str1 =  paste("High :", "between R2 and R3", sep=" ")
          }
          else if(test[i,3] >= resistance_1 && test[i,3] < resistance_2){
            str1 =  paste("High :", "between R1 and R2", sep=" ")
          }
          else if(test[i,3] >= pivot_point && test[i,3] < resistance_1){
            str1 =  paste("High :", "between PP and R1", sep=" ")
          }
          else if(test[i,3] >= support_1 && test[i,3] < pivot_point){
            str1 =  paste("High :", "between S1 and PP", sep=" ")
          }
          else if(test[i,3] >= support_2 && test[i,3] < support_1){
            str1 =  paste("High :", "between S2 and S1", sep=" ")
          }
          else if(test[i,3] >= support_3 && test[i,3] < support_2){
            str1 =  paste("High :", "between S3 and S2", sep=" ")
          }
          else if(test[i,3] < support_3){
            str1 =  paste("High :", "<S3", sep=" ")
          }
          
          if(test[i,4] >= resistance_3){
            str2 =  paste("Low :", ">=R3", sep=" ")
          }
          else if(test[i,4] >= resistance_2 && test[i,4] < resistance_3){
            str2 =  paste("Low :", "between R2 and R3", sep=" ")
          }
          else if(test[i,4] >= resistance_1 && test[i,4] < resistance_2){
            str2 =  paste("Low :", "between R1 and R2", sep=" ")
          }
          else if(test[i,4] >= pivot_point && test[i,4] < resistance_1){
            str2 =  paste("Low :", "between PP and R1", sep=" ")
          }
          else if(test[i,4] >= support_1 && test[i,4] < pivot_point){
            str2 =  paste("Low :", "between S1 and PP", sep=" ")
          }
          else if(test[i,4] >= support_2 && test[i,4] < support_1){
            str2 =  paste("Low :", "between S2 and S1", sep=" ")
          }
          else if(test[i,4] >= support_3 && test[i,4] < support_2){
            str2 =  paste("Low :", "between S3 and S2", sep=" ")
          }
          else if(test[i,4] < support_3){
            str2 =  paste("Low :", "<S3", sep=" ")
          }
          
          df[i,19] = str1
          df[i,20] = str2
          
          # browser()
          
          if(i>1){
            # percentage_change <- paste(round((as.numeric(df[i,5])-as.numeric(test[i,8]))/(as.numeric(df[i,5]))*100,2)," %")
            percentage_change <- paste(round((as.numeric(df[i,5])-as.numeric(test[i,11]))/(as.numeric(test[i,11]))*100,2)," %")
          }
          else{
            percentage_change <- 0
          }
          
          df[i,9] <- percentage_change
          
          # if(df[i,5] > test[i,11]){
          #   df[i,8] <- 'Bullish'
          # }
          # else{
          #   df[i,8] <- 'Bearish'
          # }
        }
        
      }
      
      # browser()
      # View(df)
      
      
      
      
      # DT::datatable(df,extensions = c('FixedColumns'),
      #               options = list(scrollX = TRUE,
      #                              pageLength=10,
      #                              searchHighlight = TRUE,
      #                              filter = 'top'
      #               )) %>% formatStyle(
      #                 'category',
      #                 backgroundColor = styleEqual(c("Bullish", "Bearish"), c('green', 'red'))
      #               )
      df <- df %>%
        arrange(desc(Date))
      
      df$Open <- round(df$Open,2)
      df$High <- round(df$High,2)
      df$Low <- round(df$Low,2)
      df$Close <- round(df$Close,2)
      df$Adjusted <- round(df$Adjusted,2)
      df$upper_shadow <- round(df$upper_shadow,3)
      df$lower_shadow <- round(df$lower_shadow,3)
      
      
      DT::datatable(df,
                    extensions = 'Buttons',
                    options = list(scrollX = TRUE,
                                       pageLength=10,
                                       autoWidth = TRUE,
   #                                 dom = 'Bfrtip',
   #                                 buttons = c('excel','csv','print'),
                                   rowCallback=JS(
                                     'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1)
            $(row).css("background","#D7DBDD")
   }')
                                   ),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) %>% formatStyle(
        'category',
        backgroundColor = styleEqual(c("Bullish", "Bearish"), c('green', 'red'))
      )
      # DT::datatable(df,
      #           rownames = TRUE,
      #           options = list(
      #             scrollX = TRUE,
      #             pageLength=10,
      #             fixedColumns = TRUE,
      #             autoWidth = TRUE,
      #             ordering = FALSE,
      #             dom = '<"top"l>t<"bottom"f>',
      #             buttons = c('copy', 'csv', 'excel', 'pdf')
      #           ),
      #           filter = list(
      #                   position = 'top', clear = FALSE
      #                              ),
      #           class = "display", #if you want to modify via .css
      #           extensions = "Buttons"
      # )%>% formatStyle(
      #        'category',
      #        backgroundColor = styleEqual(c("Bullish", "Bearish"), c('green', 'red'))
      #      )
      
    })
  })
  
  output$twitter_sentiment <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
      
      twitter_data <- get_twitter_data()
      
      #Convert data from long to wide
      freq2 <- twitter_data %>% 
        spread(key = sentiment3, value = Freq)
      
      
      DT::datatable(freq2,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
                    ))
      
      
    })
  })
  
  output$twitter_sentiment_plot <- plotly::renderPlotly({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
      
      twitter_data <- get_twitter_data()
      
      ggplotly(ggplot() + 
                 geom_bar(mapping = aes(x = twitter_data$date, y = twitter_data$Freq, fill = twitter_data$sentiment3), stat = "identity") +
                 ylab('Sentiment Frequency') +
                 xlab('Date'))
    })
  })
  
  
  output$key_stats_income <- DT::renderDataTable({
    
    if(input$stock_input != ""){
      
      withProgress(message = 'Making plot', value = 0, {
        incProgress(0.1, detail = paste("Doing part"))
        Sys.sleep(0.1)
        
        # income_statement <- get_income_statement(input$stock_input)
        
        
        money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
        # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
        
        row_number <- which(grepl(input$stock_input, money_control_data$Company))
        
        if(input$stock_category == "Consolidated"){
          
        if(input$time_period == "Quarterly"){
          money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CIQ&scId=",money_control_data[row_number,5],sep="")
        }
        else if(input$time_period == "Half Yearly"){
          money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CIH&scId=",money_control_data[row_number,5],sep="")
        }
        else if(input$time_period == "Nine Months"){
          money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CI9&scId=",money_control_data[row_number,5],sep="")
        }
        else{
          money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CIY&scId=",money_control_data[row_number,5],sep="")
        }
        }
        else if(input$stock_category == "Standalone"){
          
          if(input$time_period == "Quarterly"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SIQ&scId=",money_control_data[row_number,5],sep="")
          }
          else if(input$time_period == "Half Yearly"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SIH&scId=",money_control_data[row_number,5],sep="")
          }
          else if(input$time_period == "Nine Months"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SI9&scId=",money_control_data[row_number,5],sep="")
          }
          else{
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SIY&scId=",money_control_data[row_number,5],sep="")
          }
        }
        
        print(money_control_url)
        
        # browser()
        
        market_financials <- read_html(money_control_url)
        
        tbls_ls <- market_financials %>%
          html_nodes("table") %>%
          html_table(fill = TRUE)
        
        if(length(tbls_ls) > 1){
          df1 <- tbls_ls[[1]]
          df2 <- tbls_ls[[2]]
          
          colnames(df1) <- c("Metric",colnames(df1[,2:ncol(df1)]))
          colnames(df2) <- colnames(df1)
          
          income_statement <- rbind(df1,df2)
        }
        else{
          income_statement <- as.data.frame(tbls_ls)
        }
        
      
      
        DT::datatable(income_statement,
                      extensions = 'Buttons',
                      options=list(dom = 'Bfrtip',
                                   buttons = c('excel','csv','print'),
                                   rowCallback=JS(
                                     'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1) 
            $(row).css("background","#D7DBDD")
   }'))
                      )
        
        
        
      })
      
    }
  })
  
  output$income_statements_plot <- renderHighchart({
    
    if(input$stock_input != ""){
      
      withProgress(message = 'Making plot', value = 0, {
        incProgress(0.1, detail = paste("Doing part"))
        Sys.sleep(0.1)
        
        # browser()
        
        money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
        # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
        
        row_number <- which(grepl(input$stock_input, money_control_data$Company))
        
        if(input$stock_category == "Consolidated"){
          if(input$time_period == "Quarterly"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CIQ&scId=",money_control_data[row_number,5],sep="")
          }
          else if(input$time_period == "Half Yearly"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CIH&scId=",money_control_data[row_number,5],sep="")
          }
          else if(input$time_period == "Nine Months"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CI9&scId=",money_control_data[row_number,5],sep="")
          }
          else{
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CIY&scId=",money_control_data[row_number,5],sep="")
          }
        }
        else if(input$stock_category == "Standalone"){
          if(input$time_period == "Quarterly"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SIQ&scId=",money_control_data[row_number,5],sep="")
          }
          else if(input$time_period == "Half Yearly"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SIH&scId=",money_control_data[row_number,5],sep="")
          }
          else if(input$time_period == "Nine Months"){
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SI9&scId=",money_control_data[row_number,5],sep="")
          }
          else{
            money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SIY&scId=",money_control_data[row_number,5],sep="")
          }
        }
        
        print(money_control_url)
        
        
        
        # browser()
        
        market_financials <- read_html(money_control_url)
        
        tbls_ls <- market_financials %>%
          html_nodes("table") %>%
          html_table(fill = TRUE)
        
        if(length(tbls_ls) > 1){
          df1 <- tbls_ls[[1]]
          df2 <- tbls_ls[[2]]
          
          colnames(df1) <- c("Metric",colnames(df1[,2:ncol(df1)]))
          colnames(df2) <- colnames(df1)
          
          income_statement <- rbind(df1,df2)
        }
        else{
          income_statement <- as.data.frame(tbls_ls)
        }
        
        # browser()
        
        final <- cbind(income_statement[,1],income_statement[,6],income_statement[,5],income_statement[,4],income_statement[,3],income_statement[,2])
        
        colnames(final) <- c("Metric",colnames(income_statement)[6],colnames(income_statement)[5],colnames(income_statement)[4],colnames(income_statement)[3],colnames(income_statement)[2])
        
       
        
        temp <- t(final)
        temp <- as.data.frame(temp)
        names(temp) <- as.matrix(temp[1, ])
        temp <- temp[-1, ]
        temp[] <- lapply(temp, function(x) type.convert(as.character(x)))
        temp <- cbind(Dates = rownames(temp), temp)
        rownames(temp) <- 1:nrow(temp)
        # browser()
        if(colnames(temp)[2] == "Interest Earned"){
          highchart() %>%
            hc_add_series(temp, "column", hcaes(x = Dates, y =  as.numeric(gsub(",", "", 	`Interest Earned`))), name = "Interest Earned",dataLabels = list(enabled = TRUE, format='{point.y}')) %>%
            hc_add_series(temp, "line", hcaes(x = Dates, y =  as.numeric(gsub(",", "", 	`Total Income`))), name = "Total Income",dataLabels = list(enabled = TRUE, format='{point.y}')) %>%
            hc_exporting(enabled = TRUE,filename = "custom-file-name")
        }
        else{
        
        highchart() %>%
            hc_add_series(temp, "column", hcaes(x = Dates, y =  as.numeric(gsub(",", "", Sales))), name = "Sales", dataLabels = list(enabled = TRUE, format='{point.y}')) %>%
            hc_add_series(temp, "line", hcaes(x = Dates, y =  as.numeric(gsub(",", "", 	`Total Income`))), name = "Total Income" ,dataLabels = list(enabled = TRUE, format='{point.y}')) %>%
            hc_exporting(enabled = TRUE,filename = "custom-file-name")
        }
        # highchart() %>%
        #   hc_add_series(income_statement[,6], type = "column", name = colnames(income_statement)[6])  %>%
        #   hc_add_series(income_statement[,5], type = "column", name = colnames(income_statement)[5])  %>%
        #   hc_add_series(income_statement[,4], type = "column", name = colnames(income_statement)[4])  %>%
        #   hc_add_series(income_statement[,3], type = "column", name = colnames(income_statement)[3])  %>%
        #   hc_add_series(income_statement[,2], type = "column", name = colnames(income_statement)[2]) %>%
        #   
        #   hc_xAxis(categories = income_statement[,1]) %>%
        #   hc_plotOptions(series = list(showInLegend = TRUE,
        #                                dataLabels = list(enabled = TRUE)))
        
        
        
      })
      
    }
   
  })
  
  output$key_stats_balance <- DT::renderDataTable({
    
    if(input$stock_input != ""){
      
      withProgress(message = 'Making plot', value = 0, {
        incProgress(0.1, detail = paste("Doing part"))
        Sys.sleep(0.1)
        
        # balance_sheet <- get_balance_sheet(input$stock_input)
        
        
        money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
        # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
        
        row_number <- which(grepl(input$stock_input, money_control_data$Company))
        
        if(input$stock_category == "Consolidated"){
        money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CB&scId=",money_control_data[row_number,5],sep="")
        }
        else if(input$stock_category == "Standalone"){
          money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SB&scId=",money_control_data[row_number,5],sep="")
        }
        
        
        # browser()
        
        market_financials <- read_html(money_control_url)
        
        tbls_ls <- market_financials %>%
          html_nodes("table") %>%
          html_table(fill = TRUE)
        
        # tbls_ls <- as.data.frame(tbls_ls)
        
        df1 <- tbls_ls[[1]]
        
        df2 <- tbls_ls[[2]]
        
        df3 <- tbls_ls[[3]]
        
        colnames(df1) <- c("Metric",colnames(df1[,2:ncol(df1)]))
        colnames(df2) <- colnames(df1)
        colnames(df3) <- colnames(df1)
        
        income_statement <- rbind(df1,df2,df3)
        
        DT::datatable(income_statement,
                      extensions = 'Buttons',
                      options=list(dom = 'Bfrtip',
                                   buttons = c('excel','csv','print'),
                                   rowCallback=JS(
                                     'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1) 
            $(row).css("background","#D7DBDD")
   }'))
                      )
      })
      
    }
  })
  
  
  
  output$key_stats_cash_flows <- DT::renderDataTable({
    
    if(input$stock_input != ""){
      
      withProgress(message = 'Making plot', value = 0, {
        incProgress(0.1, detail = paste("Doing part"))
        Sys.sleep(0.1)
        
        # income_statement <- get_income_statement(input$stock_input)
        
        
        money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
        # money_control_data <- read.csv(paste0('/cloud/project/Stocks_Analysis/data/', 'Money_Control_Tickers.csv'), stringsAsFactors = F)
        
        row_number <- which(grepl(input$stock_input, money_control_data$Company))
        
        if(input$stock_category == "Consolidated"){
        money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=CC&scId=",money_control_data[row_number,5],sep="")
        }
        if(input$stock_category == "Standalone"){
          money_control_url <- paste("https://www.moneycontrol.com/mc/widget/financials/getFinancialData?classic=true&requestType=SC&scId=",money_control_data[row_number,5],sep="")
        }
        
        
        # browser()
        
        market_financials <- read_html(money_control_url)
        
        tbls_ls <- market_financials %>%
          html_nodes("table") %>%
          html_table(fill = TRUE)
        
        income_statement <- as.data.frame(tbls_ls)
        
        
        DT::datatable(income_statement,
                      extensions = 'Buttons',
                      options=list(dom = 'Bfrtip',
                                   buttons = c('excel','csv','print'),
                                   rowCallback=JS(
                                     'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1) 
            $(row).css("background","#D7DBDD")
   }'))
                      )
      })
      
    }
    
    # withProgress(message = 'Making plot', value = 0, {
    #   incProgress(0.2, detail = paste("Doing part"))
    #   
    #   cash_flows <- get_cash_flows(input$stock_input)
    #   
    #   
    #   DT::datatable(cash_flows,extensions = c('FixedColumns'),
    #                 options = list(scrollX = TRUE, pageLength=10, backgroundColor = "#F7080880"
    #                 ))
    #   
    #   
    # })
  })
  
  
  output$options_breakeven_point <- DT::renderDataTable({

    
    # browser()
    
    result <- getpayoofdiagram()
    
    final_output <- data.frame(Metric = character(0),Value = character(0))
    
    
    final_output[1,"Metric"] <- "Break Evens"
    final_output[1,"Value"] <- min(result[result$payoff>=0,]$prices)
    final_output[2,"Metric"] <- "Max Loss"
    final_output[2,"Value"] <- input$bot_lot_size*min(result$payoff)
    final_output[3,"Metric"] <- "Max Profit"
    final_output[3,"Value"] <- input$bot_lot_size*max(result$payoff)
    
    
    
    DT::datatable(final_output
                  
    )
    
    
    
  })
  
  output$options_market_price <- renderValueBox({
    
    login_params = list(api_key = 'LPUVlRxd')
    object = create_connection_object(list(api_key="LPUVlRxd"))
    
    session_data = generate_session(object,"J95213","start@123")
    
    if(input$options_input == "BANKNIFTY"){
      ltp <- as.numeric(get_ltp_data(object = session_data,exchange = "NSE",tradingsymbol= "BANKNIFTY",symboltoken= "26009")$data$ltp)
      
    }else{
      ltp <- as.numeric(get_ltp_data(object = session_data,exchange = "NSE",tradingsymbol= "NIFTY",symboltoken= "26000")$data$ltp)
      
    }
    
    infoBox(
      "Spot Price",
      round(as.numeric(ltp),2),
      icon = icon("fas fa-dollar-sign")
    )
    
  })
  
  
  output$options_oi_pcr <- renderValueBox({
    
    options_ip <- input$options_input
    options_expiry <- input$options_expiry
    
    
    
    # url = 'https://www.nseindia.com/api/option-chain-indices?symbol=BANKNIFTY'
    url = paste('https://www.nseindia.com/api/option-chain-indices?symbol=',options_ip, sep="")
    
    response = GET(url, add_headers("user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.79 Safari/537.36"))
    
    temp <- content(response)$records$data
    
    data_raw <- tibble::enframe(unlist(temp))
    # data_raw
    
    data_raw <- data.frame(data_raw)
    
    
    d <- within(data_raw, {
      name <- as.character(name)
      ID <- ave(name, name, FUN=seq_along)
    })
    
    
    
    final_strike_data <- reshape2::dcast(d, ID ~ name, value.var="value")
    
    colnames(final_strike_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice")
    
    final_options_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice
                          from final_strike_data t1 
                          left join final_strike_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice
                                "
    )
    
    final_options_data <- final_options_data %>% dplyr::filter((expiryDate == as.character(options_expiry)))
    
    
    final_options_data[is.na(final_options_data)] <- 0
    
    oi_pcr <- sum(as.numeric(final_options_data$Put_openInterest))*1.00/sum(as.numeric(final_options_data$Call_openInterest))
    
    
    
    infoBox(
      "OI PCR",
      round(oi_pcr,2),
      icon = icon("fas fa-percentage")
    )
    
  })
  
  output$options_volume_pcr <- renderValueBox({
    
    options_ip <- input$options_input
    options_expiry <- input$options_expiry
    
    
    
    # url = 'https://www.nseindia.com/api/option-chain-indices?symbol=BANKNIFTY'
    url = paste('https://www.nseindia.com/api/option-chain-indices?symbol=',options_ip, sep="")
    
    response = GET(url, add_headers("user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.79 Safari/537.36"))
    
    temp <- content(response)$records$data
    
    data_raw <- tibble::enframe(unlist(temp))
    # data_raw
    
    data_raw <- data.frame(data_raw)
    
    
    d <- within(data_raw, {
      name <- as.character(name)
      ID <- ave(name, name, FUN=seq_along)
    })
    
    
    
    final_strike_data <- reshape2::dcast(d, ID ~ name, value.var="value")
    
    colnames(final_strike_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice")
    
    final_options_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice
                          from final_strike_data t1 
                          left join final_strike_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice
                                "
    )
    
    final_options_data <- final_options_data %>% dplyr::filter((expiryDate == as.character(options_expiry)))
    
    
    final_options_data[is.na(final_options_data)] <- 0
    
    volume_pcr <- sum(as.numeric(final_options_data$Put_totalTradedVolume))*1.00/sum(as.numeric(final_options_data$Call_totalTradedVolume))
    
    
    
    infoBox(
      "Volume PCR",
      round(volume_pcr,2),
      icon = icon("fas fa-percentage")
    )
    
  })
  
  output$options_intraday_sign <- renderValueBox({
    
    options_ip <- input$options_input
    options_expiry <- input$options_expiry
    
    
    
    # url = 'https://www.nseindia.com/api/option-chain-indices?symbol=BANKNIFTY'
    url = paste('https://www.nseindia.com/api/option-chain-indices?symbol=',options_ip, sep="")
    
    response = GET(url, add_headers("user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.79 Safari/537.36"))
    
    temp <- content(response)$records$data
    
    data_raw <- tibble::enframe(unlist(temp))
    # data_raw
    
    data_raw <- data.frame(data_raw)
    
    
    d <- within(data_raw, {
      name <- as.character(name)
      ID <- ave(name, name, FUN=seq_along)
    })
    
    
    
    final_strike_data <- reshape2::dcast(d, ID ~ name, value.var="value")
    
    colnames(final_strike_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice")
    
    final_options_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice
                          from final_strike_data t1 
                          left join final_strike_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice
                                "
    )
    
    final_options_data <- final_options_data %>% dplyr::filter((expiryDate == as.character(options_expiry)))
    
    
    final_options_data[is.na(final_options_data)] <- 0
    
    direction_call <- ifelse(sum(as.numeric(final_options_data$Call_changeinOpenInterest)) > sum(as.numeric(final_options_data$Put_changeinOpenInterest)),"Bearish","Bullish")
    
    
    
    
    infoBox(
      "Intraday",
      direction_call,
      icon = icon("fas fa-compass")
    )
    
  })
  
  
  output$market_cap <- renderInfoBox({
   # browser()
    if(input$stock_input != ""){
    infoBox(
      "Market Cap",
      round(as.numeric(stocks_valuation$data$MKTCAP),0),
      icon = icon("fas fa-dollar-sign")
    )
    }
    else{
      infoBox(
        "Market Cap",
        0,
        icon = icon("fas fa-dollar-sign")
      )
    }
  })
  
  output$current_PE <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "Current P/E",
      # as.numeric(stocks_valuation$data$PE),
      as.numeric(stocks_valuation$data$PECONS),
      icon = icon("fas fa-tag")
    )
    }
    else{
      infoBox(
        "Current P/E",
        0,
        icon = icon("fas fa-tag")
      )
    }
  })
  
  output$industry_PE <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "Industry P/E",
      as.numeric(stocks_valuation$data$IND_PE),
      icon = icon("fas fa-industry")
    )
    }
    else{
      infoBox(
        "Industry P/E",
        0,
        icon = icon("fas fa-industry")
      )
    }
  })
  
  output$p_b <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "Price/Book",
      round(as.numeric(stocks_valuation$data$pricecurrent)/as.numeric(stocks_valuation$data$BV),2),
      icon = icon("fas fa-money-check")
    )
    }
    else{
      infoBox(
        "Price/Book",
        0,
        icon = icon("fas fa-money-check")
      )
    }
  })
  
  output$SC_TTM <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "EPS (TTM)",
      # as.numeric(stocks_valuation$data$SC_TTM),
      as.numeric(stocks_valuation$data$sc_ttm_cons),
      icon = icon("fas fa-share-square")
    )
    }
    else{
      infoBox(
        "EPS (TTM)",
        0,
        icon = icon("fas fa-share-square")
      )
    }
  })
  
  output$FV <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "Face Value (RS)",
      as.numeric(stocks_valuation$data$FV),
      icon = icon("fas fa-magnet")
    )
    }
    else{
      infoBox(
        "Face Value (RS)",
        0,
        icon = icon("fas fa-magnet")
      )
    }
  })
  
  output$DIVPR <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "Dividend (%)",
      as.numeric(stocks_valuation$data$DYCONS),
      icon = icon("fas fa-divide")
    )
    }
    else{
      infoBox(
        "Dividend Yield",
        0,
        icon = icon("fas fa-divide")
      )
    }
  })
  
  output$P_C <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "Put/Call",
      as.numeric(stocks_valuation$data$P_C),
      icon = icon("fas fa-phone-volume")
    )
    }
    else{
      infoBox(
        "P/C",
        0,
        icon = icon("fas fa-phone-volume")
      )
    }
  })
  
  output$DELV <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "Deliverables (%)",
      as.numeric(stocks_valuation$data$DELV),
      icon = icon("fas fa-truck")
    )
    }
    else{
      infoBox(
        "Deliverables (%)",
        0,
        icon = icon("fas fa-truck")
      )
    }
  })
  
  output$AvgDelVolPer_3day <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "3 Day Delivery Average %",
      as.numeric(stocks_valuation$data$AvgDelVolPer_3day),
      icon = icon("fas fa-dice-three")
    )
    }
    else{
      infoBox(
        "3 Day Delivery Average %",
        0,
        icon = icon("fas fa-dice-three")
      )
    }
  })
  
  output$AvgDelVolPer_5day <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "5 Day Delivery Average %",
      as.numeric(stocks_valuation$data$AvgDelVolPer_5day),
      icon = icon("fab fa-html5")
    )
    }
    else{
      infoBox(
        "5 Day Delivery Average %",
        0,
        icon = icon("fab fa-html5")
      )
    }
  })
  
  output$AvgDelVolPer_8day <- renderInfoBox({
    if(input$stock_input != ""){
    infoBox(
      "8 Day Delivery Average %",
      as.numeric(stocks_valuation$data$AvgDelVolPer_8day),
      icon = icon("fas fa-spider")
    )
    }
    else{
      infoBox(
        "8 Day Delivery Average %",
        0,
        icon = icon("fas fa-spider")
      )
    }
  })
  
  
  output$nifty_pre_open <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      # browser()
      print('Before nifty')
      nifty_data <- nse2r::nse_preopen_nifty()
      print('After nifty')
      nifty_data <- nifty_data %>%
        arrange(desc(percent_change))
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) %>% formatStyle(
        c('change','percent_change'),
        color  = styleInterval(c(0), c('red', 'green')),
        fontWeight = 'bold'
      ) %>%
        formatStyle(
          'corp_action',
          color  = 'orange',
          fontWeight = 'bold'
        )
      
    })
    
  })
  
  output$nifty_bank_pre_open <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_preopen_nifty_bank()
      
      nifty_data <- nifty_data %>%
        arrange(desc(percent_change))
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) %>% formatStyle(
        c('change','percent_change'),
        color  = styleInterval(c(0), c('red', 'green')),
        fontWeight = 'bold'
      )%>%
        formatStyle(
          'corp_action',
          color  = 'orange',
          fontWeight = 'bold'
        )
      
    })
    
  })
  
  output$nifty_top_gainers <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_stock_top_gainers()
      
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) %>%
        formatStyle(
          'last_corp_announcement',
          color  = 'orange',
          fontWeight = 'bold'
        )%>%
        formatStyle(
          'percent_change',
          color  = 'green',
          fontWeight = 'bold'
        )
      
    })
    
  })
  
  output$nifty_top_losers <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_stock_top_losers()
      
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) %>%
        formatStyle(
          'last_corp_announcement',
          color  = 'orange',
          fontWeight = 'bold'
        ) %>%
        formatStyle(
          'percent_change',
          color  = 'red',
          fontWeight = 'bold'
        )
      
    })
    
  })
  
  output$nifty_most_traded <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_stock_most_traded()
      
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) 
      
    })
    
  })
  
  output$nifty_52_week_high <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_stock_year_high()
      
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      )%>%
        formatStyle(
          c('new_high','percent_change'),
          color  = 'green',
          fontWeight = 'bold'
        )  
      
    })
    
  })
  
  output$nifty_52_week_low <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_stock_year_low()
      
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      )%>%
        formatStyle(
          c('new_low','percent_change'),
          color  = 'green',
          fontWeight = 'bold'
        )  
      
    })
    
  })
  
  output$future_top_gainers <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_fo_top_gainers()
      
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      )%>%
        formatStyle(
          'last_corp_announcement',
          color  = 'orange',
          fontWeight = 'bold'
        ) %>%
        formatStyle(
          'percent_change',
          color  = 'green',
          fontWeight = 'bold'
        ) 
      
    })
    
  })
  
  output$future_top_losers <- DT::renderDataTable({
    
    withProgress(message = 'Fetching Data : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      
      nifty_data <- nse2r::nse_fo_top_losers()
      
      
      DT::datatable(nifty_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = FALSE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      )%>%
        formatStyle(
          'last_corp_announcement',
          color  = 'orange',
          fontWeight = 'bold'
        ) %>%
        formatStyle(
          'percent_change',
          color  = 'red',
          fontWeight = 'bold'
        ) 
      
    })
    
  })
  
  
  output$stocks_candle <- renderPlotly({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.2, detail = paste("Doing part"))
      # browser()
      # if(input$candle_stick_range == "1d"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "5d"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "1mo"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=30m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "6mo"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "ytd"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "1y"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "5y"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else {
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # browser()
      stock_row <- input$bot_buy_and_sell_rows_selected
      
      Signal_df <- read.csv(paste0(getwd(),"/data/Signal_df.csv", sep = ""))
      
      stock <- Signal_df[stock_row,"Stock"]
      
      if(length(stock) == 0){
        stock = input$bot_stock_default
      }
      
      if(input$bot_timeframe == "1m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "2m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "5m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "15m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "1h"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "4h"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "1d"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      # else if(input$candle_stick_range == "1y"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "5y"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      else {
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      
      
      
      
      
      # response_data <- fromJSON("https://query1.finance.yahoo.com/v8/finance/chart/INDIGO.NS?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet")
      
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # browser()
      
      if(nrow(final_data) == 0){
        stock_timestamp <- response_data$chart$result[[2]][[1]]
        Close <- response_data$chart$result[[3]]$quote[[1]]$close
        High <- response_data$chart$result[[3]]$quote[[1]]$high
        Low <- response_data$chart$result[[3]]$quote[[1]]$low
        Open <- response_data$chart$result[[3]]$quote[[1]]$open
        Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      }
      

      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      else{
        final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      
      final_data <- na.omit(final_data)
      
      # browser()
      
      isSupport <- function(df,i){
        support = (df[i,"Low"] < df[i-1,"Low"]) & (df[i,"Low"] < df[i+1,"Low"]) & (df[i+1,"Low"] < df[i+2,"Low"]) & (df[i-1,"Low"] < df[i-2,"Low"])
        return(support)
      }
      
      isResistance <- function(df,i){
        resistance = (df[i,"High"] > df[i-1,"High"]) & (df[i,"High"] > df[i+1,"High"]) & (df[i+1,"High"] > df[i+2,"High"]) & (df[i-1,"High"] > df[i-2,"High"])
        return(resistance)
      }
      
      support_data = data.frame()
      resistance_data = data.frame()
      
      for (i in 5:nrow(final_data)-2) {
        if(isSupport(final_data,i)){
          support_data = rbind(support_data,final_data[i,])
        }
        if(isResistance(final_data,i)){
          resistance_data = rbind(resistance_data,final_data[i,])
        }
      }
      # browser()
      current_close <- final_data[nrow(final_data),"Close"]
      
      
      ######      5 Min Dynamic Support and Resistance #################
      
      # if(!is_empty(support_data)){
      #   support_data <- support_data %>% mutate(differ = current_close - Low)
      #   support_data <- support_data %>% filter(differ < 0)
      #   support_data <- support_data[order(-support_data$differ),]
      #   if(nrow(support_data) > 0){
      #     rownames(support_data) <- 1:nrow(support_data)
      #     supp_5_min <- support_data[1,"Low"]
      #   }else{
      #     supp_5_min <- current_close
      #   }
      #   
      # }else {
      #   supp_5_min <- current_close
      # }
      # 
      # if(!is_empty(resistance_data)){
      #   resistance_data <- resistance_data %>% mutate(differ = High - current_close)
      #   resistance_data <- resistance_data %>% filter(differ > 0)
      #   resistance_data <- resistance_data[order(resistance_data$differ),]
      #   if(nrow(resistance_data) > 0){
      #     rownames(resistance_data) <- 1:nrow(resistance_data)
      #     res_5_min <- resistance_data[1,"High"]
      #   }else{
      #     res_5_min <- current_close
      #   }
      # }
      # else {
      #   res_5_min <- current_close
      # }
      
      # fig <- final_data %>% plot_ly(x = ~Date, type="candlestick",
      #                               open = ~Open, close = ~Close,
      #                               high = ~High, low = ~Low) 
      # fig <- fig %>% layout(title = "Basic Candlestick Chart",xaxis = list(type = "category"))
      # 
      # fig
      # browser()
      
      
      #########     15 Min Dynamic Support and Resitance ##############
      
      
      # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # stock_timestamp <- response_data$chart$result[[1]]$timestamp
      # Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      # High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      # Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      # Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      # Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      # final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      # 
      # # browser()
      # 
      # if(nrow(final_data) == 0){
      #   stock_timestamp <- response_data$chart$result[[2]][[1]]
      #   Close <- response_data$chart$result[[3]]$quote[[1]]$close
      #   High <- response_data$chart$result[[3]]$quote[[1]]$high
      #   Low <- response_data$chart$result[[3]]$quote[[1]]$low
      #   Open <- response_data$chart$result[[3]]$quote[[1]]$open
      #   Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
      #   
      #   
      #   final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      # }
      # 
      # 
      # colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      # 
      # # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      # if(typeof(final_data_15$V1) == "list"){
      #   final_data_15 <- final_data_15[-c(which(final_data_15$Close == "NULL")),]
      #   new_stock_timestamp <- unlist(final_data_15$V1)
      #   Close <- unlist(final_data_15$Close)
      #   High <- unlist(final_data_15$High)
      #   Open <- unlist(final_data_15$Open)
      #   Low <- unlist(final_data_15$Low)
      #   Volume <- unlist(final_data_15$Volume)
      #   
      #   final_data_15 <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
      #   
      #   final_data_15$dates <- as.POSIXct(final_data_15$new_stock_timestamp, origin="1970-01-01")
      #   
      #   final_data_15 <- final_data_15 %>% select(dates, Open, High, Low, Close,Volume)
      # }
      # else{
      #   final_data_15$dates <- as.POSIXct(final_data_15$V1, origin="1970-01-01")
      #   
      #   final_data_15 <- final_data_15 %>% select(dates, Open, High, Low, Close,Volume)
      # }
      # 
      # support_data = data.frame()
      # resistance_data = data.frame()
      # 
      # for (i in 5:nrow(final_data_15)-2) {
      #   if(isSupport(final_data_15,i)){
      #     support_data = rbind(support_data,final_data_15[i,])
      #   }
      #   if(isResistance(final_data_15,i)){
      #     resistance_data = rbind(resistance_data,final_data_15[i,])
      #   }
      # }
      # 
      # if(!is_empty(support_data)){
      #   support_data <- support_data %>% mutate(differ = current_close - Low)
      #   support_data <- support_data %>% filter(differ < 0)
      #   support_data <- support_data[order(-support_data$differ),]
      #   if(nrow(support_data) > 0){
      #     rownames(support_data) <- 1:nrow(support_data)
      #     supp_15_min <- support_data[1,"Low"]
      #   }else{
      #     supp_15_min <- current_close
      #   }
      #   
      # }else {
      #   supp_15_min <- current_close
      # }
      # 
      # if(!is_empty(resistance_data)){
      #   resistance_data <- resistance_data %>% mutate(differ = High - current_close)
      #   resistance_data <- resistance_data %>% filter(differ > 0)
      #   resistance_data <- resistance_data[order(resistance_data$differ),]
      #   if(nrow(resistance_data) > 0){
      #     rownames(resistance_data) <- 1:nrow(resistance_data)
      #     res_15_min <- resistance_data[1,"High"]
      #   }else{
      #     res_15_min <- current_close
      #   }
      # }
      # else {
      #   res_15_min <- current_close
      # }
      
      
      # browser()
      stock_data <- getSymbols(stock,src = "yahoo" , from = "2020-07-01", to = Sys.Date(),periodicity = "daily", auto.assign = FALSE)
      stock_data <- data.frame(Date = index(stock_data), coredata(stock_data) )
      names(stock_data) <- c("dates", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      
      df <- tail(stock_data,1)
      
      # browser()
      
      pivot_point = numeric(0)
      pivot_s1 = numeric(0)
      pivot_r1 = numeric(0)
      pivot_s2 = numeric(0)
      pivot_r2 = numeric(0)
      
      if(input$ip_technical == "classical_sr"){
        
        pivot_point = (df$High + df$Low + df$Close)/3
        
        support_1 = round((2*pivot_point) - df$High,2)
        
        resistance_1 = round((2*pivot_point) - df$Low,2)
        
        support_2 = round(pivot_point - (resistance_1 - support_1),2)
        
        resistance_2 = round((pivot_point - support_1 ) + resistance_1,2)
        
        resistance_3 = round((pivot_point - support_2 ) + resistance_2,2)
        
        support_3 = round(pivot_point - (resistance_2 - support_2),2)
        

        pivot_s1 <- support_1
        pivot_r1 <- resistance_1
        pivot_s2 <- support_2
        pivot_r2 <- resistance_2
        
      }else if(input$ip_technical == "fib_sr"){
        Fibonacci_levels = data.frame()
        
        pivot_point = (df$High + df$Low + df$Close)/3
        
        price_difference = (df$High - df$Low)
        
        R1 = round((23.6*price_difference/100) + pivot_point,2)
        
        R2 = round((38.2*price_difference/100) + pivot_point,2)
        
        R3 = round((50*price_difference/100) + pivot_point,2)
        
        R4 = round((61.8*price_difference/100) + pivot_point,2)
        
        R5 = round((100*price_difference/100) + pivot_point,2)
        
        S1 = round(pivot_point - (23.6*price_difference/100),2)
        
        S2 = round(pivot_point - (38.2*price_difference/100),2)
        
        S3 = round(pivot_point - (50*price_difference/100),2)
        
        S4 = round(pivot_point - (61.8*price_difference/100),2)
        
        S5 = round(pivot_point - (100*price_difference/100),2)
        
        pivot_s1 <- S2
        pivot_r1 <- R2
        pivot_s2 <- S4
        pivot_r2 <- R4
        
        Fibonacci_levels <- rbind(Fibonacci_levels, c(R4,R2,S2,S4))
        
        colnames(Fibonacci_levels) = c("Fibonacci R2","Fibonacci R1","Fibonacci S1","Fibonacci S2")
        
      }else{
        Stock_df <- na.omit(getSymbols(stock, src = "yahoo", from = "2019-05-10", to = Sys.Date() , auto.assign = FALSE))
        
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        # cat(colnames(Stock_df))
        # cat(ncol(Stock_df))
        # cat(knitr::knit_print(DT::datatable(Stock_df, width = "100%")))
        
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
        
        Stock_df <- data.frame(Date = index(Stock_df), coredata(Stock_df) )
        
        # print(tail(Stock_df,5))
        
        # Stock_df$v7_MA = ma(Stock_df$Close, order=7)
        # Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
        
        rental_ma <- ts(na.omit(Stock_df$Close), frequency=30)
        decomp_rental <- stl(rental_ma, s.window="periodic")
        #plot(decomp_rental)
        adj_rental <- seasadj(decomp_rental)
        
        fit_s<-auto.arima(adj_rental, seasonal=TRUE)
        fcast <- forecast(fit_s, h=10)
        
        df <- fortify(fcast)
        
        # print(df)
        
        temp_end_Date = as.Date(Sys.Date())
        
        temp_prediction_date <- temp_end_Date + 24
        
        date_range <- seq.Date(temp_end_Date, temp_prediction_date, 1)
        
        next_days <- date_range[!weekdays(date_range) %in% (c("Saturday", "Sunday")) ]
        
        next_days <- as.data.frame(next_days)
        
        next_days <- next_days[1:10,]
        
        original_dates <- Stock_df$Date
        
        original_dates <- as.data.frame(original_dates)
        next_days <- as.data.frame(next_days)
        
        names(original_dates) <- c("Date")
        names(next_days) <- c("Date")
        
        final_dates <- rbind(original_dates,next_days)
        
        df$Date <- as.Date(final_dates$Date)
        
        temp <- head(df[which(df$`Point Forecast`>0),],1)
        
        forecasted <- as.numeric(temp$`Point Forecast`)
        ci_low_80 <- as.numeric(temp$`Lo 80`)
        ci_high_80 <- as.numeric(temp$`Hi 80`)
        ci_low_95 <- as.numeric(temp$`Lo 95`)
        ci_high_95 <- as.numeric(temp$`Hi 95`)
        
        pivot_point <- round(forecasted,2)
        pivot_s1 <- round(ci_low_80,2)
        pivot_r1 <- round(ci_high_80,2)
        pivot_s2 <- round(ci_low_95,2)
        pivot_r2<- round(ci_high_95,2)
      }
      
      names(final_data) <- c("dates", "Open", "High", "Low", "Close", "Volume")
      
      # Clean data frame and calculate EMA and RSI indicators
      xbt <- final_data %>% 
        select(dates, Open, High, Low, Close, Volume) %>%
        # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
        mutate(
          ema = TTR::EMA(Close, 10),
          rsi = TTR::RSI(Close, 10),
          # supp_5_min = supp_5_min,
          # res_5_min = res_5_min,
          # supp_15_min = supp_15_min,
          # res_15_min = res_15_min,
          pivot_point = pivot_point,
          pivot_s1 = pivot_s1,
          pivot_r1 = pivot_r1,
          pivot_s2 = pivot_s2,
          pivot_r2 = pivot_r2
        )
      
      
      # Plot Candlestick chart and same to variable
      p1 <- xbt %>% plot_ly(
        type = "candlestick",
        x = ~dates, 
        open = ~Open, high = ~High, low = ~Low, close = ~Close,
        name = "candles"
      ) %>%
        add_lines(x = ~dates, y = ~ema, inherit = F, line = list(color = "red", width = 2), name = "EMA") %>%
        # add_lines(x = ~dates, y = ~supp_5_min, inherit = F, line = list(color = "black", width = 2), name = "5 min Support") %>%
        # add_lines(x = ~dates, y = ~res_5_min, inherit = F, line = list(color = "blue", width = 2), name = "5 min Resistance") %>%
        # add_lines(x = ~dates, y = ~supp_15_min, inherit = F, line = list(color = "black", width = 2), name = "15 min Support") %>%
        # add_lines(x = ~dates, y = ~res_15_min, inherit = F, line = list(color = "blue", width = 2), name = "15 min Resistance") %>%
        add_lines(x = ~dates, y = ~pivot_point, inherit = F, line = list(color = "pink", width = 2), name = "Pivot Point") %>%
        add_lines(x = ~dates, y = ~pivot_s1, inherit = F, line = list(color = "red", width = 2), name = "Support 1") %>%
        add_lines(x = ~dates, y = ~pivot_r1, inherit = F, line = list(color = "green", width = 2), name = "Resistance 1") %>%
        add_lines(x = ~dates, y = ~pivot_s2, inherit = F, line = list(color = "red", width = 2), name = "Support 2") %>%
        add_lines(x = ~dates, y = ~pivot_r2, inherit = F, line = list(color = "green", width = 2), name = "Resistance 2") %>%
        layout(title = "XBTUSD 5m", xaxis = list(rangeslider = list(visible = F),type = "category"))
      
      #Plot Volume 
      
      p2 <- xbt %>% plot_ly(type = "bar", x = ~dates, y = ~Volume,
                            marker = list(color = 'barcols'),
                            name = "Vol")%>%
        layout(title = "Volume",xaxis = list(rangeslider = list(visible = F),type = "category"))
      
      
      # Plot RSI as separate plot
      p3 <- xbt %>% plot_ly(
        type = "scatter", mode = "lines", 
        x = ~dates,  y = ~rsi, name = "RSI")%>%
        layout(title = "RSI", xaxis = list(rangeslider = list(visible = F),type = "category"))
      
      #Plot MACD
      macd <- data.frame(TTR::MACD(xbt$Close, 12, 26, 9))
      macd$diff <- macd$macd - macd$signal
      
      p4 <- xbt %>% plot_ly() %>%
        
        add_lines(x = ~dates, y = macd$macd,
                  line = list(width = 1, color = "#8c8c8c"), name = "MACD") %>%
        
        add_lines(x = ~dates, y = macd$signal,
                  line = list(width = 1, color = "#ff6666"), name = "Signal") %>%
        
        add_bars(x =~dates, y = macd$diff,
                 marker = list(color = "#bfbfbf"), name = "Diff") %>%
        layout(title = paste(stock), xaxis = list(rangeslider = list(visible = F),type = "category"))
      
      
      
      
      # Join 2 plots together with the same X axis
      fig <- subplot(p1,p2,p3,p4, nrows = 4, shareX = T, heights = c(0.4, 0.2,0.2,0.2))
      
      fig
      
    })
  })
  
  output$stocks_candle_realtime <- renderHighchart({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(0.1, detail = paste("Doing part"))
      # Sys.sleep(0.1)
      
      # browser()
      if(input$candle_stick_range == "1d"){
        # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5d"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=30m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "6mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "ytd"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else {
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      
      
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # browser()
      
      if(nrow(final_data) == 0){
        stock_timestamp <- response_data$chart$result[[2]][[1]]
        Close <- response_data$chart$result[[3]]$quote[[1]]$close
        High <- response_data$chart$result[[3]]$quote[[1]]$high
        Low <- response_data$chart$result[[3]]$quote[[1]]$low
        Open <- response_data$chart$result[[3]]$quote[[1]]$open
        Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      }
      
      
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      else{
        final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      
      final_data <- na.omit(final_data)
      # browser()
      
      test <- mutate(final_data, last_close = lag(Close))
      
      test <- mutate(test, last_volume = lag(Volume))
      
      
      
      test$count_pos <- ifelse(final_data$Close >= test$last_close, 1, 0)
      test$count_volume_pos <- ifelse(final_data$Volume >= test$last_volume, 1, 0)
      
      
      test$counter <- sequence(rle(as.character(test$count_pos))$lengths)
      test$vol_counter <- sequence(rle(as.character(test$count_volume_pos))$lengths)
      
      
      test$previous_value <- 0
      test$previous_volume_value <- 0
      test$Call <- ""
      test$Volume_Call <- ""
      
      for(i in 1:nrow(test)){
        if(i>1){
          test[i,"previous_value"] <- test[i-as.numeric(test[i,"counter"]),"Close"]
          test[i,"previous_volume_value"] <- test[i-as.numeric(test[i,"vol_counter"]),"Volume"]
          if((test[i,"Close"] <=  test[i,"previous_value"]) && (test[i,"counter"] == 1)){
            test[i,"Call"] <- "Buy"
          }
          else if((test[i,"Close"] >  test[i,"previous_value"]) && (test[i,"counter"] != 1) && (test[i,"count_pos"] == 1)){
            test[i,"Call"] <- "Sell"
          }
          
          if((test[i,"Volume"] <=  test[i,"previous_volume_value"]) && (test[i,"vol_counter"] == 1)){
            test[i,"Volume_Call"] <- "Buy"
          }
          else if((test[i,"Volume"] >  test[i,"previous_volume_value"]) && (test[i,"vol_counter"] != 1) && (test[i,"count_volume_pos"] == 1)){
            test[i,"Volume_Call"] <- "Sell"
          }
        }
      }
      # browser()
      
      test <- mutate(test,sma = SMA(Close,n=50))
      
      test$MACD <- MACD(test$Close, nFast=12, nSlow=26,
                        nSig=9, maType=SMA)
      
      test$sma[is.na(test$sma)] <- 0
      
      test$MACD[is.na(test$MACD)] <- 0
      
      test <- mutate(test,sma_delta = Delt(sma,Close)*100 )
      
      test$rsi = RSI(test$Close, n=14)
      test$rsi[is.na(test$rsi)] <- 0
      
      test$sma_call <- ifelse(test$sma_delta >= -0.1 & test$sma_delta <= 0.1, 1 ,0)
      
      test$temp_final_call <- ifelse(test$Call ==  test$Volume_Call,test$Call,"")
      
      # test$temp_final_call <- test$Call
      
      # PRoviding the MACD and RSI indicator
      
      # temp <- ifelse((test$temp_final_call == "Buy") & (test$MACD > 0) ,1,ifelse((test$temp_final_call == "Sell") & (test$MACD < 0),-1,0))
      
      temp <- ifelse((test$temp_final_call == "Buy") & (test$sma_call=1) & (test$MACD > 0) & (test$rsi >= 25 ) ,1,ifelse((test$temp_final_call == "Sell") &(test$sma_call=1) & (test$MACD > 0) & (test$rsi <= 75), -1,0))
      
      temp <- as.data.frame(temp)
      
      # test$final_call <- ifelse(test$temp_final_call1 ==  "Buy",1,ifelse(test$temp_final_call1 ==  "Sell",-1,0))
      test$final_call <- as.numeric(temp$signal)
      test <- as.data.frame(test)
      
      # test$dates <- as_datetime(test$dates,tz = "Asia/Kolkata")
      
      test$dates <- test$dates + hm("5:30")  
      
      # browser()
      
      # K=10
      # 
      # max_stocks <- cbind(rollmax(final_data$Open, k=K,fill = FALSE),rollmax(final_data$High, k=K,fill = FALSE),rollmax(final_data$Low, k=K,fill = FALSE),rollmax(final_data$Close, k=K,fill = FALSE))
      # 
      # # stock_data <- reclass(apply(final_data,2,na.locf),match.to=final_data)
      # 
      # max_stocks <- na.locf(reclass(apply(max_stocks,1,mean),match.to=max_stocks))
      # 
      # max_stocks <- cbind(as.POSIXct(final_data$dates),max_stocks)
      # 
      # max_stocks <- as.data.frame(max_stocks)
      # 
      # 
      # 
      # max_stocks <- max_stocks[!max_stocks$max_stocks == 0.00, ]
      # 
      # result <- max_stocks[-1]
      # row.names(result) <- max_stocks$V1
      # 
      # ###################################################################
      # detectSupportResistance <- function(timeSeries, tolerance=0.5, nChunks=10, nPoints=3, plotChart=TRUE)
      # {
      #   #detect maximums and minimums
      #   N = length(timeSeries)
      #   print(N)
      #   stp = floor(N / nChunks)
      #   print(stp)
      #   minz = array(0.0, dim=nChunks)
      #   whichMinz = array(0, dim=nChunks)
      #   print(whichMinz)
      #   maxz = array(0.0, dim=nChunks)
      #   whichMaxz = array(0, dim=nChunks)
      #   for(j in 1:(nChunks-1))
      #   {
      #     lft = (j-1)*stp + 1  #left and right elements of each chunk
      #     # print(lft)
      #     rght = j*stp
      #     # print(rght)
      #     whichMinz[j] = which.min(timeSeries[lft:rght]) + lft
      #     print(whichMinz[j])
      #     minz[j] = min(timeSeries[lft:rght])
      #     print(minz[j])
      #     whichMaxz[j] = which.max(timeSeries[lft:rght]) + lft
      #     maxz[j] = max(timeSeries[lft:rght])
      #   }
      #   print(minz)
      #   print(maxz)
      # 
      #   #last chunk
      #   lft = j*stp + 1  #left and right elements of each chunk
      #   rght = N
      #   whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + lft
      #   minz[nChunks] = min(timeSeries[lft:rght])
      #   whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + lft
      #   maxz[nChunks] = max(timeSeries[lft:rght])
      # 
      #   result = list()
      #   result[["minima"]] = NULL
      #   result[["minimaAt"]] = NULL
      #   result[["maxima"]] = NULL
      #   result[["maximaAt"]] = NULL
      #   span = tolerance*(max(maxz) - min(minz))
      # 
      #   rang = order(minz)[1:nPoints]
      #   if((minz[rang[nPoints]] - minz[rang[1]]) <= span)
      #   {
      #     result[["minima"]] = minz[rang[1:nPoints]]
      #     result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
      #   }
      #   print(rang)
      # 
      #   rang = order(maxz, decreasing = TRUE)[1:nPoints]
      #   if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span)
      #   {
      #     result[["maxima"]] = maxz[rang[1:nPoints]]
      #     result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
      #   }
      # 
      #   print(rang)
      # 
      #   if(plotChart)
      #   {
      #     print("hello")
      #     ts.plot(timeSeries)
      #     points(whichMinz, minz, col="blue")
      #     points(whichMaxz, maxz, col="red")
      #     print(fitted(lm(result[["minima"]] ~  result[["minimaAt"]])))
      #     model <- lm(result[["minima"]] ~  result[["minimaAt"]])
      #     fit <- augment(model) %>% arrange(result[["minimaAt"]])
      #     # print(fit)
      #     # highchart() %>%
      #     #   hc_xAxis(categories = test$dates) %>%
      #     #   hc_add_series(data = test$Close) %>%
      #     #   hc_add_series(
      #     #     fit, type = "line", hcaes(x = `result[["minimaAt"]]`, y = .fitted),
      #     #     name = "Fit", id = "fit"
      #     #   ) %>%
      #     #   hc_add_series(data = test$final_call, yAxis = 1) %>%
      #     #   hc_yAxis_multiples(
      #     #     list(lineWidth = 3, lineColor='#7cb5ec', title=list(text="Price")),
      #     #     list(lineWidth = 3, lineColor="#434348", title=list(text="Call"))) %>%
      #     #   hc_exporting(enabled = TRUE, filename = "custom-file-name")
      # 
      #     if(!is.null(result[["minima"]])  &&  !is.null(result[["minimaAt"]]))
      #       abline(lm(result[["minima"]] ~  result[["minimaAt"]]))
      #     if(!is.null(result[["maxima"]])  &&  !is.null(result[["maximaAt"]]))
      #       abline(lm(result[["maxima"]] ~  result[["maximaAt"]]))
      # 
      #     # print(test$dates)
      #     # browser()
      #     # highchart() %>%
      #     #   hc_xAxis(categories = test$dates) %>%
      #     #   hc_add_series(data = test$Close) %>%
      #     #   hc_add_series(
      #     #     fit, type = "line", hcaes(x = result[["minimaAt"]], y = .fitted),
      #     #     name = "Fit", id = "fit"
      #     #   ) %>%
      #     #   hc_add_series(data = test$final_call, yAxis = 1) %>%
      #     #   hc_yAxis_multiples(
      #     #     list(lineWidth = 3, lineColor='#7cb5ec', title=list(text="Price")),
      #     #     list(lineWidth = 3, lineColor="#434348", title=list(text="Call"))) %>%
      #     #   hc_exporting(enabled = TRUE, filename = "custom-file-name")
      #   }
      #   # ts.plot(timeSeries)
      # 
      #   return(result)
      # }
      # 
      # 
      # stocks_ts <- ts(result)
      # 
      # testing <- detectSupportResistance(stocks_ts)
      
      # browser()
      
      highchart() %>%
        hc_xAxis(categories = test$dates) %>%
        hc_add_series(data = test$Close) %>%
        hc_add_series(data = test$final_call, yAxis = 1) %>%
        hc_yAxis_multiples(
          list(lineWidth = 3, lineColor='#7cb5ec', title=list(text="Price")),
          list(lineWidth = 3, lineColor="#434348", title=list(text="Call"))) %>%
        hc_exporting(enabled = TRUE, filename = "custom-file-name")
      
    })
  })
  
  output$stocks_candle_realtime_data <- DT::renderDataTable({
    
    withProgress(message = 'Fetching fundamentals : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      # browser()
      if(input$candle_stick_range == "1d"){
        # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5d"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=30m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "6mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "ytd"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else {
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      

      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # browser()
      
      if(nrow(final_data) == 0){
        stock_timestamp <- response_data$chart$result[[2]][[1]]
        Close <- response_data$chart$result[[3]]$quote[[1]]$close
        High <- response_data$chart$result[[3]]$quote[[1]]$high
        Low <- response_data$chart$result[[3]]$quote[[1]]$low
        Open <- response_data$chart$result[[3]]$quote[[1]]$open
        Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      }
      
      
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      else{
        final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      final_data <- na.omit(final_data)
      # browser()
      
      test <- mutate(final_data, last_close = lag(Close))
      
      # test <- mutate(test, last_volume = lag(Volume))
      
      
      test$count_pos <- ifelse(final_data$Close >= test$last_close, 1, 0)
      # test$count_volume_pos <- ifelse(final_data$Volume >= test$last_volume, 1, 0)
      
      
      test$counter <- sequence(rle(as.character(test$count_pos))$lengths)
      # test$vol_counter <- sequence(rle(as.character(test$count_volume_pos))$lengths)
      
      
      test$previous_value <- 0
      # test$previous_volume_value <- 0
      test$Call <- ""
      test$Volume_Call <- ""
      
      for(i in 1:nrow(test)){
        if(i>1){
          
          if(test[i,"count_pos"] == 1 && (test[i,"counter"] == 1)){
            
            test[i,"previous_value"] <- test[i-as.numeric(test[i,"counter"])-as.numeric(test[i-1,"counter"]),"Close"]
            
          }
          else{
            test[i,"previous_value"] <- test[i-as.numeric(test[i,"counter"]),"Close"]
          }
          
          
          if(test[i,"count_pos"] == 1){
            # test[i,"previous_volume_value"] <- test[i-as.numeric(test[i,"vol_counter"]),"Volume"]
            if((test[i,"Close"] >=  test[i,"previous_value"]) && (test[i,"counter"] == 1)){
              test[i,"Call"] <- "Buy"
            }
            else if((test[i,"Close"] <  test[i,"previous_value"]) && (test[i,"counter"] != 1) && (test[i,"count_pos"] == 1)){
              test[i,"Call"] <- "Sell"
            }
          }
          else if (test[i,"count_pos"] == 0){
            # test[i,"previous_volume_value"] <- test[i-as.numeric(test[i,"vol_counter"]),"Volume"]
            if((test[i,"Close"] >  test[i,"previous_value"]) && (test[i,"counter"] == 1)){
              test[i,"Call"] <- "Buy"
            }
            else if((test[i,"Close"] <=  test[i,"previous_value"]) && (test[i,"counter"] != 1) && (test[i,"count_pos"] == 1)){
              test[i,"Call"] <- "Sell"
            }
          }
          
          
          
          
          # if((test[i,"Volume"] <=  test[i,"previous_volume_value"]) && (test[i,"vol_counter"] == 1)){
          #   test[i,"Volume_Call"] <- "Buy"
          # }
          # else if((test[i,"Volume"] >  test[i,"previous_volume_value"]) && (test[i,"vol_counter"] != 1) && (test[i,"count_volume_pos"] == 1)){
          #   test[i,"Volume_Call"] <- "Sell"
          # }
        }
      }
      # browser()
      
      test <- mutate(test,sma = SMA(Close,n=50))
      
      test$MACD <- MACD(test$Close, nFast=12, nSlow=26,
                        nSig=9, maType=SMA)
      
      test$sma[is.na(test$sma)] <- 0
      
      test$MACD[is.na(test$MACD)] <- 0
      
      test <- mutate(test,sma_delta = Delt(sma,Close)*100 )
      
      test$rsi = RSI(test$Close, n=14)
      test$rsi[is.na(test$rsi)] <- 0
      
      test$sma_call <- ifelse(test$sma_delta >= -0.1 & test$sma_delta <= 0.1, 1 ,0)
      
      # test$temp_final_call <- ifelse(test$Call ==  test$Volume_Call,test$Call,"")
      
      test$temp_final_call <- test$Call
      
      # PRoviding the MACD and RSI indicator
      
      # temp <- ifelse((test$temp_final_call == "Buy") & (test$MACD > 0) ,1,ifelse((test$temp_final_call == "Sell") & (test$MACD < 0),-1,0))
      
      temp <- ifelse((test$temp_final_call == "Buy") & (test$sma_call=1) & (test$MACD > 0) & (test$rsi >= 25 ) ,1,ifelse((test$temp_final_call == "Sell") &(test$sma_call=1) & (test$MACD > 0) & (test$rsi <= 75), -1,0))
      
      temp <- as.data.frame(temp)
      
      # test$final_call <- ifelse(test$temp_final_call1 ==  "Buy",1,ifelse(test$temp_final_call1 ==  "Sell",-1,0))
      test$final_call <- as.numeric(temp$signal)
      test <- as.data.frame(test)
      # browser()
      if(input$candle_stick_range == "1d"){
        final_data <- test %>% select(dates,Open,High,Low,Close,Volume,Call,count_pos,final_call,Volume_Call)
        if(as.character(wday(Sys.Date(), label = TRUE)) == c("Sat")){
          final_data <- final_data %>% filter(as.Date(dates) == Sys.Date() - 1)
        }
        else if(as.character(wday(Sys.Date(), label = TRUE)) == c("Sun")){
          final_data <- final_data %>% filter(as.Date(dates) == Sys.Date() - 2)
        }
        else{
          final_data <- final_data %>% filter(as.Date(dates) == Sys.Date())
        }
        
      } 
      else{
        final_data <- test %>% select(dates,Open,High,Low,Close,Volume,Call,count_pos,final_call,Volume_Call)
      }
      
      final_data$dates <- as.POSIXct(final_data$dates, '%d-%m-%Y %H:%M:%S')
      
      final_data <- final_data %>% arrange(dates)
      
      DT::datatable(final_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = TRUE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      )  %>% formatStyle(
        'final_call',
        backgroundColor = styleEqual(c(1, 0, -1), c('green', 'white','red'))
      ) %>% formatDate(c(1), method = 'toLocaleString') 
      
      
    })
    
  })
  
  
  output$stocks_buy_call <- DT::renderDataTable({
    
    withProgress(message = 'Fetching fundamentals : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      # browser()
      if(input$candle_stick_range == "1d"){
        # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5d"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=30m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "6mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "ytd"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else {
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      
      
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # browser()
      
      if(nrow(final_data) == 0){
        stock_timestamp <- response_data$chart$result[[2]][[1]]
        Close <- response_data$chart$result[[3]]$quote[[1]]$close
        High <- response_data$chart$result[[3]]$quote[[1]]$high
        Low <- response_data$chart$result[[3]]$quote[[1]]$low
        Open <- response_data$chart$result[[3]]$quote[[1]]$open
        Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      }
      
      
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      else{
        final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      final_data <- na.omit(final_data)
      
      myMACD <- function (price,S,L,K){
        MACD <- EMA(price,S) - EMA(price,L)
        signal <- EMA(MACD,K)
        output <- cbind(MACD,signal)
        colnames(output) <- c("MACD","signal")
        return(output)
      }  
      
      macd_data <- myMACD(final_data$Close,12,26,9)
      
      macd_data <- as.data.frame(macd_data)
      
      final_macd_data <- cbind(final_data,macd_data)
      
      final_macd_data$cross_over_signal <- Lag(ifelse(final_macd_data$MACD < final_macd_data$signal, -1, 1))
      
      
      buy_data <- final_macd_data[which(final_macd_data$MACD > 2 & final_macd_data$cross_over_signal == 1),]
      
      buy_data$dates <- buy_data$dates + hm("5:30") 
      buy_data$Close <- round(buy_data$Close,2)
      buy_data$High <- round(buy_data$High,2)
      buy_data$Open <- round(buy_data$Open,2)
      buy_data$Low <- round(buy_data$Low,2)
      buy_data$Volume <- round(buy_data$Volume,2)
      buy_data$MACD <- round(buy_data$MACD,2)
      buy_data$signal <- round(buy_data$signal,2)
      
      DT::datatable(buy_data, options = list(scrollX = TRUE,
                                               pageLength=10,
                                               autoWidth = TRUE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) 
      
      
    })
    
  })
  
  
  output$options_chain_data  <- DT::renderDataTable({
    
    browser()
    
    options_ip <- input$options_input
    options_expiry <- input$options_expiry
    
    
    
    # url = 'https://www.nseindia.com/api/option-chain-indices?symbol=BANKNIFTY'
    url = paste('https://www.nseindia.com/api/option-chain-indices?symbol=',options_ip, sep="")
    
    response = GET(url, add_headers("user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.79 Safari/537.36"))
    
    temp <- content(response)$records$data
    
    data_raw <- tibble::enframe(unlist(temp))
    # data_raw
    
    data_raw <- data.frame(data_raw)
    
    
    d <- within(data_raw, {
      name <- as.character(name)
      ID <- ave(name, name, FUN=seq_along)
    })
    
    
    
    final_strike_data <- reshape2::dcast(d, ID ~ name, value.var="value")
    
    colnames(final_strike_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice")
    
    final_options_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice
                          from final_strike_data t1 
                          left join final_strike_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice
                                "
    )
    
    final_options_data <- final_options_data %>% dplyr::filter((expiryDate == as.character(options_expiry)))
    
    final_options_data <- final_options_data %>% dplyr::select(c("Call_openInterest","Call_changeinOpenInterest","Call_totalTradedVolume","Call_impliedVolatility","Call_lastPrice","Call_pChange","Call_change","Call_bidQty","Call_bidprice","Call_askQty","Call_askPrice","strikePrice","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_lastPrice","Put_pChange","Put_impliedVolatility","Put_totalTradedVolume","Put_changeinOpenInterest","Put_openInterest"))
    
    colnames(final_options_data) <- c("CE OI","CE CHNG IN OI","CE VOLUME","CE IV","CE LTP","CE Price CHNG","CE CHNG","CE BID QTY","CE BID PRICE","CE ASK QTY","CE ASK PRICE","STRIKEPRICE","PE ASK PRICE","PE ASK QTY","PE BID PRICE","PE BID QTY","PE CHNG","PE LTP","PE Price CHNG","PE IV","PE VOLUME","PE CHNG IN OI","PE OI")
    
    # final_options_data <- final_options_data[order(final_options_data$strikePrice),]
    
    final_options_data <- final_options_data %>% arrange(STRIKEPRICE)
    
    final_options_data$`CE CHNG IN OI` <- round(as.numeric(final_options_data$`CE CHNG IN OI`),2)
    final_options_data$`CE CHNG` <- round(as.numeric(final_options_data$`CE CHNG`),2)
    final_options_data$`CE Price CHNG` <- round(as.numeric(final_options_data$`CE Price CHNG`),2)
    final_options_data$`PE Price CHNG` <- round(as.numeric(final_options_data$`PE Price CHNG`),2)
    
    login_params = list(api_key = 'LPUVlRxd')
    object = create_connection_object(list(api_key="LPUVlRxd"))
    
    session_data = generate_session(object,"J95213","start@123")
    
    latest_strike_price <- 0
    if(input$options_input == "BANKNIFTY"){
      ltp <- as.numeric(get_ltp_data(object = session_data,exchange = "NSE",tradingsymbol= "BANKNIFTY",symboltoken= "26009")$data$ltp)
      latest_strike_price <- ltp - (ltp)%%100
    }else{
      ltp <- as.numeric(get_ltp_data(object = session_data,exchange = "NSE",tradingsymbol= "NIFTY",symboltoken= "26000")$data$ltp)
      latest_strike_price <- ltp - (ltp)%%50
    }
    # browser()
    price_index <- which(final_options_data$STRIKEPRICE == latest_strike_price,arr.ind = TRUE)
    
    temp_data <- final_options_data[(price_index-10):(price_index+10),]
    
    row.names(temp_data) <- NULL
    
    temp_data$`CE Interpretation` <- ifelse(temp_data$`CE OI`>0 & temp_data$`CE Price CHNG` > 0,"Long Buildups",ifelse(temp_data$`CE OI`<0 & temp_data$`CE Price CHNG` < 0,"Long Unwinding",ifelse(temp_data$`CE OI`>0 & temp_data$`CE Price CHNG` < 0,"Shorts Buildups",ifelse(temp_data$`CE OI`<0 & temp_data$`CE Price CHNG` > 0,"Short Coverings","-"))))
    temp_data$`PE Interpretation` <- ifelse(temp_data$`PE OI`>0 & temp_data$`PE Price CHNG` > 0,"Long Buildups",ifelse(temp_data$`PE OI`<0 & temp_data$`PE Price CHNG` < 0,"Long Unwinding",ifelse(temp_data$`PE OI`>0 & temp_data$`PE Price CHNG` < 0,"Shorts Buildups",ifelse(temp_data$`PE OI`<0 & temp_data$`PE Price CHNG` > 0,"Short Coverings","-"))))
    
    
    temp_data <- temp_data %>% select(c("CE Interpretation","CE OI","CE CHNG IN OI","CE VOLUME","CE IV","CE LTP","CE Price CHNG","CE CHNG","CE BID QTY","CE BID PRICE","CE ASK QTY","CE ASK PRICE","STRIKEPRICE","PE ASK PRICE","PE ASK QTY","PE BID PRICE","PE BID QTY","PE CHNG","PE LTP","PE Price CHNG","PE IV","PE VOLUME","PE CHNG IN OI","PE OI","PE Interpretation"))
    
    write.csv(temp_data,paste0(getwd(),"/data/options_chain_data.csv", sep = ""))
    
    DT::datatable(temp_data,extensions = c('FixedColumns'),
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top'
                  )) %>% formatStyle(
                    c('CE CHNG IN OI','PE CHNG IN OI'),
                    color = styleInterval(cuts = 0, values = c("red", "green")),
                    fontWeight = "bold"
                  ) %>% formatStyle(
                    'STRIKEPRICE',
                    backgroundColor = styleEqual(c(latest_strike_price), c('gray'))
                  ) %>% formatStyle(
                    'CE OI',
                    background = styleColorBar(as.numeric(temp_data$`CE OI`), 'steelblue'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
                  ) %>% formatStyle(
                    'PE OI',
                    background = styleColorBar(as.numeric(temp_data$`PE OI`), 'steelblue'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
                  )
    
  })
  
  output$options_day_history  <- DT::renderDataTable({
    
    options_ip <- input$options_input
    options_expiry <- input$options_expiry
    
    
    if(options_ip == "BANKNIFTY"){
      temp_data <- read.csv("~/Desktop/Options_Chain/indian_banknifty_50_options_chain_raw.csv")
    }else{
      temp_data <- read.csv("~/Desktop/Options_Chain/indian_nifty_50_options_chain_raw.csv")
    }
    
    # temp_data <- read.csv("~/Desktop/Options_Chain/indian_nifty_50_options_chain_raw.csv")
    colnames(temp_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice","current_time")
    
    # options_expiry <- "02-Sep-2021"
    # strike_price <- 16850
    
    stock_row <- input$options_chain_data_rows_selected
    
    Signal_df <- read.csv(paste0(getwd(),"/data/options_chain_data.csv", sep = ""))
    
    strike_price <- Signal_df[stock_row,"STRIKEPRICE"]
    
    left_temp_data <- temp_data %>% dplyr::filter((Call_expiryDate == as.character(options_expiry)) &(Call_strikePrice == strike_price))
    right_temp_data <- temp_data %>% dplyr::filter((Put_expiryDate == as.character(options_expiry)) &(Put_strikePrice == strike_price)) 
    
    left_temp_data <- left_temp_data[complete.cases(left_temp_data), ]
    right_temp_data <- right_temp_data[complete.cases(right_temp_data), ]
    
    temp_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice,t1.current_time
                              from left_temp_data t1
                              inner join right_temp_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice and t1.current_time = t2.current_time
                                    ")
    
    
    current_data <- temp_data[as.Date(temp_data$current_time) == Sys.Date(),]
    
    rownames(current_data) <- NULL
    
    current_data$`CE Interpretation` <- ifelse(current_data$Call_openInterest>0 & current_data$Call_pChange > 0,"Long Buildups",ifelse(current_data$Call_openInterest<0 & current_data$Call_pChange < 0,"Long Unwinding",ifelse(current_data$Call_openInterest>0 & current_data$Call_pChange < 0,"Shorts Buildups",ifelse(current_data$Call_openInterest<0 & current_data$Call_pChange > 0,"Short Coverings","-"))))
    current_data$`PE Interpretation` <- ifelse(current_data$Put_changeinOpenInterest>0 & current_data$Put_pChange > 0,"Long Buildups",ifelse(current_data$Put_changeinOpenInterest<0 & current_data$Put_pChange < 0,"Long Unwinding",ifelse(current_data$Put_changeinOpenInterest>0 & current_data$Put_pChange < 0,"Shorts Buildups",ifelse(current_data$Put_changeinOpenInterest<0 & current_data$Put_pChange > 0,"Short Coverings","-"))))
    
    
    formatted_data <- current_data %>% select(current_time,`CE Interpretation`,Call_lastPrice,Call_impliedVolatility,Call_openInterest,Call_strikePrice,expiryDate,`PE Interpretation`,Put_lastPrice,Put_impliedVolatility,Put_openInterest) %>% arrange(current_time)
    
    
    # View(formatted_data)
    
    DT::datatable(formatted_data,extensions = c('FixedColumns'),
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top'
                  ))
    
    
  })
  
  
  output$multi_strike_data<-renderHighchart({
    
    # browser()
    
    options_ip <- input$options_input
    options_expiry <- input$options_expiry
    
    if(options_ip == "BANKNIFTY"){
      temp_data <- read.csv("~/Desktop/Options_Chain/indian_banknifty_50_options_chain_raw.csv")
    }else{
      temp_data <- read.csv("~/Desktop/Options_Chain/indian_nifty_50_options_chain_raw.csv")
    }
    colnames(temp_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice","current_time")
    
    
    stock_row <- input$options_chain_data_rows_selected
    
    Signal_df <- read.csv(paste0(getwd(),"/data/options_chain_data.csv", sep = ""))
    
    strike_price <- Signal_df[stock_row,"STRIKEPRICE"]
    
    
    left_temp_data <- temp_data %>% dplyr::filter((Call_expiryDate == as.character(options_expiry)) &(Call_strikePrice == strike_price))
    right_temp_data <- temp_data %>% dplyr::filter((Put_expiryDate == as.character(options_expiry)) &(Put_strikePrice == strike_price)) 
    
    left_temp_data <- left_temp_data[complete.cases(left_temp_data), ]
    right_temp_data <- right_temp_data[complete.cases(right_temp_data), ]
    
    
    # temp_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice,t1.current_time
    #                           from temp_data t1
    #                           inner join temp_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice
    #                                 ")
    
    temp_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice,t1.current_time
                              from left_temp_data t1
                              inner join right_temp_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice and t1.current_time = t2.current_time
                                    ")
    
    current_Data <-temp_data %>% dplyr::filter((expiryDate == as.character(options_expiry)) &(strikePrice == strike_price))
    
    current_Data <- current_Data %>% select(c("Call_openInterest","current_time","Put_openInterest","Call_lastPrice","Put_lastPrice","Call_impliedVolatility","Put_impliedVolatility"))
    
    # current_Data$current_time <- as.POSIXct(current_Data$current_time,tz=Sys.timezone()) + hm("5:30")
    current_Data$current_time <- anytime(current_Data$current_time)
    current_Data$Call_openInterest<- as.numeric(current_Data$Call_openInterest)
    current_Data$Put_openInterest<- as.numeric(current_Data$Put_openInterest)
    current_Data$Call_lastPrice<- as.numeric(current_Data$Call_lastPrice)
    current_Data$Put_lastPrice<- as.numeric(current_Data$Put_lastPrice)
    current_Data$Call_impliedVolatility<- as.numeric(current_Data$Call_impliedVolatility)
    current_Data$Put_impliedVolatility<- as.numeric(current_Data$Put_impliedVolatility)
    
    
    highchart() %>%
      hc_xAxis(categories=current_Data$current_time+ hm("5:30")) %>% 
      hc_yAxis_multiples(list(title = list(text = "Last Traded Price"), opposite = FALSE),list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Open Interest"))) %>%
      hc_add_series(name = "CE LTP", data = current_Data$Call_lastPrice) %>%
      hc_add_series(name = "PE LTP", data = current_Data$Put_lastPrice) %>%
      hc_add_series(name = "CE IV", data = current_Data$Call_impliedVolatility, yAxis = 1) %>%
      hc_add_series(name = "PE IV", data = current_Data$Put_impliedVolatility, yAxis = 1) %>%
      hc_add_series(name = "CE Open Interest", data = current_Data$Call_openInterest, yAxis = 1) %>%
      hc_add_series(name = "PE Open Interest", data = current_Data$Put_openInterest, yAxis = 1) %>%
      hc_colors(c("red","blue")) %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_chart(
        zoomType = "xy",
        events = list(
          selection = JS("function(event) {
                        Shiny.onInputChange('event', [event.xAxis[0].min,     event.xAxis[0].max]);
                          return true;
                          } ")
        )
      )
    
    
    
    # chart_output <- highchart(type = "stock") %>% 
    #   hc_add_series(current_Data, "line", hcaes(current_time, Call_openInterest), name = "Call Open Interest") %>% 
    #   hc_exporting(enabled = TRUE,filename = "custom-file-name")
    # 
    # chart_output
    
    # p <- ggplot(current_Data, aes(x = current_time, y = Call_openInterest)) +  
    #   geom_line(size = 1)+
    #   labs(title = "Portfolio Growth",
    #        subtitle = "Comparing Multiple Portfolios",
    #        caption = "Portfolio 3 is a Standout!",
    #        x = "", y = "Portfolio Value",
    #        color = "Portfolio")
    # 
    # ggplotly(p)
    
    # p <- ggplot(current_Data, aes(x=current_time, y=as.numeric(Call_openInterest))) +
    #   geom_line()
    # 
    # p
  
  })
  
  output$options_greek_value <- DT::renderDataTable({
    # browser()
    
    options_data <- data.frame(Greek = character(0),Call = numeric(),Put = numeric())
    
    
    
    # call_delta_value <- callgreek("delta",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # call_theta_value <- callgreek("theta",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # call_vega_value <- callgreek("vega",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # call_gamma_value <- callgreek("gamma",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # call_rho_value <- callgreek("rho",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # call_premium_value <- callgreek("premium",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # 
    # put_delta_value <- putgreek("delta", as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # put_theta_value <- putgreek("theta",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # put_vega_value <- putgreek("vega",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # put_gamma_value <- putgreek("gamma",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # put_rho_value <- putgreek("rho",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    # put_premium_value <- putgreek("premium",  as.numeric(input$options_spot_price),as.numeric(input$options_strike_price), as.numeric(input$options_volatility), (as.numeric(input$options_expiry_date)/365), as.numeric(input$options_interest), as.numeric(input$options_dividend))
    
    # call_greeks_data <- greeks(bscall(s=as.numeric(input$options_spot_price),k=as.numeric(input$options_strike_price),v=as.numeric(input$options_volatility),r=as.numeric(input$options_interest),tt=(as.numeric(input$options_expiry_date)/365),d=0),complete =TRUE)
    # put_greeks_data <- greeks(bsput(s=as.numeric(input$options_spot_price),k=as.numeric(input$options_strike_price),v=as.numeric(input$options_volatility),r=as.numeric(input$options_interest),tt=(as.numeric(input$options_expiry_date)/365),d=0),complete =TRUE)
    
    call_greeks_data <- greeks2(bscall, list(s=input$options_spot_price, k=input$options_strike_price, v=input$options_volatility, r=input$options_interest, tt=(input$options_expiry_date/365), d=input$options_dividend))
    put_greeks_data <- greeks2(bsput, list(s=input$options_spot_price, k=input$options_strike_price, v=input$options_volatility, r=input$options_interest, tt=(input$options_expiry_date/365), d=input$options_dividend))
    
    call_delta_value <- call_greeks_data[2]
    call_theta_value <- call_greeks_data[6]
    call_vega_value <- call_greeks_data[4]
    call_gamma_value <- call_greeks_data[3]
    call_rho_value <- call_greeks_data[5]
    call_premium_value <- call_greeks_data[1]
    
    put_delta_value <- put_greeks_data[2]
    put_theta_value <- put_greeks_data[6]
    put_vega_value <- put_greeks_data[4]
    put_gamma_value <- put_greeks_data[3]
    put_rho_value <- put_greeks_data[5]
    put_premium_value <- put_greeks_data[1]
    
    options_data[1,"Greek"] <- "Delta"
    options_data[1,"Call"] <- round(call_delta_value,3)
    options_data[1,"Put"] <- round(put_delta_value,3)
    options_data[2,"Greek"] <- "Theta"
    options_data[2,"Call"] <- round(call_theta_value,3)
    options_data[2,"Put"] <- round(put_theta_value,3)
    options_data[3,"Greek"] <- "Vega"
    options_data[3,"Call"] <- round(call_vega_value,3)
    options_data[3,"Put"] <- round(put_vega_value,3)
    options_data[4,"Greek"] <- "Gamma"
    options_data[4,"Call"] <- round(call_gamma_value,4)
    options_data[4,"Put"] <- round(put_gamma_value,4)
    options_data[5,"Greek"] <- "Rho"
    options_data[5,"Call"] <- round(call_rho_value,3)
    options_data[5,"Put"] <- round(put_rho_value,3)
    options_data[6,"Greek"] <- "Premium"
    options_data[6,"Call"] <- round(call_premium_value,2)
    options_data[6,"Put"] <- round(put_premium_value,2)
    
    # greesk_data <- greeks(bscall(s=34584.35,k=34600,v=0.1595,r=0.03388,tt=(4/365),d=0),complete =TRUE)
    
    
    DT::datatable(options_data)
    
  })
  
  
  getpayoofdiagram <- function(){
    
    first_strike_price <- as.numeric(input$bot_first_strike_price)
    second_strike_price <- as.numeric(input$bot_second_strike_price)
    
    first_range <- min(first_strike_price,second_strike_price) - 1000
    second_range <- max(second_strike_price,second_strike_price) + 1000
    
    
    prices <- seq(first_range, second_range,1) # Vector of prices
    # prices <- seq(14800, 16900,1)
    k_low = min(first_strike_price,second_strike_price)  # Low Strike price for call
    k_high = max(first_strike_price,second_strike_price) # High Strike Price for call 
    premium_low = as.numeric(input$bot_first_premium)
    premium_high = as.numeric(input$bot_scond_premium)
    
    if(input$bot_first_transaction == "Buy" & input$bot_second_transaction == "Sell"){
      
      # Intrinsic Value and Payoff long call
      intrinsicValueLongCall <- prices - k_low - premium_low
      payoffLongCall <- pmax(-premium_low,intrinsicValueLongCall)
      # Intrinsic Value and Payoff short call
      intrinsicValueShortCall <- prices - k_high - premium_high
      payoffShortCall <- pmin(premium_high,-intrinsicValueShortCall)
      
    }else if(input$bot_first_transaction == "Buy" & input$bot_second_transaction == "Buy"){
      # call option payoff at expiration 
      intrinsicValuesCall <- prices - k_low - premium_low
      payoffLongCall <- pmax(-premium_low,intrinsicValuesCall)
      
      # put option payoff at expiration
      intrinsicValuesPut <- k_high - prices - premium_high
      payoffShortCall <- pmax(-premium_high,intrinsicValuesPut)
    }
    
   
    # Strategy Payoff
    payoff <- rowSums(cbind(payoffLongCall,payoffShortCall))
    # Generate a dataframe with the payoffLongCall, payoffShortCall and payoff vectors
    # in order to plot the strategy payoffs using ggplot
    results <- data.frame(cbind(prices,payoffLongCall,payoffShortCall,payoff))
    
    results$shortline <- ifelse(results$payoff < 0 ,results$payoffShortCall,0)
    results$longline <- ifelse(results$payoff >= 0,results$payoffLongCall,0)
    
    return(results)
    
    
    
  }
  
  output$options_payoff_chart<-renderPlotly({
    
    # browser()
    results <- getpayoofdiagram()
    
    ggplot(results, aes(x=prices)) + 
      geom_line(aes(y = payoffLongCall, color = "LongCall")) + 
      geom_line(aes(y = payoffShortCall, color="ShortCall"))+
      geom_line(aes(y=payoff, color = 'Payoff')) +
      geom_ribbon(data=results,aes(x = prices,ymin = shortline, ymax = payoff), inherit.aes = FALSE,fill = "red")+
      geom_ribbon(data=results,aes(x = prices,ymin = payoff, ymax = longline), inherit.aes = FALSE,fill = "green")+
      scale_colour_manual("", 
                          breaks = c("LongCall", "ShortCall", "Payoff"),
                          values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
      ggtitle("Bull Call Spread Payoff") 
    
    
    
  })
  
  getpercentageChange <- function(){
    # browser()
    if(input$bot_timeframe == "1d"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(input$bot_timeframe == "5d"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(input$bot_timeframe == "1mo"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=30m&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(input$bot_timeframe == "6mo"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(input$bot_timeframe == "ytd"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(input$bot_timeframe == "1y"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else if(input$bot_timeframe == "5y"){
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    else {
      response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$bot_stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=",input$bot_timeframe,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
    }
    
    
    
    
    
    # response_data <- fromJSON("https://query1.finance.yahoo.com/v8/finance/chart/INDIGO.NS?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet")
    
    stock_timestamp <- response_data$chart$result[[1]]$timestamp
    Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
    High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
    Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
    Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
    Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
    final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
    
    # browser()
    
    if(nrow(final_data) == 0){
      stock_timestamp <- response_data$chart$result[[2]][[1]]
      Close <- response_data$chart$result[[3]]$quote[[1]]$close
      High <- response_data$chart$result[[3]]$quote[[1]]$high
      Low <- response_data$chart$result[[3]]$quote[[1]]$low
      Open <- response_data$chart$result[[3]]$quote[[1]]$open
      Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
      
      
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
    }
    
    
    colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
    # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
    if(typeof(final_data$V1) == "list"){
      final_data <- final_data[-c(which(final_data$Close == "NULL")),]
      new_stock_timestamp <- unlist(final_data$V1)
      Close <- unlist(final_data$Close)
      High <- unlist(final_data$High)
      Open <- unlist(final_data$Open)
      Low <- unlist(final_data$Low)
      Volume <- unlist(final_data$Volume)
      
      final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
      
      final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
      
      final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
    }
    else{
      final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
      
      final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
    }
    final_data <- na.omit(final_data)
    
    final_data$ema_20_Close <-EMA(final_data$Close,n=20)
    # final_data$ema_200_Close <-EMA(final_data$Close,n=200)
    final_data$ema_10_Close <-EMA(final_data$Close,n=10)
    
    final_data <- mutate(final_data, last_close = lag(Close))
    
    final_data <- na.omit(final_data)
    
    
        
    
    return(final_data)
    
  }
  
  observeEvent(input$report, {
    # rmarkdown::render("BOT_Trading.Rmd","all")
    rmarkdown::render("Next_Day_Movement.Rmd","all")
  })
  
  observeEvent(input$ml_submit,{
    
    
    stock <- input$ml_stock_input
    # browser()
    

    stocks_data <- na.omit(getSymbols(stock, src = "yahoo", from = "2008-01-01", to = Sys.Date()+1, auto.assign = FALSE))
    stocks_data2 <- stocks_data
    
    ichimoku <- function(HLC, nFast=9, nMed=26, nSlow=52) {
      turningLine <- (runMax(Hi(HLC), nFast)+runMin(Lo(HLC), nFast))/2
      baseLine <- (runMax(Hi(HLC), nMed)+runMin(Lo(HLC), nMed))/2
      
      out = cbind(turningLine,baseLine)
      return(out)
    }
    # browser()
    
    
    myATR <- function(x) ATR(HLC(x))[,'atr'] # Average True Range, measures volatility of series  
    mySMI <- function(x) SMI(HLC(x))[, "SMI"] #  Stochastic Momentum Index 
    myADX <- function(x) ADX(HLC(x))[,'ADX'] # Welles Wilder's Directional Movement Index 
    myAroon <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator # Identify starting trends
    myBB <- function(x) BBands(HLC(x))[, "pctB"] # Bollinger Bands
    myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1] # Chaikin Volatility
    myCLV <- function(x) EMA(CLV(HLC(x)))[, 1] # Close Location Value 
    myEMV <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2] # Arms' Ease of Movement Value 
    myMACD <- function(x) MACD(Cl(x))[,2] # Moving Average Convergence Divergence
    myMFI <- function(x) MFI(HLC(x), Vo(x)) # Money Flow Index
    mySAR <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1] # Parabolic Stop-and-Reverse
    myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1] # volatility
    
    # browser()
    
    stocks_data$ATR <- myATR(stocks_data)
    stocks_data$SMI <- mySMI(stocks_data)
    stocks_data$ADX <- myADX(stocks_data)
    stocks_data$Aroon <- myAroon(stocks_data)
    stocks_data$BB <- myBB(stocks_data)
    stocks_data$ChaikinVol <- myChaikinVol(stocks_data)
    stocks_data$CLV <- myCLV(stocks_data)
    # stocks_data$EMV <- myEMV(stocks_data)
    stocks_data$MACD <- myMACD(stocks_data)
    stocks_data$MFI <- myMFI(stocks_data)
    stocks_data$SAR <- mySAR(stocks_data)
    stocks_data$Volat <- myVolat(stocks_data)
    
    ichimoku_dt = ichimoku(stocks_data2)
    # browser()
    stocks_data <- cbind(stocks_data,ichimoku_dt)
    # View(stocks_data)
    stocks_data <- data.frame(Date = index(stocks_data), coredata(stocks_data) )
    
    colnames(stocks_data) <- c("Date","Open","High","Low","Close","Volume","Adjusted","turningLine","baseLine","ATR","SMI","ADX","Aroon","BB","ChaikinVol","CLV","MACD","MFI","SAR","Volat")
    
    # logistic_stock_data <- stocks_data
    
    stocks_data$avg_price <- round((stocks_data$Open + stocks_data$Close)/2,2)
    
    # stocks_data <- mutate(stocks_data, next_day_close = lead(Close),next_day_open = lead(Open))
    # stocks_data$next_day_avg_price <- round((stocks_data$next_day_open + stocks_data$next_day_close)/2,2)
    
    # head(stocks_data)
    
    xbt <- stocks_data %>% 
      select(Date, Open, High, Low, Close, Volume,Adjusted,avg_price,turningLine,baseLine,ATR,SMI,ADX,Aroon,BB,ChaikinVol,CLV,MACD,MFI,SAR,Volat) %>%
      # select(Date, Open, High, Low, Close, Volume,Adjusted,avg_price) %>%
      # select(Date, Open, High, Low, Close, Volume,Adjusted,avg_price,next_day_avg_price,turningLine,baseLine) %>%
      mutate(
        rsi = TTR::RSI(Close, 14),
        ema_10 = TTR::EMA(Close, 10),
        sma_10 = TTR::SMA(Close,10),
        ema_20 = TTR::EMA(Close,20),
        sma_20 = TTR::SMA(Close,20),
        ema_30 = TTR::EMA(Close, 30),
        sma_30 = TTR::SMA(Close,30),
        ema_50 = TTR::EMA(Close,50),
        sma_50 = TTR::SMA(Close,50),
        ema_100 = TTR::EMA(Close, 100),
        sma_100 = TTR::SMA(Close,100),
        ema_200 = TTR::EMA(Close,200),
        sma_200 = TTR::SMA(Close,200),
        VWMA_20 = TTR::VWMA(Close,Volume,n=20),
        HMA_9 = TTR::HMA(Close,n=9)
      )
    xbt <- as.data.frame(xbt)
    
    xbt <- na.omit(xbt)
    
    xbt = select(xbt,-c("Low","Open","High","Date","Adjusted"))
    
    logistic_stock_data <- xbt
    
    xbt = select(xbt,-c("Close"))
    
    # xbt = select(xbt,-c("Low","Open","Close","High","Date","Adjusted","avg_price","turningLine","baseLine"))
    
    xbt <- xbt[!is.infinite(rowSums(xbt)),]
    
    linear_model <- lm(avg_price ~ .,data= xbt)
    # linear_model <- lm(next_day_avg_price ~ .,data= xbt)
    
    updateNumericInput(session, "ml_Volume", value = xbt[nrow(xbt),"Volume"])
    updateNumericInput(session, "ml_rsi", value = xbt[nrow(xbt),"rsi"])
    updateNumericInput(session, "ml_ema10", value = xbt[nrow(xbt),"ema_10"])
    updateNumericInput(session, "ml_sma10", value = xbt[nrow(xbt),"sma_10"])
    updateNumericInput(session, "ml_ema20", value = xbt[nrow(xbt),"ema_20"])
    updateNumericInput(session, "ml_sma20", value = xbt[nrow(xbt),"sma_20"])
    updateNumericInput(session, "ml_ema30", value = xbt[nrow(xbt),"ema_30"])
    updateNumericInput(session, "ml_sma30", value = xbt[nrow(xbt),"sma_30"])
    updateNumericInput(session, "ml_ema50", value = xbt[nrow(xbt),"ema_50"])
    updateNumericInput(session, "ml_sma50", value = xbt[nrow(xbt),"sma_50"])
    updateNumericInput(session, "ml_ema100", value = xbt[nrow(xbt),"ema_100"])
    updateNumericInput(session, "ml_sma100", value = xbt[nrow(xbt),"sma_100"])
    updateNumericInput(session, "ml_ema200", value = xbt[nrow(xbt),"ema_200"])
    updateNumericInput(session, "ml_sma200", value = xbt[nrow(xbt),"sma_200"])
    updateNumericInput(session, "ml_vwma20", value = xbt[nrow(xbt),"VWMA_20"])
    updateNumericInput(session, "ml_HMA9", value = xbt[nrow(xbt),"HMA_9"])
    
    updateNumericInput(session, "ml_ATR", value = xbt[nrow(xbt),"ATR"])
    updateNumericInput(session, "ml_SMI", value = xbt[nrow(xbt),"SMI"])
    updateNumericInput(session, "ml_ADX", value = xbt[nrow(xbt),"ADX"])
    updateNumericInput(session, "ml_Aroon", value = xbt[nrow(xbt),"Aroon"])
    updateNumericInput(session, "ml_BB", value = xbt[nrow(xbt),"BB"])
    updateNumericInput(session, "ml_ChaikinVol", value = xbt[nrow(xbt),"ChaikinVol"])
    updateNumericInput(session, "ml_CLV", value = xbt[nrow(xbt),"CLV"])
    updateNumericInput(session, "ml_MACD", value = xbt[nrow(xbt),"MACD"])
    updateNumericInput(session, "ml_MFI", value = xbt[nrow(xbt),"MFI"])
    updateNumericInput(session, "ml_SAR", value = xbt[nrow(xbt),"SAR"])
    updateNumericInput(session, "ml_Volat", value = xbt[nrow(xbt),"Volat"])
    updateNumericInput(session, "ml_turningLine", value = xbt[nrow(xbt),"turningLine"])
    updateNumericInput(session, "ml_baseLine", value = xbt[nrow(xbt),"baseLine"])
    
    # browser()
    
    
    linear_imp <- varImp(linear_model, scale = FALSE)
    
    linear_imp <- as.data.frame(linear_imp)
    
    linear_imp <- arrange(linear_imp,desc(Overall))
    
    linear_imp <- cbind(Variable = rownames(linear_imp), linear_imp)
    rownames(linear_imp) <- 1:nrow(linear_imp)

    
    linear_imp = mutate(linear_imp, 
                          Overall_pct = round(Overall / sum(Overall) *100,2))
    
    
    final_importance <- linear_imp %>% select(c("Variable","Overall_pct"))
    
    imp_variables <- head(final_importance,10)$Variable
    
    # browser()
    
    final_stocks_data <- xbt %>% select(c(paste(imp_variables),"avg_price"))
    
    linear_model <- lm(avg_price ~ .,data= final_stocks_data)

    
    output$linear_equation <- renderText({
      
      cc <- linear_model$coefficients
      
      eqn <- paste("Y =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e")
      
      eqn
    })
    
    output$linear_summary <- renderPrint({
      
      
      summary(linear_model)
      
    })
    
    output$linear_output <- renderText({
      
      new_list <- list()
      for(j in 1:length(imp_variables)) {
        # browser()
        if(imp_variables[j] == "Volume"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_Volume)
        }else if(imp_variables[j] == "rsi"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_rsi)
        }else if(imp_variables[j] == "ema_10"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ema10)
        }else if(imp_variables[j] == "sma_10"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_sma10)
        }else if(imp_variables[j] == "ema_20"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ema20)
        }else if(imp_variables[j] == "sma_20"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_sma20)
        }else if(imp_variables[j] == "ema_30"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ema30)
        }else if(imp_variables[j] == "sma_30"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_sma30)
        }else if(imp_variables[j] == "ema_50"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ema50)
        }else if(imp_variables[j] == "sma_50"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_sma50)
        }else if(imp_variables[j] == "ema_100"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ema100)
        }else if(imp_variables[j] == "sma_100"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_sma100)
        }else if(imp_variables[j] == "ema_200"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ema200)
        }else if(imp_variables[j] == "sma_200"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_sma200)
        }else if(imp_variables[j] == "VWMA_20"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_vwma20)
        }else if(imp_variables[j] == "HMA_9"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_HMA9)
        }else if(imp_variables[j] == "ATR"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ATR)
        }else if(imp_variables[j] == "SMI"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_SMI)
        }else if(imp_variables[j] == "ADX"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ADX)
        }else if(imp_variables[j] == "Aroon"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_Aroon)
        }else if(imp_variables[j] == "BB"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_BB)
        }else if(imp_variables[j] == "ChaikinVol"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_ChaikinVol)
        }else if(imp_variables[j] == "CLV"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_CLV)
        }else if(imp_variables[j] == "MACD"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_MACD)
        }else if(imp_variables[j] == "MFI"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_MFI)
        }else if(imp_variables[j] == "SAR"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_SAR)
        }else if(imp_variables[j] == "Volat"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_Volat)
        }else if(imp_variables[j] == "turningLine"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_turningLine)
        }else if(imp_variables[j] == "baseLine"){
          new_list[[imp_variables[j]]] = as.numeric(input$ml_baseLine)
        }
        
      }
      # browser()
      test_data = as.data.frame(new_list)
      
      # prediction <- predict(linear_model, newdata= list(Volume=as.numeric(input$ml_Volume), rsi=as.numeric(input$ml_rsi), ema_10=as.numeric(input$ml_ema10),sma_10=as.numeric(input$ml_sma10), ema_20=as.numeric(input$ml_ema20), sma_20=as.numeric(input$ml_sma20),ema_30=as.numeric(input$ml_ema30), sma_30=as.numeric(input$ml_sma30), ema_50=as.numeric(input$ml_ema50),sma_50=as.numeric(input$ml_sma50), ema_100=as.numeric(input$ml_ema100), sma_100=as.numeric(input$ml_sma100), ema_200=as.numeric(input$ml_ema200),sma_200=as.numeric(input$ml_sma200), VWMA_20=as.numeric(input$ml_vwma20), HMA_9=as.numeric(input$ml_HMA9),ATR=as.numeric(input$ml_ATR), SMI=as.numeric(input$ml_SMI), ADX=as.numeric(input$ml_ADX),Aroon=as.numeric(input$ml_Aroon), BB=as.numeric(input$ml_BB), ChaikinVol=as.numeric(input$ml_ChaikinVol), CLV=as.numeric(input$ml_CLV),MACD=as.numeric(input$ml_MACD), MFI=as.numeric(input$ml_MFI), SAR=as.numeric(input$ml_SAR), Volat=as.numeric(input$ml_Volat), turningLine=as.numeric(input$ml_turningLine),baseLine=as.numeric(input$ml_baseLine)) )
      prediction <- predict(linear_model,newdata = test_data)
      
      paste("The Predicted Price using Regressor is <font color=\"#FF0000\"><b>", round(prediction,2), "</b></font>")
    })
    
    output$logistic_output <- renderText({
      
      # browser()
      logistic_stock_data <- as.data.frame(logistic_stock_data)
      logistic_stock_data = select(logistic_stock_data,-c("avg_price"))
      
      logistic_stock_data$direction <- 0
      
      logistic_stock_data$direction <- ifelse(logistic_stock_data$Close > lead(logistic_stock_data$Close,1),1,0)
      # logistic_stock_data$direction <- ifelse(logistic_stock_data$Close > Lag(logistic_stock_data$Close,1),1,0)
      
      test_data_frame <- tail(logistic_stock_data,1)
      
      logistic_stock_data <- logistic_stock_data[which(!is.na(logistic_stock_data$direction)),]
      logistic_stock_data <- na.omit(logistic_stock_data)
      
      result <- logistic_stock_data[-1]
      row.names(result) <- logistic_stock_data$Date
      logistic_stock_data <- result

      set.seed(3456)
      
      # dt = sort(sample(nrow(logistic_stock_data), nrow(logistic_stock_data)*.7))
      # 
      # train <- logistic_stock_data[dt,]
      # test <- logistic_stock_data[-dt,]
      # 
      formula<- paste("direction ~ .",sep="")
      # 
      # 
      # model <- glm(formula,family="binomial",train)
      
      logistic_stock_data <- logistic_stock_data[!is.infinite(rowSums(logistic_stock_data)),]
      
      log_model <- glm(formula,family="binomial",logistic_stock_data)
      
      logistic_imp <- varImp(log_model, scale = FALSE)
      
      logistic_imp <- as.data.frame(logistic_imp)
      
      logistic_imp <- arrange(logistic_imp,desc(Overall))
      
      logistic_imp <- cbind(Variable = rownames(logistic_imp), logistic_imp)
      rownames(logistic_imp) <- 1:nrow(logistic_imp)
      
      # logistic_imp$Overall <- round(logistic_imp$Overall,2)
      # browser()
      
      logistic_imp = mutate(logistic_imp, 
                            Overall_pct = round(Overall / sum(Overall) *100,2))
      
      
      final_importance <- logistic_imp %>% select(c("Variable","Overall_pct"))
      
      imp_variables <- head(final_importance,10)$Variable
      
      final_stocks_data <- logistic_stock_data %>% select(c(paste(imp_variables),"direction"))
      
      log_model <- glm(formula,family="binomial",final_stocks_data)
      
      # test_data <- tail(final_stocks_data,1)
      # pred <- predict(log_model,newdata = test_data,type = "response")
      
      test_data_frame <- tail(final_stocks_data,1) %>% select(c(paste(imp_variables),"direction"))
      
      pred <- predict(log_model,newdata = test_data_frame,type = "response")
      
  
      pred_direction <- ifelse(pred > 0.5,"Buy","Sell")
      
      pred_direction
      
      paste("The Call using Logistic classification is <font color=\"#FF0000\"><b>", pred_direction, "</b></font>")
    })
    
    
    
   
    
    
    
    output$ml_linear_table <- DT::renderDataTable({
      
      nse_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv", sep = ""))
      
      prediction_model <- data.frame("Stock"=character(0),"Current_Close"=numeric(0),"Model_Close"=numeric(0))
      
      for(i in 1:nrow(nse_data)){
        
        stock = nse_data[i,2]
        
        stocks_data <- na.omit(getSymbols(stock, src = "yahoo", from = "2008-01-01", to = Sys.Date() + 1, auto.assign = FALSE))
        
        prediction <- tryCatch(
          {
            
            stocks_data2 <- stocks_data
            
            ichimoku <- function(HLC, nFast=9, nMed=26, nSlow=52) {
              turningLine <- (runMax(Hi(HLC), nFast)+runMin(Lo(HLC), nFast))/2
              baseLine <- (runMax(Hi(HLC), nMed)+runMin(Lo(HLC), nMed))/2
              
              out = cbind(turningLine,baseLine)
              return(out)
            }
            
            ichimoku_dt = ichimoku(stocks_data2)
            stocks_data <- cbind(stocks_data,ichimoku_dt)
            # View(stocks_data)
            stocks_data <- data.frame(Date = index(stocks_data), coredata(stocks_data) )
            
            colnames(stocks_data) <- c("Date","Open","High","Low","Close","Volume","Adjusted","turningLine","baseLine")
            
            stocks_data <- mutate(stocks_data, next_day_close = lead(Close),next_day_open = lead(Open))
            
            stocks_data$avg_price <- round((stocks_data$Open + stocks_data$Close)/2,2)
            
            # stocks_data$next_day_avg_price <- round((stocks_data$next_day_open + stocks_data$next_day_close)/2,2)
            
            head(stocks_data)
            
            xbt <- stocks_data %>% 
              select(Date, Open, High, Low, Close, Volume,Adjusted,avg_price) %>%
              mutate(
                rsi = TTR::RSI(Close, 14),
                ema_10 = TTR::EMA(Close, 10),
                sma_10 = TTR::SMA(Close,10),
                ema_20 = TTR::EMA(Close,20),
                sma_20 = TTR::SMA(Close,20),
                ema_30 = TTR::EMA(Close, 30),
                sma_30 = TTR::SMA(Close,30),
                ema_50 = TTR::EMA(Close,50),
                sma_50 = TTR::SMA(Close,50),
                ema_100 = TTR::EMA(Close, 100),
                sma_100 = TTR::SMA(Close,100),
                ema_200 = TTR::EMA(Close,200),
                sma_200 = TTR::SMA(Close,200),
                VWMA_20 = TTR::VWMA(Close,Volume,n=20),
                HMA_9 = TTR::HMA(Close,n=9)
              )
            xbt <- as.data.frame(xbt)
            
            xbt <- na.omit(xbt)
            
            xbt = select(xbt,-c("Low","Open","Close","High","Date","Adjusted"))
            
            
            linear_model <- lm(avg_price ~ .,data= xbt)
            
            prediction <- predict( linear_model, newdata= list(Volume=as.numeric(xbt[nrow(xbt),"Volume"]), rsi=as.numeric(xbt[nrow(xbt),"rsi"]), ema_10=as.numeric(xbt[nrow(xbt),"ema_10"]),sma_10=as.numeric(xbt[nrow(xbt),"sma_10"]), ema_20=as.numeric(xbt[nrow(xbt),"ema_20"]), sma_20=as.numeric(xbt[nrow(xbt),"sma_20"]),ema_30=as.numeric(xbt[nrow(xbt),"ema_30"]), sma_30=as.numeric(xbt[nrow(xbt),"sma_30"]), ema_50=as.numeric(xbt[nrow(xbt),"ema_50"]),sma_50=as.numeric(xbt[nrow(xbt),"sma_50"]), ema_100=as.numeric(xbt[nrow(xbt),"ema_100"]), sma_100=as.numeric(xbt[nrow(xbt),"sma_100"]), ema_200=as.numeric(xbt[nrow(xbt),"ema_200"]),sma_200=as.numeric(xbt[nrow(xbt),"sma_200"]), VWMA_20=as.numeric(xbt[nrow(xbt),"VWMA_20"]), HMA_9=as.numeric(xbt[nrow(xbt),"HMA_9"]))) 
            
            prediction
            
            print(stock)
            print(prediction)
            
            current_row <- nrow(prediction_model)+1
            
            prediction_model[current_row,"Stock"] <- stock
            prediction_model[current_row,"Current_Close"] <- as.numeric(stocks_data[nrow(stocks_data),"Close"])
            prediction_model[current_row,"Model_Close"] <- as.numeric(as.character(prediction))
            
          },
          error=function(cond) {
            print(stock) 
            print("Having error")
            current_row <- nrow(prediction_model)+1
            prediction_model[current_row,"Stock"] <- stock
            prediction_model[current_row,"Current_Close"] <- as.numeric(stocks_data[nrow(stocks_data),"Close"])
            prediction_model[current_row,"Model_Close"] <- "0"
          },
          warning=function(cond) {
            print(stock)
            print("Warning")
            print(cond)
            return(NULL)
          },
          finally={
            
          }
        )
      }
      
      prediction_model$position <- ifelse(as.numeric(unlist(prediction_model["Current_Close"])) > as.numeric(unlist(prediction_model["Model_Close"])),"Over Valued","Under Valued")
      
      prediction_model$percentage_change <- abs(round(((as.numeric(unlist(prediction_model["Current_Close"])) - as.numeric(unlist(prediction_model["Model_Close"])))*1.00/as.numeric(unlist(prediction_model["Model_Close"])))*100,2))
      
      prediction_model <- prediction_model[order(prediction_model$percentage_change),]
      
      prediction_model
      
      
      DT::datatable(prediction_model,extensions = c('FixedColumns'),
                    options = list(scrollX = TRUE,
                                   pageLength=10,
                                   searchHighlight = TRUE,
                                   filter = 'top'
                    ))
      
    })
    
    
    
    
    
  })
  
  
  supply_zone_detection <- function(df,stock,df_supply_and_demand){
    # browser()
    if(nrow(df) >= 3){
      for (ind in 2:(nrow(df)-1)) {
        # browser()
        if(df[ind-1,'Open'] < df[ind-1,'Close'] && ##Green Candle
           (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5 *(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im Balance Candle
           (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
           (df[ind+1,'Open'] > df[ind+1,'Close']) && # Red Candle
           (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))
        ){
          # browser()
          df_supply_and_demand[ind,'stock'] = stock
          df_supply_and_demand[ind,'pattern'] = "Supply Reversal Pattern"
          df_supply_and_demand[ind,'Date'] = df[ind,'Date']
          # df_supply_and_demand[ind,'zone_1'] = round(min(df[ind,'Open'],df[ind,'Close']),2)
          # df_supply_and_demand[ind,'zone_2'] = round(max(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
          
          df_supply_and_demand[ind,'zone_1'] = df[ind,'Low']
          df_supply_and_demand[ind,'zone_2'] = df[ind,'High']
          
          if(df[ind+1,'Open'] > df[ind,'Open']){
            df_supply_and_demand[ind,'strength'] = "Strong"
          }else{
            df_supply_and_demand[ind,'strength'] = "Normal"
          }
          
        }
        else if((df[ind-1,'Open'] > df[ind-1,'Close']) && # Red Candle
                (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im-Balance Candle
                
                (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
                (df[ind+1,'Open'] > df[ind+1,'Close']) && # Red Candle
                (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))  #Im-Balance Candle
                
        ){
          df_supply_and_demand[ind,'stock'] = stock
          df_supply_and_demand[ind,'pattern'] = "Supply Continuous Pattern"
          df_supply_and_demand[ind,'Date'] = df[ind,'Date']
          # df_supply_and_demand[ind,'zone_1'] = round(min(df[ind,'Open'],df[ind,'Close']),2)
          # df_supply_and_demand[ind,'zone_2'] = round(max(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
          
          df_supply_and_demand[ind,'zone_1'] = df[ind,'Low']
          df_supply_and_demand[ind,'zone_2'] = df[ind,'High']
          
          
          if(df[ind+1,'Open'] < df[ind,'Open']){
            df_supply_and_demand[ind,'strength'] = "Strong"
          }else{
            df_supply_and_demand[ind,'strength'] = "Normal"
          }
          
        }
        # print(ind)
      }
      
    }
    
    return(df_supply_and_demand)
  }
  
  demand_zone_detection <- function(df,stock,df_supply_and_demand){
    if(nrow(df) >= 3){
      for (ind in 2:(nrow(df)-1)) {
        # browser()
        if((df[ind-1,'Open'] > df[ind-1,'Close']) &&
           (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) &&
           (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
           (df[ind+1,'Open'] < df[ind+1,'Close']) && # Green Candle
           (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))
        ){
          df_supply_and_demand[ind,'stock'] = stock
          df_supply_and_demand[ind,'pattern'] = "Demand Reversal Pattern"
          df_supply_and_demand[ind,'Date'] = df[ind,'Date']
          # df_supply_and_demand[ind,'zone_1'] = round(max(df[ind,'Open'],df[ind,'Close']),2)
          # df_supply_and_demand[ind,'zone_2'] = round(min(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
          
          df_supply_and_demand[ind,'zone_1'] = df[ind,'High']
          df_supply_and_demand[ind,'zone_2'] = df[ind,'Low']
          
          if(df[ind+1,'Open'] > df[ind,'Open']){
            df_supply_and_demand[ind,'strength'] = "Strong"
          }else{
            df_supply_and_demand[ind,'strength'] = "Normal"
          }
          
        }
        else if((df[ind-1,'Open'] < df[ind-1,'Close']) && # Green Candle
                (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im-Balance Candle
                (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
                (df[ind+1,'Open'] < df[ind+1,'Close']) && # Green Candle
                (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))  #Im-Balance Candle
                
        ){
          # browser()
          df_supply_and_demand[ind,'stock'] = stock
          df_supply_and_demand[ind,'pattern'] = "Demand Continuous Pattern"
          df_supply_and_demand[ind,'Date'] = df[ind,'Date']
          # df_supply_and_demand[ind,'zone_1'] = round(max(df[ind,'Open'],df[ind,'Close']),2)
          # df_supply_and_demand[ind,'zone_2'] = round(min(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
          
          df_supply_and_demand[ind,'zone_1'] = df[ind,'High']
          df_supply_and_demand[ind,'zone_2'] = df[ind,'Low']
          
          if(df[ind+1,'Open'] > df[ind,'Open']){
            df_supply_and_demand[ind,'strength'] = "Strong"
          }else{
            df_supply_and_demand[ind,'strength'] = "Normal"
          }
        }
        # print(ind)
      }
    }
    return(df_supply_and_demand)
  }
  
  observeEvent(input$bot_submit,{
  
  output$bot_buy_and_sell <- DT::renderDataTable({
    
    # Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
    
    
    # final_data <- getpercentageChange()
    # 
    # final_data$trend = ''
    # final_data$trigger_price = ''
    # final_data$value_change = ''
    # final_data$percentage_change = ''
    # final_data$target = ''
    # final_data$stoploss = ''
    # 
    # # head(final_data)
    # 
    # 
    # position <- 0 # 1 means we have already entered poistion, 0 means not already entered
    # counter <- 0
    # percentChange <- list()   # empty list to collect %changes 
    # buyposition_index = 1
    # sellposition_index = 1
    # 
    # outputdf = data.frame(time=character(0),call=character(0),percentage_change=character(0),target = character(0),stoploss = character(0),previous = character(0))
    # 
    # for(i in 1:nrow(final_data)){
    #   
    #   # browser()
    #   if((final_data[i,'Close'] > final_data[i,"last_close"]) & (final_data[i,'Close'] > final_data[i,"ema_20_Close"]) & (final_data[i,'ema_10_Close'] > final_data[i,"ema_20_Close"])){
    #     final_data[i,'trend'] <- 'Uptrend'
    #     final_data[i,'trigger_price'] <- final_data[buyposition_index,'Close']
    #     final_data[i,'value_change'] <- (final_data[buyposition_index,'Close'] - final_data[i,'Close'])
    #     final_data[i,'percentage_change'] <- (final_data[buyposition_index,'Close'] - final_data[i,'Close']) / ((final_data[buyposition_index,'Close'])*100)
    #     
    #     if(position==0){
    #       # browser()
    #       buyP=final_data[i,'Close']  #buy price
    #       targetP = buyP + (input$bot_profit*buyP)/100
    #       stopLossP = final_data[i,'Low'] - (input$bot_profit*final_data[i,'Low'])/100
    #       position=1   # turn position
    #       buyposition_index = i
    #       subset_data <- final_data[buyposition_index:sellposition_index,]
    #       
    #       current_row = nrow(outputdf)
    #       outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #       outputdf[current_row+1,'call'] <- paste("Buy at the price ",round(buyP,2),"")
    #       outputdf[current_row+1,'target'] <- round(targetP,2)
    #       outputdf[current_row+1,'stoploss'] <- round(stopLossP,2)
    #       outputdf[current_row+1,'previous'] <- round(final_data[sellposition_index,'Close'],2)
    #       
    #       target_data <- subset_data[subset_data$High >= targetP,]
    #       
    #       outputdf[current_row+1,'target_hit'] <- ifelse(nrow(target_data) == 0 ,FALSE,TRUE)
    #       
    #       
    #       # print(paste("Buy at the price ",buyP,""))
    #       # percentChange <- append(percentChange,paste("Buy at the price ",buyP,""))
    #     }
    #     
    #   }
    #   else if((final_data[i,'Close'] < final_data[i,"last_close"]) & (final_data[i,'Close'] < final_data[i,"ema_20_Close"]) & (final_data[i,'ema_10_Close'] < final_data[i,"ema_20_Close"])){
    #     final_data[i,'trend'] <- 'Downtrend'
    #     final_data[i,'trigger_price'] <- final_data[sellposition_index,'Close']
    #     final_data[i,'value_change'] <- (final_data[i,'Close'] - final_data[sellposition_index,'Close'])
    #     final_data[i,'percentage_change'] <- (final_data[i,'Close'] - final_data[sellposition_index,'Close']) / ((final_data[i,'Close'])*100)
    #     
    #     if(position==1){
    #       # have a poistion in down trend
    #       position=0     # selling position
    #       sellP=final_data[i,'Close']    # sell price
    #       sellposition_index = i
    #       
    #       perc=(sellP/buyP-1)*100
    #       targetP = sellP - (input$bot_profit*sellP)/100
    #       stopLossP = final_data[i,'High'] + (input$bot_profit*final_data[i,'High'])/100
    #       
    #       subset_data <- final_data[buyposition_index:sellposition_index,]
    #       
    #       # browser()
    #       current_row = nrow(outputdf)
    #       outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #       outputdf[current_row+1,'call'] <- paste("Sell at the price ",round(sellP,2),"")
    #       outputdf[current_row+1,'percentage_change'] <-perc
    #       outputdf[current_row+1,'target'] <- round(targetP,2)
    #       outputdf[current_row+1,'stoploss'] <- round(stopLossP,2)
    #       outputdf[current_row+1,'previous'] <- round(final_data[buyposition_index,'Close'],2)
    #       
    #       target_data <- subset_data[subset_data$Low <= targetP,]
    #       
    #       outputdf[current_row+1,'target_hit'] <- ifelse(nrow(target_data) == 0 ,FALSE,TRUE)
    #       # print(paste("Sell at the price ",sellP,""))
    #       # percentChange <- append(percentChange,paste("Buy at the price ",buyP,""))
    #       # 
    #       # percentChange <- append(percentChange, list(perc))
    #       
    #     }   
    #     
    #   }
    #   else{
    #     next
    #   }
    #   
    #   # if(hour(as.POSIXct(final_data[i,"dates"], origin = "1970-01-01")) == 15 & minute(as.POSIXct(final_data[i,"dates"], origin = "1970-01-01")) == 10){
    #   #   
    #   #   if(position == 1){
    #   #     position == 0
    #   #     sellP=final_data[i,'Close']
    #   #     perc=(sellP/buyP-1)*100
    #   #     current_row = nrow(outputdf)
    #   #     outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #   #     outputdf[current_row+1,'call'] <- paste("Sell at the price ",sellP ,"")
    #   #     outputdf[current_row+1,'percentage_change'] <-perc
    #   #   }
    #   #   else{
    #   #     position == 1
    #   #     buyP=final_data[i,'Close']
    #   #   }
    #   #   
    #   # }
    #   
    #   if(counter==nrow(final_data) & position==1){
    #     # browser()
    #     position=0
    #     sellP=final_data[i,'Close']
    #     # print(paste("Sell at the price ",sellP,""))
    #     # percentChange <- append(percentChange, paste("Sell at the price ",sellP,""))
    #     # perc=(sellP/buyP-1)*100
    #     # percentChange <- append(percentChange, list(perc))
    #     targetP = sellP - (input$bot_profit*sellP)/100
    #     stopLossP = final_data[i,'High'] + (input$bot_profit*final_data[i,'High'])/100
    #     subset_data <- final_data[buyposition_index:sellposition_index,]
    #     
    #     current_row = nrow(outputdf)
    #     outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #     outputdf[current_row+1,'call'] <- paste("Sell at the price ",round(sellP,2),"")
    #     outputdf[current_row+1,'percentage_change'] <-perc
    #     
    #     outputdf[current_row+1,'target'] <- round(targetP,2)
    #     outputdf[current_row+1,'stoploss'] <- round(stopLossP,2)
    #     
    #     outputdf[current_row+1,'previous'] <- round(final_data[buyposition_index,'Close'],2)
    #     target_data <- subset_data[subset_data$Low <= targetP,]
    #     
    #     outputdf[current_row+1,'target_hit'] <- ifelse(nrow(target_data) == 0 ,FALSE,TRUE)
    #   }
    #   
    # }
    # 
    # counter = counter + 1
    
    # browser()
    
    if(input$nifty_selection == "nifty_50"){
      nse_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv", sep = ""))
    }else if(input$nifty_selection == "us_30"){
      nse_data <- read.csv(paste0(getwd(),"/data/US - 30 Stocks.csv", sep = ""))
    }else{
      nse_data <- read.csv(paste0(getwd(),"/data/ind_nifty500list.csv", sep = ""))
    }
    
    # nse_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv", sep = ""))
    
    Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
    
    increment = 1
    
    for (i in 1:length(input$bot_strategy)) {
      if(input$bot_strategy[i] == "sweths_violation"){

        for(i in 1:nrow(nse_data)){
          stock = nse_data[i,2]
          # print(stock)
          
          # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
          
          if(input$bot_timeframe == "1m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "2m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "5m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "15m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "4h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1d"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          # else if(input$candle_stick_range == "1y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          # else if(input$candle_stick_range == "5y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          else {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          
          
          stock_timestamp <- response_data$chart$result[[1]]$timestamp
          Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
          High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
          Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
          Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
          Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
          
          # browser()
          
          if(nrow(final_data) == 0){
            stock_timestamp <- response_data$chart$result[[2]][[1]]
            Close <- response_data$chart$result[[3]]$quote[[1]]$close
            High <- response_data$chart$result[[3]]$quote[[1]]$high
            Low <- response_data$chart$result[[3]]$quote[[1]]$low
            Open <- response_data$chart$result[[3]]$quote[[1]]$open
            Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
            
            
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          }
          
          
          colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
          
          if(typeof(final_data$V1) == "list"){
            final_data <- final_data[-c(which(final_data$Close == "NULL")),]
            new_stock_timestamp <- unlist(final_data$V1)
            Close <- unlist(final_data$Close)
            High <- unlist(final_data$High)
            Open <- unlist(final_data$Open)
            Low <- unlist(final_data$Low)
            Volume <- unlist(final_data$Volume)
            
            final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
            
            final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          else{
            final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          final_data <- na.omit(final_data)
          
          
          # final_data <- final_data %>% 
          #   select(dates, Open, High, Low, Close, Volume) %>%
          #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
          #   mutate(
          #     sma_35 = TTR::SMA(Close, 35),
          #     rsi = TTR::RSI(Close, 14),
          #     vwap = TTR::VWAP(Close,Volume,10)
          #   )
          # 
          # 
          # today = Sys.Date()
          # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
          # currentWorkingDay <- f(today)
          # 
          # 
          # 
          # final_data <- final_data[as.Date(final_data$dates) == currentWorkingDay,]
          
          # browser()
          
          if(weekdays(Sys.Date()) == "Sunday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
          }else if(weekdays(Sys.Date()) == "Saturday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
          }else{
            if(hour(Sys.time()) >= 9){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
            }else{
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
            }
            
          }
          
          trigger_price = 0
          stage = ""
          
          if((final_data[1,"Close"]>final_data[1,"Open"]) && abs(final_data[1,"Close"] - final_data[1,"Open"])>= 0.7*abs(final_data[1,"High"] - final_data[1,"Low"])){
            trigger_price = final_data[1,"Low"]
            stage = "Green"
          }
          else if((final_data[1,"Close"]<final_data[1,"Open"]) && abs(final_data[1,"Close"] - final_data[1,"Open"])>= 0.7*abs(final_data[1,"High"] - final_data[1,"Low"])){
            trigger_price = final_data[1,"High"]
            stage = "Red"
          }
          else{
            next
          }
          
          satisfied_df = data.frame()
          
          for(j in 5:nrow(final_data)){
            
            if(stage == "Green"){
              if(final_data[j,"Close"] < trigger_price){
                satisfied_df = rbind(satisfied_df,final_data[j,])
                call ="Sell"
                print(stock)
                print("Moving up")
              }
            }
            else if(stage == "Red"){
              if(final_data[j,"Close"] > trigger_price){
                satisfied_df = rbind(satisfied_df,final_data[j,])
                call = "Buy"
                print(stock)
                print("Moving Down")
              }
            }
            else{
              next
            }
            
          }
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            # print(satisfied_df)
            satisfied_df = head(satisfied_df,1)
            Signal_df[increment,"Strategy"] <- "Sweths Violation"
            Signal_df[increment,"Stock"]=stock
            Signal_df[increment,"Signal"]=call
            Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
            Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
            # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
            # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
            # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
            
            increment = increment + 1
          }
          
        }
        
      }
      else if(input$bot_strategy[i] == "cowboy"){
        # final_levels_df = data.frame("Stock" = character(0),"Rider_Bullish" = character(0),"Bullish_Level"=character(0),"Rider_Bearish"=character(0),"Bearish_Level"=character(0))
        # 
        # for(i in 1:nrow(nse_data)){
        #   stock = nse_data[i,2]
        #   today = Sys.Date()
        #   f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
        #   previousWorkingDay <- f(today) - 1
        #   
        #   stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = previousWorkingDay, auto.assign = FALSE)
        #   # stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = "2021-01-15", auto.assign = FALSE)
        #   
        #   
        #   stock_data <- tail(stock_data,4)
        #   
        #   # print(stock_data)
        #   
        #   temp_df <- data.frame("Stock" = character(0),"Rider_Bullish" = character(0),"Bullish_Level"=character(0),"Rider_Bearish"=character(0),"Bearish_Level"=character(0))
        #   
        #   temp_df[1,'Stock'] <- stock
        #   
        #   stock_data <- data.frame(stock_data)
        #   
        #   
        #   if(abs((stock_data[4,2] - stock_data[3,2])/(stock_data[3,2])*100) < 0.5){
        #     temp_df[1,"Rider_Bullish"] = "Yes"
        #     temp_df[1,"Bullish_Level"] = max(stock_data[4,2],stock_data[3,2])
        #   }
        #   else{
        #     temp_df[1,"Rider_Bullish"] = "No"
        #     temp_df[1,"Bullish_Level"] = 100000
        #   }
        #   if(abs((stock_data[4,3] - stock_data[3,3])/(stock_data[3,3])*100) < 0.5){
        #     temp_df[1,"Rider_Bearish"] = "Yes"
        #     temp_df[1,"Bearish_Level"] = min(stock_data[4,3],stock_data[3,3])
        #   }
        #   else{
        #     temp_df[1,"Rider_Bearish"] = "No"
        #     temp_df[1,"Bearish_Level"] = 0
        #   }
        #   # print(temp_df)
        #   final_levels_df = rbind(final_levels_df,temp_df)
        # }
        
        if(input$nifty_selection == "nifty_50"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/cowboy_data.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/us_cowboy_data.csv", sep = ""))
        }else{
          final_levels_df <- read.csv(paste0(getwd(),"/data/nifty_500_cowboy_data.csv", sep = ""))
        }
        
        
        
        final_levels_df <- subset(final_levels_df, select = -c(X) )
        
        # View(final_levels_df)
        
        # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
        
        # inc = 1
        
        for(i in 1:nrow(nse_data)){
          stock = nse_data[i,2]
          print(stock)
          # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
          
          if(input$bot_timeframe == "1m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "2m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "5m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "15m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "4h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1d"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          # else if(input$candle_stick_range == "1y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          # else if(input$candle_stick_range == "5y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          else {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          
          # browser()
          stock_timestamp <- response_data$chart$result[[1]]$timestamp
          Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
          High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
          Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
          Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
          Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
          
          
          
          if(nrow(final_data) == 0){
            stock_timestamp <- response_data$chart$result[[2]][[1]]
            Close <- response_data$chart$result[[3]]$quote[[1]]$close
            High <- response_data$chart$result[[3]]$quote[[1]]$high
            Low <- response_data$chart$result[[3]]$quote[[1]]$low
            Open <- response_data$chart$result[[3]]$quote[[1]]$open
            Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
            
            
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          }
          
          
          colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
          
          if(typeof(final_data$V1) == "list"){
            final_data <- final_data[-c(which(final_data$Close == "NULL")),]
            new_stock_timestamp <- unlist(final_data$V1)
            Close <- unlist(final_data$Close)
            High <- unlist(final_data$High)
            Open <- unlist(final_data$Open)
            Low <- unlist(final_data$Low)
            Volume <- unlist(final_data$Volume)
            
            final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
            
            final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          else{
            final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          final_data <- na.omit(final_data)
          
          # final_data <- final_data %>% 
          #   select(dates, Open, High, Low, Close, Volume) %>%
          #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
          #   mutate(
          #     sma_35 = TTR::SMA(Close, 35),
          #     rsi = TTR::RSI(Close, 14),
          #     vwap = TTR::VWAP(Close,Volume,10)
          #   )
          # 
          # 
          # today = Sys.Date()
          # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
          # currentWorkingDay <- f(today)
          # 
          # 
          # 
          if(weekdays(Sys.Date()) == "Sunday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
          }else if(weekdays(Sys.Date()) == "Saturday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
          }else{
            if(hour(Sys.time()) >= 9){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
            }else{
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
            }
            
          }
          
          # print(final_data)
          
          sub_df = final_levels_df[final_levels_df$Stock == stock,]
          
          if(nrow(sub_df) > 0){
          
          rownames(sub_df) <- 1
          
          # print(sub_df)
          
          if(sub_df[1,2] == "Yes"){
            satisfied_df = data.frame()
            
            for(i in 1:nrow(final_data)){
              if((final_data[i,"Close"]) > sub_df[1,3]){
                satisfied_df = rbind(satisfied_df,final_data[i,])
              }
              else{
                next
              }
            }
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              satisfied_df = head(satisfied_df,1)
              # print(satisfied_df)
              rownames(satisfied_df) <- 1
              
              time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
              # print(time_min)
              
              if(time_min <= "15:10:00"){
              
              Signal_df[increment,"Strategy"] <- "Cowboy"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]="Buy"
              Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
              # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
              # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
              
              increment = increment + 1
              }else{
                next
              }
            }
          }
          else{
            next
          }
          
          if(sub_df[1,4] == "Yes"){
            satisfied_df = data.frame()
            
            for(i in 1:nrow(final_data)){
              if((final_data[i,"Close"]) < sub_df[1,5]){
                # print("Sell")
                satisfied_df = rbind(satisfied_df,final_data[i,])
                # print(satisfied_df)
              }
              else{
                next
              }
              
            }
            
            
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              # browser()
              satisfied_df = head(satisfied_df,1)
              rownames(satisfied_df) <- 1
              
              time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
              # print(time_min)
              
              if(time_min <= "15:10:00"){
              Signal_df[increment,"Strategy"] <- "Cowboy"
              
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]="Sell"
              Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
              # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
              # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
              
              increment = increment + 1
              }else{
                next
              }
            }
          }
          else{
            next
          }
        }
          
          
        }
      }
      else if(input$bot_strategy[i] == "reds_rocket"){
        # browser()
        # final_levels_df = data.frame("Stock" = character(0),"Reds_High" = numeric(0),"Reds_Low"=numeric(0))
        # 
        # inc = 1
        # 
        # for(i in 1:nrow(nse_data)){
        #   stock = nse_data[i,2]
        #   today = Sys.Date()
        #   f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
        #   previousWorkingDay <- f(today) - 1
        #   
        #   stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = previousWorkingDay, auto.assign = FALSE)
        #   # stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = "2021-01-15", auto.assign = FALSE)
        #   
        #   
        #   stock_data <- tail(stock_data,4)
        #   
        #   # print(stock_data)
        #   
        #   stock_data <- as.data.frame(stock_data)
        #   
        #   rownames(stock_data) <- 1:4
        #   
        #   l1_day_range <- abs(stock_data[4,2] - stock_data[4,3])
        #   l2_day_range <- abs(stock_data[3,2] - stock_data[3,3])
        #   l3_day_range <- abs(stock_data[2,2] - stock_data[2,3])
        #   l4_day_range <- abs(stock_data[1,2] - stock_data[1,3])
        #   
        #   l2_day_high <- stock_data[3,2]
        #   l1_day_high <- stock_data[4,2]
        #   
        #   l2_day_low <- stock_data[3,3]
        #   l1_day_low <- stock_data[4,3]
        #   
        #   if((l1_day_range < l2_day_range) && (l1_day_range < l3_day_range) && (l1_day_range < l4_day_range)){
        #     if(l1_day_low > l2_day_low && l1_day_high < l2_day_high){
        #       
        #       final_levels_df[inc,"Stock"] = stock
        #       final_levels_df[inc,"Reds_High"] = l1_day_high
        #       final_levels_df[inc,"Reds_Low"] = l1_day_low
        #       
        #       
        #       
        #       inc = inc + 1
        #     }
        #     
        #   }
        #   else{
        #     next
        #   }
        # }
        
        if(input$nifty_selection == "nifty_50"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/reds_rocket.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/us_reds_rocket.csv", sep = ""))
        }else{
          final_levels_df <- read.csv(paste0(getwd(),"/data/nifty_500_reds_rocket.csv", sep = ""))
        }
        
        # final_levels_df <- read.csv(paste0(getwd(),"/data/reds_rocket.csv", sep = ""))
        
        final_levels_df <- subset(final_levels_df, select = -c(X) )
        
        
        if(nrow(final_levels_df) > 0){
          # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
          
          # increment = 1
          
          for(i in 1:nrow(final_levels_df)){
            stock = final_levels_df[i,"Stock"]
            print(stock)
            # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
            
            if(input$bot_timeframe == "1m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "2m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "5m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "15m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "4h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1d"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            # else if(input$candle_stick_range == "1y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            # else if(input$candle_stick_range == "5y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            else {
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            
            stock_timestamp <- response_data$chart$result[[1]]$timestamp
            Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
            High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
            Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
            Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
            Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
            
            # browser()
            
            if(nrow(final_data) == 0){
              stock_timestamp <- response_data$chart$result[[2]][[1]]
              Close <- response_data$chart$result[[3]]$quote[[1]]$close
              High <- response_data$chart$result[[3]]$quote[[1]]$high
              Low <- response_data$chart$result[[3]]$quote[[1]]$low
              Open <- response_data$chart$result[[3]]$quote[[1]]$open
              Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
              
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
            }
            
            
            colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
            
            if(typeof(final_data$V1) == "list"){
              final_data <- final_data[-c(which(final_data$Close == "NULL")),]
              new_stock_timestamp <- unlist(final_data$V1)
              Close <- unlist(final_data$Close)
              High <- unlist(final_data$High)
              Open <- unlist(final_data$Open)
              Low <- unlist(final_data$Low)
              Volume <- unlist(final_data$Volume)
              
              final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
              
              final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            else{
              final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            final_data <- na.omit(final_data)
            
            # final_data <- final_data %>% 
            #   select(dates, Open, High, Low, Close, Volume) %>%
            #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
            #   mutate(
            #     sma_35 = TTR::SMA(Close, 35),
            #     rsi = TTR::RSI(Close, 14),
            #     vwap = TTR::VWAP(Close,Volume,10)
            #   )
            # 
            # 
            # today = Sys.Date()
            # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
            # currentWorkingDay <- f(today)
            # 
            # # browser()
            # 
            if(weekdays(Sys.Date()) == "Sunday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
            }else{
              if(hour(Sys.time()) >= 9){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
              }else{
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
              }
              
            }
            
            
            print(final_data)
            
            final_data$Call <- ""
            
            satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
            
            for(j in 1:nrow(final_data)){
              
              
              if((final_data[j,"Close"]) > final_levels_df[i,"Reds_High"]){
                # print(satisfied_df)
                satisfied_df = rbind(satisfied_df,final_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                # print("Buy")
                # print(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
              }
              else if((final_data[j,"Close"]) < final_levels_df[i,"Reds_Low"]){
                
                satisfied_df = rbind(satisfied_df,final_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                # print("Sell")
                # print(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
              }
              else{
                next
              }
            }
            # print(nrow(satisfied_df))
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              satisfied_df = head(satisfied_df,1)
              # print(satisfied_df)
              rownames(satisfied_df) <- 1
              
              time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
              # print(time_min)
              
              if(time_min <= "15:10:00"){
              Signal_df[increment,"Strategy"] <- "Reds Rocket"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
              # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
              # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
              
              increment = increment + 1
              }else{
                next
              }
            }
            
          }
        }
        
      }
      else if(input$bot_strategy[i] == "reds_brahmos"){
        # final_levels_df = data.frame("Stock" = character(0),"Reds_High" = numeric(0),"Reds_Low"=numeric(0))
        # 
        # inc = 1
        # 
        # for(i in 1:nrow(nse_data)){
        #   stock = nse_data[i,2]
        #   today = Sys.Date()
        #   f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
        #   previousWorkingDay <- f(today) - 1
        #   
        #   stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = previousWorkingDay, auto.assign = FALSE)
        #   # stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = "2021-01-15", auto.assign = FALSE)
        #   
        #   
        #   stock_data <- tail(stock_data,6)
        #   
        #   # print(stock_data)
        #   
        #   stock_data <- as.data.frame(stock_data)
        #   
        #   rownames(stock_data) <- 1:6
        #   
        #   l1_day_range <- abs(stock_data[6,2] - stock_data[6,3])
        #   l2_day_range <- abs(stock_data[5,2] - stock_data[5,3])
        #   l3_day_range <- abs(stock_data[4,2] - stock_data[4,3])
        #   l4_day_range <- abs(stock_data[3,2] - stock_data[3,3])
        #   l5_day_range <- abs(stock_data[2,2] - stock_data[2,3])
        #   l6_day_range <- abs(stock_data[1,2] - stock_data[1,3])
        #   
        #   l2_day_high <- stock_data[5,2]
        #   l1_day_high <- stock_data[6,2]
        #   
        #   l2_day_low <- stock_data[5,3]
        #   l1_day_low <- stock_data[6,3]
        #   
        #   if((l1_day_range < l2_day_range) && (l1_day_range < l3_day_range) && (l1_day_range < l4_day_range) && (l1_day_range < l5_day_range) && (l1_day_range < l6_day_range)){
        #    
        #       
        #       final_levels_df[inc,"Stock"] = stock
        #       final_levels_df[inc,"Reds_High"] = l1_day_high
        #       final_levels_df[inc,"Reds_Low"] = l1_day_low
        #       
        #       inc = inc + 1
        # }
        # 
        # }
        
        if(input$nifty_selection == "nifty_50"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/reds_brahmos.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/us_reds_brahmos.csv", sep = ""))
        }else{
          final_levels_df <- read.csv(paste0(getwd(),"/data/nifty_500_reds_brahmos.csv", sep = ""))
        }
        
        # final_levels_df <- read.csv(paste0(getwd(),"/data/reds_brahmos.csv", sep = ""))
        
        final_levels_df <- subset(final_levels_df, select = -c(X) )
        
        if(nrow(final_levels_df) > 0){
          
          # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
          
          # increment = 1
          
          for(i in 1:nrow(final_levels_df)){
            stock = final_levels_df[i,"Stock"]
            print(stock)
            # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
            
            if(input$bot_timeframe == "1m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "2m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "5m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "15m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "4h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1d"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            # else if(input$candle_stick_range == "1y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            # else if(input$candle_stick_range == "5y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            else {
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            
            stock_timestamp <- response_data$chart$result[[1]]$timestamp
            Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
            High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
            Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
            Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
            Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
            
            # browser()
            
            if(nrow(final_data) == 0){
              stock_timestamp <- response_data$chart$result[[2]][[1]]
              Close <- response_data$chart$result[[3]]$quote[[1]]$close
              High <- response_data$chart$result[[3]]$quote[[1]]$high
              Low <- response_data$chart$result[[3]]$quote[[1]]$low
              Open <- response_data$chart$result[[3]]$quote[[1]]$open
              Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
              
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
            }
            
            
            colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
            
            if(typeof(final_data$V1) == "list"){
              final_data <- final_data[-c(which(final_data$Close == "NULL")),]
              new_stock_timestamp <- unlist(final_data$V1)
              Close <- unlist(final_data$Close)
              High <- unlist(final_data$High)
              Open <- unlist(final_data$Open)
              Low <- unlist(final_data$Low)
              Volume <- unlist(final_data$Volume)
              
              final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
              
              final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            else{
              final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            final_data <- na.omit(final_data)
            
            # final_data <- final_data %>% 
            #   select(dates, Open, High, Low, Close, Volume) %>%
            #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
            #   mutate(
            #     sma_35 = TTR::SMA(Close, 35),
            #     rsi = TTR::RSI(Close, 14),
            #     vwap = TTR::VWAP(Close,Volume,10)
            #   )
            # 
            # 
            # today = Sys.Date()
            # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
            # currentWorkingDay <- f(today)
            
            
            
            if(weekdays(Sys.Date()) == "Sunday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
            }else{
              if(hour(Sys.time()) >= 9){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
              }else{
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
              }
              
            }
            
            print(final_data)
            
            final_data$Call <- ""
            
            satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
            
            for(j in 1:nrow(final_data)){
              
              
              if((final_data[j,"Close"]) > final_levels_df[i,"Reds_High"]){
                # print(satisfied_df)
                satisfied_df = rbind(satisfied_df,final_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                # print("Buy")
                # print(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
              }
              else if((final_data[j,"Close"]) < final_levels_df[i,"Reds_Low"]){
                
                satisfied_df = rbind(satisfied_df,final_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                # print("Sell")
                # print(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
              }
              else{
                next
              }
            }
            # print(nrow(satisfied_df))
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              satisfied_df = head(satisfied_df,1)
              # print(satisfied_df)
              rownames(satisfied_df) <- 1
              
              time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
              # print(time_min)
              
              if(time_min <= "15:10:00"){
              Signal_df[increment,"Strategy"] <- "Reds Brahmos"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
              # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
              # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
              
              increment = increment + 1
              }else{
                next
              }
            }
            
          }
        }
        
        
      }
      else if(input$bot_strategy[i] == "blackout"){
        # final_levels_df = data.frame("Stock" = character(0),"target" = numeric(0),"stage" = character(0))
        # 
        # inc = 1
        # 
        # for(i in 1:nrow(nse_data)){
        #   stock = nse_data[i,2]
        #   # print(stock)
        #   today = Sys.Date()
        #   f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
        #   previousWorkingDay <- f(today) - 1
        #   
        #   stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = previousWorkingDay, auto.assign = FALSE)
        #   # stock_data <- getSymbols(stock, src = "yahoo", from = "2021-01-01", to = "2021-01-15", auto.assign = FALSE)
        #   
        #   
        #   stock_data <- tail(stock_data,4)
        #   
        #   # print(stock_data)
        #   
        #   stock_data <- as.data.frame(stock_data)
        #   
        #   rownames(stock_data) <- 1:4
        #   
        #   l1_low <- stock_data[4,3]
        #   l2_low <- stock_data[3,3]
        #   l3_low <- stock_data[2,3]
        #   l4_low <- stock_data[1,3]
        #   
        #   l1_high <- stock_data[4,2]
        #   l2_high <- stock_data[3,2]
        #   l3_high <- stock_data[2,2]
        #   l4_high <- stock_data[1,2]
        #   
        #   if((l1_low > l2_low) && (l1_high > l2_high) && (l2_low > l3_low) && (l2_high > l3_high) && (l3_low > l4_low) && (l3_high > l4_high)){
        #     l1_open <- stock_data[4,1]
        #     l1_close <- stock_data[4,4]
        #     real_body <- abs(l1_open - l1_close)
        #     body_high <- max(l1_open,l1_close)
        #     if((l1_high - body_high) > 2*(real_body)){
        #       final_levels_df[inc,"Stock"] = stock
        #       final_levels_df[inc,"target"] = l1_low
        #       final_levels_df[inc,"stage"] = "Short"
        #       
        #       inc = inc + 1
        #     }
        #     
        #     
        #   }
        #   else if((l1_low < l2_low) && (l1_high < l2_high) && (l2_low < l3_low) && (l2_high < l3_high) && (l3_low < l4_low) && (l3_high < l4_high)){
        #     l1_open <- stock_data[4,1]
        #     l1_close <- stock_data[4,4]
        #     real_body <- abs(l1_open - l1_close)
        #     body_low <- min(l1_open,l1_close)
        #     if((l1_low - body_low) > 2*(real_body)){
        #       final_levels_df[inc,"Stock"] = stock
        #       final_levels_df[inc,"target"] = l1_high
        #       final_levels_df[inc,"stage"] = "Long"
        #       inc = inc + 1
        #     }
        #     
        #     
        #   }
        #   else{
        #     next
        #   }
        # }
        
        if(input$nifty_selection == "nifty_50"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/blackout.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/us_blackout.csv", sep = ""))
        }else{
          final_levels_df <- read.csv(paste0(getwd(),"/data/nifty_500_blackout.csv", sep = ""))
        }
        
        # final_levels_df <- read.csv(paste0(getwd(),"/data/blackout.csv", sep = ""))
        
        final_levels_df <- subset(final_levels_df, select = -c(X))
        
        if(nrow(final_levels_df) > 0){
          # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
          
          # increment = 1
          
          if(nrow(final_levels_df) > 0){
            
            for(i in 1:nrow(final_levels_df)){
              stock = final_levels_df[i,"Stock"]
              print(stock)
              # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
              
              if(input$bot_timeframe == "1m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "2m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "5m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "15m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "1h"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "4h"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "1d"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              # else if(input$candle_stick_range == "1y"){
              #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              # }
              # else if(input$candle_stick_range == "5y"){
              #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              # }
              else {
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              
              stock_timestamp <- response_data$chart$result[[1]]$timestamp
              Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
              High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
              Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
              Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
              Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
              
              # browser()
              
              if(nrow(final_data) == 0){
                stock_timestamp <- response_data$chart$result[[2]][[1]]
                Close <- response_data$chart$result[[3]]$quote[[1]]$close
                High <- response_data$chart$result[[3]]$quote[[1]]$high
                Low <- response_data$chart$result[[3]]$quote[[1]]$low
                Open <- response_data$chart$result[[3]]$quote[[1]]$open
                Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
                
                
                final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
              }
              
              
              colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
              
              if(typeof(final_data$V1) == "list"){
                final_data <- final_data[-c(which(final_data$Close == "NULL")),]
                new_stock_timestamp <- unlist(final_data$V1)
                Close <- unlist(final_data$Close)
                High <- unlist(final_data$High)
                Open <- unlist(final_data$Open)
                Low <- unlist(final_data$Low)
                Volume <- unlist(final_data$Volume)
                
                final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
                
                final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
                
                final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
              }
              else{
                final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
                
                final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
              }
              final_data <- na.omit(final_data)
              
              # final_data <- final_data %>% 
              #   select(dates, Open, High, Low, Close, Volume) %>%
              #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
              #   mutate(
              #     sma_35 = TTR::SMA(Close, 35),
              #     rsi = TTR::RSI(Close, 14),
              #     vwap = TTR::VWAP(Close,Volume,10)
              #   )
              # 
              # 
              # today = Sys.Date()
              # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
              # currentWorkingDay <- f(today)
              
              
              
              if(weekdays(Sys.Date()) == "Sunday"){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
              }else if(weekdays(Sys.Date()) == "Saturday"){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
              }else{
                if(hour(Sys.time()) >= 9){
                  final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
                }else{
                  final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
                }
                
              }
              # 
              # print(final_data)
              
              
              final_data$Call <- ""
              
              
              
              satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
              
              if(final_levels_df[i,"stage"] == "Short"){
                for(j in 1:nrow(final_data)){
                  if((final_data[j,"Close"]) < final_levels_df[i,"target"]){
                    satisfied_df = rbind(satisfied_df,final_data[j,])
                    rownames(satisfied_df) <- 1:nrow(satisfied_df)
                    satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
                  }
                  
                }
              }
              else{
                for(j in 1:nrow(final_data)){
                  if((final_data[j,"Close"]) > final_levels_df[i,"target"]){
                    satisfied_df = rbind(satisfied_df,final_data[j,])
                    rownames(satisfied_df) <- 1:nrow(satisfied_df)
                    satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
                  }
                  
                }
              }
              # print(satisfied_df)
              
              if(nrow(satisfied_df) == 0){
                next
              }
              else{
                satisfied_df = head(satisfied_df,1)
                # print(satisfied_df)
                rownames(satisfied_df) <- 1
                
                time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
                # print(time_min)
                
                if(time_min <= "15:10:00"){
                Signal_df[increment,"Strategy"] <- "Blackout"
                Signal_df[increment,"Stock"]=stock
                Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
                Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
                Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
                # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
                # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
                # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
                
                increment = increment + 1
                }else{
                  next
                }
              }
            }
            
          }
        }
        
      }
      else if(input$bot_strategy[i] == "volume_breakout"){
        
        if(input$nifty_selection == "nifty_50"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/gaps_strategy.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/us_gaps_strategy.csv", sep = ""))
        }else{
          final_levels_df <- read.csv(paste0(getwd(),"/data/nifty_500_gaps_strategy.csv", sep = ""))
        }
        
        final_levels_df <- subset(final_levels_df, select = -c(X))
        
        
        
        if(nrow(final_levels_df) > 0){
          
          for(i in 1:nrow(final_levels_df)){
            
            # browser()
            
            stock = final_levels_df[i,"Stock"]
            
            
            if(input$bot_timeframe == "1m"){
              
              if(stock == "%5ENSEBANK"){
                
                #### Bank Nifty Sep Futures data
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57918",
                                                 interval = "ONE_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else if(stock == "%5ENSEI"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57919",
                                                 interval = "ONE_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else{
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              
              
            }
            else if(input$bot_timeframe == "2m"){
              if(stock == "%5ENSEBANK"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57918",
                                                 interval = "THREE_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else if(stock == "%5ENSEI"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57919",
                                                 interval = "THREE_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else{
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            }
            else if(input$bot_timeframe == "5m"){
              if(stock == "%5ENSEBANK"){
                
                # browser()
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57918",
                                                 interval = "FIVE_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else if(stock == "%5ENSEI"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57919",
                                                 interval = "FIVE_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else{
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            }
            else if(input$bot_timeframe == "15m"){
              if(stock == "%5ENSEBANK"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57918",
                                                 interval = "FIFTEEN_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else if(stock == "%5ENSEI"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57919",
                                                 interval = "FIFTEEN_MINUTE",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else{
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            }
            else if(input$bot_timeframe == "1h"){
              if(stock == "%5ENSEBANK"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57918",
                                                 interval = "ONE_HOUR",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else if(stock == "%5ENSEI"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57919",
                                                 interval = "ONE_HOUR",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else{
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            }
            else if(input$bot_timeframe == "4h"){
              if(stock == "%5ENSEBANK"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57918",
                                                 interval = "ONE_HOUR",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else if(stock == "%5ENSEI"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57919",
                                                 interval = "ONE_HOUR",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else{
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            }
            else if(input$bot_timeframe == "1d"){
              if(stock == "%5ENSEBANK"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57918",
                                                 interval = "ONE_DAY",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else if(stock == "%5ENSEI"){
                
                response_data <- get_candle_data(object = session_data,
                                                 exchange = "NFO",
                                                 symboltoken = "57919",
                                                 interval = "ONE_DAY",
                                                 fromdate = paste(Sys.Date()," 09:00",sep=""),
                                                 todate = paste(Sys.Date()," 15:30",sep=""))
                
              }else{
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            }
            
            
            
            
            
          
          
          if(stock %in% c('%5ENSEBANK','%5ENSEI')){
            
            
            final_data <- data.frame(matrix(unlist(response_data), nrow=length(response_data), byrow=TRUE),stringsAsFactors=FALSE)
            colnames(final_data) <- c("dates","Open","High","Low","Close","Volume")
            
            
            final_data <- as.data.frame(cbind(anytime::anytime(final_data$dates),as.numeric(unlist(final_data$Close)),as.numeric(unlist(final_data$High)),as.numeric(unlist(final_data$Low)),as.numeric(unlist(final_data$Open)),as.numeric(unlist(final_data$Volume))))
            
            colnames(final_data) <- c("dates","Close","High","Low","Open","Volume")
            
            # final_data$dates <- anytime::anytime(final_data$V1)
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            
            final_data['price_change'] <- abs(final_data["Low"] - final_data["High"])
            
            final_data <- na.omit(final_data)
            
            for (i in 1:nrow(final_data)) {
              if(as.numeric(final_data[i,"Close"]) > as.numeric(final_data[i,"Open"])){
                final_data[i,'perc_change'] <- round(final_data[i,'price_change']*1.00/final_data[i,'Low']*100,2)
              }else{
                final_data[i,'perc_change'] <- round(final_data[i,'price_change']*1.00/final_data[i,'High']*100,2)
              }
              
            }
            
            satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
            
            final_data <- final_data %>% filter(hour( anytime::anytime(final_data$dates)) < 15)
            
            final_data$VolumeRank <-  rank(-final_data$Volume,ties.method= "first")
            
            final_data <-final_data[order(final_data$VolumeRank),]
            
            breakout_high_value <- final_data[final_data$VolumeRank == 1,]$High
            breakout_low_value <- final_data[final_data$VolumeRank == 1,]$Low
            
            breakout_time <- final_data[final_data$VolumeRank == 1,]$dates
            
            temp_final_data <- final_data[final_data$dates > breakout_time,]
            
            rownames(temp_final_data) <- NULL
            
            for (rw in 1:nrow(temp_final_data)) {
              if(temp_final_data[rw,"Close"] > breakout_high_value && abs(as.numeric(as.character(temp_final_data[rw,]$Close)) - breakout_high_value)/breakout_high_value*100 <= 0.4){
                temp_final_data[rw,"Signal"] <- "Buy"
              }else if(temp_final_data[rw,"Close"] < breakout_low_value && abs(as.numeric(as.character(temp_final_data[rw,]$Close)) - breakout_low_value)/breakout_low_value*100 <= 0.4){
                temp_final_data[rw,"Signal"] <- "Sell"
              }else{
                temp_final_data[rw,"Signal"] <- ""
              }
              
            }
            
            temp_final_data <- temp_final_data[temp_final_data$VolumeRank <= 15,]
            
            temp_final_data <- temp_final_data[order(temp_final_data$dates),]
            
            as.numeric(temp_final_data$dates)
            
            temp_final_data <- temp_final_data %>%
              mutate(Flag = (Signal == lag(Signal, default = first(Signal))))
            
            final_temp_volume_df <- head(filter(temp_final_data,Flag = TRUE, Signal !=""),1)
            
            final_temp_volume_df <- rbind(final_temp_volume_df,filter(temp_final_data,Flag == FALSE & Signal != ""))
            
            # final_temp_volume_df <- filter(temp_final_data,Flag == FALSE & Signal != "")
            
            
            # browser()
            
            if(nrow(final_temp_volume_df) >0 ){
              
              for (indx in 1:nrow(final_temp_volume_df)) {
                
                time_min = format(as_datetime(as.character(anytime::anytime(final_temp_volume_df[indx,"dates"])),tz="Asia/Kolkata"), format="%H:%M:%S")
                
                if(time_min <= "15:10:00"){
                  Signal_df[increment,"Strategy"] <- "Volume_Breakout"
                  Signal_df[increment,"Stock"]=stock
                  Signal_df[increment,"Signal"]=final_temp_volume_df[indx,"Signal"]
                  Signal_df[increment,"Datetime"]=final_temp_volume_df[indx,"dates"]
                  Signal_df[increment,"Value"]=final_temp_volume_df[indx,"Close"]
                  
                  increment = increment + 1
                }else{
                  next
                }
                
              }
              
            }
            
          }else{
            
            
            
            stock_timestamp <- response_data$chart$result[[1]]$timestamp
            Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
            High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
            Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
            Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
            Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
            
            # browser()
            
            if(nrow(final_data) == 0){
              stock_timestamp <- response_data$chart$result[[2]][[1]]
              Close <- response_data$chart$result[[3]]$quote[[1]]$close
              High <- response_data$chart$result[[3]]$quote[[1]]$high
              Low <- response_data$chart$result[[3]]$quote[[1]]$low
              Open <- response_data$chart$result[[3]]$quote[[1]]$open
              Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
              
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
            }
            
            
            colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
            
            if(typeof(final_data$V1) == "list"){
              final_data <- final_data[-c(which(final_data$Close == "NULL")),]
              new_stock_timestamp <- unlist(final_data$V1)
              Close <- unlist(final_data$Close)
              High <- unlist(final_data$High)
              Open <- unlist(final_data$Open)
              Low <- unlist(final_data$Low)
              Volume <- unlist(final_data$Volume)
              
              final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
              
              final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            else{
              final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            
            # print(final_data)
            
            final_data <- na.omit(final_data)
            
            
            
            if(weekdays(Sys.Date()) == "Sunday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
            }else{
              if(hour(Sys.time()) >= 9){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
              }else{
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
              }
              
            }
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            
            final_data['price_change'] <- abs(final_data["Low"] - final_data["High"])
            
            
            for (i in 1:nrow(final_data)) {
              if(as.numeric(final_data[i,"Close"]) > as.numeric(final_data[i,"Open"])){
                final_data[i,'perc_change'] <- round(final_data[i,'price_change']*1.00/final_data[i,'Low']*100,2)
              }else{
                final_data[i,'perc_change'] <- round(final_data[i,'price_change']*1.00/final_data[i,'High']*100,2)
              }
              
            }
            
            satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
            
            final_data <- final_data %>% filter(hour( anytime::anytime(final_data$dates)) < 15)
            
            final_data$VolumeRank <-  rank(-final_data$Volume,ties.method= "first")
            
            final_data <-final_data[order(final_data$VolumeRank),]
            
            breakout_high_value <- final_data[final_data$VolumeRank == 1,]$High
            breakout_low_value <- final_data[final_data$VolumeRank == 1,]$Low
            
            breakout_time <- final_data[final_data$VolumeRank == 1,]$dates
            
            temp_final_data <- final_data[final_data$dates > breakout_time,]
            
            rownames(temp_final_data) <- NULL
            
            if(nrow(temp_final_data) > 0){
            
            for (rw in 1:nrow(temp_final_data)) {
              if(temp_final_data[rw,"Close"] > breakout_high_value && abs(as.numeric(as.character(temp_final_data[rw,]$Close)) - breakout_high_value)/breakout_high_value*100 <= 0.4){
                temp_final_data[rw,"Signal"] <- "Buy"
              }else if(temp_final_data[rw,"Close"] < breakout_low_value && abs(as.numeric(as.character(temp_final_data[rw,]$Close)) - breakout_low_value)/breakout_low_value*100 <= 0.4){
                temp_final_data[rw,"Signal"] <- "Sell"
              }else{
                temp_final_data[rw,"Signal"] <- ""
              }
              
            }
            
            temp_final_data <- temp_final_data[temp_final_data$VolumeRank <= 15,]
            
            temp_final_data <- temp_final_data[order(temp_final_data$dates),]
            
            as.numeric(temp_final_data$dates)
            
            temp_final_data <- temp_final_data %>%
              mutate(Flag = (Signal == lag(Signal, default = first(Signal))))
            
            final_temp_volume_df <- head(filter(temp_final_data,Flag = TRUE, Signal !=""),1)
            
            final_temp_volume_df <- rbind(final_temp_volume_df,filter(temp_final_data,Flag == FALSE & Signal != ""))
            
            # final_temp_volume_df <- filter(temp_final_data,Flag == FALSE & Signal != "")
            
            
            # browser()
            
            if(nrow(final_temp_volume_df) >0 ){
              
              for (indx in 1:nrow(final_temp_volume_df)) {
                
                time_min = format(as_datetime(as.character(anytime::anytime(final_temp_volume_df[indx,"dates"])),tz="Asia/Kolkata"), format="%H:%M:%S")
                
                if(time_min <= "15:10:00"){
                  Signal_df[increment,"Strategy"] <- "Volume_Breakout"
                  Signal_df[increment,"Stock"]=stock
                  Signal_df[increment,"Signal"]=final_temp_volume_df[indx,"Signal"]
                  Signal_df[increment,"Datetime"]=final_temp_volume_df[indx,"dates"]
                  Signal_df[increment,"Value"]=final_temp_volume_df[indx,"Close"]
                  
                  increment = increment + 1
                }else{
                  next
                }
                
              }
              
            }
            
          }
            
            
          }

          }
        }
        
        Signal_df <- Signal_df[!duplicated(Signal_df), ]
        
      }
      else if(input$bot_strategy[i]=="inside_candle"){
        # browser()
        for(i in 1:nrow(nse_data)){
          stock = nse_data[i,2]
          # print(stock)
          
          if(stock %in% c('%5ENSEBANK','%5ENSEI')){
            print("Pass")
          }
          else{
            # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
            
            if(input$bot_timeframe == "1m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "2m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "5m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "15m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "4h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1d"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            # else if(input$candle_stick_range == "1y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            # else if(input$candle_stick_range == "5y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            else {
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            
            
            stock_timestamp <- response_data$chart$result[[1]]$timestamp
            Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
            High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
            Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
            Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
            Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
            
            # browser()
            
            if(nrow(final_data) == 0){
              stock_timestamp <- response_data$chart$result[[2]][[1]]
              Close <- response_data$chart$result[[3]]$quote[[1]]$close
              High <- response_data$chart$result[[3]]$quote[[1]]$high
              Low <- response_data$chart$result[[3]]$quote[[1]]$low
              Open <- response_data$chart$result[[3]]$quote[[1]]$open
              Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
              
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
            }
            
            
            colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
            
            if(typeof(final_data$V1) == "list"){
              final_data <- final_data[-c(which(final_data$Close == "NULL")),]
              new_stock_timestamp <- unlist(final_data$V1)
              Close <- unlist(final_data$Close)
              High <- unlist(final_data$High)
              Open <- unlist(final_data$Open)
              Low <- unlist(final_data$Low)
              Volume <- unlist(final_data$Volume)
              
              final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
              
              final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            else{
              final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            final_data <- na.omit(final_data)
            
            
            # final_data <- final_data %>% 
            #   select(dates, Open, High, Low, Close, Volume) %>%
            #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
            #   mutate(
            #     sma_35 = TTR::SMA(Close, 35),
            #     rsi = TTR::RSI(Close, 14),
            #     vwap = TTR::VWAP(Close,Volume,10)
            #   )
            # 
            # 
            # today = Sys.Date()
            # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
            # currentWorkingDay <- f(today)
            # 
            # 
            # 
            # final_data <- final_data[as.Date(final_data$dates) == currentWorkingDay,]
            
            # browser()
            
            if(weekdays(Sys.Date()) == "Sunday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 4 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 3 ,]
            }else{
              if(hour(Sys.time()) >= 9){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
              }else{
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
              }
              
            }
            
            trigger_price = 0
            stage = ""
            
            
            final_data <- mutate(final_data, last_candle_range = lag(Close) - lag(Open), current_range = Close - Open, previous_high = lag(High), previous_low = lag(Low))
            
            satisfied_df = data.frame()
            
            for(j in 3:nrow(final_data)){
              if(abs(final_data[j-1,"current_range"]) < abs(final_data[j-1,"last_candle_range"]) && final_data[j-1,"High"] < final_data[j-1,"previous_high"]  && final_data[j-1,"Low"] > final_data[j-1,"previous_low"]){
                
                if(final_data[j,"Close"] > final_data[j-1,"High"]){
                  
                  temp_data <- final_data[j,]
                  temp_data$Call <- "Buy"
                  # browser()
                  satisfied_df = rbind(satisfied_df,temp_data)
                  rownames(satisfied_df) <- 1:nrow(satisfied_df)
                  satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
                  
                }
                else if(final_data[j,"Close"] < final_data[j-1,"Low"]){
                  temp_data <- final_data[j,]
                  temp_data$Call <- "Sell"
                  # browser()
                  satisfied_df = rbind(satisfied_df,temp_data)
                  rownames(satisfied_df) <- 1:nrow(satisfied_df)
                  satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
                  
                }
                
              }
              
            }
            
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              satisfied_df = head(satisfied_df,1)
              # print(satisfied_df)
              rownames(satisfied_df) <- 1
              
              time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
              # print(time_min)
              # browser()
              if(time_min <= "15:10:00"){
                Signal_df[increment,"Strategy"] <- "Insider_Candle"
                Signal_df[increment,"Stock"]=stock
                Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
                Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
                Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
                # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
                # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
                # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
                
                increment = increment + 1
              }else{
                next
              }
            }
          }
          
          
          
        }
      }
      else if(input$bot_strategy[i] == "gap_up"){
        
        if(input$nifty_selection == "nifty_50"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/gaps_strategy.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/us_gaps_strategy.csv", sep = ""))
        }else{
          final_levels_df <- read.csv(paste0(getwd(),"/data/nifty_500_gaps_strategy.csv", sep = ""))
        }
        
        # final_levels_df <- read.csv(paste0(getwd(),"/data/gaps_strategy.csv", sep = ""))
        
        final_levels_df <- subset(final_levels_df, select = -c(X))
        
        if(nrow(final_levels_df) > 0){
          # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
          
          # increment = 1
          
          # if(nrow(final_levels_df) > 0){
            
          # browser()
            
            for(i in 1:nrow(final_levels_df)){
              
              # browser()
              
              stock = final_levels_df[i,"Stock"]
              high_price = final_levels_df[i,"Previous_High"]
              close_price = final_levels_df[i,"Previous_Close"]
              # print(stock)
              # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
              
              if(input$bot_timeframe == "1m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "2m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "5m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "15m"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "1h"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "4h"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              else if(input$bot_timeframe == "1d"){
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              # else if(input$candle_stick_range == "1y"){
              #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              # }
              # else if(input$candle_stick_range == "5y"){
              #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              # }
              else {
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
              
              stock_timestamp <- response_data$chart$result[[1]]$timestamp
              Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
              High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
              Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
              Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
              Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
              
              # browser()
              
              if(nrow(final_data) == 0){
                stock_timestamp <- response_data$chart$result[[2]][[1]]
                Close <- response_data$chart$result[[3]]$quote[[1]]$close
                High <- response_data$chart$result[[3]]$quote[[1]]$high
                Low <- response_data$chart$result[[3]]$quote[[1]]$low
                Open <- response_data$chart$result[[3]]$quote[[1]]$open
                Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
                
                
                final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
              }
              
              
              colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
              
              if(typeof(final_data$V1) == "list"){
                final_data <- final_data[-c(which(final_data$Close == "NULL")),]
                new_stock_timestamp <- unlist(final_data$V1)
                Close <- unlist(final_data$Close)
                High <- unlist(final_data$High)
                Open <- unlist(final_data$Open)
                Low <- unlist(final_data$Low)
                Volume <- unlist(final_data$Volume)
                
                final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
                
                final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
                
                final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
              }
              else{
                final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
                
                final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
              }
              
              # print(final_data)
              
              final_data <- na.omit(final_data)
              
              
              # final_data <- final_data %>% 
              #   select(dates, Open, High, Low, Close, Volume) %>%
              #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
              #   mutate(
              #     sma_35 = TTR::SMA(Close, 35),
              #     rsi = TTR::RSI(Close, 14),
              #     vwap = TTR::VWAP(Close,Volume,10)
              #   )
              # 
              # 
              # today = Sys.Date()
              # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
              # currentWorkingDay <- f(today)
              
              
              
              if(weekdays(Sys.Date()) == "Sunday"){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
              }else if(weekdays(Sys.Date()) == "Saturday"){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
              }else{
                if(hour(Sys.time()) >= 9){
                  final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
                }else{
                  final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
                }
                
              }
              
              final_data$Call <- ""
              
              
              
              satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
              
              open_price = final_data[1,2]
              
              # browser()
              if(open_price > final_levels_df[i,"Previous_Close"]){
                print(stock)
                for(j in 5:nrow(final_data)){
                  # if(stock == "UPL.NS"){
                  
                  current_date <- final_data[j,"dates"]
                  # print(current_date)
                  
                  # day_high <- max(final_data$Close)
                  # day_low <- min(final_data$Low)
                  
                  day_high <- max(final_data[1:j,]$Close,final_data[1:j,]$Open)
                  day_low <- min(final_data[1:j,]$Close,final_data[1:j,]$Open)
                  
                  low_range <- min(final_data[j-1,4],final_data[j-2,4],final_data[j-3,4],final_data[j-4,4])
                  high_range <- max(final_data[j-1,3],final_data[j-2,3],final_data[j-3,3],final_data[j-4,3])
                  current_close <- final_data[j,"Close"]
                  if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close >= high_price) && (current_close >= day_high)){
                    satisfied_df = rbind(satisfied_df,final_data[j,])
                    rownames(satisfied_df) <- 1:nrow(satisfied_df)
                    satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
                  }
                  else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
                  # else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price)){
                    # print(current_date)
                    satisfied_df = rbind(satisfied_df,final_data[j,])
                    rownames(satisfied_df) <- 1:nrow(satisfied_df)
                    satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
                  }else{
                    next
                  }
                  
                  
                }
              }
              
              if(nrow(satisfied_df) == 0){
                next
              }
              else{
                satisfied_df = head(satisfied_df,1)
                # print(satisfied_df)
                rownames(satisfied_df) <- 1
                
                time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
                # print(time_min)
                
                if(time_min <= "15:10:00"){
                Signal_df[increment,"Strategy"] <- "Gap_up"
                Signal_df[increment,"Stock"]=stock
                Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
                Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
                Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
                # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
                # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
                # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
                
                increment = increment + 1
                }
                else{
                  next
                }
              }
              
              
            }
            
          # }
        }
        
      }
      else if(input$bot_strategy[i] == "gap_down"){
        
        if(input$nifty_selection == "nifty_50"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/gaps_strategy.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          final_levels_df <- read.csv(paste0(getwd(),"/data/us_gaps_strategy.csv", sep = ""))
        }else{
          final_levels_df <- read.csv(paste0(getwd(),"/data/nifty_500_gaps_strategy.csv", sep = ""))
        }
        # final_levels_df <- read.csv(paste0(getwd(),"/data/gaps_strategy.csv", sep = ""))
        
        final_levels_df <- subset(final_levels_df, select = -c(X))
        
        if(nrow(final_levels_df) > 0){
          # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
          
          # increment = 1
          
          # if(nrow(final_levels_df) > 0){
          
          
          
          for(i in 1:nrow(final_levels_df)){
            
            # browser()
            
            stock = final_levels_df[i,"Stock"]
            high_price = final_levels_df[i,"Previous_High"]
            close_price = final_levels_df[i,"Previous_Close"]
            prev_low_price = final_levels_df[i,"Previous_Low"]
            # print(stock)
            # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
            
            if(input$bot_timeframe == "1m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "2m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "5m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "15m"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "4h"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            else if(input$bot_timeframe == "1d"){
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            # else if(input$candle_stick_range == "1y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            # else if(input$candle_stick_range == "5y"){
            #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            # }
            else {
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
            
            # 
            stock_timestamp <- response_data$chart$result[[1]]$timestamp
            Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
            High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
            Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
            Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
            Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
            
            # browser()
            
            if(nrow(final_data) == 0){
              stock_timestamp <- response_data$chart$result[[2]][[1]]
              Close <- response_data$chart$result[[3]]$quote[[1]]$close
              High <- response_data$chart$result[[3]]$quote[[1]]$high
              Low <- response_data$chart$result[[3]]$quote[[1]]$low
              Open <- response_data$chart$result[[3]]$quote[[1]]$open
              Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
              
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
            }
            
            
            colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
            
            if(typeof(final_data$V1) == "list"){
              final_data <- final_data[-c(which(final_data$Close == "NULL")),]
              new_stock_timestamp <- unlist(final_data$V1)
              Close <- unlist(final_data$Close)
              High <- unlist(final_data$High)
              Open <- unlist(final_data$Open)
              Low <- unlist(final_data$Low)
              Volume <- unlist(final_data$Volume)
              
              final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
              
              final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            else{
              final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
              
              final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
            }
            # print(final_data)
            
            final_data <- na.omit(final_data)
            
            
            # final_data <- final_data %>% 
            #   select(dates, Open, High, Low, Close, Volume) %>%
            #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
            #   mutate(
            #     sma_35 = TTR::SMA(Close, 35),
            #     rsi = TTR::RSI(Close, 14),
            #     vwap = TTR::VWAP(Close,Volume,10)
            #   )
            # 
            # 
            # today = Sys.Date()
            # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
            # currentWorkingDay <- f(today)
            
            
            
            if(weekdays(Sys.Date()) == "Sunday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
            }else{
              if(hour(Sys.time()) >= 9){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
              }else{
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
              }
              
            }
            
            
            final_data$Call <- ""
            
            
            
            satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
            
            open_price = final_data[1,2]
            
            
            if(open_price < final_levels_df[i,"Previous_Close"]){
              print(stock)
              for(j in 5:nrow(final_data)){
                # if(stock == "BAJAJ-AUTO.NS"){
                
                current_date <- final_data[j,"dates"]
                # print(current_date)
                
                # day_close <- max(final_data$Close)
                # day_low <- min(final_data$Low)
                
                day_high <- max(final_data[1:j,]$Close,final_data[1:j,]$Open)
                day_low <- min(final_data[1:j,]$Close,final_data[1:j,]$Open)
                
                low_range <- min(final_data[j-1,4],final_data[j-2,4],final_data[j-3,4],final_data[j-4,4])
                high_range <- max(final_data[j-1,3],final_data[j-2,3],final_data[j-3,3],final_data[j-4,3])
                current_close <- final_data[j,"Close"]
                current_high <- final_data[j,"High"]
                current_low <- final_data[j,"Low"]
                # browser()
                if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close >= high_price) && (current_high >= day_high)){
                  satisfied_df = rbind(satisfied_df,final_data[j,])
                  rownames(satisfied_df) <- 1:nrow(satisfied_df)
                  satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
                }
                # else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
                else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= prev_low_price) && (current_low <= day_low)){
                  # print(current_date)
                  satisfied_df = rbind(satisfied_df,final_data[j,])
                  rownames(satisfied_df) <- 1:nrow(satisfied_df)
                  satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
                }else{
                  next
                }
                
                
              # }
            }
            
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              satisfied_df = head(satisfied_df,1)
              # print(satisfied_df)
              rownames(satisfied_df) <- 1
              
              time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
              # print(time_min)
              # browser()
              if(time_min <= "15:10:00"){
              Signal_df[increment,"Strategy"] <- "Gap_down"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
              # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
              # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
              increment = increment + 1
              }else{
                next
              }
            }
            
            
          }
          
          }
        }
        
        
        
      }
      
      else if(input$bot_strategy[i] == "abc_5_cand"){
        
        # browser()
        if(input$nifty_selection == "nifty_50"){
          nifty_50_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          nifty_50_data <- read.csv(paste0(getwd(),"/data/US - 30 Stocks.csv", sep = ""))
        }else{
          nifty_50_data <- read.csv(paste0(getwd(),"/data/ind_nifty500list.csv", sep = ""))
        }
        
        
        for(ind in 1:nrow(nifty_50_data)){
          
          stock = nifty_50_data[ind,2]
          
          print(stock)
          
          # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
          
          if(input$bot_timeframe == "1m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "2m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "5m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "15m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "4h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1d"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          # else if(input$candle_stick_range == "1y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          # else if(input$candle_stick_range == "5y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          else {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          
          stock_timestamp <- response_data$chart$result[[1]]$timestamp
          Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
          High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
          Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
          Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
          Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
          
          # browser()
          
          if(nrow(final_data) == 0){
            stock_timestamp <- response_data$chart$result[[2]][[1]]
            Close <- response_data$chart$result[[3]]$quote[[1]]$close
            High <- response_data$chart$result[[3]]$quote[[1]]$high
            Low <- response_data$chart$result[[3]]$quote[[1]]$low
            Open <- response_data$chart$result[[3]]$quote[[1]]$open
            Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
            
            
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          }
          
          
          colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
          
          if(typeof(final_data$V1) == "list"){
            final_data <- final_data[-c(which(final_data$Close == "NULL")),]
            new_stock_timestamp <- unlist(final_data$V1)
            Close <- unlist(final_data$Close)
            High <- unlist(final_data$High)
            Open <- unlist(final_data$Open)
            Low <- unlist(final_data$Low)
            Volume <- unlist(final_data$Volume)
            
            final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
            
            final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          else{
            final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          
          final_data <- na.omit(final_data)
          
          
          # final_data <- final_data %>% 
          #   select(dates, Open, High, Low, Close, Volume) %>%
          #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
          #   mutate(
          #     sma_35 = TTR::SMA(Close, 35),
          #     rsi = TTR::RSI(Close, 14),
          #     vwap = TTR::VWAP(Close,Volume,10)
          #   )
          # 
          # 
          # today = Sys.Date()
          # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
          # currentWorkingDay <- f(today)
          
          
          
          if(weekdays(Sys.Date()) == "Sunday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
          }else if(weekdays(Sys.Date()) == "Saturday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
          }else{
            if(hour(Sys.time()) >= 9){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
            }else{
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
            }
            
          }
          
          final_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          final_data
          
          # Starting with the third candle
          for(i in 6:nrow(final_data)){
            
            # Check if the candle is a green candle
            if(final_data[i,"Close"] > final_data[i,"Open"]){
              # Check if the prior candles are in the reversal trend
              if(final_data[i-1,"Low"] < final_data[i-2,"Low"] && final_data[i-2,"Low"] < final_data[i-3,"Low"]){
                
                # Get the breakout max in the reversal i.e., B Point
                reversal_high = max(final_data[i-1,"High"],final_data[i-2,"High"],final_data[i-3,"High"])
                
                # Get the breakout min in the reversal i.e., C point
                reversal_low = min(final_data[i-1,"Low"],final_data[i-2,"Low"],final_data[i-3,"Low"])
                
                # Check if the before reversal is a uptrend
                if(final_data[i-3,"High"] > final_data[i-4,"High"] && final_data[i-4,"High"] > final_data[i-5,"High"]){
                  
                  # Get the starting point of the trend i.e., A point
                  trend_low = min(final_data[i-4,"Low"],final_data[i-5,"Low"])
                  
                  # Check if the ABC pattern is completely followed
                  if(final_data[i,"Close"] > reversal_high && reversal_low > trend_low){
                    
                    satisfied_df = rbind(satisfied_df,final_data[i,])
                    rownames(satisfied_df) <- 1:nrow(satisfied_df)
                    satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
                    
                    print(final_data[i,"dates"])
                  }
                }
              }
              
            }
            else{
              # Check if the prior candles are in the reversal trend
              if(final_data[i-1,"High"] > final_data[i-2,"High"] && final_data[i-2,"High"] > final_data[i-3,"High"]){
                
                # Get the breakout max in the reversal i.e., B Point
                reversal_high = min(final_data[i-1,"Low"],final_data[i-2,"Low"],final_data[i-3,"Low"])
                
                # Get the breakout min in the reversal i.e., C point
                reversal_low = max(final_data[i-1,"High"],final_data[i-2,"High"],final_data[i-3,"High"])
                
                # Check if the before reversal is a uptrend
                if(final_data[i-3,"Low"] < final_data[i-4,"Low"] && final_data[i-4,"Low"] < final_data[i-5,"Low"]){
                  
                  # Get the starting point of the trend i.e., A point
                  trend_low = max(final_data[i-4,"High"],final_data[i-5,"High"])
                  
                  # Check if the ABC pattern is completely followed
                  if(final_data[i,"Close"] < reversal_high && reversal_low < trend_low){
                    # print("reversal")
                    # print(final_data[i,"dates"])
                    satisfied_df = rbind(satisfied_df,final_data[i,])
                    rownames(satisfied_df) <- 1:nrow(satisfied_df)
                    satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
                  }
                }
              }
            }
            
          }
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            # print(satisfied_df)
            rownames(satisfied_df) <- 1
            
            time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
            # print(time_min)
            # browser()
            if(time_min <= "15:10:00"){
              Signal_df[increment,"Strategy"] <- "5_Cand_ABC"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
              # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
              # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
              
              increment = increment + 1
            }else{
              next
            }
          }
          
          
          
        }

      }
      else if(input$bot_strategy[i] == "abc_3_cand"){
        
        # browser()
        if(input$nifty_selection == "nifty_50"){
          nifty_50_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          nifty_50_data <- read.csv(paste0(getwd(),"/data/US - 30 Stocks.csv", sep = ""))
        }else{
          nifty_50_data <- read.csv(paste0(getwd(),"/data/ind_nifty500list.csv", sep = ""))
        }
        
        for(ind in 1:nrow(nifty_50_data)){
          
          stock = nifty_50_data[ind,2]
          
          print(stock)
          
          # response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
          
          if(input$bot_timeframe == "1m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "2m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "5m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "15m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "4h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1d"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          # else if(input$candle_stick_range == "1y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          # else if(input$candle_stick_range == "5y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          else {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          
          stock_timestamp <- response_data$chart$result[[1]]$timestamp
          Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
          High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
          Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
          Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
          Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
          
          # browser()
          
          if(nrow(final_data) == 0){
            stock_timestamp <- response_data$chart$result[[2]][[1]]
            Close <- response_data$chart$result[[3]]$quote[[1]]$close
            High <- response_data$chart$result[[3]]$quote[[1]]$high
            Low <- response_data$chart$result[[3]]$quote[[1]]$low
            Open <- response_data$chart$result[[3]]$quote[[1]]$open
            Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
            
            
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          }
          
          
          colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
          
          if(typeof(final_data$V1) == "list"){
            final_data <- final_data[-c(which(final_data$Close == "NULL")),]
            new_stock_timestamp <- unlist(final_data$V1)
            Close <- unlist(final_data$Close)
            High <- unlist(final_data$High)
            Open <- unlist(final_data$Open)
            Low <- unlist(final_data$Low)
            Volume <- unlist(final_data$Volume)
            
            final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
            
            final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          else{
            final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
            
            final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
          }
          
          final_data <- na.omit(final_data)
          
          # final_data <- final_data %>% 
          #   select(dates, Open, High, Low, Close, Volume) %>%
          #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
          #   mutate(
          #     sma_35 = TTR::SMA(Close, 35),
          #     rsi = TTR::RSI(Close, 14),
          #     vwap = TTR::VWAP(Close,Volume,10)
          #   )
          
          
          # today = Sys.Date()
          # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
          # currentWorkingDay <- f(today)
          # 
          # 
          # 
          if(weekdays(Sys.Date()) == "Sunday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
          }else if(weekdays(Sys.Date()) == "Saturday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
          }else{
            if(hour(Sys.time()) >= 9){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
            }else{
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
            }
            
          }
          
          final_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          final_data
          
          
          for(i in 3:nrow(final_data)){
            
            if((final_data[i,"Close"] > final_data[i,"Open"]) && (final_data[i-1,"Close"] < final_data[i-1,"Open"]) && (final_data[i-2,"Close"] > final_data[i-2,"Open"])){
              if((final_data[i-1,"Low"] > final_data[i-2,"Low"]) && (final_data[i,"Close"] > final_data[i-2,"High"])&& (final_data[i-1,"High"] < final_data[i-2,"High"])){
                first_range = final_data[i-2,"High"] - final_data[i-2,"Low"]
                second_range = final_data[i-1,"High"] - final_data[i-1,"Low"]
                if(first_range/second_range >= 2){
                  satisfied_df = rbind(satisfied_df,final_data[i,])
                  rownames(satisfied_df) <- 1:nrow(satisfied_df)
                  satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
                }
                
              }
              
            }
            else if((final_data[i,"Close"] < final_data[i,"Open"]) && (final_data[i-1,"Close"] > final_data[i-1,"Open"]) && (final_data[i-2,"Close"] < final_data[i-2,"Open"])){
              if((final_data[i-1,"Low"] > final_data[i-2,"Low"]) && (final_data[i,"Close"] < final_data[i-2,"Low"])&& (final_data[i-1,"Low"] > final_data[i-2,"Low"])){
                first_range = final_data[i-2,"High"] - final_data[i-2,"Low"]
                second_range = final_data[i-1,"High"] - final_data[i-1,"Low"]
                if(first_range/second_range >= 2){
                  satisfied_df = rbind(satisfied_df,final_data[i,])
                  rownames(satisfied_df) <- 1:nrow(satisfied_df)
                  satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
                }
                
              }
              
            }
            
          }
          
          
          # # Starting with the third candle
          # for(i in 6:nrow(final_data)){
          #   
          #   # Check if the candle is a green candle
          #   if(final_data[i,"Close"] > final_data[i,"Open"]){
          #     # Check if the prior candles are in the reversal trend
          #     if(final_data[i-1,"Low"] < final_data[i-2,"Low"] && final_data[i-2,"Low"] < final_data[i-3,"Low"]){
          #       
          #       # Get the breakout max in the reversal i.e., B Point
          #       reversal_high = max(final_data[i-1,"High"],final_data[i-2,"High"],final_data[i-3,"High"])
          #       
          #       # Get the breakout min in the reversal i.e., C point
          #       reversal_low = min(final_data[i-1,"Low"],final_data[i-2,"Low"],final_data[i-3,"Low"])
          #       
          #       # Check if the before reversal is a uptrend
          #       if(final_data[i-3,"High"] > final_data[i-4,"High"] && final_data[i-4,"High"] > final_data[i-5,"High"]){
          #         
          #         # Get the starting point of the trend i.e., A point
          #         trend_low = min(final_data[i-4,"Low"],final_data[i-5,"Low"])
          #         
          #         # Check if the ABC pattern is completely followed
          #         if(final_data[i,"Close"] > reversal_high && reversal_low > trend_low){
          #           
          #           satisfied_df = rbind(satisfied_df,final_data[i,])
          #           rownames(satisfied_df) <- 1:nrow(satisfied_df)
          #           satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
          #           
          #           print(final_data[i,"dates"])
          #         }
          #       }
          #     }
          #     
          #   }
          #   else{
          #     # Check if the prior candles are in the reversal trend
          #     if(final_data[i-1,"High"] > final_data[i-2,"High"] && final_data[i-2,"High"] > final_data[i-3,"High"]){
          #       
          #       # Get the breakout max in the reversal i.e., B Point
          #       reversal_high = min(final_data[i-1,"Low"],final_data[i-2,"Low"],final_data[i-3,"Low"])
          #       
          #       # Get the breakout min in the reversal i.e., C point
          #       reversal_low = max(final_data[i-1,"High"],final_data[i-2,"High"],final_data[i-3,"High"])
          #       
          #       # Check if the before reversal is a uptrend
          #       if(final_data[i-3,"Low"] < final_data[i-4,"Low"] && final_data[i-4,"Low"] < final_data[i-5,"Low"]){
          #         
          #         # Get the starting point of the trend i.e., A point
          #         trend_low = max(final_data[i-4,"High"],final_data[i-5,"High"])
          #         
          #         # Check if the ABC pattern is completely followed
          #         if(final_data[i,"Close"] < reversal_high && reversal_low < trend_low){
          #           # print("reversal")
          #           # print(final_data[i,"dates"])
          #           satisfied_df = rbind(satisfied_df,final_data[i,])
          #           rownames(satisfied_df) <- 1:nrow(satisfied_df)
          #           satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
          #         }
          #       }
          #     }
          #   }
          #   
          # }
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            # print(satisfied_df)
            rownames(satisfied_df) <- 1
            
            time_min = format(as_datetime(as.character(as_datetime(satisfied_df[1,"dates"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
            # print(time_min)
            # browser()
            if(time_min <= "15:10:00"){
              Signal_df[increment,"Strategy"] <- "3_Cand_ABC"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
              # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
              # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
              increment = increment + 1
            }else{
              next
            }
          }
          
          
          
        }
        
      }else if(input$bot_strategy[i] == "demand_and_supply"){
        if(input$nifty_selection == "nifty_50"){
          nifty_50_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv", sep = ""))
        }else if(input$nifty_selection == "us_30"){
          nifty_50_data <- read.csv(paste0(getwd(),"/data/US - 30 Stocks.csv", sep = ""))
        }else{
          nifty_50_data <- read.csv(paste0(getwd(),"/data/ind_nifty500list.csv", sep = ""))
        }
        df_supply_and_demand_final = data.frame(stock=character(),pattern=character(),strength=character(),Date=as.POSIXct(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)
        
        
        for(ind in 1:nrow(nifty_50_data)){
          
          stock = nifty_50_data[ind,2]
          
          # if(stock == "TCS.NS"){
          
          # browser()
          
          print(stock)
          
          # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          
          if(input$bot_timeframe == "1m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "2m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "5m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "15m"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&useYfid=true&range=10d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "4h"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&useYfid=true&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          else if(input$bot_timeframe == "1d"){
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          # else if(input$candle_stick_range == "1y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          # else if(input$candle_stick_range == "5y"){
          #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          # }
          else {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
          
          stock_timestamp <- response_data$chart$result[[1]]$timestamp
          Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
          High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
          Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
          Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
          Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
          
          # browser()
          
          if(nrow(final_data) == 0){
            stock_timestamp <- response_data$chart$result[[2]][[1]]
            Close <- response_data$chart$result[[3]]$quote[[1]]$close
            High <- response_data$chart$result[[3]]$quote[[1]]$high
            Low <- response_data$chart$result[[3]]$quote[[1]]$low
            Open <- response_data$chart$result[[3]]$quote[[1]]$open
            Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
            
            
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          }
          
          
          colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
          
          if(typeof(final_data$V1) == "list"){
            final_data <- final_data[-c(which(final_data$Close == "NULL")),]
            new_stock_timestamp <- unlist(final_data$V1)
            Close <- unlist(final_data$Close)
            High <- unlist(final_data$High)
            Open <- unlist(final_data$Open)
            Low <- unlist(final_data$Low)
            Volume <- unlist(final_data$Volume)
            
            final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
            
            final_data$Date <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
            
            final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
          }else{
            final_data$Date <- as.POSIXct(final_data$V1, origin="1970-01-01")
            
            final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
          }
          # browser()
          
          
          
          # final_data <- final_data %>% 
          #   select(dates, Open, High, Low, Close, Volume) %>%
          #   # mutate(dates = as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OS")) %>%
          #   mutate(
          #     sma_35 = TTR::SMA(Close, 35),
          #     rsi = TTR::RSI(Close, 14),
          #     vwap = TTR::VWAP(Close,Volume,10)
          #   )
          # 
          # 
          # today = Sys.Date()
          # f <- function(d)if(format(d - 1, '%w') %in% c(0, 6)) Recall(d - 1) else d - 1
          # currentWorkingDay <- f(today)
          # 
          # 
          # 
          # final_data <- final_data[as.Date(final_data$dates) == currentWorkingDay,]
          # 
          
          if(weekdays(Sys.Date()) == "Sunday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
          }else if(weekdays(Sys.Date()) == "Saturday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
          }else{
            if(hour(Sys.time()) >= 9){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
            }else{
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
            }
            
          }
          
          data <- as.data.frame(final_data)
          
          # print(tail(data,5))
          
          if(nrow(data) >= 3){
            
            df_supply_and_demand = data.frame(stock=character(),pattern=character(),strength=character(0),Date=as.POSIXct(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)
            
            
            # supply_zone_df = supply_zone_detection(df,stock)
            
            supply_zone_df = supply_zone_detection(data,stock,df_supply_and_demand)
            
            df_supply_and_demand_final <- rbind(df_supply_and_demand_final, supply_zone_df)
            
            
            demand_zone_df = demand_zone_detection(data,stock,df_supply_and_demand)
            
            df_supply_and_demand_final <- rbind(df_supply_and_demand_final, demand_zone_df)
            
          }
          
          df_supply_and_demand_final = df_supply_and_demand_final[!rowSums((is.na(df_supply_and_demand_final))),]
          
          
          
          # print(df_supply_and_demand_final)
          
          
          # browser()
          
          temp_data = df_supply_and_demand_final[df_supply_and_demand_final$stock == stock,]
          
          for(index in 1:nrow(temp_data)){
            
           
            
            # if(df_supply_and_demand_final[index,'date_col'] == Sys.Date()){
            if(as.Date(temp_data[index,'Date']) == Sys.Date()-2){
              # print(df_supply_and_demand_final[index,])
              
              if(temp_data[index,'pattern'] == "Supply Continuous Pattern" | temp_data[index,'pattern'] == "Supply Reversal Pattern"){
                Signal_df[increment,"Strategy"] <- "Supply_and_Demand"
                Signal_df[increment,"Stock"]=stock
                Signal_df[increment,"Signal"]="SELL"
                Signal_df[increment,"Datetime"]=temp_data[index,'Date']
                Signal_df[increment,"Value"]=temp_data[index,'zone_1']
                Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
                Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
                Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
                
                increment = increment + 1
                
              }else if(temp_data[index,'pattern'] == "Demand Continuous Pattern" | temp_data[index,'pattern'] == "Demand Reversal Pattern"){
                # Signal_df[increment,"Strategy"] <- "Supply_and_Demand"
                # Signal_df[increment,"Stock"]=stock
                # Signal_df[increment,"Signal"]="BUY"
                # Signal_df[increment,"Datetime"]=df_supply_and_demand_final[index,'Date']
                # Signal_df[increment,"Value"]=df_supply_and_demand_final[index,'zone_1']
                # Signal_df[increment,"StopLoss"]=df_supply_and_demand_final[index,'zone_2']
                # target = abs(df_supply_and_demand_final[index,'zone_2'] - df_supply_and_demand_final[index,'zone_2'])
                # Signal_df[increment,"Target"]= df_supply_and_demand_final[index,'zone_1'] + target
                
                Signal_df[increment,"Strategy"] <- "Supply_and_Demand"
                Signal_df[increment,"Stock"]=stock
                Signal_df[increment,"Signal"]="BUY"
                Signal_df[increment,"Datetime"]=temp_data[index,'Date']
                Signal_df[increment,"Value"]=temp_data[index,'zone_1']
                Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
                Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
                Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
                
                increment = increment + 1
              }
              
            }
          }
          # }
        }
      }
      
      # else{
      #   next
      # }
      
    }
    
    # browser()
    
    if(nrow(Signal_df) == 0){
      # next
    }
    else{
      Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
      
      Signal_df$Datetime <- Signal_df$Datetime + hm("5:30") 
      
      Signal_df <- Signal_df[order(Signal_df$Datetime),]
      
      rownames(Signal_df) <- 1:nrow(Signal_df)
      
      Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
      
      stop_loss <- as.numeric(input$bot_loss)
      target <- as.numeric(input$bot_profit)
      # 
      # Signal_df$RSI <- round(as.numeric(Signal_df$RSI),2)
      # Signal_df$VWAP <- round(as.numeric(Signal_df$VWAP),2)
      # Signal_df$SMA_35 <- round(as.numeric(Signal_df$SMA_35),2)
      # market_direction <- as.numeric(input$market_direction)
      #
      Capital <- input$bot_capital
      # browser()
      Signal_df$StopLoss <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
      Signal_df$Target <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
      
      # Signal_df$Qty <- round(abs((2/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
      Signal_df$Qty <- round(abs((20/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
      
      # qty <- round(market_direction*((2/100)*Capital/(entry_point - stop_loss)),0)

    }
    
    
    
   
   write.csv(Signal_df,paste0(getwd(),"/data/Signal_df.csv", sep = ""))

    DT::datatable(Signal_df,extensions = c('FixedColumns'),selection = "single",
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top',
                                 rowCallback=JS(
                                   'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1)
            $(row).css("background","#D7DBDD")
   }')
                  )) %>% formatStyle(
                    'Signal',
                    backgroundColor = styleEqual(c("Buy", "Sell"), c('green', 'red'))
                  )
  
})
  
  output$bot_backtest <- DT::renderDataTable({
    
    # browser()
    
    ###### Manipulate the selected row   ###############
    
    stock_row <- input$bot_buy_and_sell_rows_selected
    
    Signal_df <- read.csv(paste0(getwd(),"/data/Signal_df.csv", sep = ""))

    stock <- Signal_df[stock_row,"Stock"]
    selected_strategy <- Signal_df[stock_row,"Strategy"]
    # browser()
    if(stock %in% (c('ONGC.NS','ITC.NS','ASIANPAINT.NS','SBIN.NS','ADANIPORTS.NS','BHARTIARTL.NS','HCLTECH.NS','HDFCBANK.NS','TECHM.NS','TATAMOTORS.NS','UPL.NS','GRASIM.NS'))){
      stock <- stock
    }else{
      stock <- "RELIANCE.NS"
      # stock <- "HCLTECH.NS"
    }
    
    
    today = Sys.Date()
    f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
    previousWorkingDay <- f(today)
    stock_data <- na.omit(getSymbols(stock, src = "yahoo", from = "2018-01-01", to = previousWorkingDay, auto.assign = FALSE))
    stock_data <- data.frame(Date = index(stock_data), coredata(stock_data))
    
    
    
    ##################3 Check for the selected Strategy ###############
    
    
    if(selected_strategy == "Cowboy")
    {
      stock_data["Rider_Bullish"] = "No"
      stock_data["Bullish_Level"] = 100000
      stock_data["Rider_Bearish"] = "No"
      stock_data["Bearish_Level"] = 0
      
      for(i in 4:nrow(stock_data)){
        if(abs((stock_data[i,3] - stock_data[(i-1),3])/(stock_data[(i-1),3])*100) < 0.5){
          stock_data[i,"Rider_Bullish"] = "Yes"
          stock_data[i,"Bullish_Level"] = max(stock_data[(i-1),3],stock_data[(i-1),3])
        }
        else{
          stock_data[i,"Rider_Bullish"] = "No"
          stock_data[i,"Bullish_Level"] = 100000
        }
        if(abs((stock_data[i,4] - stock_data[(i-1),4])/(stock_data[(i-1),4])*100) < 0.5){
          stock_data[i,"Rider_Bearish"] = "Yes"
          stock_data[i,"Bearish_Level"]= min(stock_data[i,4],stock_data[(i-1),4])
        }
        else{
          stock_data[i,"Rider_Bearish"] = "No"
          stock_data[i,"Bearish_Level"] = 0
        }
      }
      
      if(stock == "ONGC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
      }else if(stock == "ITC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
      }else if(stock == "ASIANPAINT.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
      }else if(stock == "SBIN.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
      }else if(stock == "ADANIPORTS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
      }else if(stock == "BHARTIARTL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
      }else if(stock == "HCLTECH.NS"){
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
      }else if(stock == "HDFCBANK.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
      }else if(stock == "TECHM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
      }else if(stock == "TATAMOTORS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
      }else if(stock == "UPL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
      }else if(stock == "GRASIM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
      }else{
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
      }
      
      
      # browser()
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      increment = 1
      Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0),"Date"=character(0),"StopLoss"=character(0),"Target"=character(0))
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        temp_stock <- stock_data[stock_data$Date == previous_date,]
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          rownames(temp_stock) <- 1
          # print(i)
          # print(temp_stock[1,8])
          
          if(temp_stock[1,8] == "Yes"){
            satisfied_df = data.frame()
            
            for(i in 1:nrow(current_data)){
              if((current_data[i,"Close"]) > temp_stock[1,9]){
                satisfied_df = rbind(satisfied_df,current_data[i,])
                # print(satisfied_df)
              }
              else{
                next
              }
            }
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              satisfied_df = head(satisfied_df,1)
              # print(satisfied_df)
              rownames(satisfied_df) <- 1
              Signal_df[increment,"Strategy"] <- "Cowboy"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]="Buy"
              Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              
              increment = increment + 1
            }
          }
          else{
            next
          }
          
          if(temp_stock[1,10] == "Yes"){
            satisfied_df = data.frame()
            
            for(i in 1:nrow(current_data)){
              if((current_data[i,"Close"]) < temp_stock[1,11]){
                # print("Sell")
                satisfied_df = rbind(satisfied_df,current_data[i,])
                # print(satisfied_df)
              }
              else{
                next
              }
              
            }
            
            
            if(nrow(satisfied_df) == 0){
              next
            }
            else{
              satisfied_df = head(satisfied_df,1)
              rownames(satisfied_df) <- 1
              Signal_df[increment,"Strategy"] <- "Cowboy"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]="Sell"
              Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              increment = increment + 1
            }
          }
          else{
            next
          }
        }
        
      }
      
    }else if(selected_strategy == "Sweths Violation"){
      Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      if(stock == "ONGC.NS"){
        stock_5_min_historic_data <-read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
      }else if(stock == "ITC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
      }else if(stock == "ASIANPAINT.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
      }else if(stock == "SBIN.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
      }else if(stock == "ADANIPORTS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
      }else if(stock == "BHARTIARTL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
      }else if(stock == "HCLTECH.NS"){
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
      }else if(stock == "HDFCBANK.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
      }else if(stock == "TECHM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
      }else if(stock == "TATAMOTORS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
      }else if(stock == "UPL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
      }else if(stock == "GRASIM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
      }else{
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
      }
      
      # stock_5_min_historic_data <- read.csv("data/Reliance 5 Min Data.csv")
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      for(i in 1:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        rownames(current_data) <- 1:nrow(current_data)
        
        current_date <- current_data[1,"date"]
        
        # print(current_data)
        
        trigger_price = 0
        stage = ""
        
        if((current_data[1,"Close"]>current_data[1,"Open"]) && abs(current_data[1,"Close"] - current_data[1,"Open"])>= 0.7*abs(current_data[1,"High"] - current_data[1,"Low"])){
          trigger_price = current_data[1,"Low"]
          stage = "Green"
        }
        else if((current_data[1,"Close"]<current_data[1,"Open"]) && abs(current_data[1,"Close"] - current_data[1,"Open"])>= 0.7*abs(current_data[1,"High"] - current_data[1,"Low"])){
          trigger_price = current_data[1,"High"]
          stage = "Red"
        }
        else{
          next
        }
        
        satisfied_df = data.frame()
        
        for(j in 5:nrow(current_data)){
          
          if(stage == "Green"){
            if(current_data[j,"Close"] < trigger_price){
              satisfied_df = rbind(satisfied_df,current_data[j,])
              call ="Sell"
              # print(stock)
              # print("Moving up")
            }
          }
          else if(stage == "Red"){
            if(current_data[j,"Close"] > trigger_price){
              satisfied_df = rbind(satisfied_df,current_data[j,])
              call = "Buy"
              # print(stock)
              # print("Moving Down")
            }
          }
          else{
            next
          }
          
        }
        
        if(nrow(satisfied_df) == 0){
          next
        }
        else{
          # print(satisfied_df)
          satisfied_df = head(satisfied_df,1)
          Signal_df[increment,"Strategy"] <- "Sweths Violation"
          Signal_df[increment,"Stock"]=stock
          Signal_df[increment,"Signal"]=call
          Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
          Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
          
          increment = increment + 1
        }
        
        
        
      }
    }
    else if(selected_strategy == "Reds Rocket"){
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"Reds_High" = numeric(0),"Reds_Low"=numeric(0))
      
      inc = 1
      for(i in 4:nrow(stock_data)){
        
        current_date <- stock_data[i,1]
        
        l1_day_range <- abs(stock_data[i,3] - stock_data[i,4])
        l2_day_range <- abs(stock_data[(i-1),3] - stock_data[(i-1),4])
        l3_day_range <- abs(stock_data[(i-2),3] - stock_data[(i-2),4])
        l4_day_range <- abs(stock_data[(i-3),3] - stock_data[(i-3),4])
        
        l2_day_high <- stock_data[(i-1),3]
        l1_day_high <- stock_data[i,3]
        
        l2_day_low <- stock_data[(i-1),4]
        l1_day_low <- stock_data[i,4]
        
        if((l1_day_range < l2_day_range) && (l1_day_range < l3_day_range) && (l1_day_range < l4_day_range)){
          if(l1_day_low > l2_day_low && l1_day_high < l2_day_high){
            # print(current_date)
            final_levels_df[inc,"Date"] = current_date
            final_levels_df[inc,"Stock"] = stock
            final_levels_df[inc,"Reds_High"] = l1_day_high
            final_levels_df[inc,"Reds_Low"] = l1_day_low
            
            
            
            inc = inc + 1
          }
          
        }
        else{
          next
        }
        
      }
      
      stock_data <- final_levels_df
      
      
      if(stock == "ONGC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
      }else if(stock == "ITC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
      }else if(stock == "ASIANPAINT.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
      }else if(stock == "SBIN.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
      }else if(stock == "ADANIPORTS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
      }else if(stock == "BHARTIARTL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
      }else if(stock == "HCLTECH.NS"){
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
      }else if(stock == "HDFCBANK.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
      }else if(stock == "TECHM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
      }else if(stock == "TATAMOTORS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
      }else if(stock == "UPL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
      }else if(stock == "GRASIM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
      }else{
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
      }
      
      # stock_5_min_historic_data <- read.csv("data/Reliance 5 Min Data.csv")
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          rownames(temp_stock) <- 1
          
          # print(temp_stock)
          
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          for(j in 1:nrow(current_data)){
            
            
            if((current_data[j,"Close"]) > temp_stock[1,"Reds_High"]){
              # print(satisfied_df)
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Buy")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
            }else if((current_data[j,"Close"]) < temp_stock[1,"Reds_Low"]){
              
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Sell")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
            }
            else{
              next
            }
          }
          # # print(nrow(satisfied_df))
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            # print(satisfied_df)
            Signal_df[increment,"Strategy"] <- "Reds Rocket"
            Signal_df[increment,"Stock"]=stock
            Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
            Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
            Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
            
            increment = increment + 1
          }
          
          
        }
        
      }
    }
    else if(selected_strategy == "Reds Brahmos"){
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"Reds_High" = numeric(0),"Reds_Low"=numeric(0))
      
      inc = 1
      
      for(i in 6:nrow(stock_data)){
        
        current_date <- stock_data[i,1]
        
        l1_day_range <- abs(stock_data[i,3] - stock_data[i,4])
        l2_day_range <- abs(stock_data[(i-1),3] - stock_data[(i-1),4])
        l3_day_range <- abs(stock_data[(i-2),3] - stock_data[(i-2),4])
        l4_day_range <- abs(stock_data[(i-3),3] - stock_data[(i-3),4])
        l5_day_range <- abs(stock_data[(i-4),3] - stock_data[(i-4),4])
        l6_day_range <- abs(stock_data[(i-5),3] - stock_data[(i-5),4])
        
        l2_day_high <- stock_data[(i-1),3]
        l1_day_high <- stock_data[i,3]
        
        l2_day_low <- stock_data[(i-1),4]
        l1_day_low <- stock_data[i,4]
        
        if((l1_day_range < l2_day_range) && (l1_day_range < l3_day_range) && (l1_day_range < l4_day_range) && (l1_day_range < l5_day_range) && (l1_day_range < l6_day_range)){
          
          final_levels_df[inc,"Date"] = current_date
          final_levels_df[inc,"Stock"] = stock
          final_levels_df[inc,"Reds_High"] = l1_day_high
          final_levels_df[inc,"Reds_Low"] = l1_day_low
          
          inc = inc + 1
        }
        
      }
      
      stock_data <- final_levels_df
      
      
      if(stock == "ONGC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
      }else if(stock == "ITC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
      }else if(stock == "ASIANPAINT.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
      }else if(stock == "SBIN.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
      }else if(stock == "ADANIPORTS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
      }else if(stock == "BHARTIARTL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
      }else if(stock == "HCLTECH.NS"){
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
      }else if(stock == "HDFCBANK.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
      }else if(stock == "TECHM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
      }else if(stock == "TATAMOTORS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
      }else if(stock == "UPL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
      }else if(stock == "GRASIM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
      }else{
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
      }
      
      # stock_5_min_historic_data <- read.csv("data/Reliance 5 Min Data.csv")
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          rownames(temp_stock) <- 1
          
          # print(temp_stock)
          
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          for(j in 1:nrow(current_data)){
            
            
            if((current_data[j,"Close"]) > temp_stock[1,"Reds_High"]){
              # print(satisfied_df)
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Buy")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
            }else if((current_data[j,"Close"]) < temp_stock[1,"Reds_Low"]){
              
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Sell")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
            }
            else{
              next
            }
          }
          # # print(nrow(satisfied_df))
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            # print(satisfied_df)
            Signal_df[increment,"Strategy"] <- "Reds Brahmos"
            Signal_df[increment,"Stock"]=stock
            Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
            Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
            Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
            
            increment = increment + 1
          }
          
          
        }
        
      }
    }
    else if(selected_strategy == "Blackout"){
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"target" = numeric(0),"stage" = character(0))
      
      inc = 1
      
      for(i in 4:nrow(stock_data)){
        
        current_date <- stock_data[i,1]
        
        l4_high <- stock_data[(i-3),3]
        l3_high <- stock_data[(i-2),3]
        l2_high <- stock_data[(i-1),3]
        l1_high <- stock_data[i,3]
        
        
        l4_low <- stock_data[(i-3),4]
        l3_low <- stock_data[(i-2),4]
        l2_low <- stock_data[(i-1),4]
        l1_low <- stock_data[i,4]
        
        
        if((l1_low > l2_low) && (l1_high > l2_high) && (l2_low > l3_low) && (l2_high > l3_high) && (l3_low > l4_low) && (l3_high > l4_high)){
          l1_open <- stock_data[i,2]
          l1_close <- stock_data[i,5]
          real_body <- abs(l1_open - l1_close)
          body_high <- max(l1_open,l1_close)
          if((l1_high - body_high) > 2*(real_body)){
            final_levels_df[inc,"Stock"] = stock
            final_levels_df[inc,"target"] = l1_low
            final_levels_df[inc,"stage"] = "Short"
            final_levels_df[inc,"Date"] = current_date
            inc = inc + 1
          }
          
          
        }
        else if((l1_low < l2_low) && (l1_high < l2_high) && (l2_low < l3_low) && (l2_high < l3_high) && (l3_low < l4_low) && (l3_high < l4_high)){
          l1_open <- stock_data[i,2]
          l1_close <- stock_data[i,5]
          real_body <- abs(l1_open - l1_close)
          body_low <- min(l1_open,l1_close)
          if((l1_low - body_low) > 2*(real_body)){
            final_levels_df[inc,"Stock"] = stock
            final_levels_df[inc,"target"] = l1_high
            final_levels_df[inc,"stage"] = "Long"
            final_levels_df[inc,"Date"] = current_date
            inc = inc + 1
          }
          
          
        }
        else{
          next
        }
        
      }
      
      stock_data <- final_levels_df
      
      if(stock == "ONGC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
      }else if(stock == "ITC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
      }else if(stock == "ASIANPAINT.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
      }else if(stock == "SBIN.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
      }else if(stock == "ADANIPORTS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
      }else if(stock == "BHARTIARTL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
      }else if(stock == "HCLTECH.NS"){
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
      }else if(stock == "HDFCBANK.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
      }else if(stock == "TECHM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
      }else if(stock == "TATAMOTORS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
      }else if(stock == "UPL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
      }else if(stock == "GRASIM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
      }else{
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
      }
      
      # stock_5_min_historic_data <- read.csv("data/Reliance 5 Min Data.csv")
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        
        
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          rownames(temp_stock) <- 1
          # print(temp_stock)
          stage <- temp_stock[1,"stage"]
          target_value <- temp_stock[1,"target"]
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          if(stage == "Short"){
            for(j in 1:nrow(current_data)){
              if((current_data[j,"Close"]) < target_value){
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
              }
              
            }
          }
          else{
            for(j in 1:nrow(current_data)){
              if((current_data[j,"Close"]) > target_value){
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
              }
              
            }
          }
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            # print(satisfied_df)
            Signal_df[increment,"Strategy"] <- "Blackout"
            Signal_df[increment,"Stock"]=stock
            Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
            Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
            Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
            
            increment = increment + 1
          }
          
        }
      }
    }else if(selected_strategy == "Gap_up"){
      
      # browser()
      
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"Previous_Open" = numeric(0),"Previous_High"=numeric(0),"Previous_Low"=numeric(0),"Previous_Close"=numeric(0))
      
      inc = 1
      
      for(i in 2:nrow(stock_data)){
        
        # current_date <- stock_data[i,1]
        current_stock_data <- stock_data[i-1,]
        
        current_stock_data <- as.data.frame(current_stock_data)
        
        rownames(current_stock_data) <- 1
        
        final_levels_df[inc,"Stock"] = stock
        final_levels_df[inc,"Previous_Open"] = current_stock_data[1,2]
        final_levels_df[inc,"Previous_High"] = current_stock_data[1,3]
        final_levels_df[inc,"Previous_Low"] = current_stock_data[1,4]
        final_levels_df[inc,"Previous_Close"] = current_stock_data[1,5]
        final_levels_df[inc,"Date"] = current_stock_data[1,1]
        
        inc = inc + 1
        
      }
      
      stock_data <- final_levels_df
      
      if(stock == "ONGC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
      }else if(stock == "ITC.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
      }else if(stock == "ASIANPAINT.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
      }else if(stock == "SBIN.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
      }else if(stock == "ADANIPORTS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
      }else if(stock == "BHARTIARTL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
      }else if(stock == "HCLTECH.NS"){
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
      }else if(stock == "HDFCBANK.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
      }else if(stock == "TECHM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
      }else if(stock == "TATAMOTORS.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
      }else if(stock == "UPL.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
      }else if(stock == "GRASIM.NS"){
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
      }else{
        stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
        # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
      }
      
      # stock_5_min_historic_data <- read.csv("data/Reliance 5 Min Data.csv")
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        

        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]

        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]

        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]


        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          
          # browser()
          
          rownames(temp_stock) <- 1
          # print(temp_stock)
          # stage <- temp_stock[1,"stage"]
          target_value <- temp_stock[1,"target"]
          
          high_price = temp_stock[1,"Previous_High"]
          previous_close = temp_stock[1,"Previous_Close"]
          
          
          current_data$Call <- ""

          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))

          
          open_price = current_data[1,2]
          
          if(open_price > previous_close){
            
            
            for(j in 5:nrow(current_data)){
              # if(stock == "UPL.NS"){
              
              current_date <- current_data[i,"dates"]
              # print(current_date)
              
              day_high <- max(current_data$Close)
              day_low <- min(current_data$Low)
              
              low_range <- min(current_data[j-1,4],current_data[j-2,4],current_data[j-3,4],current_data[j-4,4])
              high_range <- max(current_data[j-1,3],current_data[j-2,3],current_data[j-3,3],current_data[j-4,3])
              current_close <- current_data[j,"Close"]
              if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close >= high_price) && (current_close >= day_high)){
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
              }
              # else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
              else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price)){
                # print(current_date)
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
              }else{
                next
              }
              
              
            }
            
            
            for(i in 5:nrow(current_data)){
              # if(stock == "UPL.NS"){
              
              current_date <- current_data[i,"Datetime"]
              # print(current_date)
              
              day_high <- max(current_data$Close)
              
              low_range <- min(current_data[i-1,4],current_data[i-2,4],current_data[i-3,4],current_data[i-4,4])
              high_range <- max(current_data[i-1,3],current_data[i-2,3],current_data[i-3,3],current_data[i-4,3])
              current_close <- current_data[i,"Close"]
              if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close >= high_price) && (current_close >= day_high)){
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
              }
              # else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
              else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price)){
                print(current_date)
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
              }else{
                next
              }
              
            }
          }
          
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            # print(satisfied_df)
            Signal_df[increment,"Strategy"] <- "Gap_up"
            Signal_df[increment,"Stock"]=stock
            Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
            Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
            Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
            
            increment = increment + 1
          }

        }
      }
    }
    
    
    
    # browser()
    
    print(Signal_df)
    
    #########    Setting the target for the selected stock   ###########
    
    if(nrow(Signal_df) == 0){
      next
    }else{
      # Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
      
      # Signal_df$Datetime <- Signal_df$Datetime + hm("5:30")
      
      # Signal_df <- Signal_df[order(Signal_df$Datetime),]
      
      rownames(Signal_df) <- 1:nrow(Signal_df)
      
      Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
      
      stop_loss <- as.numeric(input$bot_loss)
      target <- as.numeric(input$bot_profit)
      
      # stop_loss <- 1
      # target <- 1
      
      
      Signal_df$Date <- as.Date(Signal_df$Datetime)
      
      # entry_point <- input$entry_point
      
      # market_direction <- as.numeric(input$market_direction)
      #
      Capital <- 10000
      # browser()
      Signal_df$StopLoss <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
      Signal_df$Target <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
      
      # Signal_df$Qty <- round(abs((2/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
      
      # qty <- round(market_direction*((2/100)*Capital/(entry_point - stop_loss)),0)
      
    }
    
    final_signal_df = data.frame(Strategy = character(0),Call_time = character(0),Call = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0),Value=character(0))
    
    
    # browser()
    
    print(final_signal_df)
    
    
    for(i in 1:nrow(Signal_df)){
      stock_row <- input$bot_buy_and_sell_rows_selected
      
      # Signal_df <- read.csv(paste0(getwd(),"/data/Signal_df.csv", sep = ""))
      
      current_stock <- Signal_df[stock_row,"Stock"]
      selected_strategy <- Signal_df[stock_row,"Strategy"]
      # browser()
      if(current_stock %in% (c('ONGC.NS','ITC.NS','ASIANPAINT.NS','SBIN.NS','ADANIPORTS.NS','BHARTIARTL.NS','HCLTECH.NS','HDFCBANK.NS','TECHM.NS','TATAMOTORS.NS','UPL.NS','GRASIM.NS'))){
        current_stock <- current_stock
      }else{
        current_stock <- "RELIANCE.NS"
        # current_stock <- "HCL.NS"
      }
      
      # browser()
      
      print(stock)
    
      stock <- current_stock
      
      stock <- Signal_df[i,"Stock"]
      
      # if(stock == current_stock){
        
        call_time <- Signal_df[i,"Datetime"]
        # call_time <- as_datetime(as.character(as_datetime(call_time) - hm("5:30")),tz="Asia/Kolkata")
        # print(call_time)
        signal_val <- Signal_df[i,"Signal"]
        call_val <- Signal_df[i,"Value"]
        StopLoss <- Signal_df[i,"StopLoss"]
        Target <- Signal_df[i,"Target"]
        Strategy <- Signal_df[i,"Strategy"]
        
        current_date <- Signal_df[i,"Date"]
        current_signal <- Signal_df[i,"Signal"]
        # print(current_date)
        # print(call_time)
        
        # if(current_date ==  "2020-08-14"){
        
        # print(Signal_df[i,])
        # browser()
        
        
        #####   Reading the 5 min data    ############
        if(stock == "ONGC.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
        }else if(stock == "ITC.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
        }else if(stock == "ASIANPAINT.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
        }else if(stock == "SBIN.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
        }else if(stock == "ADANIPORTS.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
        }else if(stock == "BHARTIARTL.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
        }else if(stock == "HCLTECH.NS"){
          # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
        }else if(stock == "HDFCBANK.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
        }else if(stock == "TECHM.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
        }else if(stock == "TATAMOTORS.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
        }else if(stock == "UPL.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
        }else if(stock == "GRASIM.NS"){
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
        }else{
          stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
          # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
        }
        
        
        
        stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
        
        stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
        
        
        final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
        
        
        current_data <- final_5_min_stocks[final_5_min_stocks$date == current_date,]
        
        sub_data <- current_data[current_data$Datetime > call_time,]
        # print(current_data)
        # print(sub_data)
        satisfied_df = data.frame(Strategy = character(0),Call_time = character(0),Call = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0),Value=character(0))
        
        incr = 1
        
        
        
        # if(nrow(sub_data) > 0){
          rownames(sub_data) <- 1:nrow(sub_data)
          
          if(signal_val == "Buy"){
            for(j in 1:nrow(sub_data)){
              
              curr_hr <- hour(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              
              # print(sub_data[j,"Datetime"])
              if((sub_data[j,"High"]) >= Target){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"Target"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
              }
              else if((sub_data[j,"Low"]) <= StopLoss){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"SL"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                # print(abs(destring(sub_data[j,"Low"]) - destring(signal_val)))
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
              }
              else if(curr_hr == 15 && curr_min == 15){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
              }
              else{
                
              }
            }
            
          }
          else{
            # print("Hello")
            # print(sub_data)
            for(j in 1:nrow(sub_data)){
              
              curr_hr <- hour(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              
              if((sub_data[j,"High"]) <= Target){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"Target"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
              }
              else if((sub_data[j,"Low"]) >= StopLoss){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"SL"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(destring(call_val))),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
              }
              else if(curr_hr == 15 && curr_min == 15){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
              }
              else{
                
              }
            }
          }
        # }
        
        
        if(nrow(satisfied_df) == 0){
          next
        }
        else{
          satisfied_df = head(satisfied_df,1)
          
          final_signal_df = rbind(final_signal_df,satisfied_df)
        }
        
      # }
      
      
      
      
      
      
     
      # }
      
      
    }
    
    # browser()
    
    print(final_signal_df)
    
    final_signal_df[is.na(final_signal_df$Target),]$Target <- ""
    final_signal_df[is.na(final_signal_df$SL),]$SL <- ""
    
    final_signal_df$sign <- ifelse((final_signal_df$Call=="Buy"),-1,1)
    
    # View(final_signal_df)
    Signal_df$sign <- ifelse(Signal_df$Signal=="Buy",1,-1)
    Signal_df
    # browser()
    
    # for(i in 1:nrow(Signal_df)){
    #   if(i == 1){
    #     Signal_df[i,"Capital"] = input$bot_capital
    #     Signal_df[i,"Qty"] <- round(abs((20/100)*Signal_df[i,"Capital"]/(Signal_df[i,"Target"] - Signal_df[i,"StopLoss"])),0)
    #   }
    #   else {
    #     Signal_df[i,"Capital"] = Signal_df[(i-1),"Capital"]
    #     Signal_df[i,"Qty"] <- round(abs((20/100)*Signal_df[(i-1),"Capital"]/(Signal_df[i,"Target"] - Signal_df[i,"StopLoss"])),0)
    #   }
    # }
    
    Signal_df$Datetime <- as_datetime(as.character(as_datetime(Signal_df$Datetime) + hm("5:30")),tz="Asia/Kolkata")
    final_signal_df$achieved_ts <- as_datetime(as.character(as_datetime(final_signal_df$achieved_ts) + hm("5:30")),tz="Asia/Kolkata")
    # final_signal_df$StartTime <- as_datetime(as.character(as_datetime(final_signal_df$StartTime) + hm("5:30")),tz="Asia/Kolkata")
    final_signal_df$Call_time <- as_datetime(as.character(as_datetime(final_signal_df$Call_time) + hm("5:30")),tz="Asia/Kolkata")
    
    colnames(Signal_df)
    colnames(final_signal_df)
    
    
    combined_data <- sqldf("with main_table as
                      (
                        select Strategy,stock,Datetime,Value as price,Target, StopLoss,sign
                        from Signal_df
                        union all
                        select Strategy,stock,achieved_ts as Datetime,Value as price,0 as Target, 0 as StopLoss,sign
                        from final_signal_df
                      )
                      select * from main_table order by Datetime asc
                      ")
    
    target_combined_data <- sqldf("select Strategy,sd1.stock,sd1.Datetime as StartTime,sd1.Value as price,sd1.Target, sd1.StopLoss,sd1.sign as initial_sign,sd2.achieved_ts,sd2.hit_price,sd2.sign as final_sign
      from Signal_df sd1
      left join 
       (
      select Call_time,stock,achieved_ts,Value as hit_price,sign
     from final_signal_df
      ) sd2 on sd1.Datetime = sd2.Call_time and sd1.stock = sd2.stock
       ")
    
    # target_combined_data$price_diff <- ifelse(target_combined_data$initial_sign == 1,as.numeric(target_combined_data$hit_price) - as.numeric(target_combined_data$price),as.numeric(target_combined_data$price) - as.numeric(target_combined_data$hit_price))
    target_combined_data$price_diff <- round(as.numeric(target_combined_data$hit_price) - as.numeric(target_combined_data$price),2)
    for(i in 1:nrow(target_combined_data)){
      if(i==1){
        capital <- input$bot_capital
        target_combined_data[i,"QTY"] <- round(abs((20/100)*capital/(target_combined_data[i,"Target"] - target_combined_data[i,"StopLoss"])),0)
        target_combined_data[i,"corrected_capital"] <- round(capital + ((target_combined_data[i,"QTY"])*(target_combined_data[i,"price_diff"])),2)
      }
      else{
        capital <- target_combined_data[(i-1),"corrected_capital"]
        target_combined_data[i,"QTY"] <- round(abs((20/100)*capital/(target_combined_data[i,"Target"] - target_combined_data[i,"StopLoss"])),0)
        target_combined_data[i,"corrected_capital"] <- round(capital + ((target_combined_data[i,"QTY"])*(target_combined_data[i,"price_diff"])),2)
      }
    }
    # target_combined_data$QTY <- round(abs((20/100)*Capital/(target_combined_data$Target - target_combined_data$StopLoss)),0)
    
    if(stock == "ONGC.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ONGC.csv", sep = ""))
    }else if(stock == "ITC.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ITC.csv", sep = ""))
    }else if(stock == "ASIANPAINT.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ASIANPAINT.csv", sep = ""))
    }else if(stock == "SBIN.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - SBIN.csv", sep = ""))
    }else if(stock == "ADANIPORTS.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - ADANI PORTS.csv", sep = ""))
    }else if(stock == "BHARTIARTL.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - BHARTIARTL.csv", sep = ""))
    }else if(stock == "HCLTECH.NS"){
      # stock_5_min_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
    }else if(stock == "HDFCBANK.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"Stocks 5 min data - HDFCBANK.csv", sep = ""))
    }else if(stock == "TECHM.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"Stocks 5 min data - TechM.csv", sep = ""))
    }else if(stock == "TATAMOTORS.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
    }else if(stock == "UPL.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - UPL.csv", sep = ""))
    }else if(stock == "GRASIM.NS"){
      stock_5_min_data <- read.csv(paste0(getwd(),"Nifty 50 5 min data - GRASIM.csv", sep = ""))
    }else{
      stock_5_min_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
      # stock_5_min_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
    }
    # stock_5_min_data <- read.csv("data/Reliance 5 Min Data.csv")
    # library(sqldf)
    # library(dplyr)
    # library(lubridate)
    # library(quantmod)
    # library(taRifx)
    
    stock_5_min_data <- data.frame(stock_5_min_data)
    
    stock_5_min_data <- stock_5_min_data %>%
      arrange(Datetime) %>%
      mutate(
        pnl = Close/lag(Close) - 1
      )
    
    
    stock_5_min_data[is.na(stock_5_min_data$pnl),]$pnl <- 0
    
    
    stock_5_min_data$Datetime <- as_datetime(as.character(as_datetime(stock_5_min_data$Datetime) + hm("5:30")),tz="Asia/Kolkata")
    Signal_df$Datetime <- as_datetime(as.character(as_datetime(Signal_df$Datetime) + hm("5:30")),tz="Asia/Kolkata")
    
    combined_data
    
    final_combined <- sqldf("select sm.Datetime,
                                sm.pnl,
                              -- cd.Strategy,
                              --  cd.stock,
                              case when cd.price > 0 then cd.price else sm.Close end as price,
                                cd.sign
                         from stock_5_min_data sm
                        left join combined_data cd on sm.Datetime = cd.Datetime")
    
    
    final_combined
    
    # final_combined[which(is.na(final_combined$price)),]$price <- 0
    final_combined[which(is.na(final_combined$sign)),]$sign <- 0
    
    
    
    write.csv(final_combined,paste0(getwd(),"/data/bot_backtest.csv", sep = ""))

    # combined_data$Datetime <- as_datetime(as.character(as_datetime(combined_data$Datetime), tz = "Asia/Kolkata"))
    # target_combined_data$Datetime <- as_datetime(as.character(as_datetime(target_combined_data$Datetime), tz = "Asia/Kolkata"))
    
    target_combined_data$StartTime <- as_datetime(as.character(as_datetime(target_combined_data$StartTime) + hm("5:30")),tz="Asia/Kolkata")
    
    DT::datatable(target_combined_data,extensions = c('FixedColumns'),
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top'
                  ))
    
  })
  
  observeEvent(input$bot_refresh,{
  output$perfPlot <- renderPlot({
    # observeEvent(input$strategy_back_test_submit,{
    # browser()
    
    final_combined <- read.csv(paste0(getwd(),"/data/bot_backtest.csv", sep = ""))
    
    final_combined <- subset(final_combined, select = -c(X) )
    
    stock_ts <- final_combined$sign
    
    stock_strat <- ifelse(final_combined$sign > 1,0,1)
    
    for (i in 1:nrow(final_combined)) {
      stock_strat[i] <- ifelse(final_combined[i,"sign"] == 1,1,ifelse(final_combined[i,"sign"] == -1,0,stock_strat[i-1]))
    }
    stock_strat[is.na(stock_strat)] <- 1
    
    
    
    stock_stratcomp <- cbind(final_combined$Datetime,final_combined$sign,stock_strat)
    
    
    colnames(stock_stratcomp) <- c('Datetime','SIGNAL','POSITION')
    
    stock_stratcomp <- as.data.frame(stock_stratcomp)
    head(stock_stratcomp,5)
    
    stock_ret <- diff(log(final_combined$price))
    # stock_ret <- diff(final_combined$price)
    stock_ret <- append(0,stock_ret)
    
    stock_benchmark <- stock_ret
    
    
    stocks_ret <- stock_ret*stock_strat
    stock_ret_commission_adj <- ifelse((stock_ts == 1|stock_ts == -1) & stock_strat != Lag(stock_ts), (stock_ret-0.05)*stock_strat, stock_ret*stock_strat)
    stock_comp <- cbind(final_combined$Datetime,stocks_ret, stock_ret_commission_adj, stock_benchmark)
    
    colnames(stock_comp) <- c('Datetime','Stock Return','Stock Commision Adjustment','Stock Benchmark')
    stock_comp <- as.data.frame(stock_comp)
    
    # stock_comp
    # 
    # temp_data <- stock_comp[which(as.Date(as_datetime(as.character(as_datetime(final_combined$Datetime) + hm("5:30")),tz="Asia/Kolkata")) >= "2019-06-01"),]
    
    rownames(stock_comp) <- 1:nrow(stock_comp)
    
    nrow(stock_comp)
    
    
    
    dates_col <- seq(as.Date("1970-01-01"), by = "day", length.out = nrow(stock_comp))
    
    final_stock <- stock_comp %>% select (c("Stock Return","Stock Commision Adjustment","Stock Benchmark"))
    final_stock$`Stock Return` <- as.double(final_stock$`Stock Return`)
    final_stock$`Stock Commision Adjustment` <- as.double(final_stock$`Stock Commision Adjustment`)
    final_stock$`Stock Benchmark` <- as.double(final_stock$`Stock Benchmark`)
    
    x <- xts(coredata(final_stock),as.POSIXct(dates_col))
    
    charts.PerformanceSummary(x, main = 'Stock Performance')
    
    
  })
  })
  
  
  
  
  
  
  output$bot_performance <- DT::renderDataTable({
    
    # browser()
    
    Signal_df <- read.csv(paste0(getwd(),"/data/Signal_df.csv", sep = ""))
    
    Signal_df <- subset(Signal_df, select = -c(X) )
    
    
    
    final_signal_df = data.frame(dates = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0), points = character(0))
    
    for(i in 1:nrow(Signal_df))
    {
      # browser()
      stock <- Signal_df[i,"Stock"]
      call_time <- Signal_df[i,"Datetime"]
      call_time <- as_datetime(as.character(as_datetime(call_time) - hm("5:30")),tz="Asia/Kolkata")
      signal_val <- Signal_df[i,"Signal"]
      StopLoss <- Signal_df[i,"StopLoss"]
      Target <- Signal_df[i,"Target"]
      Strategy <- Signal_df[i,"Strategy"]
      
      print(stock)
      
      response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=",input$bot_timeframe,"&useYfid=true&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
      
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # browser()
      
      if(nrow(final_data) == 0){
        stock_timestamp <- response_data$chart$result[[2]][[1]]
        Close <- response_data$chart$result[[3]]$quote[[1]]$close
        High <- response_data$chart$result[[3]]$quote[[1]]$high
        Low <- response_data$chart$result[[3]]$quote[[1]]$low
        Open <- response_data$chart$result[[3]]$quote[[1]]$open
        Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      }
      
      
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      else{
        final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      
      final_data <- na.omit(final_data)
      
      sub_data <- final_data[final_data$dates > call_time,]
      
      satisfied_df = data.frame(Strategy = character(0),dates = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0))
      
      
     incr = 1

      
      if(nrow(sub_data) > 0){
        rownames(sub_data) <- 1:nrow(sub_data)
        
        if(signal_val == "Buy"){
          for(j in 1:nrow(sub_data)){
            if((sub_data[j,"High"]) >= Target){
              satisfied_df[incr,"Strategy"] <- Strategy
              satisfied_df[incr,"stock"] <- stock
              satisfied_df[incr,"dates"] <- call_time
              satisfied_df[incr,"Target"] <- "Yes"
              satisfied_df[incr,"achieved_ts"] <- sub_data[j,"dates"]
              satisfied_df[incr,"points"] <- abs(destring(sub_data[j,"High"]) - destring(signal_val))
              
              incr = incr + 1
            }
            else if((sub_data[j,"Low"]) <= StopLoss){
              satisfied_df[incr,"Strategy"] <- Strategy
              satisfied_df[incr,"stock"] <- stock
              satisfied_df[incr,"dates"] <- call_time
              satisfied_df[incr,"SL"] <- "Yes"
              satisfied_df[incr,"achieved_ts"] <- sub_data[j,"dates"]
              satisfied_df[incr,"points"] <- abs(destring(sub_data[j,"Low"]) - destring(signal_val))
              incr = incr + 1
            }
            else{
              
            }
          }
          
        }
        else{
          for(j in 1:nrow(sub_data)){
            if((sub_data[j,"High"]) <= Target){
              satisfied_df[incr,"Strategy"] <- Strategy
              satisfied_df[incr,"stock"] <- stock
              satisfied_df[incr,"dates"] <- call_time
              satisfied_df[incr,"Target"] <- "Yes"
              satisfied_df[incr,"achieved_ts"] <- sub_data[j,"dates"]
              satisfied_df[incr,"points"] <- abs(destring(sub_data[j,"High"]) - destring(signal_val))
              incr = incr + 1
            }
            else if((sub_data[j,"Low"]) >= StopLoss){
              satisfied_df[incr,"Strategy"] <- Strategy
              satisfied_df[incr,"stock"] <- stock
              satisfied_df[incr,"dates"] <- call_time
              satisfied_df[incr,"SL"] <- "Yes"
              satisfied_df[incr,"achieved_ts"] <- sub_data[j,"dates"]
              satisfied_df[incr,"points"] <- abs(destring(sub_data[j,"Low"]) - destring(signal_val))
              incr = incr + 1
            }
            else{
              
            }
          }
        }
      }
     # browser()
     if(nrow(satisfied_df) == 0){
       next
     }
     else{
       satisfied_df = head(satisfied_df,1)
       
       final_signal_df = rbind(final_signal_df,satisfied_df)
     }
    }
    
    if(nrow(final_signal_df) > 0){
      final_signal_df$dates <- as.POSIXct(as.numeric(as.character(final_signal_df$dates)),origin="1970-01-01")
      
      final_signal_df$dates <- final_signal_df$dates + hm("5:30")
      
      final_signal_df$achieved_ts <- as.POSIXct(as.numeric(as.character(final_signal_df$achieved_ts)),origin="1970-01-01")
      
      final_signal_df$achieved_ts <- final_signal_df$achieved_ts + hm("5:30")
    }
    
    write.csv(final_signal_df,paste0(getwd(),"/data/bot_performance.csv", sep = ""))
      
    DT::datatable(final_signal_df,extensions = c('FixedColumns'),
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top'
                  ))
  })
  
  output$bot_stats <- DT::renderDataTable({
    # browser()
    Signal_df <- read.csv(paste0(getwd(),"/data/bot_performance.csv", sep = ""))
    
    Signal_Stats <- data.frame("Metric" = character(0),"Value" = character(0))
    
    Signal_Stats[1,"Metric"] <- "Total Calls"
    Signal_Stats[2,"Metric"] <- "Total Hits"
    Signal_Stats[3,"Metric"] <- "Total SL Hits"
    Signal_Stats[4,"Metric"] <- "Win %"
    Signal_Stats[5,"Metric"] <- "Profits"
    Signal_Stats[6,"Metric"] <- "Loss"
    Signal_Stats[7,"Metric"] <- "Max Gain"
    Signal_Stats[8,"Metric"] <- "Max Loss"
    
    if(nrow(Signal_df) > 0){
      Signal_df <- subset(Signal_df, select = -c(X) )
      
      
      Signal_Stats[1,"Value"] <- nrow(Signal_df)
      Signal_Stats[2,"Value"] <- nrow(Signal_df %>% filter(Target == "Yes"))
      Signal_Stats[3,"Value"] <- nrow(Signal_df %>% filter(SL == "Yes"))
      Signal_Stats[4,"Value"] <- round(nrow(Signal_df %>% filter(Target == "Yes"))/nrow(Signal_df),2)*100
     
    }
    
    
    DT::datatable(Signal_Stats,extensions = c('FixedColumns'),
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top'
                  ))
    
    
  })
   
  })
  
  
  output$bot_strategy_output <- renderText({
    
    # final_data <- getpercentageChange()
    # 
    # final_data$trend = ''
    # final_data$trigger_price = ''
    # final_data$value_change = ''
    # final_data$percentage_change = ''
    # 
    # # head(final_data)
    # 
    # 
    # position <- 0 # 1 means we have already entered poistion, 0 means not already entered
    # counter <- 0
    # percentChange <- list()   # empty list to collect %changes 
    # buyposition_index = 1
    # sellposition_index = 1
    # 
    # outputdf = data.frame(time=character(0),call=character(0),percentage_change=character(0))
    # 
    # for(i in 1:nrow(final_data)){
    #   
    #   # browser()
    #   if((final_data[i,'Close'] > final_data[i,"last_close"]) & (final_data[i,'Close'] > final_data[i,"ema_20_Close"]) & (final_data[i,'ema_10_Close'] > final_data[i,"ema_20_Close"])){
    #     final_data[i,'trend'] <- 'Uptrend'
    #     final_data[i,'trigger_price'] <- final_data[buyposition_index,'Close']
    #     final_data[i,'value_change'] <- (final_data[buyposition_index,'Close'] - final_data[i,'Close'])
    #     final_data[i,'percentage_change'] <- (final_data[buyposition_index,'Close'] - final_data[i,'Close']) / ((final_data[buyposition_index,'Close'])*100)
    #     
    #     if(position==0){
    #       buyP=final_data[i,'Close']  #buy price
    #       position=1   # turn position
    #       buyposition_index = i
    #       current_row = nrow(outputdf)
    #       outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #       outputdf[current_row+1,'call'] <- paste("Buy at the price ",buyP,"")
    #       # print(paste("Buy at the price ",buyP,""))
    #       # percentChange <- append(percentChange,paste("Buy at the price ",buyP,""))
    #     }
    #     
    #   }
    #   else if((final_data[i,'Close'] < final_data[i,"last_close"]) & (final_data[i,'Close'] < final_data[i,"ema_20_Close"]) & (final_data[i,'ema_10_Close'] < final_data[i,"ema_20_Close"])){
    #     final_data[i,'trend'] <- 'Downtrend'
    #     final_data[i,'trigger_price'] <- final_data[sellposition_index,'Close']
    #     final_data[i,'value_change'] <- (final_data[i,'Close'] - final_data[sellposition_index,'Close'])
    #     final_data[i,'percentage_change'] <- (final_data[i,'Close'] - final_data[sellposition_index,'Close']) / ((final_data[i,'Close'])*100)
    #     
    #     if(position==1){
    #       # have a poistion in down trend
    #       position=0     # selling position
    #       sellP=final_data[i,'Close']    # sell price
    #       sellposition_index = i
    #       
    #       perc=(sellP/buyP-1)*100
    #       # browser()
    #       current_row = nrow(outputdf)
    #       outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #       outputdf[current_row+1,'call'] <- paste("Sell at the price ",sellP,"")
    #       outputdf[current_row+1,'percentage_change'] <-perc
    #       
    #       # print(paste("Sell at the price ",sellP,""))
    #       # percentChange <- append(percentChange,paste("Buy at the price ",buyP,""))
    #       # 
    #       # percentChange <- append(percentChange, list(perc))
    #       
    #     }   
    #     
    #   }
    #   else{
    #     next
    #   }
    #   
    #   # if(hour(as.POSIXct(final_data[i,"dates"], origin = "1970-01-01")) == 15 & minute(as.POSIXct(final_data[i,"dates"], origin = "1970-01-01")) == 10){
    #   #   
    #   #   if(position == 1){
    #   #     position == 0
    #   #     sellP=final_data[i,'Close']
    #   #     perc=(sellP/buyP-1)*100
    #   #     current_row = nrow(outputdf)
    #   #     outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #   #     outputdf[current_row+1,'call'] <- paste("Sell at the price ",sellP ,"")
    #   #     outputdf[current_row+1,'percentage_change'] <-perc
    #   #   }
    #   #   else{
    #   #     position == 1
    #   #     buyP=final_data[i,'Close']
    #   #   }
    #   #   
    #   # }
    #   
    #   if(counter==nrow(final_data) & position==1){
    #     
    #     position=0
    #     sellP=final_data[i,'Close']
    #     # print(paste("Sell at the price ",sellP,""))
    #     # percentChange <- append(percentChange, paste("Sell at the price ",sellP,""))
    #     # perc=(sellP/buyP-1)*100
    #     # percentChange <- append(percentChange, list(perc))
    #     
    #     current_row = nrow(outputdf)
    #     outputdf[current_row+1,'time'] <- str_remove(as.POSIXct(final_data[i,'dates'],origin="1970-01-01")," IST")
    #     outputdf[current_row+1,'call'] <- paste("Sell at the price ",sellP,"")
    #     outputdf[current_row+1,'percentage_change'] <-perc
    #     
    #   }
    #   
    # }
    # 
    # counter = counter + 1
    # 
    # gains=0
    # numGains=0
    # losses=0
    # numLosses=0
    # totReturn=1
    # 
    # for(i in 1:nrow(outputdf)){
    #   change = percentChange[i][[1]]
    #   print(change)
    #   if(change > 0){
    #     gains = gains + change
    #     numGains = numGains + 1
    #   }
    #   else{
    #     losses = losses + change
    #     numLosses = numLosses + 1
    #   }
    #   totReturn = totReturn*((change/100)+1)
    # }
    # totReturn=round((totReturn-1)*100,2)
    
  })
  
  output$stocks_sell_call <- DT::renderDataTable({
    
    withProgress(message = 'Fetching fundamentals : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      # browser()
      if(input$candle_stick_range == "1d"){
        # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5d"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=30m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "6mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "ytd"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else {
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      
      
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # browser()
      
      if(nrow(final_data) == 0){
        stock_timestamp <- response_data$chart$result[[2]][[1]]
        Close <- response_data$chart$result[[3]]$quote[[1]]$close
        High <- response_data$chart$result[[3]]$quote[[1]]$high
        Low <- response_data$chart$result[[3]]$quote[[1]]$low
        Open <- response_data$chart$result[[3]]$quote[[1]]$open
        Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      }
      
      
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      
      # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      else{
        final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      
      final_data <- na.omit(final_data)
      
      myMACD <- function (price,S,L,K){
        MACD <- EMA(price,S) - EMA(price,L)
        signal <- EMA(MACD,K)
        output <- cbind(MACD,signal)
        colnames(output) <- c("MACD","signal")
        return(output)
      }  
      
      macd_data <- myMACD(final_data$Close,12,26,9)
      
      macd_data <- as.data.frame(macd_data)
      
      final_macd_data <- cbind(final_data,macd_data)
      
      final_macd_data$cross_over_signal <- Lag(ifelse(final_macd_data$MACD < final_macd_data$signal, -1, 1))
      
      sell_data <- final_macd_data[which(final_macd_data$MACD > 2 & final_macd_data$cross_over_signal == -1),] 
      
      sell_data$dates <- sell_data$dates + hm("5:30") 
      sell_data$Close <- round(sell_data$Close,2)
      sell_data$High <- round(sell_data$High,2)
      sell_data$Open <- round(sell_data$Open,2)
      sell_data$Low <- round(sell_data$Low,2)
      sell_data$Volume <- round(sell_data$Volume,2)
      sell_data$MACD <- round(sell_data$MACD,2)
      sell_data$signal <- round(sell_data$signal,2)
      
      DT::datatable(sell_data, options = list(scrollX = TRUE,
                                             pageLength=10,
                                             autoWidth = TRUE),
                    filter = list(
                      position = 'top', clear = FALSE
                    )
      ) 
      
      
    })
    
  })
  
  output$intraday_support_and_resistance <- renderPlot({
    
    withProgress(message = 'Fetching fundamentals : ', value = 0, {
      incProgress(0.5, detail = paste("Doing calculations"))
      # browser()
      if(input$candle_stick_range == "1d"){
        # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5d"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=30m&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "6mo"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "ytd"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "1y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$candle_stick_range == "5y"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else {
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",input$stock_input,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&useYfid=true&range=",input$candle_stick_range,"&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      
      
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # browser()
      
      if(nrow(final_data) == 0){
        stock_timestamp <- response_data$chart$result[[2]][[1]]
        Close <- response_data$chart$result[[3]]$quote[[1]]$close
        High <- response_data$chart$result[[3]]$quote[[1]]$high
        Low <- response_data$chart$result[[3]]$quote[[1]]$low
        Open <- response_data$chart$result[[3]]$quote[[1]]$open
        Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      }
      
      
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      
      # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }else{
        final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
      }
      
      final_data <- na.omit(final_data)
      # browser()
      
      K=10
      
      max_stocks <- cbind(rollmax(final_data$Open, k=K,fill = FALSE),rollmax(final_data$High, k=K,fill = FALSE),rollmax(final_data$Low, k=K,fill = FALSE),rollmax(final_data$Close, k=K,fill = FALSE))
      
      # stock_data <- reclass(apply(final_data,2,na.locf),match.to=final_data)
      
      max_stocks <- na.locf(reclass(apply(max_stocks,1,mean),match.to=max_stocks))
      
      max_stocks <- cbind(as.POSIXct(final_data$dates),max_stocks)
      
      max_stocks <- as.data.frame(max_stocks)
      
      
      
      max_stocks <- max_stocks[!max_stocks$max_stocks == 0.00, ]
      
      result <- max_stocks[-1]
      row.names(result) <- max_stocks$V1
      
      ###################################################################
      
      
      
      
      
      
      
      
      detectSupportResistance <- function(timeSeries, tolerance=0.5, nChunks=10, nPoints=3, plotChart=TRUE)
      {
        #detect maximums and minimums
        N = length(timeSeries)
        print(N)
        stp = floor(N / nChunks)
        print(stp)
        minz = array(0.0, dim=nChunks)
        whichMinz = array(0, dim=nChunks)
        print(whichMinz)
        maxz = array(0.0, dim=nChunks)
        whichMaxz = array(0, dim=nChunks)
        for(j in 1:(nChunks-1)) 
        {
          lft = (j-1)*stp + 1  #left and right elements of each chunk
          # print(lft)
          rght = j*stp
          # print(rght)
          whichMinz[j] = which.min(timeSeries[lft:rght]) + lft
          print(whichMinz[j])
          minz[j] = min(timeSeries[lft:rght])
          print(minz[j])
          whichMaxz[j] = which.max(timeSeries[lft:rght]) + lft
          maxz[j] = max(timeSeries[lft:rght])
        }  
        print(minz)
        print(maxz)
        
        #last chunk
        lft = j*stp + 1  #left and right elements of each chunk
        rght = N
        whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + lft
        minz[nChunks] = min(timeSeries[lft:rght])
        whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + lft
        maxz[nChunks] = max(timeSeries[lft:rght])
        
        result = list()
        result[["minima"]] = NULL
        result[["minimaAt"]] = NULL
        result[["maxima"]] = NULL
        result[["maximaAt"]] = NULL
        span = tolerance*(max(maxz) - min(minz))
        
        rang = order(minz)[1:nPoints]
        if((minz[rang[nPoints]] - minz[rang[1]]) <= span)
        {
          result[["minima"]] = minz[rang[1:nPoints]]
          result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
        } 
        print(rang)
        
        rang = order(maxz, decreasing = TRUE)[1:nPoints]
        if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span)
        {
          result[["maxima"]] = maxz[rang[1:nPoints]]
          result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
        } 
        
        print(rang)
        
        if(plotChart)
        {
          print("hello")
          ts.plot(timeSeries)
          points(whichMinz, minz, col="blue")
          points(whichMaxz, maxz, col="red")
          # print(fitted(lm(result[["minima"]] ~  result[["minimaAt"]])))
          # model <- lm(result[["minima"]] ~  result[["minimaAt"]])
          # fit <- augment(model) %>% arrange(result[["minimaAt"]])
          
          # print(fit)
          # highchart() %>%
          #   hc_xAxis(categories = test$dates) %>%
          #   hc_add_series(data = test$Close) %>%
          #   hc_add_series(
          #     fit, type = "line", hcaes(x = `result[["minimaAt"]]`, y = .fitted),
          #     name = "Fit", id = "fit"
          #   ) %>%
          #   hc_add_series(data = test$final_call, yAxis = 1) %>%
          #   hc_yAxis_multiples(
          #     list(lineWidth = 3, lineColor='#7cb5ec', title=list(text="Price")),
          #     list(lineWidth = 3, lineColor="#434348", title=list(text="Call"))) %>%
          #   hc_exporting(enabled = TRUE, filename = "custom-file-name")
          
          if(!is.null(result[["minima"]])  &&  !is.null(result[["minimaAt"]]))
            abline(lm(result[["minima"]] ~  result[["minimaAt"]]))
          if(!is.null(result[["maxima"]])  &&  !is.null(result[["maximaAt"]]))
            abline(lm(result[["maxima"]] ~  result[["maximaAt"]]))
          
          # print(test$dates)
          # browser()
          # highchart() %>%
          #   hc_xAxis(categories = test$dates) %>%
          #   hc_add_series(data = test$Close) %>%
          #   hc_add_series(
          #     fit, type = "line", hcaes(x = result[["minimaAt"]], y = .fitted),
          #     name = "Fit", id = "fit"
          #   ) %>%
          #   hc_add_series(data = test$final_call, yAxis = 1) %>%
          #   hc_yAxis_multiples(
          #     list(lineWidth = 3, lineColor='#7cb5ec', title=list(text="Price")),
          #     list(lineWidth = 3, lineColor="#434348", title=list(text="Call"))) %>% 
          #   hc_exporting(enabled = TRUE, filename = "custom-file-name")
        }
        # ts.plot(timeSeries)
        
        return(result)    
      }
      
      
      stocks_ts <- ts(result)
      
      testing <- detectSupportResistance(stocks_ts)
      
      
    })
    
  })
  
  output$Risk_reward_ratio<- renderText({ 
    
    stop_loss <- input$stop_loss
    
    entry_point <- input$entry_point
    
    target <- input$target
    
    market_direction <- as.numeric(input$market_direction)
    
    risk <- round(market_direction*(entry_point - stop_loss),2)
    
    reward <- round(market_direction*(target - entry_point),2)
    
    HTML(paste("Risk : Reward => ",risk," : ",reward, sep = ""))
  })
  
  output$Quantity<- renderText({ 
    
    stop_loss <- input$stop_loss
    
    entry_point <- input$entry_point
    
    market_direction <- as.numeric(input$market_direction)
    
    Capital <- input$capital
    
    qty <- round(market_direction*((2/100)*Capital/(entry_point - stop_loss)),0)
    
    HTML(paste("Quantity to Trade => ",qty, sep = ""))
  })
  
  output$Profit_book<- renderText({ 
    
    stop_loss <- input$stop_loss
    
    entry_point <- input$entry_point
    
    target <- input$target
    
    market_direction <- as.numeric(input$market_direction)
    
    Capital <- input$capital
    
    qty <- round(market_direction*((2/100)*Capital/(entry_point - stop_loss)),0)
    
    profit <- round(qty*(target - entry_point),0)
    
    HTML(paste("Profit = ",profit, sep = ""))
  })
  
  output$Loss_book<- renderText({ 
    
    stop_loss <- input$stop_loss
    
    entry_point <- input$entry_point
    
    target <- input$target
    
    market_direction <- as.numeric(input$market_direction)
    
    Capital <- input$capital
    
    qty <- round(market_direction*((2/100)*Capital/(entry_point - stop_loss)),0)
    
    loss <- round(qty*(stop_loss - entry_point),0)
    
    HTML(paste("Loss = ",loss, sep = ""))
  })
  
  info_keeper <- reactiveValues(
    input_info = list()
  )
  
  stocks_info <- list()
  final_stocks_risk <- list()
  
  observeEvent(input$corr_btn, {
    # browser()
    # copy the current contents to info_keeper
    isolate(
      {
        for (i in seq_along(info_keeper$input_info))
        {
          id <- info_keeper$input_info[[i]][1]
          info_keeper$input_info[[i]][3] <- input[[id]]
        }
      })
    
    
    
    
    # add new text input to the info_keeper
    isolate(
      {
        newid <- paste(
          "text", isolate(length(info_keeper$input_info)) + 1, sep = "")
        info_keeper$input_info <- c(
          info_keeper$input_info, list(c(newid, input$name, "")))
      })
    
    # invoke the update of the text inputs
    info_keeper
  })
  
  output$newInputs <- renderUI({
    # browser()
    lapply(info_keeper$input_info, function(a)
      textInput(a[1], a[2], value = a[3]))
  })
  
  
  
  observeEvent(input$corr_action,{
    
    if(input$portfolio_tabs == "Correlation Analysis"){
  
  output$correlation_plot <- renderPlot({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
      
      # selected_stocks <- input$corr_input
      # 
      # # browser()
      # 
      # isolate({
      #     for (i in seq_along(info_keeper$input_info))
      #     {
      #       id <- info_keeper$input_info[[i]][1]
      #       selected_stocks <- c(selected_stocks,input[[id]])
      #       
      #     }
      #   })
      # # browser()
      # 
      # symbols <- selected_stocks
      symbols <- c(input$portfolio_stock1,input$portfolio_stock2,input$portfolio_stock3,input$portfolio_stock4,input$portfolio_stock5,input$portfolio_stock6,input$portfolio_stock7,input$portfolio_stock8,input$portfolio_stock9,input$portfolio_stock10)
      
      getSymbols(symbols, src = "yahoo", from = as.Date(input$corr_daterange[1]), to = as.Date(input$corr_daterange[2]), auto.assign = TRUE)
      # browser()
      
      # prices <- list()
      # for(i in 1:length(symbols)){
      #   prices[[i]] <- Ad(get(symbols[i]))
      # }
      # prices <- do.call(cbind, prices)
      # colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
      # 
      # returns <- Return.calculate(prices)
      # returns <- na.omit(returns)
      # 
      # result <- table.Correlation(returns, returns)
      # result <- result[,1]
      # 
      # corMatrix <- matrix(result, nrow=length(symbols), ncol=length(symbols), byrow=T)
      # corMatrix <- round(corMatrix, digits=2)
      # 
      # dimnames(corMatrix) = list(colnames(prices), colnames(prices))
      
      ClosePrices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
      corMatrix <- cor(as.matrix(ClosePrices), method = "pearson", use = "complete.obs")
      
      ggcorrplot(corMatrix, hc.order = TRUE, type = "lower",
                 lab = TRUE)
      
    })
    
  })
    }else if(input$portfolio_tabs == "Optimised Weights"){
  
  # selected_stocks <- input$corr_input
  
  # browser()
  
  # isolate({
  #   for (i in seq_along(info_keeper$input_info))
  #   {
  #     id <- info_keeper$input_info[[i]][1]
  #     selected_stocks <- c(selected_stocks,input[[id]])
  #     
  #   }
  # })
  # browser()
  # 
  # symbols <- selected_stocks
  
  symbols <- c(input$portfolio_stock1,input$portfolio_stock2,input$portfolio_stock3,input$portfolio_stock4,input$portfolio_stock5,input$portfolio_stock6,input$portfolio_stock7,input$portfolio_stock8,input$portfolio_stock9,input$portfolio_stock10)
  
  myStocks <-lapply(symbols,function(x) {getSymbols(x,from = as.Date(input$corr_daterange[1]),to = as.Date(input$corr_daterange[2]),periodicity = "daily",auto.assign=FALSE)} )
  
  names(myStocks) <- symbols
  
  adjustedPrices <- lapply(myStocks, Ad)
  adjustedPrices <- do.call(merge, adjustedPrices)
  
  stockReturns <- Return.calculate(adjustedPrices)
  
  stockReturns <- Return.calculate(adjustedPrices)[-1]
  
  wts <- c(input$portfolio_1_size1,input$portfolio_1_size2,input$portfolio_1_size3,input$portfolio_1_size4,input$portfolio_1_size5,input$portfolio_1_size6,input$portfolio_1_size7,input$portfolio_1_size8,input$portfolio_1_size9,input$portfolio_1_size10)
  
  portReturns <- Return.portfolio(stockReturns, wts)
  
  portReturnsRebalanced <- Return.portfolio(stockReturns, wts, rebalance_on = "months")
  
  # portReturns <- Return.portfolio(stockReturns, c(0.5, 0.5))
  # 
  # portReturnsRebalanced <- Return.portfolio(stockReturns, c(0.5, 0.5), rebalance_on = "months")
  # 
  # allPortReturns <- cbind(portReturns, portReturnsRebalanced)
  # colnames(allPortReturns) <- c("Non-Rebalanced", "Monthly Rebalanced")
  # table.AnnualizedReturns(allPortReturns, Rf = 0.1/252)
  
  port <- portfolio.spec(assets = colnames((stockReturns)))
  
  port <- add.constraint(portfolio = port,
                         type = "full_investment")
  port <- add.constraint(portfolio = port,
                         type = "long_only")
  port <- add.constraint(portfolio = port,
                         type = "box",
                         min = 0.0,
                         max = 0.7)
  
  
  # rportfolios <- random_portfolios(port, permutations = 50000, rp_method = "sample")
  
  portMinVar <- port
  portMinVar <- add.objective(portfolio = portMinVar,
                              type = "risk",
                              name = "StdDev")
  
  
  portMeanVar <- port
  portMeanVar <- add.objective(portfolio = portMeanVar,
                               type = "risk",
                               name = "StdDev")
  portMeanVar <- add.objective(portfolio = portMeanVar,
                               type = "return",
                               name = "mean")
  
  output$port_optimised_returns <- renderPrint({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
      
      # browser()
      
      if(input$port_selection == "minimum-variance"){
        set.seed(10260)
        minVarOpt <- optimize.portfolio(R = stockReturns,
                                        portfolio = portMinVar,
                                        optimize_method = "random")
        
        minVarReturns <- Return.portfolio(stockReturns, weight = extractWeights(minVarOpt), rebalance_on = "months")
        final_table <- table.AnnualizedReturns(R = minVarReturns, Rf = 0.1/250)
        
      }else{
        set.seed(10260)
        meanVarOpt <- optimize.portfolio(R = stockReturns,
                                         portfolio = portMeanVar,
                                         optimize_method = "random")
        
        meanVarReturns <- Return.portfolio(stockReturns, weight = extractWeights(meanVarOpt), rebalance_on = "months")
        
        final_table <- table.AnnualizedReturns(R = meanVarReturns, Rf = 0.1/250)
      }
      
      final_table
      
      
      
    })
    
  })
  
  output$port_optimised_weights <- renderPrint({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
      
      if(input$port_selection == "minimum-variance"){
        set.seed(10260)
        minVarOpt <- optimize.portfolio(R = stockReturns,
                                        portfolio = portMinVar,
                                        optimize_method = "random")
        
        
        final_data <- minVarOpt
        
      }else{
        set.seed(10260)
        meanVarOpt <- optimize.portfolio(R = stockReturns,
                                         portfolio = portMeanVar,
                                         optimize_method = "random")
        
        
        
        final_data <- meanVarOpt
      }
      
      final_data
      
    })
    
  })
  
    }else if(input$portfolio_tabs == "Portfolio Performance"){
  
  output$port_returns_plot<-renderPlotly({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
    
    # browser()
    
    symbols <- c(input$portfolio_stock1,input$portfolio_stock2,input$portfolio_stock3,input$portfolio_stock4,input$portfolio_stock5,input$portfolio_stock6,input$portfolio_stock7,input$portfolio_stock8,input$portfolio_stock9,input$portfolio_stock10)
    
    date_from <- input$corr_daterange[1]
    date_to <- input$corr_daterange[2]
    
    # stock_returns_monthly <- symbols %>%
    #   tq_get(get  = "stock.prices",
    #          from = date_from,
    #          to   = date_to) %>%
    #   group_by(symbol) %>%
    #   tq_transmute(select     = adjusted, 
    #                mutate_fun = periodReturn, 
    #                period     = "monthly", 
    #                col_rename = "Ra")
    
    
    # wts <- c(input$portfolio_1_size1,input$portfolio_1_size2,input$portfolio_1_size3,input$portfolio_1_size4,input$portfolio_1_size5,input$portfolio_1_size6,input$portfolio_1_size7,input$portfolio_1_size8,input$portfolio_1_size9,input$portfolio_1_size10)
    
    # portfolio_growth_monthly <- stock_returns_monthly %>%
    #   tq_portfolio(assets_col   = symbol, 
    #                returns_col  = Ra, 
    #                weights      = wts, 
    #                col_rename   = "investment.growth",
    #                wealth.index = TRUE) %>%
    #   mutate(investment.growth = investment.growth * input$initial_cap)
    
    
    # portfolio_growth_monthly %>%
    #   ggplot(aes(x = date, y = investment.growth)) +
    #   geom_line(size = 2, color = palette_light()[[1]]) +
    #   labs(title = "Portfolio Growth",
    #        
    #        # subtitle = "50% AAPL, 0% GOOG, and 50% NFLX",
    #        subtitle = paste0(input$portfolio_1_size1," of ",input$portfolio_stock1," , ",input$portfolio_1_size2," of ",input$portfolio_stock2," , ",input$portfolio_1_size3," of ",input$portfolio_stock3," , ",input$portfolio_1_size4," of ",input$portfolio_stock4," , ",input$portfolio_1_size5," of ",input$portfolio_stock5," , ",input$portfolio_1_size6," of ",input$portfolio_stock6," , ",input$portfolio_1_size7," of ",input$portfolio_stock7," , ",input$portfolio_1_size8," of ",input$portfolio_stock8," , ",input$portfolio_1_size9," of ",input$portfolio_stock9," , ",input$portfolio_1_size10," of ",input$portfolio_stock10),
    #        caption = "Now we can really visualize performance!",
    #        x = "", y = "Portfolio Value") +
    #   geom_smooth(method = "loess") +
    #   theme_tq() +
    #   scale_color_tq() +
    #   scale_y_continuous(labels = scales::dollar)
    
    weights <- c(
      input$portfolio_1_size1,input$portfolio_1_size2,input$portfolio_1_size3,input$portfolio_1_size4,input$portfolio_1_size5,input$portfolio_1_size6,input$portfolio_1_size7,input$portfolio_1_size8,input$portfolio_1_size9,input$portfolio_1_size10,
      input$portfolio_2_size1,input$portfolio_2_size2,input$portfolio_2_size3,input$portfolio_2_size4,input$portfolio_2_size5,input$portfolio_2_size6,input$portfolio_2_size7,input$portfolio_2_size8,input$portfolio_2_size9,input$portfolio_2_size10
    )
    
    # stocks <- c("AAPL", "GOOG", "NFLX")
    weights_table <-  tibble(symbols) %>%
      tq_repeat_df(n = 2) %>%
      bind_cols(tibble(weights)) %>%
      group_by(portfolio)
    
    # weights_table
    
    stock_returns_monthly <- symbols %>%
      tq_get(get  = "stock.prices",
             from = date_from,
             to   = date_to) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Ra")
    
    stock_returns_monthly_multi <- stock_returns_monthly %>%
      tq_repeat_df(n = 2)
    stock_returns_monthly_multi
    
    portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = Ra, 
                   weights      = weights_table, 
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = investment.growth * input$initial_cap)
    
    # p <- portfolio_growth_monthly_multi %>%
    #   ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
    #   # geom_line(size = 2) +
    #   geom_line(colour = "grey", aes(date, investment.growth)) +
    #   labs(title = "Portfolio Growth",
    #        subtitle = "Comparing Multiple Portfolios",
    #        caption = "Portfolio 3 is a Standout!",
    #        x = "", y = "Portfolio Value",
    #        color = "Portfolio") +
    #   geom_smooth(method = "loess") +
    #   theme_tq() +
    #   scale_color_tq() +
    #   scale_y_continuous(labels = scales::dollar)
    
    # browser()
    p <- ggplot(portfolio_growth_monthly_multi, aes(x = date, y = investment.growth)) +  
      geom_line(size = 1, aes(color = factor(portfolio)))+
      labs(title = "Portfolio Growth",
           subtitle = "Comparing Multiple Portfolios",
           caption = "Portfolio 3 is a Standout!",
           x = "", y = "Portfolio Value",
           color = "Portfolio") +
      scale_y_continuous(labels = scales::dollar)

    ggplotly(p)
    
  })
  })
  output$port_mom_returns_plot<-renderPlotly({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
    # browser()
    symbols <- c(input$portfolio_stock1,input$portfolio_stock2,input$portfolio_stock3,input$portfolio_stock4,input$portfolio_stock5,input$portfolio_stock6,input$portfolio_stock7,input$portfolio_stock8,input$portfolio_stock9,input$portfolio_stock10)
    
    date_from <- input$corr_daterange[1]
    date_to <- input$corr_daterange[2]
    
    weights <- c(
      input$portfolio_1_size1,input$portfolio_1_size2,input$portfolio_1_size3,input$portfolio_1_size4,input$portfolio_1_size5,input$portfolio_1_size6,input$portfolio_1_size7,input$portfolio_1_size8,input$portfolio_1_size9,input$portfolio_1_size10
    )
    
    # stocks <- c("AAPL", "GOOG", "NFLX")
    weights_table <-  tibble(symbols) %>%
      tq_repeat_df(n = 1) %>%
      bind_cols(tibble(weights)) %>%
      group_by(portfolio)
    
    # weights_table
    
    stock_returns_monthly <- symbols %>%
      tq_get(get  = "stock.prices",
             from = date_from,
             to   = date_to) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Ra")
    
    p <- stock_returns_monthly %>%
      ggplot(aes(x = date, y = Ra)) +
      geom_bar(stat = "identity", fill = palette_light()[[1]]) +
      labs(title = "Portfolio Returns",
           subtitle = "50% AAPL, 0% GOOG, and 50% NFLX",
           caption = "Shows an above-zero trend meaning positive returns",
           x = "", y = "Monthly Returns") +
      geom_smooth(method = "lm") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::percent)
    
    ggplotly(p)
  
  })
    })
  
  output$port_distribution <- renderPlotly({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
    
    # browser()
    symbols <- c(input$portfolio_stock1,input$portfolio_stock2,input$portfolio_stock3,input$portfolio_stock4,input$portfolio_stock5,input$portfolio_stock6,input$portfolio_stock7,input$portfolio_stock8,input$portfolio_stock9,input$portfolio_stock10)
    
    weights1 <- c(
      input$portfolio_1_size1,input$portfolio_1_size2,input$portfolio_1_size3,input$portfolio_1_size4,input$portfolio_1_size5,input$portfolio_1_size6,input$portfolio_1_size7,input$portfolio_1_size8,input$portfolio_1_size9,input$portfolio_1_size10
    )
    
    weights2 <- c(
      input$portfolio_2_size1,input$portfolio_2_size2,input$portfolio_2_size3,input$portfolio_2_size4,input$portfolio_2_size5,input$portfolio_2_size6,input$portfolio_2_size7,input$portfolio_2_size8,input$portfolio_2_size9,input$portfolio_2_size10
    )
    
    port1 <- data.frame(symbols,weights1)
    
    port1 <- port1 %>% filter(weights1 > 0)
    
    port2 <- data.frame(symbols,weights2)
    
    port2 <- port2 %>% filter(weights2 > 0)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = port1, labels = ~symbols, values = ~weights1,
                           name = "Portfolio 1", domain = list(x = c(0, 0.5), y = c(0.5, 1)))
    fig <- fig %>% add_pie(data = port2, labels = ~symbols, values = ~weights2,
                           name = "Portfolio 2", domain = list(x = c(0.5, 1), y = c(0.5, 1)))
    # fig <- fig %>% layout(title = "Portfolio Distributions", showlegend = F,
    #                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
  })
  })
  
    }else if(input$portfolio_tabs == "Portfolio Metrics"){
  
  output$port_optimised_metrics <- DT::renderDataTable({
    
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1, detail = paste("Doing part"))
    
    symbols <- c(input$portfolio_stock1,input$portfolio_stock2,input$portfolio_stock3,input$portfolio_stock4,input$portfolio_stock5,input$portfolio_stock6,input$portfolio_stock7,input$portfolio_stock8,input$portfolio_stock9,input$portfolio_stock10)
    
    date_from <- input$corr_daterange[1]
    date_to <- input$corr_daterange[2]
    
    # browser()
    
    Ra <- symbols %>%
      tq_get(get  = "stock.prices",
             from = date_from,
             to   = date_to) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Ra")
    
    Rb <- input$port_bench_mark %>%
      tq_get(get  = "stock.prices",
             from = date_from,
             to   = date_to) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Rb")
    
    RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
    
    
    stock_returns_monthly <- symbols %>%
      tq_get(get  = "stock.prices",
             from = date_from,
             to   = date_to) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Ra")
    
    baseline_returns_monthly <- input$port_bench_mark %>%
      tq_get(get  = "stock.prices",
             from = date_from,
             to   = date_to) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Rb")
    
    wts <- c(input$portfolio_1_size1,input$portfolio_1_size2,input$portfolio_1_size3,input$portfolio_1_size4,input$portfolio_1_size5,input$portfolio_1_size6,input$portfolio_1_size7,input$portfolio_1_size8,input$portfolio_1_size9,input$portfolio_1_size10)
    
    portfolio_returns_monthly <- stock_returns_monthly %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = Ra, 
                   weights     = wts, 
                   col_rename  = "Ra")
    
    RaRb_single_portfolio <- left_join(portfolio_returns_monthly, 
                                       baseline_returns_monthly,
                                       by = "date")
    
    portfolio_final <- data.frame("ActivePremium"=numeric(0),
                                  "Alpha"=numeric(0),
                                  "Beta"=numeric(0),
                                  "Correlation"=numeric(0),
                                  "ESSharpe"=numeric(0),
                                  "StdDevSharpe"=numeric(0),
                                  "VaRSharpe"=numeric(0),
                                  "ArithmeticMean"=numeric(0),
                                  "GeometricMean"=numeric(0),
                                  "Kurtosis"=numeric(0),
                                  "LCLMean"=numeric(0),
                                  "UCLMean"=numeric(0),
                                  "Maximum"=numeric(0),
                                  "Median"=numeric(0),
                                  "Minimum"=numeric(0),
                                  "SEMean"=numeric(0),
                                  "Skewness"=numeric(0),
                                  "Stdev"=numeric(0),
                                  "Variance"=numeric(0),
                                  "AnnualizedReturn"=numeric(0),
                                  "AnnualizedSharpe"=numeric(0),
                                  "AnnualizedStdDev"=numeric(0),
                                  "MaximumDrawdown"=numeric(0),
                                  "GainDeviation"=numeric(0),
                                  "LossDeviation"=numeric(0),
                                  "Annualiseddownsiderisk"=numeric(0),
                                  "Downsidepotential"=numeric(0),
                                  "monthlydownsiderisk"=numeric(0),
                                  "Omega"=numeric(0),
                                  "Sortinoratio"=numeric(0),
                                  "Upsidepotential"=numeric(0),
                                  "BetaCoKurtosis"=numeric(0),
                                  "BetaCoSkewness"=numeric(0),
                                  "BetaCoVariance"=numeric(0),
                                  "CoKurtosis"=numeric(0),
                                  "CoSkewness"=numeric(0),
                                  "BetaCoSkewness"=numeric(0),
                                  "AnnualisedTrackingError"=numeric(0),
                                  "InformationRatio"=numeric(0),
                                  "TrackingError"=numeric(0),
                                  "VaR"=numeric(0)
    )
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
    
    
    
    portfolio_final[1,"ActivePremium"] <- round(temp$ActivePremium,2)
    portfolio_final[1,"Alpha"] <- round(temp$Alpha,2)
    portfolio_final[1,"Beta"] <- round(temp$Beta,2)
    portfolio_final[1,"Correlation"] <- round(temp$Correlation,2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = SharpeRatio)
    
    portfolio_final[1,"ESSharpe"] <- round(temp[,1],2)
    portfolio_final[1,"StdDevSharpe"] <- round(temp[,2],2)
    portfolio_final[1,"VaRSharpe"] <- round(temp[,3],2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats)
    
    portfolio_final[1,"ArithmeticMean"] <- round(temp$ArithmeticMean,2)
    portfolio_final[1,"GeometricMean"] <- round(temp$GeometricMean,2)
    portfolio_final[1,"Kurtosis"] <- round(temp$Kurtosis,2)
    portfolio_final[1,"LCLMean"] <- round(temp$`LCLMean(0.95)`,2)
    portfolio_final[1,"UCLMean"] <- round(temp$`UCLMean(0.95)`,2)
    portfolio_final[1,"Maximum"] <- round(temp$Maximum,2)
    portfolio_final[1,"Median"] <- round(temp$Median,2)
    portfolio_final[1,"Minimum"] <- round(temp$Minimum,2)
    
    portfolio_final[1,"SEMean"] <- round(temp$SEMean,2)
    portfolio_final[1,"Skewness"] <- round(temp$Skewness,2)
    portfolio_final[1,"Stdev"] <- round(temp$Stdev,2)
    portfolio_final[1,"Variance"] <- round(temp$Variance,2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)
    
    portfolio_final[1,"AnnualizedReturn"] <- round(temp$AnnualizedReturn,2)
    portfolio_final[1,"AnnualizedSharpe"] <- round(temp$`AnnualizedSharpe(Rf=0%)`,2)
    portfolio_final[1,"AnnualizedStdDev"] <- round(temp$AnnualizedStdDev,2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRisk)
    
    portfolio_final[1,"MaximumDrawdown"] <- round(temp$MaximumDrawdown,2)
    portfolio_final[1,"GainDeviation"] <- round(temp$GainDeviation,2)
    portfolio_final[1,"LossDeviation"] <- round(temp$LossDeviation,2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRiskRatio)
    
    portfolio_final[1,"Annualiseddownsiderisk"] <- round(temp$Annualiseddownsiderisk,2)
    portfolio_final[1,"Downsidepotential"] <- round(temp$Downsidepotential,2)
    portfolio_final[1,"monthlydownsiderisk"] <- round(temp$monthlydownsiderisk,2)
    portfolio_final[1,"Omega"] <- round(temp$Omega,2)
    portfolio_final[1,"Sortinoratio"] <- round(temp$Sortinoratio,2)
    portfolio_final[1,"Upsidepotential"] <- round(temp$Upsidepotential,2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.HigherMoments)
    
    portfolio_final[1,"BetaCoKurtosis"] <- round(temp$BetaCoKurtosis,2)
    portfolio_final[1,"BetaCoSkewness"] <- round(temp$BetaCoSkewness,2)
    portfolio_final[1,"BetaCoVariance"] <- round(temp$BetaCoVariance,2)
    portfolio_final[1,"CoKurtosis"] <- round(temp$CoKurtosis,2)
    portfolio_final[1,"CoSkewness"] <- round(temp$CoSkewness,2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.InformationRatio)
    
    portfolio_final[1,"AnnualisedTrackingError"] <- round(temp$AnnualisedTrackingError,2)
    portfolio_final[1,"InformationRatio"] <- round(temp$InformationRatio,2)
    portfolio_final[1,"TrackingError"] <- round(temp$TrackingError,2)
    
    temp <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR)
    
    portfolio_final[1,"VaR"] <- round(temp$VaR,2)
    
    
    
    
    
    DT::datatable(t(portfolio_final),extensions = c('FixedColumns'),
                  options = list(scrollX = TRUE,
                                 pageLength=20,
                                 searchHighlight = TRUE,
                                 filter = 'top'
                  ))
  })
  })
    }else if(input$portfolio_tabs == "Risk Vs Returns"){
      
      withProgress(message = 'Running for 500000 Random Portfolios', value = 0, {
        incProgress(1, detail = paste("Please wait"))
        
        # browser()

      symbols <- c(input$portfolio_stock1,input$portfolio_stock2,input$portfolio_stock3,input$portfolio_stock4,input$portfolio_stock5,input$portfolio_stock6,input$portfolio_stock7,input$portfolio_stock8,input$portfolio_stock9,input$portfolio_stock10)
      
      date_from <- input$corr_daterange[1]
      date_to <- input$corr_daterange[2]
      
      getSymbols(symbols, from=date_from, to = date_to)
      
      ClosePrices <- lapply(symbols, function(x) Ad(get(x)))
      
      prices.data <- ClosePrices[1][[1]]
      
      
      for(i in 2:length(ClosePrices)){
        prices.data <- merge.zoo(prices.data,ClosePrices[i][[1]])
      }
      
      colnames(prices.data) <- symbols
      
      returns.data <- sapply(prices.data, CalculateReturns)
      returns.data <- na.omit(returns.data)
      
      # Save mean return vector and sample covariance matrix
      meanReturns <- colMeans(returns.data)
      covMat <- cov(returns.data)
      
      # Start with the names of the assets
      p <- portfolio.spec(assets = colnames(returns.data))
      
      # Box
      p <- add.constraint(p, type = "box", min = 0.05, max = 0.8)
      # Leverage
      p <- add.constraint(portfolio = p, type = "full_investment")
      
      # Generate random portfolios
      randomport<- random_portfolios(p, permutations = 50000, rp_method = "sample")
      
      
      p <- add.constraint(portfolio = p, type = "full_investment")
      p <- add.constraint(p, type="long_only")
      # Get minimum variance portfolio
      minvar.port <- add.objective(p, type = "risk", name = "var")
      
      # Optimize
      minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                       rp = randomport)
      
      # Generate maximum return portfolio
      maxret.port <- add.objective(p, type = "return", name = "mean")
      
      # Optimize
      maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                       rp = randomport)
      
      # Generate vector of returns
      #minret <- 0.02/100
      minret <- min(meanReturns)
      maxret <- max(meanReturns)
      #maxret <- maxret.opt$weights %*% meanReturns
      
      vec <- seq(minret, maxret, length.out = 100)
      
      eff.frontier <- data.frame(Risk =vector("numeric", length(vec)) ,
                                 Return = vector("numeric", length(vec)))
      
      frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
      colnames(frontier.weights) <- colnames(returns.data)
      
      for(i in 1:length(vec)){
        
        # Creates a new portfolio object using p and adds mean as an objective
        
        p <- add.constraint(p, type = "return", name = "mean", return_target = vec[i])
        
        # Creates a new portfolio object using p and adds var as an objective
        p <- add.objective(p, type = "risk", name = "var")
        
        # Creates a new portfolio object using p and adds a weight_concentration
        # objective. The conc_aversion parameter controls how much concentration is
        # penalized. The portfolio concentration is defined as the Herfindahl Hirschman
        # Index of the weights.
        p <- add.objective(p, type = "weight_concentration", name = "HHI",
                           conc_aversion = 0.01)
        
        eff.opt <- optimize.portfolio(returns.data, p, optimize_method = "ROI")
        
        eff.frontier$Risk[i] <- sqrt(t(eff.opt$weights) %*% covMat %*% eff.opt$weights)
        
        eff.frontier$Return[i] <- eff.opt$weights %*% meanReturns
        
        
        
        frontier.weights[i,] = eff.opt$weights
        
        # print(paste(round(i/length(vec) * 100, 0), "% done..."))
        
      }
      
      eff.frontier$Sharperatio <- eff.frontier$Return / eff.frontier$Risk
      
      
      feasible.sd <- apply(randomport, 1, function(x){
        return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
      })
      
      feasible.means <- apply(randomport, 1, function(x){
        return(x %*% meanReturns)
      })
      
      feasible.sr <- feasible.means / feasible.sd
      
      output$port_efficient_frontier <- renderPlotly({
      
      p <- plot_ly() %>%
        add_trace(x = feasible.sd, y = feasible.means, color = feasible.sr, 
                  mode = "markers", type = "scattergl", showlegend = F,
                  
                  marker = list(size = 3, opacity = 0.5, 
                                colorbar = list(title = "Sharpe Ratio"))) %>%
        add_trace(data = eff.frontier, x = ~Risk, y = ~Return,mode = "markers", type = "scattergl")%>% 
        layout(title = "Efficient Frontier",
               yaxis = list(title = "Mean Returns", tickformat = ".2%"),
               xaxis = list(title = "Standard Deviation", tickformat = ".2%"))
      p
      
      })

      output$port_weights_across_frontier <- renderPlotly({
        
        # browser()
        
      d <- as.tibble(frontier.weights)%>%tidyr::gather(Stock,Weights)%>%add_column(Index=rep(1:100,10))
      
      
      p <- plot_ly(d, x = ~Index, y = ~Weights, color = ~Stock, type = "bar") %>%
        layout(title = "Portfolio weights across frontier", barmode = "stack",
               xaxis = list(title = "Index"),
               yaxis = list(title = "Weights(%)", tickformat = ".0%"))
      p
      
      })
      })
      
      
    }
   
    
    
  })
  
  
  
  # contact notice
  output$follow<- renderText({ 
    HTML(paste0("<b> Feel free to reach out to me via below social platforms or write at saitejareddy123@gmail.com with any queries/comments on the dashboard!!</b>"))
  })
  
  output$Disclaimer<- renderText({ 
    HTML(paste0("<b> Disclaimer : This dashboard is purely built for education purpose only!!</b>"))
  })
  
  
}