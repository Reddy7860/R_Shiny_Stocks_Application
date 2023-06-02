library(smartapi)
library("RJSONIO")
library(magrittr)
library(dplyr)
library(lubridate)

nifty_50_data <- read.csv("~/Desktop/Reddy_Stocks_Application/data/Nifty50_Stocks.csv")

increment = 1
Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))

for(ind in 1:nrow(nifty_50_data)){
  
  stock = nifty_50_data[ind,2]
  
  print(stock)
  
  response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
  
  stock_timestamp <- response_data$chart$result[[1]]$timestamp
  Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
  High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
  Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
  Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
  Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
  
  
  # stock_timestamp <- response_data$chart$result[[2]][[1]]
  # Close <- response_data$chart$result[[3]]$quote[[1]]$close
  # High <- response_data$chart$result[[3]]$quote[[1]]$high
  # Low <- response_data$chart$result[[3]]$quote[[1]]$low
  # Open <- response_data$chart$result[[3]]$quote[[1]]$open
  # Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
  
 #  # final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(Close),as.numeric(High),as.numeric(Low),as.numeric(Open),as.numeric(Volume)))
 #  final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
 #  colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
 #  # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
 #  
 # # print(final_data)
  
  final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
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
  
  print(final_data)
  
  # print(final_data)
  
  final_data$Call <- ""
  
  satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
  
  
  for(i in 3:nrow(final_data)){
    
    if((final_data[i,"Close"] > final_data[i,"Open"]) && (final_data[i-1,"Close"] < final_data[i-1,"Open"]) && (final_data[i-2,"Close"] > final_data[i-2,"Open"])){
      if((final_data[i-1,"Low"] > final_data[i-2,"Low"]) && (final_data[i,"Close"] > final_data[i-2,"High"])&& (final_data[i-1,"High"] < final_data[i-2,"High"])){
        first_range = final_data[i-2,"High"] - final_data[i-2,"Low"]
        second_range = final_data[i-1,"High"] - final_data[i-1,"Low"]
        if(first_range/second_range >= 2){
          satisfied_df = rbind(satisfied_df,final_data[i,])
          rownames(satisfied_df) <- 1:nrow(satisfied_df)
          satisfied_df[nrow(satisfied_df),"Call"] <- "BUY"
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
          satisfied_df[nrow(satisfied_df),"Call"] <- "SELL"
        }
        
      }
      
    }
    
  }

  
  if(nrow(satisfied_df) == 0){
    next
  }
  else{
    satisfied_df = tail(satisfied_df,1)
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
      
      increment = increment + 1
    }else{
      next
    }
  }
  
  
  
}

login_params = list(api_key = 'LPUVlRxd')

login_object = create_connection_object(login_params)

session_data <- generate_session(login_object,"J95213","startteja123")


temp_df <- Signal_df

Signal_df <- temp_df

if(nrow(Signal_df) > 0){
  Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
  Signal_df$Datetime <- Signal_df$Datetime
  Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
  stop_loss <- 1
  target <- 0.5
  
  Capital <- 300
  
  Signal_df$StopLoss <- ifelse(Signal_df$Signal == "BUY",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
  Signal_df$Target <- ifelse(Signal_df$Signal == "BUY",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
  
  Signal_df$Qty <- round(abs((20/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
  
  Signal_df <-Signal_df[order(Signal_df$Datetime),]
  
  row.names(Signal_df) <- 1:nrow(Signal_df)
  
  print(Signal_df)
  
  
  for(i in 1:nrow(Signal_df)){
    if(as.numeric(difftime(Sys.time(), Signal_df[i,4], units ="mins")) <= 4){
      print(Signal_df[i,2])
      
      # login_params = list(api_key = 'LPUVlRxd')
      # 
      # login_object = create_connection_object(login_params)
      # 
      # session_data <- generate_session(login_object,"J95213","startteja123")
      
      limit_price = 0
      
      
      
      trading_symbol = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(Signal_df[i,2]),]$TradingSymbol
      symbol_token = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(Signal_df[i,2]),]$Symbol.Token
      
      transaction_type = as.character(Signal_df[i,3])
      exchange_counter = "NSE"
      order_type = "LIMIT"
      product_type = "INTRADAY"
      duration_day = "DAY"
      stoploss = as.numeric(Signal_df[i,6])
      
      squareoff = as.numeric(Signal_df[i,7])
      # qty = Signal_df[i,8]
      qty = as.numeric(Signal_df[i,8])
      limit_price = as.numeric((Signal_df[i,5]))

      order_place <- place_order(object = session_data,
                                 variety= "NORMAL",
                                 tradingsymbol= as.character(trading_symbol),
                                 symboltoken= as.character(symbol_token),
                                 transactiontype= as.character(transaction_type),
                                 exchange= as.character(exchange_counter),
                                 ordertype= as.character(order_type),
                                 producttype= as.character(product_type),
                                 duration= as.character(duration_day),
                                 price= as.numeric(limit_price),
                                 squareoff= as.numeric(squareoff),
                                 stoploss= as.numeric(stoploss),
                                 quantity= as.numeric(qty)
      )
      
      if(order_place > 0){
        if(transaction_type == "BUY"){
          transaction_type = "SELL"
        }else{
          transaction_type = "BUY"
        }
        
        order_place <- place_order(object = session_data,
                                   variety= "NORMAL",
                                   tradingsymbol= as.character(trading_symbol),
                                   symboltoken= as.character(symbol_token),
                                   transactiontype= transaction_type,
                                   exchange= "NSE",
                                   ordertype = "LIMIT",
                                   producttype= "INTRADAY",
                                   duration= "DAY",
                                   price= as.numeric(squareoff),
                                   squareoff= 0,
                                   stoploss= 0,
                                   quantity= qty
        )
        print(order_place)
      }

      # print(order_place)

      
    }
  }
}
