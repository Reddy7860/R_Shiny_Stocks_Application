library(smartapi)

##### Reading data from the main file  #####
nifty_50_data <- read.csv("~/Desktop/Reddy_Stocks_Application/data/Nifty50_Stocks.csv")

##### Reading data with Yesterdays information #####
final_levels_df <- read.csv("~/Desktop/Reddy_Stocks_Application/data/gaps_strategy.csv")

final_levels_df <- subset(final_levels_df, select = -c(X))


increment = 1
Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))

if(nrow(final_levels_df) > 0){
  
  for(i in 1:nrow(final_levels_df)){
    
    # browser()
    
    stock = final_levels_df[i,"Stock"]
    high_price = final_levels_df[i,"Previous_High"]
    close_price = final_levels_df[i,"Previous_Close"]
    # print(stock)
    response_data <- fromJSON(paste0("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet",""))
    
    stock_timestamp <- response_data$chart$result[[1]]$timestamp
    Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
    High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
    Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
    Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
    Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
    
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
    }
    else{
      final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
      
      final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
    }
    # print(final_data)
    
    
    final_data$Call <- ""
    
    
    
    satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
    
    open_price = final_data[1,2]
    
    
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
          print(final_data[j,"dates"])
          satisfied_df = rbind(satisfied_df,final_data[j,])
          rownames(satisfied_df) <- 1:nrow(satisfied_df)
          satisfied_df[nrow(satisfied_df),"Call"] <- "BUY"
        }
        else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
          # else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price)){
          # print(current_date)
          print(final_data[j,"dates"])
          satisfied_df = rbind(satisfied_df,final_data[j,])
          rownames(satisfied_df) <- 1:nrow(satisfied_df)
          satisfied_df[nrow(satisfied_df),"Call"] <- "SELL"
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
        
        increment = increment + 1
      }
      else{
        next
      }
    }
  }
  
}


temp_df <- Signal_df

Signal_df <- temp_df

if(nrow(Signal_df) > 0){
  Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
  Signal_df$Datetime <- Signal_df$Datetime
  Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
  stop_loss <- 1
  target <- 0.5
  
  Capital <- 5000
  
  Signal_df$StopLoss <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
  Signal_df$Target <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
  
  Signal_df$Qty <- round(abs((20/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
  
  Signal_df <-Signal_df[order(Signal_df$Datetime),]
  
  row.names(Signal_df) <- 1:nrow(Signal_df)
  
  print(Signal_df)
  
  
  for(i in 1:nrow(Signal_df)){
    if(as.numeric(difftime(Sys.time(), Signal_df[i,4], units ="mins")) <= 4){
      print(Signal_df[i,2])
      
      login_params = list(api_key = 'LPUVlRxd')
      
      login_object = create_connection_object(login_params)
      
      session_data <- generate_session(login_object,"J95213","startteja123")
      
      limit_price = 0
      
      
      
      trading_symbol = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(Signal_df[i,2]),]$TradingSymbol
      symbol_token = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(Signal_df[i,2]),]$Symbol.Token
      
      # if(input$angel_order_type == "LIMIT"){
        
      # }
      # if(Signal_df[i,2]=="NESTLEIND.NS"){
      #   
      #   
      #   
      # }
        # trading_symbol = "NESTLEIND-EQ"
        # symbol_token = "17963"
        transaction_type = Signal_df[i,3]
        # print(transaction_type)
        exchange_counter = "NSE"
        order_type = "LIMIT"
        product_type = "INTRADAY"
        duration_day = "DAY"
        squareoff = Signal_df[i,6]
        
        stoploss = Signal_df[i,7]
        # qty = Signal_df[i,8]
        qty = as.numeric(Signal_df[i,8])
        limit_price = as.numeric((Signal_df[i,5]))
      # }
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

      print(order_place)
    }
  }
}





# 
# order<-order_book(session_data)
# 
# length(order$data)
# 
# View(order$data)
# 
# typeof(order$data)
# 
# df <- data.frame(matrix(unlist(order$data), nrow=length(order$data), byrow=TRUE))
# 
# df
# 
# View(df)



# library(smartapi)
# 
# update.packages("smartapi")
# 
# historicdata<-get_candle_data(session_data,"NSE","3045","ONE_MINUTE","2021-04-13 09:00","2021-04-13 09:20")
# 
# order_book(session_data)
# 
# get_ltp_data(session_data, "NSE", "NESTLEIND-EQ", "17963")
