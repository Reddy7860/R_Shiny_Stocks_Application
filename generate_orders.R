# Importing Libraries

library(smartapi)
library("RJSONIO")
library(dplyr)
library(lubridate)
library(beepr)
library(reticulate)
library("smartapi")
library("sqldf")
library(telegram.bot)
library(anytime)

# gc()

start_time = Sys.time()
print(paste("Started Executing the Script : ",Sys.time(),sep=""))

angel_script <- read.csv('~/Downloads/angel_script.csv')

system("say Running the script")


#Session Creation

login_params = list(api_key = 'LPUVlRxd')
# login_params = list(api_key = 'NsXKahCV')

login_object = create_connection_object(login_params)

user_id = "J95213"

# Sys.sleep(1)

session_data <- generate_session(login_object,user_id,"start@123")
# session_data <- generate_session(login_object,"S970011","Welcome@123")


nse_data <- data.frame(c("BANKNIFTY","Nifty50"),c("%5ENSEBANK","%5ENSEI"),c("BANKNIFTY-EQ","Nifty50-EQ"))

colnames(nse_data) <- c("Symbol","Yahoo Symbol","TradingSymbol")


Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))


increment = 1

bot_strategy <- c("sweths_violation","cowboy","reds_rocket","reds_brahmos","blackout","gap_up","gap_down","volume_breakout","abc_5_cand","abc_3_cand")


# stock <- "%5ENSEBANK"

stock = "%5ENSEI"


for (i in 1:length(bot_strategy)) {
  
  # print(bot_strategy[i])
  currnet_time_min = format(Sys.time(),format = "%H:%M:%S")
  
  if(bot_strategy[i] == "sweths_violation"){
    
    currnet_time_min = format(Sys.time(),format = "%H:%M:%S")
    
    if(currnet_time_min >= "09:40:00"){
      
      print("violation strategy")
      
      # browser()
      
      for(i in 1:nrow(nse_data)){
        stock = nse_data[i,2]
        
        tryCatch(
          {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          },
          error = function(cond){
            print(cond)
            
            # Sys.sleep(1)
            
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
        )
        
        stock_timestamp <- response_data$chart$result[[1]]$timestamp
        Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
        High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
        Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
        Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
        Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        # browser()
        
        if(nrow(final_data) == 0){
          
          stock_timestamp <- response_data$chart$result$timestamp[[1]]
          Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
          High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
          Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
          Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
          Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
          
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          
        }
        
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
        }else{
          final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
        }
        final_data <- na.omit(final_data)
        
        
        if(weekdays(Sys.Date()) == "Sunday"){
          final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -2 ,]
        }else if(weekdays(Sys.Date()) == "Saturday"){
          final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
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
              # print(stock)
              # print("Moving up")
            }
          }
          else if(stage == "Red"){
            if(final_data[j,"Close"] > trigger_price){
              satisfied_df = rbind(satisfied_df,final_data[j,])
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
          Signal_df[increment,"Datetime"]=satisfied_df[1,"dates"]
          Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
          # Signal_df[increment,"RSI"]=satisfied_df[1,"rsi"]
          # Signal_df[increment,"VWAP"]=satisfied_df[1,"vwap"]
          # Signal_df[increment,"SMA_35"]=satisfied_df[1,"sma_35"]
          
          increment = increment + 1
        }
        
      }
      
    }
  }
  else if(bot_strategy[i] == "cowboy"){
    
    print("cowboy strategy")
    
    final_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/cowboy_data.csv", sep = ""))
    
    final_levels_df <- subset(final_levels_df, select = -c(X) )
    
    
    
    for(i in 1:nrow(nse_data)){
      stock = nse_data[i,2]
      # print(stock)
      
      tryCatch(
        {
          response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        },
        error = function(cond){
          print(cond)
          
          # Sys.sleep(1)
          
          response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        }
      )
      
      
      # browser()
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      
      
      if(nrow(final_data) == 0){
        
        stock_timestamp <- response_data$chart$result$timestamp[[1]]
        Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
        High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
        Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
        Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
        Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
        
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
        
      }
      
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
      
      
      if(weekdays(Sys.Date()) == "Sunday"){
        final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
      }else if(weekdays(Sys.Date()) == "Saturday"){
        final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
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
  else if(bot_strategy[i] == "reds_rocket"){
    
    print("rocket strategy")
    
    final_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/reds_rocket.csv", sep = ""))
    
    final_levels_df <- subset(final_levels_df, select = -c(X) )
    
    for(i in 1:nrow(nse_data)){
      stock = nse_data[i,2]
      # print(stock)
      
      temp_final_levels_df <- final_levels_df[final_levels_df$Stock == stock,]
      
      if(nrow(temp_final_levels_df) > 0){
        
        
        for(i in 1:nrow(temp_final_levels_df)){
          stock = temp_final_levels_df[i,"Stock"]
          # print(stock)
          
          tryCatch(
            {
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            },
            error = function(cond){
              print(cond)
              
              # Sys.sleep(1)
              
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
          )
          
          stock_timestamp <- response_data$chart$result[[1]]$timestamp
          Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
          High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
          Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
          Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
          Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
          
          # browser()
          
          if(nrow(final_data) == 0){
            
            stock_timestamp <- response_data$chart$result$timestamp[[1]]
            Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
            High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
            Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
            Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
            Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
            
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
            
          }
          
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
          
          
          if(weekdays(Sys.Date()) == "Sunday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
          }else if(weekdays(Sys.Date()) == "Saturday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
          }else{
            if(hour(Sys.time()) >= 9){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
            }else{
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
            }
            
          }
          
          
          # print(final_data)
          
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
    
    
    
    
  }
  else if(bot_strategy[i] == "reds_brahmos"){
    
    print("Brahmos strategy")
    
    final_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/reds_brahmos.csv", sep = ""))
    
    
    final_levels_df <- subset(final_levels_df, select = -c(X) )
    
    for(i in 1:nrow(nse_data)){
      stock = nse_data[i,2]
      # print(stock)
      
      temp_final_levels_df <- final_levels_df[final_levels_df$Stock == stock,]
      
      if(nrow(temp_final_levels_df) > 0){
        
        
        
        for(i in 1:nrow(temp_final_levels_df)){
          stock = temp_final_levels_df[i,"Stock"]
          # print(stock)
          
          tryCatch(
            {
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            },
            error = function(cond){
              print(cond)
              
              # Sys.sleep(1)
              
              response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
            }
          )
          
          
          stock_timestamp <- response_data$chart$result[[1]]$timestamp
          Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
          High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
          Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
          Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
          Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
          
          # browser()
          
          if(nrow(final_data) == 0){
            
            stock_timestamp <- response_data$chart$result$timestamp[[1]]
            Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
            High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
            Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
            Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
            Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
            
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
            
          }
          
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
          
          
          
          
          
          if(weekdays(Sys.Date()) == "Sunday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
          }else if(weekdays(Sys.Date()) == "Saturday"){
            final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
          }else{
            if(hour(Sys.time()) >= 9){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
            }else{
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
            }
            
          }
          
          # print(final_data)
          
          final_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          for(j in 1:nrow(final_data)){
            
            
            if((final_data[j,"Close"]) > final_levels_df[i,"Reds_High"]){
              # browser()
              # print(satisfied_df)
              satisfied_df = rbind(satisfied_df,final_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Buy")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
            }
            else if((final_data[j,"Close"]) < final_levels_df[i,"Reds_Low"]){
              # browser()
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
    
    
  }
  else if(bot_strategy[i] == "blackout"){
    
    print("Blackout strategy")
    
    final_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/blackout.csv", sep = ""))
    
    
    final_levels_df <- subset(final_levels_df, select = -c(X))
    
    
    for(i in 1:nrow(nse_data)){
      stock = nse_data[i,2]
      # print(stock)
      
      temp_final_levels_df <- final_levels_df[final_levels_df$Stock == stock,]
      
      if(nrow(temp_final_levels_df) > 0){
        # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
        
        # increment = 1
        
        if(nrow(temp_final_levels_df) > 0){
          
          for(i in 1:nrow(temp_final_levels_df)){
            stock = temp_final_levels_df[i,"Stock"]
            # print(stock)
            
            tryCatch(
              {
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              },
              error = function(cond){
                print(cond)
                
                # Sys.sleep(1)
                
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            )
            
            
            stock_timestamp <- response_data$chart$result[[1]]$timestamp
            Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
            High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
            Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
            Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
            Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
            
            # browser()
            
            if(nrow(final_data) == 0){
              
              stock_timestamp <- response_data$chart$result$timestamp[[1]]
              Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
              High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
              Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
              Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
              Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
              
            }
            
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
            
            
            if(weekdays(Sys.Date()) == "Sunday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
            }else{
              if(hour(Sys.time()) >= 9){
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
              }else{
                final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
              }
              
            }
            
            
            
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
  }
  else if(bot_strategy[i] == "gap_up"){
    
    if(currnet_time_min >= "09:35:00"){
      
      print("Gapup strategy")
      
      
      
      final_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/gaps_strategy.csv", sep = ""))
      
      
      final_levels_df <- subset(final_levels_df, select = -c(X))
      
      for(i in 1:nrow(nse_data)){
        
        # browser()
        
        stock = nse_data[i,2]
        # print(stock)
        
        temp_final_levels_df <- final_levels_df[final_levels_df$Stock == stock,]
        
        row.names(temp_final_levels_df) <- NULL
        
        # print(temp_final_levels_df)
        
        if(nrow(temp_final_levels_df) > 0){
          
          
          for(ind in 1:nrow(temp_final_levels_df)){
            
            # browser()
            
            stock = temp_final_levels_df[ind,"Stock"]
            high_price = temp_final_levels_df[ind,"Previous_High"]
            close_price = temp_final_levels_df[ind,"Previous_Close"]
            
            
            tryCatch(
              {
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              },
              error = function(cond){
                print(cond)
                
                # Sys.sleep(1)
                
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            )
            
            
            stock_timestamp <- response_data$chart$result[[1]]$timestamp
            Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
            High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
            Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
            Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
            Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
            final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
            
            # browser()
            
            if(nrow(final_data) == 0){
              
              stock_timestamp <- response_data$chart$result$timestamp[[1]]
              Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
              High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
              Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
              Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
              Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
              
            }
            
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
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
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
            if(open_price > temp_final_levels_df[ind,"Previous_Close"]){
              # print(stock)
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
      
    }
  }else if(bot_strategy[i] == "gap_down"){
    
    if(currnet_time_min >= "09:35:00"){
      
      print("Gap down strategy")
      
      final_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/gaps_strategy.csv", sep = ""))
      
      
      final_levels_df <- subset(final_levels_df, select = -c(X))
      
      for(i in 1:nrow(nse_data)){
        stock = nse_data[i,2]
        # print(stock)
        
        temp_final_levels_df <- final_levels_df[final_levels_df$Stock == stock,]
        row.names(temp_final_levels_df) <- NULL
        
        if(nrow(temp_final_levels_df) > 0){
          # Signal_df = data.frame("Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
          
          # increment = 1
          
          # if(nrow(final_levels_df) > 0){
          
          
          
          for(ind in 1:nrow(temp_final_levels_df)){
            
            # browser()
            
            stock = temp_final_levels_df[ind,"Stock"]
            high_price = temp_final_levels_df[ind,"Previous_High"]
            close_price = temp_final_levels_df[ind,"Previous_Close"]
            prev_low_price = temp_final_levels_df[ind,"Previous_Low"]
            
            tryCatch(
              {
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              },
              error = function(cond){
                print(cond)
                
                # Sys.sleep(1)
                
                response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
              }
            )
            
            
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
              
              stock_timestamp <- response_data$chart$result$timestamp[[1]]
              Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
              High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
              Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
              Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
              Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
              
              final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
              
            }
            
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
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
            }else if(weekdays(Sys.Date()) == "Saturday"){
              final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
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
            
            
            if(open_price < temp_final_levels_df[ind,"Previous_Close"]){
              # print(stock)
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
      
    }
  }else if(bot_strategy[i] == "volume_breakout"){
    # browser()
    
    # Sys.sleep(1)
    
    if(currnet_time_min >= "09:20:00"){
      print("Volume Breakout strategy")
      
      nifty_50_data <- nse_data
      
      for(ind in 1:nrow(nifty_50_data)){
        
        stock = nifty_50_data[ind,2]
        
        # print(stock)
        
        if(stock == "%5ENSEI"){
          
          # Sep Futures
          # response_data <- get_candle_data(object = session_data,
          #                                  exchange = "NFO",
          #                                  symboltoken = "48740",
          #                                  interval = "FIVE_MINUTE",
          #                                  fromdate = paste(Sys.Date()," 09:00",sep=""),
          #                                  todate = paste(Sys.Date()," 15:30",sep=""))
          
          response_data <- get_candle_data(object = session_data,
                                           exchange = "NFO",
                                           symboltoken = "71321",
                                           interval = "FIVE_MINUTE",
                                           fromdate = paste(Sys.Date()," 09:00",sep=""),
                                           todate = paste(Sys.Date()," 15:30",sep=""))
          
        }else{
          # Sep Futures
          # response_data <- get_candle_data(object = session_data,
          #                                  exchange = "NFO",
          #                                  symboltoken = "48738",
          #                                  interval = "FIVE_MINUTE",
          #                                  fromdate = paste(Sys.Date() ," 09:00",sep=""),
          #                                  todate = paste(Sys.Date()," 15:30",sep=""))
          
          response_data <- get_candle_data(object = session_data,
                                           exchange = "NFO",
                                           symboltoken = "71319",
                                           interval = "FIVE_MINUTE",
                                           fromdate = paste(Sys.Date() ," 09:00",sep=""),
                                           todate = paste(Sys.Date()," 15:30",sep=""))
          
          
          
        }
        
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
            
            if(as.numeric(difftime(Sys.time(), anytime::anytime(final_temp_volume_df[indx,"dates"]), units ="mins")) >= 4){
              
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
    
  }else if(bot_strategy[i] == "abc_5_cand"){
    
    # browser()
    if(currnet_time_min >= "09:40:00"){
      
      print("5 Cand ABC strategy")
      
      nifty_50_data <- nse_data
      
      
      
      for(ind in 1:nrow(nifty_50_data)){
        
        stock = nifty_50_data[ind,2]
        
        # print(stock)
        
        
        tryCatch(
          {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          },
          error = function(cond){
            print(cond)
            
            # Sys.sleep(1)
            
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
        )
        
        
        stock_timestamp <- response_data$chart$result[[1]]$timestamp
        Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
        High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
        Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
        Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
        Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        # browser()
        
        if(nrow(final_data) == 0){
          
          stock_timestamp <- response_data$chart$result$timestamp[[1]]
          Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
          High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
          Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
          Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
          Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
          
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          
        }
        
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
        }else{
          final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
        }
        
        final_data <- na.omit(final_data)
        
        
        
        
        
        if(weekdays(Sys.Date()) == "Sunday"){
          final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
        }else if(weekdays(Sys.Date()) == "Saturday"){
          final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
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
                  
                  # print(final_data[i,"dates"])
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
  }else if(bot_strategy[i] == "abc_3_cand"){
    # print(currnet_time_min)
    if(currnet_time_min >= "09:40:00"){
      
      print("3 Cand ABC strategy")
      
      nifty_50_data <- nse_data
      
      
      for(ind in 1:nrow(nifty_50_data)){
        
        stock = nifty_50_data[ind,2]
        
        # print(stock)
        
        tryCatch(
          {
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          },
          error = function(cond){
            print(cond)
            
            # Sys.sleep(1)
            
            response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
          }
        )
        
        
        stock_timestamp <- response_data$chart$result[[1]]$timestamp
        Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
        High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
        Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
        Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
        Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        # browser()
        
        if(nrow(final_data) == 0){
          
          stock_timestamp <- response_data$chart$result$timestamp[[1]]
          Close <- response_data$chart$result$indicators$quote[[1]]$close[[1]]
          High <- response_data$chart$result$indicators$quote[[1]]$high[[1]]
          Low <- response_data$chart$result$indicators$quote[[1]]$low[[1]]
          Open <- response_data$chart$result$indicators$quote[[1]]$open[[1]]
          Volume <- response_data$chart$result$indicators$quote[[1]]$volume[[1]]
          
          final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
          
        }
        
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
        
        
        if(weekdays(Sys.Date()) == "Sunday"){
          final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
        }else if(weekdays(Sys.Date()) == "Saturday"){
          final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1 ,]
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
      
    }
  }
  
  
}

# browser()


if(nrow(Signal_df) == 0){
  # next
}else{
  
  Signal_df <- Signal_df[!duplicated(Signal_df), ]
  
  ## Get only the 5 minutes completed candles
  Signal_df <- Signal_df[as.numeric(format(Signal_df$Datetime, format = "%M")) %% 5 == 0,]
  
  Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
  
  # Signal_df$Datetime <- Signal_df$Datetime + hm("5:30") 
  # print(Signal_df$Datetime)
  # print(format(Signal_df$Datetime, format = "%M") / 5)
  
  Signal_df <- Signal_df[order(Signal_df$Datetime),]
  
  rownames(Signal_df) <- 1:nrow(Signal_df)
  
  Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
  
  stop_loss <- as.numeric(1)
  target <- as.numeric(1)
  
  #
  Capital <- 10000
  # browser()
  Signal_df$StopLoss <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
  Signal_df$Target <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
  
  # Signal_df$Qty <- round(abs((2/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
  Signal_df$Qty <- round(abs((20/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
  
  Signal_df$Spot_Price <- 0
  Signal_df$expiry <- as.Date("2028-09-20")
  Signal_df$Strike_Buy_Price <- 0
  Signal_df$premium_StopLoss <- 0
  Signal_df$premium_Target <- 0
  Signal_df$lotsize <- 0
  Signal_df$premium_Qty <- 0
  Signal_df$historic_profit <- 0
  Signal_df$current_script <- ""
  Signal_df$token <- 0
  
  Signal_df <- as.data.frame(Signal_df %>%
                               mutate(exec_rnk = order(order(Datetime, decreasing=FALSE))))
  Signal_df$order_place <- 0
  Signal_df$order_id <- 0
  Signal_df$target_order_id <- 0
  
  Signal_df$stop_loss_order_id <- 0
  Signal_df$robo_order_id <- 0
  Signal_df$cancel_order_id <- 0
  Signal_df$final_order_id <- 0
  Signal_df$conclusion <- ""
  
  Signal_df$execution_time = Sys.time()
  Signal_df$target_hit  <- ""
  Signal_df$avg_buy_price  <- ""
  Signal_df$avg_sell_price  <- ""
  Signal_df$avg_qty  <- ""
  Signal_df$adjusted_target  <- ""
  Signal_df$adjusted_stoploss  <- ""
  
  # qty <- round(market_direction*((2/100)*Capital/(entry_point - stop_loss)),0)
  
}







if(nrow(Signal_df) > 0 ){
  source_python('~/Downloads/Reddy_Stocks_Application/get_current_options_data.py')
  
  for (i in 1:nrow(Signal_df)) {
    
    # browser()
    
    side_dir <- ifelse(Signal_df[i,"Signal"] == "Buy","CE","PE")
    
    if(Signal_df[i,"Stock"] == "%5ENSEBANK"){
      
      #### Get the spot price value at the money
      
      Signal_df[i,"Spot_Price"] <- round(ifelse(Signal_df[i,"Value"] %% 100 > 50,(Signal_df[i,"Value"] + (100 - Signal_df[i,"Value"] %% 100)),Signal_df[i,"Value"] - Signal_df[i,"Value"] %% 100),2)
      
      #### Get the two steps in the money
      
      Signal_df[i,"Spot_Price"] <- ifelse(side_dir == "CE", Signal_df[i,"Spot_Price"] - 200, Signal_df[i,"Spot_Price"] + 200)
      
      fetchurl <- paste0("OPTIDXBANKNIFTY","09-12-2021",side_dir,as.character(Signal_df[i,"Spot_Price"]),".00")
      lookup_symbol <- paste0("BANKNIFTY","09DEC21",as.character(Signal_df[i,"Spot_Price"]),side_dir)
      
    }else{
      #### Get the spot price value at the money
      
      Signal_df[i,"Spot_Price"] <- round(ifelse(Signal_df[i,"Value"] %% 100 > 25,(Signal_df[i,"Value"] + (50 - Signal_df[i,"Value"] %% 50)),Signal_df[i,"Value"] - Signal_df[i,"Value"] %% 50),2)
      
      #### Get the two steps in the money
      
      Signal_df[i,"Spot_Price"] <- ifelse(side_dir == "CE", Signal_df[i,"Spot_Price"] - 100, Signal_df[i,"Spot_Price"] + 100)
      
      fetchurl <- paste0("OPTIDXNIFTY","09-12-2021",side_dir,as.character(Signal_df[i,"Spot_Price"]),".00")
      lookup_symbol <- paste0("NIFTY","09DEC21",as.character(Signal_df[i,"Spot_Price"]),side_dir)
    }
    
    Signal_df[i,"expiry"] = as.Date("2021-12-09")
    
    # print(fetchurl)
    # print(lookup_symbol)
    
    current_script <- angel_script[angel_script$symbol == lookup_symbol,]
    
    # print(current_script)
    
    token <- current_script[1,"token"]
    # print(token)
    
    
    tryCatch(
      {
        fetch_current_details(fetchurl)
      },
      error = function(cond){
        print(cond)
        fetch_current_details(fetchurl)
      }
    )
    
    put_data <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/current_option_price.csv", sep = ""))
    
    # print(put_data)
    put_data$Timestamp <- as.POSIXct(as.numeric(as.character(put_data$Timestamp))/1000,origin="1970-01-01") -  hm("5:30")
    
    put_data$Timestamp <- format(put_data$Timestamp, "%Y-%m-%d %H:%M:00")
    
    # print(tail(put_data))
    
    # print(Signal_df[i,"Datetime"])
    
    # browser()
    
    ### This condition is because we are skipping a lot of limit orders. This will make sure we execute the LTP based on the last traded price
    
    if(as.numeric(difftime(Sys.time(), Signal_df[i,"Datetime"], units ="mins")) >= 5){
      ltp <- max(put_data[put_data$Timestamp == Signal_df[i,"Datetime"] + minutes(4),]$Price)
    }else{
      ltp <- max(put_data[put_data$Timestamp == Signal_df[i,"Datetime"],]$Price)
    }
    
    # ltp <- max(put_data[put_data$Timestamp == Signal_df[i,"Datetime"],]$Price)
    
    
    Signal_df[i,"Strike_Buy_Price"] <- ltp
    
    # browser()
    
    stop_loss <- as.numeric(10)
    target <- as.numeric(10)
    
    risk_on_capital <- 0.05
    
    Capital <- 10000
    
    lot_size <- Capital * risk_on_capital / ltp
    
    Signal_df[i,"premium_StopLoss"] <- round(Signal_df[i,"Strike_Buy_Price"]-((stop_loss*Signal_df[i,"Strike_Buy_Price"])/100),2)
    Signal_df[i,"premium_Target"] <- round(Signal_df[i,"Strike_Buy_Price"]+((target*Signal_df[i,"Strike_Buy_Price"])/100),2)
    
    if(Signal_df[i,"Stock"] == "%5ENSEBANK"){
      
      # browser()
      
      # Signal_df[i,"lotsize"] <- 25*round(lot_size,0)
      if(Signal_df[i,"Strategy"] == "Cowboy"){
        bk_test_data <- read.csv("~/Downloads/Backtesting_Aplication/data/Cowboy_BankNifty_Backtest.csv")
      }else if(Signal_df[i,"Strategy"] == "Sweths Violation"){
        bk_test_data <- read.csv("~/Downloads/Backtesting_Aplication/data/Sweths_Violation_BankNifty_Backtest.csv")
      }else if(Signal_df[i,"Strategy"] == "Reds Rocket"){
        bk_test_data <- read.csv("~/Downloads/Backtesting_Aplication/data/Reds_Rocket_BankNifty_Backtest.csv")
      }else if(Signal_df[i,"Strategy"] == "Reds Brahmos"){
        bk_test_data <- read.csv("~/Downloads/Backtesting_Aplication/data/Reds_Brahmos_BankNifty_Backtest.csv")
      }else if(Signal_df[i,"Strategy"] == "Gap_up"){
        bk_test_data <- read.csv("~/Downloads/Backtesting_Aplication/data/Gapup_BankNifty_Backtest.csv")
      }else{
        bk_test_data <- read.csv("~/Downloads/Backtesting_Aplication/data/Gapup_BankNifty_Backtest.csv")
      }
      
      bk_test_data$month <- lubridate::month(anytime(bk_test_data$StartTime), label = TRUE)
      bk_test_data$year <- lubridate::year(anytime(bk_test_data$StartTime))
      bk_test_data$wday <- lubridate::wday(anytime(bk_test_data$StartTime), label = TRUE)
      bk_test_data$hour <- lubridate::hour(anytime(bk_test_data$StartTime))
      
      
      dayHour <- bk_test_data  %>% dplyr::group_by(hour,wday) %>% dplyr::summarise(profits = mean(price_diff, na.rm = TRUE), N = length(wday))
      
      dayHour$sign <- ifelse(dayHour$profits > 0,1,0)
      
      dayHour <- data.frame(dayHour)
      
      execution_week <- as.character(lubridate::wday(Signal_df[i,]$Datetime,label = T))
      execution_hour <- lubridate::hour(Signal_df[i,]$Datetime)
      
      if(any(dayHour == execution_week)){
        temp_dt <- dayHour[dayHour$wday==execution_week,]
        if(any(temp_dt == execution_hour)){
          
          current_profit <- temp_dt[temp_dt$hour == execution_hour,]$profits
          
          if(current_profit > 0){
            
            Signal_df[i,"lotsize"] <- 25*round(Signal_df[i,"Qty"],0)
            Signal_df[i,"premium_Qty"] <- round((abs((20/100)*Capital/( Signal_df[i,"premium_Target"] - Signal_df[i,"premium_StopLoss"]))/25),0)
            Signal_df[i,"historic_profit"] <- 1
            
          }else{
            Signal_df[i,"lotsize"] <- 25
            Signal_df[i,"premium_Qty"] <- round((abs((20/100)*Capital/( Signal_df[i,"premium_Target"] - Signal_df[i,"premium_StopLoss"]))/25),0)
            Signal_df[i,"historic_profit"] <- -1
            
          }
          
        }else{
          Signal_df[i,"lotsize"] <- 25
          Signal_df[i,"premium_Qty"] <- round((abs((20/100)*Capital/( Signal_df[i,"premium_Target"] - Signal_df[i,"premium_StopLoss"]))/25),0)
          Signal_df[i,"historic_profit"] <- -1
        }
        
      }else{
        Signal_df[i,"lotsize"] <- 25
        Signal_df[i,"premium_Qty"] <- round((abs((20/100)*Capital/( Signal_df[i,"premium_Target"] - Signal_df[i,"premium_StopLoss"]))/25),0)
        Signal_df[i,"historic_profit"] <- -1
      }
      
      
      
      
    }else{
      # Signal_df[i,"lotsize"] <- 50*round(lot_size,0)
      Signal_df[i,"lotsize"] <- 50
      Signal_df[i,"premium_Qty"] <- round((abs((20/100)*Capital/( Signal_df[i,"premium_Target"] - Signal_df[i,"premium_StopLoss"]))/50),0)
      Signal_df[i,"historic_profit"] <- -1
    }
    
    
    Signal_df[i,"current_script"] <- lookup_symbol
    Signal_df[i,"token"] <- token
    Signal_df[i,"conclusion"] <- "New Order"
  }
  
}


end_time = Sys.time()
print(paste("Ended Executing the Script : ",Sys.time(),sep=""))

print(difftime(end_time,start_time,units = "secs"))
