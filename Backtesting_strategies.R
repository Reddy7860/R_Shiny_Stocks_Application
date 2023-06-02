### Loading Libraries
library("RJSONIO")
library(magrittr)
library(dplyr)
library(lubridate)
library("sqldf")
require(sqldf)
library(quantmod)
library(taRifx)


stocks <- c("HDFCBANK.NS","RELIANCE.NS")

final_data_combined = data.frame()

for(stock in stocks){
 
  # browser()
  today = Sys.Date()
  f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
  previousWorkingDay <- f(today)
  stock_data <- na.omit(getSymbols(stock, src = "yahoo", from = "2018-01-01", to = previousWorkingDay, auto.assign = FALSE))
  stock_data <- data.frame(Date = index(stock_data), coredata(stock_data))
  
  
  
  ####  Cowboy strategy Overview
  
  #### Creating the levels at Daily level
  
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
    # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
    stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HCL TECH.csv", sep = ""))
  }else if(stock == "HDFCBANK.NS"){
    stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HDFCBANK.csv", sep = ""))
  }else if(stock == "TECHM.NS"){
    stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - TechM.csv", sep = ""))
  }else if(stock == "TATAMOTORS.NS"){
    stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
  }else if(stock == "UPL.NS"){
    stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - UPL.csv", sep = ""))
  }else if(stock == "GRASIM.NS"){
    stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - GRASIM.csv", sep = ""))
  }else{
    stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Reliance 5 Min Data.csv", sep = ""))
    # stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/HCL - 5 min.csv", sep = ""))
  }
  
  
  
  ### Checing the conditions met cases
  
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
  
  
  #########    Setting the target for the selected stock   ###########
  
  if(nrow(Signal_df) == 0){
    next
  }else{
    # Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
    
    # Signal_df$Datetime <- Signal_df$Datetime + hm("5:30")
    
    # Signal_df <- Signal_df[order(Signal_df$Datetime),]
    
    rownames(Signal_df) <- 1:nrow(Signal_df)
    
    Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
    
    stop_loss <- 1
    target <- 1
    
    
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
      stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HDFCBANK.csv", sep = ""))
    }else if(stock == "TECHM.NS"){
      stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - TechM.csv", sep = ""))
    }else if(stock == "TATAMOTORS.NS"){
      stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
    }else if(stock == "UPL.NS"){
      stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - UPL.csv", sep = ""))
    }else if(stock == "GRASIM.NS"){
      stock_5_min_historic_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - GRASIM.csv", sep = ""))
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
    
    satisfied_df = data.frame(Strategy = character(0),Call_time = character(0),Call = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0),Value=character(0))
    
    incr = 1
    
    
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
    
    
    if(nrow(satisfied_df) == 0){
      next
    }
    else{
      satisfied_df = head(satisfied_df,1)
      
      final_signal_df = rbind(final_signal_df,satisfied_df)
    }
    
    
    
  }
  
  
  print(final_signal_df)
  
  final_signal_df[is.na(final_signal_df$Target),]$Target <- ""
  final_signal_df[is.na(final_signal_df$SL),]$SL <- ""
  
  final_signal_df$sign <- ifelse((final_signal_df$Call=="Buy"),-1,1)
  
  # View(final_signal_df)
  Signal_df$sign <- ifelse(Signal_df$Signal=="Buy",1,-1)
  Signal_df
  
  
  
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
      capital <- 10000
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
    stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - HDFCBANK.csv", sep = ""))
  }else if(stock == "TECHM.NS"){
    stock_5_min_data <- read.csv(paste0(getwd(),"/data/Stocks 5 min data - TechM.csv", sep = ""))
  }else if(stock == "TATAMOTORS.NS"){
    stock_5_min_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - TATAMOTORS.csv", sep = ""))
  }else if(stock == "UPL.NS"){
    stock_5_min_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - UPL.csv", sep = ""))
  }else if(stock == "GRASIM.NS"){
    stock_5_min_data <- read.csv(paste0(getwd(),"/data/Nifty 50 5 min data - GRASIM.csv", sep = ""))
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
  
  target_combined_data$StartTime <- as_datetime(as.character(as_datetime(target_combined_data$StartTime) + hm("5:30")),tz="Asia/Kolkata")
  
  final_data_combined <- rbind(final_data_combined,target_combined_data)
  

  
  write.csv(final_combined,paste0(getwd(),"/data/bot_backtest.csv", sep = ""))
  
  # combined_data$Datetime <- as_datetime(as.character(as_datetime(combined_data$Datetime), tz = "Asia/Kolkata"))
  # target_combined_data$Datetime <- as_datetime(as.character(as_datetime(target_combined_data$Datetime), tz = "Asia/Kolkata"))
  
  
}


View(final_data_combined)
