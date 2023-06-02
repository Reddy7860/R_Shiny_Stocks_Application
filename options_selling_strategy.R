library("smartapi")
library("stringr")
library("dplyr")

rm(list=ls())

login_params = list(api_key = 'LPUVlRxd')

login_object = create_connection_object(login_params)

session_data <- generate_session(login_object,"J95213","startteja123")

Spot_Price <- as.numeric("17150")
side_dir <- "CE"


angel_script <- read.csv('~/Downloads/angel_script.csv')

nifty_script_data <- angel_script[angel_script$name == "NIFTY" & angel_script$expiry == "09SEP2021",]
rownames(nifty_script_data) <- NULL
nifty_script_data$side_dir <- str_sub(nifty_script_data$symbol, start= -2)


current_calls_df = data.frame(current_symbol = character(0),current_token = character(0),ltp_price=numeric(0),quantity = numeric(0),expirydate=character(0),strikeprice=character(0),orderid=character(0),transactiontype=character(0),exchange=character(0),ordertype=character(0),producttype=character(0),duration=character(0),status=character(0))
counter = 1

if(hour(Sys.time()) == 10){
  
  #### Run this if the time is 10 on Thursday
  for (ind in 1:nrow(nifty_script_data)) {
    
    current_Spot <- nifty_script_data[ind,"strike"]
    
    current_symbol <- nifty_script_data[ind,"symbol"]
    
    current_token <- nifty_script_data[ind,"token"]
    
    current_dir <- nifty_script_data[ind,"side_dir"]
    
    tryCatch(
      {
        ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
        
        ltp_price <- ltp_data$data$ltp
      },
      error = function(cond){
        print(cond)
        
        Sys.sleep(1)
        
        ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
        
        ltp_price <- ltp_data$data$ltp
      }
    )

    
    if(dplyr::between(ltp_price, 20, 30)){
      
      current_calls_df[counter,"current_symbol"] <- current_symbol
      current_calls_df[counter,"current_token"] <- current_token
      current_calls_df[counter,"ltp_price"] <- ltp_price
      current_calls_df[counter,"quantity"] <- 1
      current_calls_df[counter,"expirydate"] <- "09SEP2021"
      current_calls_df[counter,"strikeprice"] <- current_Spot
      current_calls_df[counter,"orderid"] <- 1
      current_calls_df[counter,"transactiontype"] <- "SELL"
      current_calls_df[counter,"exchange"] <- "NFO"
      current_calls_df[counter,"ordertype"] <- "LIMIT"
      current_calls_df[counter,"producttype"] <- "CARRYFORWARD"
      current_calls_df[counter,"duration"] <- "DAY"
      current_calls_df[counter,"side_dir"] <- current_dir
      current_calls_df[counter,"status"] <- "open"
      
      counter = counter + 1
      
    }
    
  }
  
  current_calls_df <- data.frame(current_calls_df %>% group_by(side_dir) %>% slice(which.min(abs(ltp_price - 25))))
  
  save(current_calls_df,file="~/Desktop/Reddy_Stocks_Application/current_calls_df.Rda")
  
}else if (nrow(current_calls_df) > 0){
  
  load("~/Desktop/Reddy_Stocks_Application/current_calls_df.Rda")
  
  call_premium_price <- 0
  put_premium_price <- 0
  
  for (ord in 1:nrow(current_calls_df)) {
    
    ### Check oly the open orders from all of them
    if(current_calls_df[ord,"status"] == "open"){
      if(str_sub(current_calls_df[ord,"current_symbol"], start= -2) == "CE"){
        ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_calls_df[ord,"current_symbol"]),symboltoken = as.character(current_calls_df[ord,"current_token"]))
        call_premium_price <- ltp_data$data$ltp
      }else{
        ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_calls_df[ord,"current_symbol"]),symboltoken = as.character(current_calls_df[ord,"current_token"]))
        put_premium_price <- ltp_data$data$ltp
      }
    }else{
      print("order completed")
    }
  }
  
  temp_current_calls_df = data.frame(current_symbol = character(0),current_token = character(0),ltp_price=numeric(0),quantity = numeric(0),expirydate=character(0),strikeprice=character(0),orderid=character(0),transactiontype=character(0),exchange=character(0),ordertype=character(0),producttype=character(0),duration=character(0),status=character(0))
  smallest_value <- 0
  
  if((abs(call_premium_price-put_premium_price)/max(put_premium_price,call_premium_price)) >= 0.5){
    
    ## If market is Bullish
    if(put_premium_price < call_premium_price){
      ## Square off the Put premium
      
      current_calls_df[current_calls_df$side_dir == "PE",]$status <- "complete"
      
      ## Take the new Put premium near the Call premium price
      
      put_premium_data <- nifty_script_data[nifty_script_data$side_dir == "PE",]
      rownames(put_premium_data) <- NULL
      
      
      for (ind in 1:nrow(put_premium_data)) {
        
        current_Spot <- put_premium_data[ind,"strike"]
        
        current_symbol <- put_premium_data[ind,"symbol"]
        
        current_token <- put_premium_data[ind,"token"]
        
        tryCatch(
          {
            ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
            
            ltp_price <- ltp_data$data$ltp
          },
          error = function(cond){
            print(cond)
            
            Sys.sleep(1)
            
            ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
            
            ltp_price <- ltp_data$data$ltp
          }
        )
        
        if(ind == 1){
          smallest_value <- ltp_price
          
          temp_current_calls_df[1,"current_symbol"] <- current_symbol
          temp_current_calls_df[1,"current_token"] <- current_token
          temp_current_calls_df[1,"ltp_price"] <- ltp_price
          temp_current_calls_df[1,"quantity"] <- 1
          temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
          temp_current_calls_df[1,"strikeprice"] <- current_Spot
          temp_current_calls_df[1,"orderid"] <- 1
          temp_current_calls_df[1,"transactiontype"] <- "SELL"
          temp_current_calls_df[1,"exchange"] <- "NFO"
          temp_current_calls_df[1,"ordertype"] <- "LIMIT"
          temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
          temp_current_calls_df[1,"duration"] <- "DAY"
          temp_current_calls_df[1,"side_dir"] <- "PE"
          temp_current_calls_df[1,"status"] <- "open"
          
        }else{
          
          
          if(ltp_price >= call_premium_price & ltp_price <= smallest_value){
            
            print(ind)
            
            smallest_value <- ltp_price
            
            temp_current_calls_df[1,"current_symbol"] <- current_symbol
            temp_current_calls_df[1,"current_token"] <- current_token
            temp_current_calls_df[1,"ltp_price"] <- ltp_price
            temp_current_calls_df[1,"quantity"] <- 1
            temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
            temp_current_calls_df[1,"strikeprice"] <- current_Spot
            temp_current_calls_df[1,"orderid"] <- 1
            temp_current_calls_df[1,"transactiontype"] <- "SELL"
            temp_current_calls_df[1,"exchange"] <- "NFO"
            temp_current_calls_df[1,"ordertype"] <- "LIMIT"
            temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
            temp_current_calls_df[1,"duration"] <- "DAY"
            temp_current_calls_df[1,"side_dir"] <- "PE"
            temp_current_calls_df[1,"status"] <- "open"
            
          }
          
        }
        
        
      }
      
    }else{
      ## Bearish Market
      
      ## Square off the Call premium
      # print(nifty_script_data)
      
      current_calls_df[current_calls_df$side_dir == "CE",]$status <- "complete"
      
      ## Take the new Call premium near the Put premium price
      call_premium_data <- nifty_script_data[nifty_script_data$side_dir == "CE",]
      rownames(call_premium_data) <- NULL
      
      for (ind in 1:nrow(call_premium_data)) {
        
        current_Spot <- call_premium_data[ind,"strike"]
        
        current_symbol <- call_premium_data[ind,"symbol"]
        
        current_token <- call_premium_data[ind,"token"]
        
        tryCatch(
          {
            ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
            
            ltp_price <- ltp_data$data$ltp
          },
          error = function(cond){
            print(cond)
            
            Sys.sleep(1)
            
            ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
            
            ltp_price <- ltp_data$data$ltp
          }
        )
        
        if(ind == 1){
          smallest_value <- ltp_price
          
          temp_current_calls_df[1,"current_symbol"] <- current_symbol
          temp_current_calls_df[1,"current_token"] <- current_token
          temp_current_calls_df[1,"ltp_price"] <- ltp_price
          temp_current_calls_df[1,"quantity"] <- 1
          temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
          temp_current_calls_df[1,"strikeprice"] <- current_Spot
          temp_current_calls_df[1,"orderid"] <- 1
          temp_current_calls_df[1,"transactiontype"] <- "SELL"
          temp_current_calls_df[1,"exchange"] <- "NFO"
          temp_current_calls_df[1,"ordertype"] <- "LIMIT"
          temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
          temp_current_calls_df[1,"duration"] <- "DAY"
          temp_current_calls_df[1,"side_dir"] <- "CE"
          temp_current_calls_df[1,"status"] <- "open"
          
        }else{
          
          if(ltp_price <= put_premium_price & ltp_price >= smallest_value){
            
            smallest_value <- ltp_price
            
            temp_current_calls_df[1,"current_symbol"] <- current_symbol
            temp_current_calls_df[1,"current_token"] <- current_token
            temp_current_calls_df[1,"ltp_price"] <- ltp_price
            temp_current_calls_df[1,"quantity"] <- 1
            temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
            temp_current_calls_df[1,"strikeprice"] <- current_Spot
            temp_current_calls_df[1,"orderid"] <- 1
            temp_current_calls_df[1,"transactiontype"] <- "SELL"
            temp_current_calls_df[1,"exchange"] <- "NFO"
            temp_current_calls_df[1,"ordertype"] <- "LIMIT"
            temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
            temp_current_calls_df[1,"duration"] <- "DAY"
            temp_current_calls_df[1,"side_dir"] <- "CE"
            temp_current_calls_df[1,"status"] <- "open"
            
          }
          
        }
        
        
      }
      
    }
  }
  
  if(nrow(temp_current_calls_df) > 0){
    current_calls_df <- rbind(current_calls_df,temp_current_calls_df)
  }
  
  
  # get_ltp_data(object = session_data,exchange = "NSE",tradingsymbol= "BANKNIFTY",symboltoken= "26009")
  nifty_ltp <- get_ltp_data(object = session_data,exchange = "NSE",tradingsymbol= "NIFTY",symboltoken= "26000")
  nifty_ltp <- nifty_ltp$data$ltp
  
  
  current_calls_df$current_spot_value <- round(ifelse(nifty_ltp %% 100 > 25,(nifty_ltp + (50 - nifty_ltp %% 50))*100,(nifty_ltp - nifty_ltp %% 50)*100),2)
  
  for (ord in 1:nrow(current_calls_df)) {
    
    if (current_calls_df[ord,"status"] == "open") {
      
      if(current_calls_df[ord,"strikeprice"] == current_calls_df[ord,"current_spot_value"]){
        
        ##### Place the Stop Loss order with 30%
        
        current_calls_df[ord,"status"] <- "complete"
        
      }
      
    }
    
  }
  
  
  save(current_calls_df,file="~/Desktop/Reddy_Stocks_Application/current_calls_df.Rda")
  
}else{
  print("No Positions")
}


library(dplyr)    





current_calls_df[current_calls_df$side_dir == "CE",]

print(current_calls_df)


