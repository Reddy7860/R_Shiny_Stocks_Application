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

print(paste("Started Executing the Script : ",Sys.time(),sep=""))

angel_script <- read.csv('~/Downloads/angel_script.csv')

client_data <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/Client_Details.csv", sep = ""))

for (cli in 1:nrow(client_data)) {
  
  # browser()
  
  login_params = list(api_key = client_data[cli,'api_key'])
  
  login_object = create_connection_object(login_params)
  
  user_id = client_data[cli,'client_id']
  
  Sys.sleep(1)
  
  session_data <- generate_session(login_object,user_id,client_data[cli,'password'])
  
  # print(session_data)
  
  if(file.exists(paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Options_Order",as.character(Sys.Date()),sep="/"),".csv",sep=""))){
    
    final_levels_df  <- read.csv(paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Options_Order",as.character(Sys.Date()),sep="/"),".csv",sep=""))
  }
  
  if(file.exists(paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Target_Orders",as.character(Sys.Date()),sep="/"),".csv",sep=""))){
    final_target_levels_df <- read.csv(paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Target_Orders",as.character(Sys.Date()),sep="/"),".csv",sep=""))
  }else{
    
    final_target_levels_df <- data.frame("Strategy" = character(),
                                         "Stock" = character(),
                                         "Signal" = character(),
                                         "Datetime"= character(),
                                         "Value"= double(),
                                         "StopLoss"= double(),
                                         "Target"= double(),
                                         "Qty"= integer(),
                                         "Spot_Price"= integer(),
                                         "expiry" = character(),
                                         "Strike_Buy_Price"= double(),
                                         "premium_StopLoss"= double(),
                                         "premium_Target" = double(),
                                         "lotsize"  = integer(),
                                         "premium_Qty" = integer(),
                                         "token"= integer(),
                                         "current_script"  = character(),
                                         "order_place" = integer(),
                                         "order_id" = double(),
                                         "target_order_id"= double(),
                                         "stop_loss_order_id"= double(),
                                         "cancel_order_id"= double(),
                                         "final_order_id"= double(),
                                         "robo_order_id" = integer(),
                                         "conclusion"= character(),
                                         "execution_time" = integer(),
                                         "target_hit" = character(),
                                         "avg_buy_price" = double(),
                                         "avg_sell_price" = double(),
                                         "avg_qty"= integer(),
                                         "adjusted_target" = double(),
                                         "adjusted_stoploss" = double())
    
    
  }
  
  
  # colnames(final_target_levels_df)
  
  # final_levels_df$execution_time <- 0
  
  
  colnames(final_levels_df)
  
  
  temp_tbl <- sqldf("select Strategy,Stock,Signal,Datetime,Value,StopLoss,Target,Qty,Spot_Price,expiry,Strike_Buy_Price,premium_StopLoss,premium_Target,lotsize,premium_Qty,token,current_script,exec_rnk,order_place,order_id,target_order_id,stop_loss_order_id,cancel_order_id,final_order_id,robo_order_id,conclusion,execution_time,target_hit,avg_buy_price,avg_sell_price,avg_qty,adjusted_target,adjusted_stoploss,
rank() over(partition by Strategy,Stock,Datetime,Value order by execution_time desc) as rnk
from final_levels_df")
  
  temp_tbl <- temp_tbl[temp_tbl$rnk == 1,]
  
  final_levels_df <- temp_tbl[!duplicated(temp_tbl), ]
  # final_levels_df <- final_levels_df[!duplicated(final_levels_df), ]
  final_levels_df <- final_levels_df[!is.na(final_levels_df$Datetime),]
  
  rownames(final_levels_df) <- NULL
  
  # login_params = list(api_key = 'LPUVlRxd')
  # 
  # login_object = create_connection_object(login_params)
  # 
  # 
  # 
  # session_data <- generate_session(login_object,user_id,"startteja123")
  
  place_target_order <- function(final_signals_df,row_index){
    
    Sys.sleep(1)
    
    # order_place <- place_order(object = session_data,
    #                            variety= "NORMAL",
    #                            tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
    #                            symboltoken= as.character(final_signals_df[row_index,"token"]),
    #                            transactiontype= "SELL",
    #                            exchange= "NFO",
    #                            ordertype = "LIMIT",
    #                            producttype= "CARRYFORWARD",
    #                            duration= "DAY",
    #                            price= round(as.numeric(final_signals_df[row_index,"premium_Target"]),0),
    #                            squareoff= 0,
    #                            stoploss= 0,
    #                            quantity= as.numeric(final_signals_df[row_index,"lotsize"]),
    #                            triggerprice =  NULL
    # )
    
    
    order_place <- place_order(object = session_data,
                               variety= "NORMAL",
                               tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
                               symboltoken= as.character(final_signals_df[row_index,"token"]),
                               transactiontype= "SELL",
                               exchange= "NFO",
                               ordertype = "LIMIT",
                               producttype= "CARRYFORWARD",
                               duration= "DAY",
                               price= round(as.numeric(final_signals_df[row_index,"adjusted_target"]),0),
                               squareoff= 0,
                               stoploss= 0,
                               quantity= as.numeric(final_signals_df[row_index,"lotsize"]),
                               triggerprice =  NULL
    )
    
    print(paste("the target order id is ",order_place))
    
    
    
    final_signals_df[row_index,"target_order_id"] <- order_place
    final_signals_df[row_index,"final_order_id"] <- order_place
    final_signals_df[row_index,"conclusion"] <- "Target Pending"
    
    
    return(final_signals_df)
  }
  
  
  place_stoploss_order <- function(final_signals_df,row_index){
    
    Sys.sleep(1)
    
    # order_place_sl <- place_order(object = session_data,
    #                               variety= "STOPLOSS",
    #                               tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
    #                               symboltoken= as.character(final_signals_df[row_index,"token"]),
    #                               transactiontype= "SELL",
    #                               exchange= "NFO",
    #                               triggerprice = round(as.numeric(final_signals_df[row_index,"premium_StopLoss"]),0)+0.5,
    #                               ordertype = "STOPLOSS_LIMIT",
    #                               producttype= "CARRYFORWARD",
    #                               duration = "DAY",
    #                               price = as.numeric(round(final_signals_df[row_index,"premium_StopLoss"],0)),
    #                               quantity= as.numeric(prev_qty),
    #                               stoploss =  as.numeric(round(final_signals_df[row_index,"premium_StopLoss"],0)),
    #                               squareoff = as.numeric(round(as.numeric(final_signals_df[row_index,"premium_Target"]),0))
    # )
    
    order_place_sl <- place_order(object = session_data,
                                  variety= "STOPLOSS",
                                  tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
                                  symboltoken= as.character(final_signals_df[row_index,"token"]),
                                  transactiontype= "SELL",
                                  exchange= "NFO",
                                  triggerprice = round(as.numeric(final_signals_df[row_index,"adjusted_stoploss"]),0)+0.5,
                                  ordertype = "STOPLOSS_LIMIT",
                                  producttype= "CARRYFORWARD",
                                  duration = "DAY",
                                  price = as.numeric(round(final_signals_df[row_index,"adjusted_stoploss"],0)),
                                  quantity= as.numeric(prev_qty),
                                  stoploss =  as.numeric(round(final_signals_df[row_index,"adjusted_stoploss"],0)),
                                  squareoff = as.numeric(round(as.numeric(final_signals_df[row_index,"adjusted_target"]),0))
    )
    
    print(paste("the stop loss order id is ",order_place_sl))
    
    # Sys.sleep(1)
    
    
    final_signals_df[row_index,"stop_loss_order_id"] <- order_place_sl
    final_signals_df[row_index,"final_order_id"] <- order_place_sl
    final_signals_df[row_index,"conclusion"] <- "Stoploss Pending"
    
    return(final_signals_df)
  }
  
  # browser()
  
  for (j in 1:nrow(final_levels_df)) {
    
    if (nchar(final_levels_df[j,"Datetime"]) == 10) {
      
      final_levels_df[j,"Datetime"] = as.POSIXct((as.numeric(final_levels_df[j,"Datetime"])),origin = "1970-01-01")
      
    }else{
      final_levels_df[j,"Datetime"] = as.POSIXct((final_levels_df[j,"Datetime"]),origin = "1970-01-01")
    }
    
  }
  
  final_levels_df$Datetime <- as.POSIXct(as.numeric(final_levels_df$Datetime),origin="1970-01-01")
  
  final_levels_df <- final_levels_df[anytime(final_levels_df$Datetime) >= Sys.Date() ,]
  
  
  final_levels_df = sqldf("select t1.Strategy,
                        t1.Stock,
                        t1.Signal,
                        t1.Datetime,
                        t1.Value,
                        t1.Stoploss,
                        t1.Target,
                        t1.Qty,
                        t1.Spot_Price,
                        t1.expiry,
                        t1.Strike_Buy_Price,
                        t1.premium_StopLoss,
                        t1.premium_Target,
                        t1.lotsize,
                        t1.premium_Qty,
                        t1.token,
                        t1.current_script,
                        t1.order_place,
                        t1.order_id,
                        COALESCE(t2.target_order_id,t1.target_order_id) as target_order_id,
                        COALESCE(t2.stop_loss_order_id,t1.stop_loss_order_id) as stop_loss_order_id,
                        COALESCE(t2.cancel_order_id,t1.cancel_order_id) as cancel_order_id,
                        COALESCE(t2.final_order_id,t1.final_order_id) as final_order_id,
                        COALESCE(t2.robo_order_id,t1.robo_order_id) as robo_order_id,
                        COALESCE(t2.conclusion,t1.conclusion) as conclusion,
                        COALESCE(t2.execution_time,t1.execution_time) as execution_time,
                        t2.target_hit,
                        t2.avg_buy_price,
                        t2.avg_sell_price,
                        t2.avg_qty,
                        t2.adjusted_target,
                        t2.adjusted_stoploss
      
      from final_levels_df t1 
      left join final_target_levels_df t2 on t1.order_id = t2.order_id ")
  
  
  final_levels_df <- final_levels_df[!duplicated(final_levels_df), ]
  final_levels_df <- final_levels_df[!is.na(final_levels_df$Strategy),]
  # rownames(final_levels_df) <- NULL
  
  rownames(final_levels_df) <- NULL
  
  final_levels_df$final_order_id <- as.character(as.numeric(final_levels_df$final_order_id))
  
  
  #### Iterate through all the placed limit orders data
  
  for (ord in 1:nrow(final_levels_df)) {
    
    
    # browser()
    #### Filter for today's placed orders
    
    if (as.Date(anytime(final_levels_df[ord,"Datetime"])) == Sys.Date()) {
      
      # browser()
      
      ##### Check for the orderes that are placed by Bot
      if(final_levels_df[ord,"order_id"] > 1){
        
        print(final_levels_df[ord,])
        
        # browser()
        #####  Check if the order is not Complated
        if(final_levels_df[ord,"conclusion"] != "Completed"){
          
          ###### Fetch then order book from Angel Broking
          my_orders <- order_book(session_data)
          Sys.sleep(1)
          
          order_data = data.frame(variety = as.character(),
                                  ordertype=as.character(),
                                  producttype=as.character(),
                                  duration=as.character(),
                                  price = as.character(),
                                  triggerprice = as.character(),
                                  quantity = as.character(),
                                  disclosedquantity = as.character(),
                                  squareoff = as.character(),
                                  stoploss = as.character(),
                                  trailingstoploss = as.character(),
                                  tradingsymbol = as.character(),
                                  transactiontype = as.character(),
                                  exchange = as.character(),
                                  symboltoken = as.character(),
                                  ordertag = as.character(),
                                  instrumenttype = as.character(),
                                  strikeprice = as.character(),
                                  optiontype = as.character(),
                                  expirydate = as.character(),
                                  lotsize = as.character(),
                                  cancelsize = as.character(),
                                  averageprice = as.character(),
                                  filledshares = as.character(),
                                  unfilledshares = as.character(),
                                  orderid = as.character(),
                                  text = as.character(),
                                  status = as.character(),
                                  updatetime = as.character(),
                                  exchorderupdatetime = as.character(),
                                  fillid = as.character(),
                                  filltime = as.character(),
                                  parentorderid = as.character()
          )
          
          if(length(my_orders$data) > 0 ){
            
            for(ind in 1:length(my_orders$data)){
              
              order_data[ind,"variety"] = my_orders$data[[ind]]$variety
              order_data[ind,"ordertype"] = my_orders$data[[ind]]$ordertype
              order_data[ind,"producttype"] = my_orders$data[[ind]]$producttype
              order_data[ind,"duration"] = my_orders$data[[ind]]$duration
              order_data[ind,"price"] = my_orders$data[[ind]]$price
              order_data[ind,"triggerprice"] = my_orders$data[[ind]]$triggerprice
              order_data[ind,"quantity"] = my_orders$data[[ind]]$quantity
              order_data[ind,"disclosedquantity"] = my_orders$data[[ind]]$disclosedquantity
              order_data[ind,"squareoff"] = my_orders$data[[ind]]$squareoff
              order_data[ind,"stoploss"] = my_orders$data[[ind]]$stoploss
              order_data[ind,"trailingstoploss"] = my_orders$data[[ind]]$trailingstoploss
              order_data[ind,"tradingsymbol"] = my_orders$data[[ind]]$tradingsymbol
              order_data[ind,"transactiontype"] = my_orders$data[[ind]]$transactiontype
              order_data[ind,"exchange"] = my_orders$data[[ind]]$exchange
              order_data[ind,"symboltoken"] = my_orders$data[[ind]]$symboltoken
              order_data[ind,"ordertag"] = my_orders$data[[ind]]$ordertag
              order_data[ind,"instrumenttype"] = my_orders$data[[ind]]$instrumenttype
              order_data[ind,"strikeprice"] = my_orders$data[[ind]]$strikeprice
              order_data[ind,"optiontype"] = my_orders$data[[ind]]$optiontype
              order_data[ind,"expirydate"] = my_orders$data[[ind]]$expirydate
              order_data[ind,"lotsize"] = my_orders$data[[ind]]$lotsize
              order_data[ind,"cancelsize"] = my_orders$data[[ind]]$cancelsize
              order_data[ind,"averageprice"] = my_orders$data[[ind]]$averageprice
              order_data[ind,"filledshares"] = my_orders$data[[ind]]$filledshares
              order_data[ind,"unfilledshares"] = my_orders$data[[ind]]$unfilledshares
              order_data[ind,"orderid"] = my_orders$data[[ind]]$orderid
              order_data[ind,"text"] = my_orders$data[[ind]]$text
              order_data[ind,"status"] = my_orders$data[[ind]]$status
              order_data[ind,"updatetime"] = my_orders$data[[ind]]$updatetime
              order_data[ind,"exchorderupdatetime"] = my_orders$data[[ind]]$exchorderupdatetime
              order_data[ind,"fillid"] = my_orders$data[[ind]]$fillid
              order_data[ind,"filltime"] = my_orders$data[[ind]]$filltime
              order_data[ind,"parentorderid"] = my_orders$data[[ind]]$parentorderid
              
            }
          }
          
          
          ##### Retrieve the last order details from order book
          last_order <- order_data[order_data$orderid == as.character(final_levels_df[ord,"order_id"]),]
          # browser()
          
          
          #######  Check if the target/stoploss order is placed
          if(final_levels_df[ord,"final_order_id"] == 0){
            
            
            if (nrow(last_order) > 0) {
              
              ###### If the Buy limit order is placed successfully
              if(last_order$status == "complete"){
                
                prev_generation <- final_levels_df[(ord),]
                
                # print(prev_generation)
                
                prev_trading_symbol = final_levels_df[ord,"current_script"]
                prev_symbol_token = final_levels_df[ord,"token"]
                
                
                prev_exchange_counter = "NFO"
                prev_order_type = "LIMIT"
                prev_product_type = "CARRYFORWARD" 
                prev_duration_day = "DAY"
                prev_stoploss = as.numeric(final_levels_df[ord,"premium_StopLoss"])
                
                prev_squareoff = as.numeric(final_levels_df[ord,"premium_Target"])
                
                prev_qty = as.numeric(final_levels_df[ord,"lotsize"])
                prev_limit_price = as.numeric((final_levels_df[ord,"Strike_Buy_Price"]))
                
                
                #### Get the last traded price of the Strike 
                current_traded_price <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(prev_trading_symbol),symboltoken = as.character(prev_symbol_token))
                
                current_traded_price <- current_traded_price$data$ltp
                
                
                #### Calculate the percentage deviation from the executed and current price
                
                price_diff <- ((prev_limit_price - current_traded_price)/prev_limit_price)*100
                
                
                # prev_order = final_levels_df[ord,]
                rownames(prev_generation) <- 1
                
                prev_final_order_id <- prev_generation[1,"final_order_id"]
                
                last_final_order <- order_data[order_data$orderid == as.character(prev_final_order_id),]
                
                # browser()
                
                if(nrow(last_final_order) > 0){
                  
                  if(last_final_order$status == "complete"){
                    
                    # browser()
                    rownames(last_final_order) <- NULL
                    
                    final_levels_df[ord,"avg_sell_price"] <- last_final_order[1,"averageprice"]
                    
                    print("Order complete")
                    final_levels_df[ord,"conclusion"] <- "Completed"
                    final_levels_df[ord,"target_hit"] = ifelse(final_levels_df[ord,"target_order_id"] == final_levels_df[ord,"final_order_id"],"Yes","No")
                    print("Target Achieved")
                    
                  }else{
                    # browser()
                    final_levels_df[ord,"adjusted_target"] <- as.numeric(last_final_order[1,"averageprice"]) + 0.2*as.numeric(last_order[1,"averageprice"])
                    final_levels_df[ord,"adjusted_stoploss"] <- as.numeric(last_final_order[1,"averageprice"]) - 0.2*as.numeric(last_order[1,"averageprice"])
                    
                    if(price_diff > 6){
                      
                      if(last_final_order$status == "open"){
                        
                        print("Cancelling the target and placing the stop loss order")
                        
                        cancel_id = cancel_order(object = session_data,
                                                 orderid = as.character(prev_final_order_id),
                                                 variety = "NORMAL")
                        
                        system("say Cancelled the target Order !!")
                        
                        final_levels_df[(ord),"cancel_id"] <- cancel_id
                        
                        Sys.sleep(1)
                        
                      }
                      
                      
                      
                      final_levels_df <- place_stoploss_order(final_levels_df,ord)
                      
                      system("say Placed the Stoploss Order !!")
                      
                    }else if((final_levels_df[(ord),"conclusion"] == "Stoploss Pending") && price_diff < -1){
                      
                      if(last_final_order$status == "open"){
                        
                        print("Cancelling the stop loss and placing the target order")
                        
                        cancel_id = cancel_order(object = session_data,
                                                 orderid = as.character(prev_final_order_id),
                                                 variety = "NORMAL")
                        
                        system("say Cancelled the stoploss Order !!")
                        
                        final_levels_df[(ord),"cancel_order_id"] <- cancel_id
                        
                        Sys.sleep(1)
                        
                      }
                      
                      final_levels_df <- place_target_order(final_levels_df,ord)
                      
                      system("say Placed the target Order !!")
                      
                      
                    }
                    
                  }
                  
                }else{
                  
                  rownames(last_order) <- NULL
                  
                  final_levels_df[ord,"avg_buy_price"] <- last_order[1,"averageprice"]
                  final_levels_df[ord,"avg_qty"] <- last_order[1,"filledshares"]
                  
                  # browser()

                    # Adding the 10 percent target to the order
                    final_levels_df[ord,"adjusted_target"] <- as.numeric(last_order[1,"averageprice"]) + 0.2*as.numeric(last_order[1,"averageprice"])
                    final_levels_df[ord,"adjusted_stoploss"] <- as.numeric(last_order[1,"averageprice"]) - 0.2*as.numeric(last_order[1,"averageprice"])
                  
                  if(price_diff > 5){
                    
                    
                    final_levels_df <- place_stoploss_order(final_levels_df,ord)
                    
                    system("say Placed the Stoploss Order !!")
                  }else{
                    final_levels_df <- place_target_order(final_levels_df,ord)
                    
                    system("say Placed the target Order !!")
                    
                  }
                  
                }
                
                
                
              }
              
            }
            
            
          }else{
            
            ###### If the Buy limit order is placed successfully
            if(last_order$status == "complete"){
              
              prev_generation <- final_levels_df[(ord),]
              
              # print(prev_generation)
              
              prev_trading_symbol = final_levels_df[ord,"current_script"]
              prev_symbol_token = final_levels_df[ord,"token"]
              
              
              prev_exchange_counter = "NFO"
              prev_order_type = "LIMIT"
              prev_product_type = "CARRYFORWARD" 
              prev_duration_day = "DAY"
              prev_stoploss = as.numeric(final_levels_df[ord,"premium_StopLoss"])
              
              prev_squareoff = as.numeric(final_levels_df[ord,"premium_Target"])
              
              prev_qty = as.numeric(final_levels_df[ord,"lotsize"])
              prev_limit_price = as.numeric((final_levels_df[ord,"Strike_Buy_Price"]))
              
              
              #### Get the last traded price of the Strike 
              current_traded_price <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(prev_trading_symbol),symboltoken = as.character(prev_symbol_token))
              
              current_traded_price <- current_traded_price$data$ltp
              
              
              #### Calculate the percentage deviation from the executed and current price
              
              price_diff <- ((prev_limit_price - current_traded_price)/prev_limit_price)*100
              
              # prev_order = final_levels_df[ord,]
              rownames(prev_generation) <- 1
              
              prev_final_order_id <- prev_generation[1,"final_order_id"]
              
              last_final_order <- order_data[order_data$orderid == as.character(prev_final_order_id),]
              
              if(last_final_order$status == "complete"){
                
                # browser()
                
                rownames(last_final_order) <- NULL
                
                final_levels_df[ord,"avg_sell_price"] <- last_final_order[1,"averageprice"]
                
                print("Order complete")
                final_levels_df[ord,"conclusion"] <- "Completed"
                final_levels_df[ord,"target_hit"] = ifelse(final_levels_df[ord,"target_order_id"] == final_levels_df[ord,"final_order_id"],"Yes","No")
                print("Target Achieved")
                
              }else{
                
                if(price_diff > 6 && final_levels_df[ord,"final_order_id"] != final_levels_df[ord,"stop_loss_order_id"] ){
                  
                  print("Cancelling the target and placing the stop loss order")
                  
                  cancel_id = cancel_order(object = session_data,
                                           orderid = as.character(prev_final_order_id),
                                           variety = "NORMAL")
                  
                  system("say Cancelled the target Order !!")
                  
                  final_levels_df[(ord),"cancel_id"] <- cancel_id
                  
                  Sys.sleep(1)
                  
                  final_levels_df <- place_stoploss_order(final_levels_df,ord)
                  
                  system("say Placed the Stoploss Order !!")
                }else if(final_levels_df[ord,"final_order_id"] != final_levels_df[ord,"target_order_id"] ){
                  
                  print("Cancelling the stop loss and placing the target order")
                  
                  cancel_id = cancel_order(object = session_data,
                                           orderid = as.character(prev_final_order_id),
                                           variety = "NORMAL")
                  
                  system("say Cancelled the stoploss Order !!")
                  
                  final_levels_df[(ord),"cancel_order_id"] <- cancel_id
                  
                  Sys.sleep(1)
                  final_levels_df <- place_target_order(final_levels_df,ord)
                  
                  system("say Placed the target Order !!")
                  
                }
                
              }
              
              
              
            }
            
          }
          
          
        }
        
        
        
      }
      
    }else{
      
    }
    
  }
  
  # browser()
  
  # if(user_id == "Y68412"){
    trade_book_data <- data.frame("exchange" = character(),
                                  "producttype" = character(),
                                  "tradingsymbol" = character(),
                                  "instrumenttype"= character(),
                                  "symbolgroup"= character(),
                                  "strikeprice"= integer(),
                                  "optiontype"= character(),
                                  "expirydate"= character(),
                                  "marketlot"= integer(),
                                  "precision" = integer(),
                                  "multiplier"= integer(),
                                  "tradevalue"= integer(),
                                  "transactiontype" = character(),
                                  "fillprice"  = integer(),
                                  "fillsize" = integer(),
                                  "orderid"= double(),
                                  "fillid"  = double(),
                                  "filltime" = character() )

    my_traded_data <- trade_book(session_data)

    if(length(my_traded_data$data) > 0 ){

      for(ind in 1:length(my_traded_data$data)){

        # browser()

        trade_book_data[ind,"exchange"] = my_traded_data$data[[ind]]$exchange
        trade_book_data[ind,"producttype"] = my_traded_data$data[[ind]]$producttype
        trade_book_data[ind,"tradingsymbol"] = my_traded_data$data[[ind]]$tradingsymbol
        trade_book_data[ind,"instrumenttype"] = my_traded_data$data[[ind]]$instrumenttype
        trade_book_data[ind,"symbolgroup"] = my_traded_data$data[[ind]]$symbolgroup
        trade_book_data[ind,"strikeprice"] = my_traded_data$data[[ind]]$strikeprice
        trade_book_data[ind,"optiontype"] = my_traded_data$data[[ind]]$optiontype
        trade_book_data[ind,"expirydate"] = my_traded_data$data[[ind]]$expirydate
        trade_book_data[ind,"marketlot"] = my_traded_data$data[[ind]]$marketlot
        trade_book_data[ind,"precision"] = my_traded_data$data[[ind]]$precision
        trade_book_data[ind,"multiplier"] = my_traded_data$data[[ind]]$multiplier
        trade_book_data[ind,"tradevalue"] = my_traded_data$data[[ind]]$tradevalue
        trade_book_data[ind,"transactiontype"] = my_traded_data$data[[ind]]$transactiontype
        trade_book_data[ind,"fillprice"] = my_traded_data$data[[ind]]$fillprice
        trade_book_data[ind,"fillsize"] = my_traded_data$data[[ind]]$fillsize
        trade_book_data[ind,"orderid"] = my_traded_data$data[[ind]]$orderid
        trade_book_data[ind,"fillid"] = my_traded_data$data[[ind]]$fillid
        trade_book_data[ind,"filltime"] = my_traded_data$data[[ind]]$filltime

      }
    }


    buy_trade_orders <- trade_book_data[trade_book_data$transactiontype == "BUY",]

    rownames(buy_trade_orders) <- NULL

    if(nrow(buy_trade_orders) > 0){

      if (nrow(final_levels_df) > 0) {



        script_orders <- final_levels_df$order_id
        script_orders <- as.character(script_orders)

        for (ord in 1:nrow(buy_trade_orders)) {

          # browser()

          if(buy_trade_orders[ord,"orderid"] %in% script_orders){
            # print("Found")
          }else{
            # browser()

            print(buy_trade_orders[ord,])

            angel_script <- read.csv('~/Downloads/angel_script.csv')

            current_script <- angel_script[angel_script$symbol == buy_trade_orders[ord,"tradingsymbol"],]

            # print(current_script)

            token <- current_script[1,"token"]

            current_traded_price <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(buy_trade_orders[ord,"tradingsymbol"]),symboltoken = as.character(token))

            current_traded_price <- current_traded_price$data$ltp


            #### Calculate the percentage deviation from the executed and current price

            price_diff <- ((buy_trade_orders[ord,"fillprice"] - current_traded_price)/buy_trade_orders[ord,"fillprice"])*100
            
            # browser()

            stop_loss_price <- as.numeric(buy_trade_orders[ord,"fillprice"]) - 0.2*as.numeric(buy_trade_orders[ord,"fillprice"])
            target_price <- as.numeric(buy_trade_orders[ord,"fillprice"]) + 0.20*as.numeric(buy_trade_orders[ord,"fillprice"])

            if (price_diff >= 15) {

              #### Placing the Stop Loss Market Order

              order_place <- place_order(object = session_data,
                                         variety= "NORMAL",
                                         tradingsymbol= as.character(buy_trade_orders[ord,"tradingsymbol"]),
                                         symboltoken= as.character(token),
                                         transactiontype= "SELL",
                                         exchange= "NFO",
                                         ordertype = "MARKET",
                                         producttype= "CARRYFORWARD",
                                         duration= "DAY",
                                         price= round(stop_loss_price,0),
                                         squareoff= 0,
                                         stoploss= 0,
                                         quantity= as.numeric(buy_trade_orders[ord,"fillsize"]),
                                         triggerprice =  NULL
              )

              system("say Placed Stop Loss of Missing Order !!")

            }else if(price_diff <= -20){

              ##### Place the Target Order

              order_place <- place_order(object = session_data,
                                         variety= "NORMAL",
                                         tradingsymbol= as.character(buy_trade_orders[ord,"tradingsymbol"]),
                                         symboltoken= as.character(token),
                                         transactiontype= "SELL",
                                         exchange= "NFO",
                                         ordertype = "MARKET",
                                         producttype= "CARRYFORWARD",
                                         duration= "DAY",
                                         price= round(target_price,0),
                                         squareoff= 0,
                                         stoploss= 0,
                                         quantity= as.numeric(buy_trade_orders[ord,"fillsize"]),
                                         triggerprice =  NULL
              )

              system("say Placed Target of Missing Order !!")

            }

          }

        }

      }

    }
  # }
  
  
  
  
  folder = paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Target_Orders",as.character(Sys.Date()),sep="/"),".csv",sep="")
  
  write.csv(final_levels_df,folder, row.names=FALSE)
  
  print(paste("Ended Executing the Script : ",Sys.time(),sep=""))
  
}


login_params = list(api_key = client_data[1,'api_key'])
login_object = create_connection_object(login_params)
session_data <- generate_session(login_object,user_id,client_data[1,'password'])


my_positons_data <- position(session_data)

current_position_data = data.frame(symboltoken = as.character(),
                           symbolname=as.character(),
                           instrumenttype=as.character(),
                           priceden=as.numeric(),
                           pricenum = as.numeric(),
                           genden = as.numeric(),
                           gennum = as.numeric(),
                           precision = as.numeric(),
                           multiplier = as.numeric(),
                           boardlotsize = as.numeric(),
                           exchange = as.character(),
                           producttype = as.character(),
                           tradingsymbol = as.character(),
                           symbolgroup = as.character(),
                           strikeprice = as.double(),
                           optiontype = as.character(),
                           expirydate = as.character(),
                           lotsize = as.numeric(),
                           cfbuyqty = as.numeric(),
                           cfsellqty = as.numeric(),
                           cfbuyamount = as.numeric(),
                           cfsellamount = as.numeric(),
                           buyavgprice = as.numeric(),
                           sellavgprice = as.numeric(),
                           avgnetprice = as.numeric(),
                           netvalue = as.numeric(),
                           netqty = as.numeric(),
                           totalbuyvalue = as.numeric(),
                           totalsellvalue = as.numeric(),
                           cfbuyavgprice = as.numeric(),
                           cfsellavgprice = as.numeric(),
                           totalbuyavgprice = as.numeric(),
                           totalsellavgprice = as.numeric(),
                           netprice = as.numeric(),
                           buyqty = as.numeric(),
                           sellqty = as.numeric(),
                           buyamount = as.numeric(),
                           sellamount = as.numeric(),
                           pnl = as.numeric(), 
                           realised = as.numeric(),
                           unrealised = as.numeric(),
                           ltp = as.numeric(),
                           close = as.numeric()
)

if(length(my_positons_data$data) > 0 ){
  
  for(ind in 1:length(my_positons_data$data)){
    
    # browser()
    
    current_position_data[ind,"symboltoken"] = my_positons_data$data[[ind]]$symboltoken
    current_position_data[ind,"symbolname"] = my_positons_data$data[[ind]]$symbolname
    current_position_data[ind,"instrumenttype"] = my_positons_data$data[[ind]]$instrumenttype
    current_position_data[ind,"priceden"] = my_positons_data$data[[ind]]$priceden
    current_position_data[ind,"pricenum"] = my_positons_data$data[[ind]]$pricenum
    current_position_data[ind,"genden"] = my_positons_data$data[[ind]]$genden
    current_position_data[ind,"gennum"] = my_positons_data$data[[ind]]$gennum
    current_position_data[ind,"precision"] = my_positons_data$data[[ind]]$precision
    current_position_data[ind,"multiplier"] = my_positons_data$data[[ind]]$multiplier
    current_position_data[ind,"boardlotsize"] = my_positons_data$data[[ind]]$boardlotsize
    current_position_data[ind,"exchange"] = my_positons_data$data[[ind]]$exchange
    current_position_data[ind,"producttype"] = my_positons_data$data[[ind]]$producttype
    current_position_data[ind,"tradingsymbol"] = my_positons_data$data[[ind]]$tradingsymbol
    current_position_data[ind,"symbolgroup"] = my_positons_data$data[[ind]]$symbolgroup
    current_position_data[ind,"strikeprice"] = my_positons_data$data[[ind]]$strikeprice
    current_position_data[ind,"optiontype"] = my_positons_data$data[[ind]]$optiontype
    current_position_data[ind,"expirydate"] = my_positons_data$data[[ind]]$expirydate
    current_position_data[ind,"lotsize"] = my_positons_data$data[[ind]]$lotsize
    
    current_position_data[ind,"cfbuyqty"] = my_positons_data$data[[ind]]$cfbuyqty
    current_position_data[ind,"cfsellqty"] = my_positons_data$data[[ind]]$cfsellqty
    current_position_data[ind,"cfbuyamount"] = my_positons_data$data[[ind]]$cfbuyamount
    current_position_data[ind,"cfsellamount"] = my_positons_data$data[[ind]]$cfsellamount
    current_position_data[ind,"buyavgprice"] = my_positons_data$data[[ind]]$buyavgprice
    current_position_data[ind,"sellavgprice"] = my_positons_data$data[[ind]]$sellavgprice
    current_position_data[ind,"avgnetprice"] = my_positons_data$data[[ind]]$avgnetprice
    current_position_data[ind,"netvalue"] = my_positons_data$data[[ind]]$netvalue
    current_position_data[ind,"netqty"] = my_positons_data$data[[ind]]$netqty
    current_position_data[ind,"totalbuyvalue"] = my_positons_data$data[[ind]]$totalbuyvalue
    current_position_data[ind,"totalsellvalue"] = my_positons_data$data[[ind]]$totalsellvalue
    current_position_data[ind,"cfbuyavgprice"] = my_positons_data$data[[ind]]$cfbuyavgprice
    
    current_position_data[ind,"cfsellavgprice"] = my_positons_data$data[[ind]]$cfsellavgprice
    current_position_data[ind,"totalbuyavgprice"] = my_positons_data$data[[ind]]$totalbuyavgprice
    current_position_data[ind,"totalsellavgprice"] = my_positons_data$data[[ind]]$totalsellavgprice
    current_position_data[ind,"netprice"] = my_positons_data$data[[ind]]$netprice
    current_position_data[ind,"buyqty"] = my_positons_data$data[[ind]]$buyqty
    current_position_data[ind,"sellqty"] = my_positons_data$data[[ind]]$sellqty
    current_position_data[ind,"buyamount"] = my_positons_data$data[[ind]]$buyamount
    current_position_data[ind,"sellamount"] = my_positons_data$data[[ind]]$sellamount
    current_position_data[ind,"pnl"] = my_positons_data$data[[ind]]$pnl
    current_position_data[ind,"realised"] = my_positons_data$data[[ind]]$realised
    current_position_data[ind,"unrealised"] = my_positons_data$data[[ind]]$unrealised
    current_position_data[ind,"ltp"] = my_positons_data$data[[ind]]$ltp
    current_position_data[ind,"close"] = my_positons_data$data[[ind]]$close
    
  }
}



# user_id = "J95213"
# 
# final_levels_df  <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Trading_Logs.csv", sep = ""))
# # final_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Trading_Orders.csv", sep = ""))
# # final_target_levels_df <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Target_Orders.csv", sep = ""))
