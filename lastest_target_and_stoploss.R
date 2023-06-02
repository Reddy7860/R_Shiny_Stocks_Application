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


start_time = Sys.time()
print(paste("Started Executing the Script : ",Sys.time(),sep=""))

angel_script <- read.csv('~/Downloads/angel_script.csv')

client_data <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/Client_Details.csv", sep = ""))


for (cli in 1:nrow(client_data)) {
  # browser()
  # previous_orders <- read.csv(paste0("~/Downloads/testing_latest_target_orders.csv", sep = ""))
  if(file.exists(paste(paste("~/Downloads/Orders_Data",as.character(client_data[cli,"client_id"]),"Updated_Targets",as.character(Sys.Date()),sep="/"),".csv",sep=""))){
    previous_orders <- read.csv(paste(paste("~/Downloads/Orders_Data",as.character(client_data[cli,"client_id"]),"Updated_Targets",as.character(Sys.Date()),sep="/"),".csv",sep=""))
  }else{
    
    previous_orders <- data.frame(symboltoken = as.character(),
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
                                  close = as.numeric(),
                                  target_order_id = as.numeric(),
                                  stop_loss_order_id = as.numeric(),
                                  final_order_id = as.numeric(),
                                  cancel_order_id = as.numeric()
                                  
                                  )
    
    
  }
  
  # browser()
  # cli =1 
  user_id = client_data[cli,'client_id']
  login_params = list(api_key = client_data[cli,'api_key'])
  login_object = create_connection_object(login_params)
  session_data <- generate_session(login_object,as.character(user_id),as.character(client_data[cli,'password']))
  
  
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
                                     close = as.numeric(),
                                     target_order_id = as.numeric(),
                                     stop_loss_order_id = as.numeric(),
                                     final_order_id = as.numeric(),
                                     cancel_order_id = as.numeric()
  )
  
  if(length(my_positons_data$data) > 0 ){
    
    for(ind in 1:length(my_positons_data$data)){
      
      # browser()
      Sys.sleep(1)
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
  
  
  current_position_data = sqldf("select t1.symboltoken,
                                t1.symbolname,
                                t1.instrumenttype,
                                t1.priceden,
                                t1.pricenum,
                                t1.genden,
                                t1.gennum,
                                t1.precision,
                                t1.multiplier,
                                t1.boardlotsize,
                                t1.exchange,
                                t1.producttype,
                                t1.tradingsymbol,
                                t1.symbolgroup,
                                t1.strikeprice,
                                t1.optiontype,
                                t1.expirydate,
                                t1.lotsize,
                                t1.cfbuyqty,
                                t1.cfsellqty,
                                t1.cfbuyamount,
                                t1.cfsellamount,
                                t1.buyavgprice,
                                t1.sellavgprice,
                                t1.avgnetprice,
                                t1.netvalue,
                                t1.netqty,
                                t1.totalbuyvalue,
                                t1.totalsellvalue,
                                t1.cfbuyavgprice,
                                t1.cfsellavgprice,
                                t1.totalbuyavgprice,
                                t1.totalsellavgprice,
                                t1.netprice,
                                t1.buyqty,
                                t1.sellqty,
                                t1.buyamount,
                                t1.sellamount,
                                t1.pnl,
                                t1.realised,
                                t1.unrealised,
                                t1.ltp,
                                t1.close,
                                COALESCE(t2.target_order_id,t1.target_order_id) as target_order_id,
                                COALESCE(t2.stop_loss_order_id,t1.stop_loss_order_id) as stop_loss_order_id,
                                COALESCE(t2.final_order_id,t1.final_order_id) as final_order_id,
                                COALESCE(t2.cancel_order_id,t1.cancel_order_id) as cancel_order_id,
                                t2.netqty as prev_netqty
      
      from current_position_data t1 
      left join previous_orders t2 on t1.tradingsymbol = t2.tradingsymbol ")
  
  current_position_data$netqty[is.na(current_position_data$netqty)] <- 0
  current_position_data$prev_netqty[is.na(current_position_data$prev_netqty)] <- 0
  
  
  for (ord in 1:nrow(current_position_data)) {
    if (as.numeric(current_position_data[ord,"netprice"]) > 0) {
      
      browser()
      print(current_position_data[ord,])
      order_price <- as.numeric(current_position_data[ord,"netprice"])
      current_close <- as.numeric(current_position_data[ord,"ltp"])
      
      price_diff <- (order_price - current_close)*100 / order_price
      
      net_qty <- as.numeric(current_position_data[ord,"netqty"])
      
      ### the Target of 15 percent
      target_price <- round(as.numeric(order_price) + 0.15 * as.numeric(order_price),2)
      ### the Stoploss of 15 percent
      stop_loss_price <- round(as.numeric(order_price) - 0.15 * as.numeric(order_price),2)
      
      # print(price_diff)
      
      ### The order quantity has changed due to a new order
      if(as.numeric(current_position_data[ord,"netqty"]) != as.numeric(current_position_data[ord,"prev_netqty"]) && as.numeric(current_position_data[ord,"prev_netqty"]) > 0){
        
        #### Cancel the previous order regardless of Stoploss or Target and place the new target order
        
        if(!is.na(current_position_data[ord,"final_order_id"])){
          
          cancel_id = cancel_order(object = session_data,
                                   orderid = as.character(current_position_data[ord,"final_order_id"]),
                                   variety = "NORMAL")
          
          system("Say Cancelled the Target Order")
          
          print("Cancelling the previous order")
          
          current_position_data[ord,"cancel_order_id"] <- cancel_id
          
        }
        
        
        order_place <- place_order(object = session_data,
                                   variety= "NORMAL",
                                   tradingsymbol= as.character(current_position_data[ord,"tradingsymbol"]),
                                   symboltoken= as.character(current_position_data[ord,"symboltoken"]),
                                   transactiontype= "SELL",
                                   exchange= "NFO",
                                   ordertype = "LIMIT",
                                   producttype= "CARRYFORWARD",
                                   duration= "DAY",
                                   price= round(as.numeric(target_price),0),
                                   squareoff= 0,
                                   stoploss= 0,
                                   quantity= as.numeric(net_qty),
                                   triggerprice =  NULL
        )
        
        
        system("Say Placed the Target Order")
        
        print(paste("Placed the target order : ",order_place))
        
        current_position_data[ord,"target_order_id"] <- order_place
        current_position_data[ord,"final_order_id"] <- order_place
        
      }else{
        
        #### The previous order and the current order is the same as there is no additional qty added
        
        if(price_diff > 8){
          
          ### Placing the first order and its a stoploss order
          if(is.na(current_position_data[ord,"stop_loss_order_id"]) && is.na(current_position_data[ord,"final_order_id"])){
            
            
            order_place_sl <- place_order(object = session_data,
                                          variety= "STOPLOSS",
                                          tradingsymbol= as.character(current_position_data[ord,"tradingsymbol"]),
                                          symboltoken= as.character(current_position_data[ord,"symboltoken"]),
                                          transactiontype= "SELL",
                                          exchange= "NFO",
                                          triggerprice = round(as.numeric(stop_loss_price),0)+0.5,
                                          ordertype = "STOPLOSS_LIMIT",
                                          producttype= "CARRYFORWARD",
                                          duration = "DAY",
                                          price = as.numeric(round(stop_loss_price,0)),
                                          quantity= as.numeric(net_qty),
                                          stoploss =  as.numeric(round(stop_loss_price,0)),
                                          squareoff = as.numeric(round(as.numeric(order_price),0))
            )
            
            system("Say Placed the Stoploss Order")
            
            print(paste("Placed the stoploss order : ",order_place_sl))
            
            current_position_data[ord,"stop_loss_order_id"] <- order_place_sl
            current_position_data[ord,"final_order_id"] <- order_place_sl
            
          }else{

              
            
            if(current_position_data[ord,"final_order_id"] != current_position_data[ord,"stop_loss_order_id"]){
              
              print("Placing the Stoploss Order")
              
              if(!is.na(current_position_data[ord,"final_order_id"])){
                
                cancel_id = cancel_order(object = session_data,
                                         orderid = as.character(current_position_data[ord,"final_order_id"]),
                                         variety = "NORMAL")
                
                system("Say Cancelled the Target Order")
                
                print("Cancelling the target order")
                
                current_position_data[ord,"cancel_order_id"] <- cancel_id
                
              }
              
              
              order_place_sl <- place_order(object = session_data,
                                            variety= "STOPLOSS",
                                            tradingsymbol= as.character(current_position_data[ord,"tradingsymbol"]),
                                            symboltoken= as.character(current_position_data[ord,"symboltoken"]),
                                            transactiontype= "SELL",
                                            exchange= "NFO",
                                            triggerprice = round(as.numeric(stop_loss_price),0)+0.5,
                                            ordertype = "STOPLOSS_LIMIT",
                                            producttype= "CARRYFORWARD",
                                            duration = "DAY",
                                            price = as.numeric(round(stop_loss_price,0)),
                                            quantity= as.numeric(net_qty),
                                            stoploss =  as.numeric(round(stop_loss_price,0)),
                                            squareoff = as.numeric(round(as.numeric(order_price),0))
              )
              
              system("Say Placed the Stoploss Order")
              
              print(paste("Placed the stoploss order : ",order_place_sl))
              
              current_position_data[ord,"stop_loss_order_id"] <- order_place_sl
              current_position_data[ord,"final_order_id"] <- order_place_sl
              
            }
             
              
             
            
          }
          
        }else{

          ### Placing the first order and its a target order
          if(is.na(current_position_data[ord,"target_order_id"]) && is.na(current_position_data[ord,"final_order_id"])){
            
            print("Placing the Target Order")
            
            order_place <- place_order(object = session_data,
                                       variety= "NORMAL",
                                       tradingsymbol= as.character(current_position_data[ord,"tradingsymbol"]),
                                       symboltoken= as.character(current_position_data[ord,"symboltoken"]),
                                       transactiontype= "SELL",
                                       exchange= "NFO",
                                       ordertype = "LIMIT",
                                       producttype= "CARRYFORWARD",
                                       duration= "DAY",
                                       price= round(as.numeric(target_price),0),
                                       squareoff= 0,
                                       stoploss= 0,
                                       quantity= as.numeric(net_qty),
                                       triggerprice =  NULL
            )
            
            system("Say Placed the Target Order")
            
            print(paste("Placed the target order : ",order_place))
            
            current_position_data[ord,"target_order_id"] <- order_place
            current_position_data[ord,"final_order_id"] <- order_place
            
          }else{
            
            if(current_position_data[ord,"final_order_id"] != current_position_data[ord,"target_order_id"]){

              print("Placing the Target Order")
              
              
              # if(as.numeric(current_position_data[ord,"netqty"]) != as.numeric(current_position_data[ord,"prev_netqty"])){
              
              if(!is.na(current_position_data[ord,"final_order_id"])){
                
                cancel_id = cancel_order(object = session_data,
                                         orderid = as.character(current_position_data[ord,"final_order_id"]),
                                         variety = "NORMAL")
                
                system("Say Cancelled the Stoploss Order")
                
                print("Cancelling the stoploss order")
                
                current_position_data[ord,"cancel_order_id"] <- cancel_id
                
              }
              
              
              order_place <- place_order(object = session_data,
                                         variety= "NORMAL",
                                         tradingsymbol= as.character(current_position_data[ord,"tradingsymbol"]),
                                         symboltoken= as.character(current_position_data[ord,"symboltoken"]),
                                         transactiontype= "SELL",
                                         exchange= "NFO",
                                         ordertype = "LIMIT",
                                         producttype= "CARRYFORWARD",
                                         duration= "DAY",
                                         price= round(as.numeric(target_price),0),
                                         squareoff= 0,
                                         stoploss= 0,
                                         quantity= as.numeric(net_qty),
                                         triggerprice =  NULL
              )
              
              
              system("Say Placed the Target Order")
              
              print(paste("Placed the target order : ",order_place))
              
              current_position_data[ord,"target_order_id"] <- order_place
              current_position_data[ord,"final_order_id"] <- order_place

            
          }
          
          }
        }
        
      }
      
      
    }else{
      print(as.numeric(current_position_data[ord,"pnl"]))
      print(as.numeric(current_position_data[ord,"unrealised"]))
    }
    
    
  }
  
  write.csv(current_position_data,paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Updated_Targets",as.character(Sys.Date()),sep="/"),".csv",sep=""), row.names=FALSE)
  
  
}



end_time = Sys.time()
print(paste("Ended Executing the Script : ",Sys.time(),sep=""))

print(difftime(end_time,start_time,units = "secs"))