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

Sys.sleep(1)

session_data <- generate_session(login_object,user_id,"start@123")
# session_data <- generate_session(login_object,"S970011","Welcome@123")


# Telegram setup

bot <- Bot(token = "1931575614:AAFhtU1xieFDqC9WAAzw15G4KdB8rdzrif4")
chat_ids = c("535162272","714628563","1808943433","844935609","996359001","1623124565","1088161376","1612368682","507042774","473977639","488310125","373868886","1594535460","960024014", "892467810","1080210611","1710009819","1542490708","1285730430","1903472891")

# chat_id1 <-  # you can retrieve it from bot$getUpdates() after sending a message to the bot
# chat_id2 <- "714628563"
# chat_id3 <- "1808943433"
# chat_id4 <- "844935609"
# chat_id5 <- "996359001"
# # chat_id6 <- "846794885"
# chat_id7 <- "1623124565"
# chat_id8 <- "1088161376" ## Saurabh Chaitanya
# chat_id9 <- "1612368682"  ## Sumanth
# chat_id10 <- "507042774" ## Sreekanth
# chat_id11 <- "473977639"  ## Vamshi
# chat_id12 <- "488310125"  ##Karthik
# chat_id13 <- "373868886"  ##Ashish
# # chat_id14 <- "1594535460"  #Raju
# chat_id15 <- "960024014"  ##JackRyan
# # chat_id16 <- "892467810"  ## Chouhan
# chat_id17 <- "1080210611"  ## Ravi
# chat_id18 <- "1710009819" ##Anil Shetye
# chat_id19 <- "1542490708"  ##Chaitra
# chat_id20 <- "1285730430"  ## Suresh
# chat_id21 <- "1903472891" ## Turimala Vinay

nse_data <- data.frame(c("BANKNIFTY","Nifty50"),c("%5ENSEBANK","%5ENSEI"),c("BANKNIFTY-EQ","Nifty50-EQ"))

colnames(nse_data) <- c("Symbol","Yahoo Symbol","TradingSymbol")

# nse_data <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/Indian_Indices.csv", sep = ""))

Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))


increment = 1

bot_strategy <- c("sweths_violation","cowboy","reds_rocket","reds_brahmos","blackout","gap_up","gap_down","volume_breakout","abc_5_cand","abc_3_cand")


# stock <- "%5ENSEBANK"

stock = "%5ENSEI"


place_limit_order <- function(final_signals_df,row_index,user_id){
  
  # browser()
  
  Sys.sleep(1)
  
  if(user_id == "Y68412"){
    
    ###### For Suresh Account we are placing the historically successful orders with 2 lot size starting Nov 23rd
    
    if(final_signals_df[row_index,"historic_profit"] == 1){
      
      final_signals_df[row_index,"lotsize"] = 50
      
      order_place <- place_order(object = session_data,
                                 variety= "NORMAL",
                                 tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
                                 symboltoken= as.character(final_signals_df[row_index,"token"]),
                                 transactiontype= "BUY",
                                 exchange= "NFO",
                                 ordertype= "LIMIT",
                                 producttype= "CARRYFORWARD",
                                 duration= "DAY",
                                 price= as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),
                                 squareoff= as.numeric(final_signals_df[row_index,"premium_Target"]),
                                 stoploss= as.numeric(final_signals_df[row_index,"premium_StopLoss"]),
                                 quantity= as.numeric(final_signals_df[row_index,"lotsize"]),
                                 triggerprice = NULL
      )
      
      # order_place <- 1
      
      print(paste("the limit order id is ",order_place))
      
      for (cht in 1:length(chat_ids)) {
        tryCatch(
          {
            # bot$sendMessage(chat_id = chat_ids[cht], text = "Today was completely a Stoploss Hunting Day")
            bot$sendMessage(chat_id = chat_ids[cht], text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
          },
          error = function(cond){
            print(cond)
            
            
          }
        )
      }
      
      
      
      
      # bot$sendMessage(chat_id = chat_id2, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id3, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id4, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id5, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # # bot$sendMessage(chat_id = chat_id6, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id7, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id8, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id9, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id10, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # # bot$sendMessage(chat_id = chat_id11, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # # bot$sendMessage(chat_id = chat_id12, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id13, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # # bot$sendMessage(chat_id = chat_id14, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id15, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # # bot$sendMessage(chat_id = chat_id16, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id17, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id18, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id19, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      # bot$sendMessage(chat_id = chat_id20, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
      
      
      final_signals_df[row_index,"order_id"] <- order_place
      
      final_signals_df[row_index,"order_place"] <- 1
      final_signals_df[row_index,"conclusion"] <- "Pending"
      
      write.table(final_signals_df[row_index,], file = "~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Trading_Orders.csv", sep = ",",row.names = FALSE,append = T,col.names = FALSE)
      
      
      
    }else{
      
      order_place <- 1
      
      final_signals_df[row_index,"order_id"] <- order_place
      
      final_signals_df[row_index,"order_place"] <- 1
      final_signals_df[row_index,"conclusion"] <- "Historical Loss"
      
      write.table(final_signals_df[row_index,], file = "~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Trading_Orders.csv", sep = ",",row.names = FALSE,append = T,col.names = FALSE)
      
      
      
    }
    
    
  }
  else if(user_id == "P177115"){
    print("Skipping the Order")
    for (cht in 1:length(chat_ids)) {
      tryCatch(
        {
          # bot$sendMessage(chat_id = chat_ids[cht], text = "Today was completely a Stoploss Hunting Day")
          bot$sendMessage(chat_id = chat_ids[cht], text = paste("*Skipping the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
        },
        error = function(cond){
          print(cond)


        }
      )
    }

  }
  else if(user_id == "S970011"){
    print("Skipping the Order")
    for (cht in 1:length(chat_ids)) {
      tryCatch(
        {
          # bot$sendMessage(chat_id = chat_ids[cht], text = "Today was completely a Stoploss Hunting Day")
          bot$sendMessage(chat_id = chat_ids[cht], text = paste("*Skipping the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
        },
        error = function(cond){
          print(cond)


        }
      )
    }

  }else if(user_id == "J95213"){
    print("Skipping the Order")

    for (cht in 1:length(chat_ids)) {
      tryCatch(
        {
          # bot$sendMessage(chat_id = chat_ids[cht], text = "Today was completely a Stoploss Hunting Day")
          bot$sendMessage(chat_id = chat_ids[cht], text = paste("*Skipping the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
        },
        error = function(cond){
          print(cond)


        }
      )
    }
  }
  else{
    
    order_place <- place_order(object = session_data,
                               variety= "NORMAL",
                               tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
                               symboltoken= as.character(final_signals_df[row_index,"token"]),
                               transactiontype= "BUY",
                               exchange= "NFO",
                               ordertype= "LIMIT",
                               producttype= "CARRYFORWARD",
                               duration= "DAY",
                               price= as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),
                               squareoff= as.numeric(final_signals_df[row_index,"premium_Target"]),
                               stoploss= as.numeric(final_signals_df[row_index,"premium_StopLoss"]),
                               quantity= as.numeric(final_signals_df[row_index,"lotsize"]),
                               triggerprice = NULL
    )
    
    # order_place <- 1
    
    print(paste("the limit order id is ",order_place))
    
    for (cht in 1:length(chat_ids)) {
      tryCatch(
        {
          # bot$sendMessage(chat_id = chat_ids[cht], text = "Today was completely a Stoploss Hunting Day")
          bot$sendMessage(chat_id = chat_ids[cht], text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
        },
        error = function(cond){
          print(cond)
          
          
        }
      )
    }
    
    
    # 
    # bot$sendMessage(chat_id = chat_id1, text = "Testing")
    # bot$sendMessage(chat_id = chat_id1, text = "Testing")
    # bot$sendMessage(chat_id = chat_id2, text = "Testing")
    # bot$sendMessage(chat_id = chat_id3, text = "Testing")
    # bot$sendMessage(chat_id = chat_id4, text = "Testing")
    # bot$sendMessage(chat_id = chat_id5, text = "Testing")
    # # bot$sendMessage(chat_id = chat_id6, text = "Fixed the issue and the RCA is some of them uninstalled Telegram")
    # bot$sendMessage(chat_id = chat_id7, text = "Testing")
    # bot$sendMessage(chat_id = chat_id8, text = "Testing")
    # bot$sendMessage(chat_id = chat_id9, text = "Testing")
    # bot$sendMessage(chat_id = chat_id10, text = "Testing")
    # bot$sendMessage(chat_id = chat_id11, text = "Testing")
    # # bot$sendMessage(chat_id = chat_id12, text = "Testing")
    # bot$sendMessage(chat_id = chat_id13, text = "Testing")
    # # bot$sendMessage(chat_id = chat_id14, text = "Good Morning, Hope you booked profit")
    # bot$sendMessage(chat_id = chat_id15, text = "Testing")
    # # bot$sendMessage(chat_id = chat_id16, text = "Good Morning, Hope you booked profit")
    # bot$sendMessage(chat_id = chat_id17, text = "Testing")
    # bot$sendMessage(chat_id = chat_id18, text = "Testing")
    
    # bot$sendMessage(chat_id = chat_id1, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id2, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id3, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id4, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id5, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # # bot$sendMessage(chat_id = chat_id6, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id7, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id8, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id9, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id10, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id11, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # # bot$sendMessage(chat_id = chat_id12, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id13, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # # bot$sendMessage(chat_id = chat_id14, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id15, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # # bot$sendMessage(chat_id = chat_id16, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id17, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id18, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    # bot$sendMessage(chat_id = chat_id19, text = paste("*Placed the Limit order in Angel Broking for Client Id :",as.character(user_id),"\n--------------------------------------------------\n ","Time : ",as.character(final_signals_df[row_index,"Datetime"]),"\n Strategy : ",as.character(final_signals_df[row_index,"Strategy"]),"\n Script : ",as.character(final_signals_df[row_index,"current_script"]),"\n Buy Price : ",as.numeric(final_signals_df[row_index,"Strike_Buy_Price"]),"\n Premium Target :",as.numeric(final_signals_df[row_index,"premium_Target"]),"\n Premium Stoploss : ",as.numeric(final_signals_df[row_index,"premium_StopLoss"]),"\n Lotsize : ",as.numeric(final_signals_df[row_index,"lotsize"]),"\n--------------------------------------------------"))
    
    final_signals_df[row_index,"order_id"] <- order_place
    
    final_signals_df[row_index,"order_place"] <- 1
    final_signals_df[row_index,"conclusion"] <- "Pending"
    
    write.table(final_signals_df[row_index,], file = "~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Trading_Orders.csv", sep = ",",row.names = FALSE,append = T,col.names = FALSE)
    
  }
  
  
  
  
  return(final_signals_df)
  
}

place_target_order <- function(final_signals_df,row_index){
  
  Sys.sleep(1)
  
  order_place <- place_order(object = session_data,
                             variety= "NORMAL",
                             tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
                             symboltoken= as.character(final_signals_df[row_index,"token"]),
                             transactiontype= "SELL",
                             exchange= "NFO",
                             ordertype = "LIMIT",
                             producttype= "CARRYFORWARD",
                             duration= "DAY",
                             price= round(as.numeric(final_signals_df[row_index,"premium_Target"]),0),
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
  
  order_place_sl <- place_order(object = session_data,
                                variety= "STOPLOSS",
                                tradingsymbol= as.character(final_signals_df[row_index,"current_script"]),
                                symboltoken= as.character(final_signals_df[row_index,"token"]),
                                transactiontype= "SELL",
                                exchange= "NFO",
                                triggerprice = round(as.numeric(final_signals_df[row_index,"premium_StopLoss"]),0)+0.5,
                                ordertype = "STOPLOSS_LIMIT",
                                producttype= "CARRYFORWARD",
                                duration = "DAY",
                                price = as.numeric(round(final_signals_df[row_index,"premium_StopLoss"],0)),
                                quantity= as.numeric(prev_qty),
                                stoploss =  as.numeric(round(final_signals_df[row_index,"premium_StopLoss"],0)),
                                squareoff = as.numeric(round(as.numeric(final_signals_df[row_index,"premium_Target"]),0))
  )
  
  print(paste("the stop loss order id is ",order_place_sl))
  
  # Sys.sleep(1)
  
  
  final_signals_df[row_index,"stop_loss_order_id"] <- order_place_sl
  final_signals_df[row_index,"final_order_id"] <- order_place_sl
  final_signals_df[row_index,"conclusion"] <- "Stoploss Pending"
  
  return(final_signals_df)
}

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
          
          Sys.sleep(1)
          
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
          
          Sys.sleep(1)
          
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
          # next
          # print("next")
        }
        
        # print(sub_df[1,4])
        
        if(sub_df[1,4] == "Yes"){
          satisfied_df = data.frame()
          
          for(i in 1:nrow(final_data)){
            # print(final_data[i,"Close"])
            # print(sub_df[1,5])
            if((final_data[i,"Close"]) < sub_df[1,5]){
              # print("Cowboy Sell")
              
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
              
              Sys.sleep(1)
              
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
            
            Sys.sleep(1)
            
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
              
              Sys.sleep(1)
              
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
            
            Sys.sleep(1)
            
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
            
            Sys.sleep(1)
            
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
    
    Sys.sleep(1)
    
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
        
        browser()
        
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
          
          Sys.sleep(1)
          
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
          
          Sys.sleep(1)
          
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
      
      fetchurl <- paste0("OPTIDXBANKNIFTY","16-12-2021",side_dir,as.character(Signal_df[i,"Spot_Price"]),".00")
      lookup_symbol <- paste0("BANKNIFTY","16DEC21",as.character(Signal_df[i,"Spot_Price"]),side_dir)
      
    }else{
      #### Get the spot price value at the money
      
      Signal_df[i,"Spot_Price"] <- round(ifelse(Signal_df[i,"Value"] %% 100 > 25,(Signal_df[i,"Value"] + (50 - Signal_df[i,"Value"] %% 50)),Signal_df[i,"Value"] - Signal_df[i,"Value"] %% 50),2)
      
      #### Get the two steps in the money
      
      Signal_df[i,"Spot_Price"] <- ifelse(side_dir == "CE", Signal_df[i,"Spot_Price"] - 100, Signal_df[i,"Spot_Price"] + 100)
      
      fetchurl <- paste0("OPTIDXNIFTY","16-12-2021",side_dir,as.character(Signal_df[i,"Spot_Price"]),".00")
      lookup_symbol <- paste0("NIFTY","16DEC21",as.character(Signal_df[i,"Spot_Price"]),side_dir)
    }
    
    Signal_df[i,"expiry"] = as.Date("2021-12-16")
    
    # print(fetchurl)
    print(lookup_symbol)
    
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
      
      print(bk_test_data)
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





# colnames(Signal_df)



client_data <- read.csv(paste0("~/Downloads/Reddy_Stocks_Application/Client_Details.csv", sep = ""))

for (cli in 1:nrow(client_data)) {
  
  # browser()
  
  login_params = list(api_key = client_data[cli,'api_key'])
  
  login_object = create_connection_object(login_params)
  
  user_id = client_data[cli,'client_id']
  
  print(paste0("Running for client id : ",user_id))
  
  Sys.sleep(1)
  
  session_data <- generate_session(login_object,user_id,client_data[cli,'password'])
  
  # print(session_data)
  
  if(file.exists(paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Options_Order",as.character(Sys.Date()),sep="/"),".csv",sep=""))){
    temp_signals_df <- read.csv(paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Options_Order",as.character(Sys.Date()),sep="/"),".csv",sep=""))
    # temp_signals_df <- na.omit(temp_signals_df)
    rownames(temp_signals_df) <- NULL
    
    temp_signals_df$Datetime <- as.POSIXct(anytime(temp_signals_df$Datetime), origin="1970-01-01")
    temp_signals_df <- temp_signals_df[!is.na(temp_signals_df$Datetime),]
    rownames(temp_signals_df) <- NULL
    temp_signals_df$expiry <- as.Date(temp_signals_df$expiry)
    Signal_df$expiry <- as.Date(Signal_df$expiry)
    
  }else{
    temp_signals_df <- data.frame("Strategy"=character(),
               "Stock"=character(),
               "Signal"=character(),
               "Datetime"=character(),
               "Value"=double(),
               "StopLoss"=double(),
               "Target"=double(),
               "Qty"=integer(),
               "Spot_Price"=integer(),
               "expiry"=as.Date(character()),
               "Strike_Buy_Price"=double(),
               "premium_StopLoss"=double(),
               "premium_Target"=double(),
               "lotsize"=integer(),
               "premium_Qty"=integer(),
               "historic_profit"=integer(),
               "token"=integer(),
               "current_script"=character(),
               "exec_rnk"=integer(),
               "order_place"=integer(),
               "order_id"=double(),
               "target_order_id"=integer(),
               "stop_loss_order_id"=integer(),
               "cancel_order_id"=integer(),
               "final_order_id"=integer(),
               "robo_order_id"=integer(),
               "conclusion"=character(),
               "execution_time"=character(),
               "target_hit"=logical(),
               "avg_buy_price"=character(),
               "avg_sell_price"=character(),
               "avg_qty"=integer(),
               "adjusted_target"=character(),
               "adjusted_stoploss"=character()
      
    )
  
  }
  
  tryCatch(
    {
      
      
      
      if(nrow(Signal_df) > 0 ){
        # source_python('~/Downloads/Reddy_Stocks_Application/get_current_options_data.py')
        
        # system("say Got a Signal !!")
        
        temp_signals_df <- sqldf("select Strategy,
                                  Stock,
                                  Signal,
                                  Datetime,
                                  Value,
                                  StopLoss,
                                  Target,
                                  Qty,
                                  Spot_Price,
                                  expiry,
                                  Strike_Buy_Price,
                                  premium_StopLoss,
                                  premium_Target,
                                  lotsize,
                                  premium_Qty,
                                  historic_profit,
                                  current_script,
                                  token,
                                  exec_rnk,
                                  order_place,
                                  order_id,
                                  target_order_id,
                                  stop_loss_order_id,
                                  cancel_order_id,
                                  final_order_id,
                                  robo_order_id,
                                  conclusion,
                                  execution_time,
                                  target_hit,
                                  avg_buy_price,
                                  avg_sell_price,
                                  avg_qty,
                                  adjusted_target,
                                  adjusted_stoploss,
                                  rank() over(partition by Strategy,Stock,Datetime order by execution_time desc) as rnk

                           from temp_signals_df")
        
        final_signals_df <- sqldf("select COALESCE(tsd.Strategy,sd.Strategy) as Strategy,
                                  COALESCE(tsd.Stock,sd.Stock) as Stock,
                                  COALESCE(tsd.Signal,sd.Signal) as Signal,
                                  COALESCE(tsd.Datetime,sd.Datetime) as Datetime,
                                  COALESCE(tsd.Value,sd.Value) as Value,
                                  COALESCE(tsd.StopLoss,sd.StopLoss) as StopLoss,
                                  COALESCE(tsd.Target,sd.Target) as Target,
                                  COALESCE(tsd.Qty,sd.Qty) as Qty,
                                  COALESCE(tsd.Spot_Price,sd.Spot_Price) as Spot_Price,
                                  COALESCE(tsd.expiry,sd.expiry) as expiry,
                                  COALESCE(sd.Strike_Buy_Price,tsd.Strike_Buy_Price) as Strike_Buy_Price,
                                  COALESCE(sd.premium_StopLoss,tsd.premium_StopLoss) as premium_StopLoss,
                                  COALESCE(sd.premium_Target,tsd.premium_Target) as premium_Target,
                                  COALESCE(sd.lotsize,tsd.lotsize) as lotsize,
                                  COALESCE(sd.premium_Qty,tsd.premium_Qty) as premium_Qty,
                                  COALESCE(sd.historic_profit,tsd.historic_profit) as historic_profit,
                                  COALESCE(tsd.token,sd.token) as token,
                                  COALESCE(tsd.current_script,sd.current_script) as current_script,
                                  COALESCE(tsd.exec_rnk,sd.exec_rnk) as exec_rnk,
                                  COALESCE(cast(tsd.order_place as numeric),cast(sd.order_place as numeric) )as order_place,
                                  COALESCE(cast(tsd.order_id as numeric),cast(sd.order_id as numeric)) as order_id,
                                  COALESCE(cast(tsd.target_order_id as numeric),cast(sd.target_order_id as numeric)) as target_order_id,
                                  COALESCE(cast(tsd.stop_loss_order_id as numeric),cast(sd.stop_loss_order_id as numeric)) as stop_loss_order_id,
                                  COALESCE(cast(tsd.cancel_order_id as numeric),cast(sd.cancel_order_id as numeric)) as cancel_order_id,
                                  COALESCE(cast(tsd.final_order_id as numeric),cast(sd.final_order_id as numeric)) as final_order_id,
                                  COALESCE(cast(tsd.robo_order_id as numeric),cast(sd.robo_order_id as numeric)) as robo_order_id,
                                  
                                  COALESCE(tsd.conclusion,sd.conclusion) as conclusion,
                                  COALESCE(tsd.execution_time,sd.execution_time) as execution_time,
                                  COALESCE(tsd.target_hit,sd.target_hit) as target_hit,
                                  COALESCE(tsd.avg_buy_price,sd.avg_buy_price) as avg_buy_price,
                                  COALESCE(tsd.avg_sell_price,sd.avg_sell_price) as avg_sell_price,
                                  COALESCE(tsd.avg_qty,sd.avg_qty) as avg_qty,
                                  COALESCE(tsd.adjusted_target,sd.adjusted_target) as adjusted_target,
                                  COALESCE(tsd.adjusted_stoploss,sd.adjusted_stoploss) as adjusted_stoploss
                           from Signal_df sd
                           left join (select * from temp_signals_df where rnk =1) tsd on sd.Strategy = tsd.Strategy and sd.Stock = tsd.Stock and sd.Datetime = tsd.Datetime and sd.current_script = tsd.current_script
                          ")
        
        final_signals_df$expiry <- as.Date(final_signals_df$expiry,origin="1970-01-01")
        print(final_signals_df)
        for (i in 1:nrow(final_signals_df)) {
          
          # browser()
          
          if(as.numeric(difftime(Sys.time(), final_signals_df[i,"Datetime"], units ="mins")) <= 20){
            
            # browser()
            
            time_min = format(final_signals_df[i,"Datetime"], format="%H:%M:%S")
            # print(time_min)
            
            if(time_min <= "15:10:00"){
              
              
              if(final_signals_df[i,"order_id"] == 0){
                
                print("Placing the limit order")
                
                
                
                
                final_signals_df <- place_limit_order(final_signals_df,i,user_id)
                
                
                # order_place <- place_order(object = session_data,
                #                            variety= "NORMAL",
                #                            tradingsymbol= as.character(final_signals_df[i,"current_script"]),
                #                            symboltoken= as.character(final_signals_df[i,"token"]),
                #                            transactiontype= "BUY",
                #                            exchange= "NFO",
                #                            ordertype= "LIMIT",
                #                            producttype= "CARRYFORWARD",
                #                            duration= "DAY",
                #                            price= as.numeric(final_signals_df[i,"Strike_Buy_Price"]),
                #                            squareoff= as.numeric(final_signals_df[i,"premium_Target"]),
                #                            stoploss= as.numeric(final_signals_df[i,"premium_StopLoss"]),
                #                            quantity= as.numeric(final_signals_df[i,"lotsize"]),
                #                            triggerprice = NULL
                # )
                
                
                # final_signals_df[i,"order_id"] <- order_place
                # 
                # final_signals_df[i,"order_place"] <- 1
                # final_signals_df[i,"conclusion"] <- "Pending"
                
                
                
                
                
                system("say Got a Signal !!")
                system("say Placed a Buy Order !!")
                
              }
              #######################################################
              
              # else{
              # # browser()
              # 
              # my_orders <- order_book(session_data)
              # Sys.sleep(1)
              # 
              # order_data = data.frame(variety = as.character(),
              #                         ordertype=as.character(),
              #                         producttype=as.character(),
              #                         duration=as.character(),
              #                         price = as.character(),
              #                         triggerprice = as.character(),
              #                         quantity = as.character(),
              #                         disclosedquantity = as.character(),
              #                         squareoff = as.character(),
              #                         stoploss = as.character(),
              #                         trailingstoploss = as.character(),
              #                         tradingsymbol = as.character(),
              #                         transactiontype = as.character(),
              #                         exchange = as.character(),
              #                         symboltoken = as.character(),
              #                         ordertag = as.character(),
              #                         instrumenttype = as.character(),
              #                         strikeprice = as.character(),
              #                         optiontype = as.character(),
              #                         expirydate = as.character(),
              #                         lotsize = as.character(),
              #                         cancelsize = as.character(),
              #                         averageprice = as.character(),
              #                         filledshares = as.character(),
              #                         unfilledshares = as.character(),
              #                         orderid = as.character(),
              #                         text = as.character(),
              #                         status = as.character(),
              #                         updatetime = as.character(),
              #                         exchorderupdatetime = as.character(),
              #                         fillid = as.character(),
              #                         filltime = as.character(),
              #                         parentorderid = as.character()
              # )
              # 
              # if(length(my_orders$data) > 0 ){
              #   
              #   for(ind in 1:length(my_orders$data)){
              #     
              #     order_data[ind,"variety"] = my_orders$data[[ind]]$variety
              #     order_data[ind,"ordertype"] = my_orders$data[[ind]]$ordertype
              #     order_data[ind,"producttype"] = my_orders$data[[ind]]$producttype
              #     order_data[ind,"duration"] = my_orders$data[[ind]]$duration
              #     order_data[ind,"price"] = my_orders$data[[ind]]$price
              #     order_data[ind,"triggerprice"] = my_orders$data[[ind]]$triggerprice
              #     order_data[ind,"quantity"] = my_orders$data[[ind]]$quantity
              #     order_data[ind,"disclosedquantity"] = my_orders$data[[ind]]$disclosedquantity
              #     order_data[ind,"squareoff"] = my_orders$data[[ind]]$squareoff
              #     order_data[ind,"stoploss"] = my_orders$data[[ind]]$stoploss
              #     order_data[ind,"trailingstoploss"] = my_orders$data[[ind]]$trailingstoploss
              #     order_data[ind,"tradingsymbol"] = my_orders$data[[ind]]$tradingsymbol
              #     order_data[ind,"transactiontype"] = my_orders$data[[ind]]$transactiontype
              #     order_data[ind,"exchange"] = my_orders$data[[ind]]$exchange
              #     order_data[ind,"symboltoken"] = my_orders$data[[ind]]$symboltoken
              #     order_data[ind,"ordertag"] = my_orders$data[[ind]]$ordertag
              #     order_data[ind,"instrumenttype"] = my_orders$data[[ind]]$instrumenttype
              #     order_data[ind,"strikeprice"] = my_orders$data[[ind]]$strikeprice
              #     order_data[ind,"optiontype"] = my_orders$data[[ind]]$optiontype
              #     order_data[ind,"expirydate"] = my_orders$data[[ind]]$expirydate
              #     order_data[ind,"lotsize"] = my_orders$data[[ind]]$lotsize
              #     order_data[ind,"cancelsize"] = my_orders$data[[ind]]$cancelsize
              #     order_data[ind,"averageprice"] = my_orders$data[[ind]]$averageprice
              #     order_data[ind,"filledshares"] = my_orders$data[[ind]]$filledshares
              #     order_data[ind,"unfilledshares"] = my_orders$data[[ind]]$unfilledshares
              #     order_data[ind,"orderid"] = my_orders$data[[ind]]$orderid
              #     order_data[ind,"text"] = my_orders$data[[ind]]$text
              #     order_data[ind,"status"] = my_orders$data[[ind]]$status
              #     order_data[ind,"updatetime"] = my_orders$data[[ind]]$updatetime
              #     order_data[ind,"exchorderupdatetime"] = my_orders$data[[ind]]$exchorderupdatetime
              #     order_data[ind,"fillid"] = my_orders$data[[ind]]$fillid
              #     order_data[ind,"filltime"] = my_orders$data[[ind]]$filltime
              #     order_data[ind,"parentorderid"] = my_orders$data[[ind]]$parentorderid
              #     
              #   }
              # }
              # 
              # browser()
              # 
              # last_order <- order_data[order_data$orderid == as.character(final_signals_df[i,"order_id"]),]
              # 
              # 
              # if(final_signals_df[i,"target_order_id"] == 0){
              #   
              #   
              #   
              #   if(last_order$status == "complete"){
              #     
              #     # prev_generation <- final_signals_df[i,]
              #     # 
              #     # # print(prev_generation)
              #     # 
              #     # prev_trading_symbol = final_signals_df[i,"current_script"]
              #     # prev_symbol_token = final_signals_df[i,"token"]
              #     # 
              #     # 
              #     # prev_exchange_counter = "NFO"
              #     # prev_order_type = "LIMIT"
              #     # prev_product_type = "CARRYFORWARD"
              #     # prev_duration_day = "DAY"
              #     # prev_stoploss = as.numeric(final_signals_df[i,"premium_StopLoss"])
              #     # 
              #     # prev_squareoff = as.numeric(final_signals_df[i,"premium_Target"])
              #     # 
              #     # prev_qty = as.numeric(final_signals_df[i,"lotsize"])
              #     # prev_limit_price = as.numeric((final_signals_df[i,"Strike_Buy_Price"]))
              #     
              #     #
              #     # order_place <- place_order(object = session_data,
              #     #                            variety= "NORMAL",
              #     #                            tradingsymbol= as.character(lookup_symbol),
              #     #                            symboltoken= as.character(token),
              #     #                            transactiontype= "BUY",
              #     #                            exchange= "NFO",
              #     #                            ordertype= "LIMIT",
              #     #                            producttype= "CARRYFORWARD",
              #     #                            duration= "DAY",
              #     #                            price= ltp,
              #     #                            squareoff= ltp+10,
              #     #                            stoploss= ltp-10,
              #     #                            quantity= Signal_df[i,"lotsize"],
              #     #                            triggerprice = NULL
              #     # )
              #     
              #     
              #     
              #     # order_place <- place_order(object = session_data,
              #     #                            variety= "NORMAL",
              #     #                            tradingsymbol= as.character(prev_trading_symbol),
              #     #                            symboltoken= as.character(prev_symbol_token),
              #     #                            transactiontype= "SELL",
              #     #                            exchange= "NFO",
              #     #                            ordertype = "LIMIT",
              #     #                            producttype= "CARRYFORWARD",
              #     #                            duration= "DAY",
              #     #                            price= round(as.numeric(prev_squareoff),0),
              #     #                            squareoff= 0,
              #     #                            stoploss= 0,
              #     #                            quantity= prev_qty,
              #     #                            triggerprice =  NULL
              #     # )
              #     # 
              #     # Sys.sleep(1)
              #     # 
              #     # final_signals_df[(i),"target_order_id"] <- order_place
              #     # final_signals_df[(i),"final_order_id"] <- order_place
              #     # # final_signals_df[(i),"target_place"] <- 1
              #     # final_signals_df[(i),"conclusion"] <- "Target Pending"
              #     
              #     print("Limit order completed and placing the target order")
              #     
              #     final_signals_df <- place_target_order(final_signals_df,i)
              #     
              #     
              #     system("say Placed the target Order !!")
              #     
              #   }
              #   
              #   
              #   
              # }
              # 
              # 
              # 
              # }
              
              ##############################################################################
              
            }
          }else{
            
            # browser()
            
            # if(execute_df[i,"cancel_order_id"] == 0){
            
            # if(as.Date(as.POSIXct(final_signals_df[i,"execution_time"], origin="1970-01-01")) == Sys.Date()){
            # if(as.Date(ifelse(nchar(final_signals_df[i,"execution_time"]) == 19,as.Date(as.POSIXct(final_signals_df[i,"execution_time"], origin="1970-01-01")),as.Date(as.POSIXct(as.numeric(substring(final_signals_df[i,"execution_time"],1,10)), origin="1970-01-01")) )) == Sys.Date()){
            
            # ########## Current Execution details ###########
            # current_stock_df <- final_signals_df[final_signals_df$Stock == final_signals_df[i,"Stock"],]
            # # print(current_stock_df)
            # 
            # ###########  Previous Order Details ###########
            # prev_order = current_stock_df[current_stock_df$exec_rnk==(i),]
            
            prev_order = final_signals_df[i,]
            
            
            rownames(prev_order) <- 1
            # print(prev_order)
            prev_order_id <- prev_order[1,"order_id"]
            prev_target_order_id <- prev_order[1,"target_order_id"]
            prev_final_order_id <- prev_order[1,"final_order_id"]
            
            if(prev_order_id > 0){
              
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
              
              
              last_order <- order_data[order_data$orderid == as.character(prev_order_id),]
              
              # browser()
              
              if((as.numeric(difftime(Sys.time(), final_signals_df[i,"Datetime"], units ="mins")) >= 30)){
                
                if(nrow(last_order) > 0){
                  
                  if(last_order$status == "open"){
                    
                    prev_order_id_details <- final_signals_df[i,"order_id"]
                    
                    print("cancelling the limit order as the time passed")
                    
                    
                    cancel_id = cancel_order(object = session_data,
                                             orderid = as.character(prev_order_id_details),
                                             variety = "NORMAL")
                    
                    system("say Cancelled the prevoius Order !!")
                    
                    final_signals_df[(i),"cancel_order_id"] <- cancel_id
                    final_signals_df[(i),"final_order_id"] <- cancel_id
                    final_signals_df[(i),"conclusion"] <- "Cancelled as Limit not achieved"
                    
                  }
                  
                }
                
                
                
                
              }
              
              ######################################################################
              
              # else{
              #   if(final_signals_df[i,"target_order_id"] == 0){
              #     
              #     browser()
              #     
              #     if(last_order$status == "complete"){
              #       
              #       # prev_generation <- final_signals_df[i,]
              #       # 
              #       # # print(prev_generation)
              #       # 
              #       # prev_trading_symbol = final_signals_df[i,"current_script"]
              #       # prev_symbol_token = final_signals_df[i,"token"]
              #       # 
              #       # 
              #       # prev_exchange_counter = "NFO"
              #       # prev_order_type = "LIMIT"
              #       # prev_product_type = "CARRYFORWARD"
              #       # prev_duration_day = "DAY"
              #       # prev_stoploss = as.numeric(final_signals_df[i,"premium_StopLoss"])
              #       # 
              #       # prev_squareoff = as.numeric(final_signals_df[i,"premium_Target"])
              #       # 
              #       # prev_qty = as.numeric(final_signals_df[i,"lotsize"])
              #       # prev_limit_price = as.numeric((final_signals_df[i,"Strike_Buy_Price"]))
              #       
              #       
              #       
              #       
              #       
              #       
              #       # order_place <- place_order(object = session_data,
              #       #                            variety= "NORMAL",
              #       #                            tradingsymbol= as.character(prev_trading_symbol),
              #       #                            symboltoken= as.character(prev_symbol_token),
              #       #                            transactiontype= "SELL",
              #       #                            exchange= "NFO",
              #       #                            ordertype = "LIMIT",
              #       #                            producttype= "CARRYFORWARD",
              #       #                            duration= "DAY",
              #       #                            price= round(as.numeric(prev_squareoff),0),
              #       #                            squareoff= 0,
              #       #                            stoploss= 0,
              #       #                            quantity= prev_qty,
              #       #                            triggerprice =  NULL
              #       # )
              #       # 
              #       # Sys.sleep(1)
              #       # 
              #       # final_signals_df[(i),"target_order_id"] <- order_place
              #       # final_signals_df[(i),"final_order_id"] <- order_place
              #       # # final_signals_df[(i),"target_place"] <- 1
              #       # final_signals_df[(i),"conclusion"] <- "Target Pending"
              #       
              #       print("placing the first target order")
              #       
              #       final_signals_df <- place_target_order(final_signals_df,i)
              #       
              #       
              #       system("say Placed the target Order !!")
              #       
              #     }
              #     
              #     
              #     
              #   }else if(final_signals_df[i,"final_order_id"] != 0){
              #     
              #     last_target_order <- order_data[order_data$orderid == as.character(prev_target_order_id),]
              #     last_final_order <- order_data[order_data$orderid == as.character(prev_final_order_id),]
              #     
              #     
              #     
              #     
              #     prev_generation <- final_signals_df[(i),]
              #     
              #     # print(prev_generation)
              #     
              #     prev_trading_symbol = final_signals_df[i,"current_script"]
              #     prev_symbol_token = final_signals_df[i,"token"]
              #     
              #     
              #     prev_exchange_counter = "NFO"
              #     prev_order_type = "LIMIT"
              #     prev_product_type = "CARRYFORWARD" 
              #     prev_duration_day = "DAY"
              #     prev_stoploss = as.numeric(final_signals_df[i,"premium_StopLoss"])
              #     
              #     prev_squareoff = as.numeric(final_signals_df[i,"premium_Target"])
              #     
              #     prev_qty = as.numeric(final_signals_df[i,"lotsize"])
              #     prev_limit_price = as.numeric((final_signals_df[i,"Strike_Buy_Price"]))
              #     
              #     current_traded_price <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(prev_trading_symbol),symboltoken = as.character(prev_symbol_token))
              #     
              #     current_traded_price <- current_traded_price$data$ltp
              #     
              #     price_diff <- ((prev_limit_price - current_traded_price)/prev_limit_price)*100
              #     
              #     
              #     if(last_final_order$status == "complete"){
              #       
              #       print("Order complete")
              #       final_signals_df[i,"conclusion"] <- "Completed"
              #       print("Target Achieved")
              #     }else if (last_final_order$status == "open"){
              #       
              #       if(price_diff > 6){
              #         
              #         print("Cancelling the target and placing the stop loss order")
              #         
              #         cancel_id = cancel_order(object = session_data,
              #                                  orderid = as.character(prev_final_order_id),
              #                                  variety = "NORMAL")
              #         
              #         system("say Cancelled the target Order !!")
              #         
              #         final_signals_df[(i),"cancel_id"] <- cancel_id
              #         
              #         Sys.sleep(1)
              #         
              #         # order_place_sl <- place_order(object = session_data,
              #         #                               variety= "STOPLOSS",
              #         #                               tradingsymbol= as.character(prev_trading_symbol),
              #         #                               symboltoken= as.character(prev_symbol_token),
              #         #                               transactiontype= "SELL",
              #         #                               exchange= "NFO",
              #         #                               triggerprice = round(as.numeric(prev_stoploss),0)+0.5,
              #         #                               ordertype = "STOPLOSS_LIMIT",
              #         #                               producttype= "CARRYFORWARD",
              #         #                               duration = "DAY",
              #         #                               price = as.numeric(round(prev_stoploss,0)),
              #         #                               quantity= as.numeric(prev_qty),
              #         #                               stoploss =  as.numeric(round(prev_stoploss,0)),
              #         #                               squareoff = as.numeric(round(prev_squareoff,0))
              #         # )
              #         # 
              #         # Sys.sleep(1)
              #         # 
              #         # 
              #         # final_signals_df[(i),"stop_loss_order_id"] <- order_place_sl
              #         # final_signals_df[(i),"final_order_id"] <- order_place_sl
              #         # final_signals_df[(i),"conclusion"] <- "Stoploss Pending"
              #         
              #         final_signals_df <- place_stoploss_order(final_signals_df,i)
              #         
              #         system("say Placed the Stoploss Order !!")
              #         
              #       }else{
              #         
              #         if((final_signals_df[(i),"conclusion"] == "Stoploss Pending") && price_diff < -1){
              #           
              #           print("Cancelling the stop loss and placing the target order")
              #           
              #           cancel_id = cancel_order(object = session_data,
              #                                    orderid = as.character(prev_final_order_id),
              #                                    variety = "NORMAL")
              #           
              #           system("say Cancelled the stoploss Order !!")
              #           
              #           final_signals_df[(i),"cancel_order_id"] <- cancel_id
              #           
              #           Sys.sleep(1)
              #           
              #           final_signals_df <- place_target_order(final_signals_df,i)
              #           
              #           
              #           # order_place <- place_order(object = session_data,
              #           #                            variety= "NORMAL",
              #           #                            tradingsymbol= as.character(prev_trading_symbol),
              #           #                            symboltoken= as.character(prev_symbol_token),
              #           #                            transactiontype= "SELL",
              #           #                            exchange= "NFO",
              #           #                            ordertype = "LIMIT",
              #           #                            producttype= "CARRYFORWARD",
              #           #                            duration= "DAY",
              #           #                            price= round(as.numeric(prev_squareoff),0),
              #           #                            squareoff= 0,
              #           #                            stoploss= 0,
              #           #                            quantity= prev_qty,
              #           #                            triggerprice =  NULL
              #           # )
              #           # 
              #           # 
              #           # 
              #           # Sys.sleep(1)
              #           # 
              #           # 
              #           # final_signals_df[(i),"target_order_id"] <- order_place
              #           # final_signals_df[(i),"final_order_id"] <- order_place
              #           # final_signals_df[(i),"cancel_order_id"] <- cancel_id
              #           # # final_signals_df[(i),"target_place"] <- 1
              #           # final_signals_df[(i),"conclusion"] <- "Target Pending"
              #           
              #           system("say Placed the target Order !!")
              #           
              #         }
              #         
              #       }
              #     }else{
              #       print("Passing to next order")
              #     }
              #     
              #     
              #   }
              #   
              # }
              
              ######################################################################
              
            }
            
            # }
            
            
            
            
          }
        }
      }
    },
    error = function(cond){
      print(cond)
      
    },
    finally={
      
      if(nrow(Signal_df) > 0){
        # print("Signal Data Frame")
        # print(Signal_df)
        print("Final Signal Data Frame")
        print(final_signals_df)
        
        final_signals_df$execution_time = Sys.time()
        
        if(file.exists(paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Options_Order",as.character(Sys.Date()),sep="/"),".csv",sep=""))){
          write.table(final_signals_df,file = paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Options_Order",as.character(Sys.Date()),sep="/"),".csv",sep=""), sep = ",",row.names = FALSE,append = T,col.names = FALSE)
        }else{
          write.table(final_signals_df,file = paste(paste("~/Downloads/Orders_Data",as.character(user_id),"Options_Order",as.character(Sys.Date()),sep="/"),".csv",sep=""), sep = ",",row.names = FALSE,append = F,col.names = TRUE)
        }
        write.table(final_signals_df, file = "~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Trading_Logs.csv", sep = ",",row.names = FALSE,append = T,col.names = FALSE)
        # write.table(final_signals_df, file = "~/Downloads/Reddy_Stocks_Application/data/Nifty_Indices_Target_Orders.csv", sep = ",",row.names = FALSE,append = T,col.names = FALSE)
        
      }
      
      # terminate_session(login_object,user_id)
      
      
      
    }
  )
  
}


end_time = Sys.time()
print(paste("Ended Executing the Script : ",Sys.time(),sep=""))

print(difftime(end_time,start_time,units = "secs"))


# Sys.sleep(2)
  


