library(quantmod) 
library(dplyr)
nse_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv"))

# increment = 1
# 
# final_levels_df = data.frame("Stock" = character(0),"Rider_Bullish" = character(0),"Bullish_Level"=character(0),"Rider_Bearish"=character(0),"Bearish_Level"=character(0))

logistic_prediction <- data.frame("Stock"=character(0),"Direction"=character(0))

for(i in 1:nrow(nse_data)){
  stock = nse_data[i,2]
  today = Sys.Date()
  f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
  previousWorkingDay <- f(today)
  
  print(stock)
  
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
  
  print(test_data_frame)
  
  pred <- predict(log_model,newdata = test_data_frame,type = "response")
  
  
  pred_direction <- ifelse(pred > 0.5,"Buy","Sell")
  
  print(pred_direction)
  
  logistic_prediction[i,"Stock"] <- stock
  logistic_prediction[i,"Direction"] <- as.character(pred_direction)
  
  
}

# View(logistic_prediction)

colnames(logistic_prediction)

for (i in 1:nrow(logistic_prediction)) {
  stock = logistic_prediction[i,"Stock"]
  
  print(stock)
 
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

  logistic_prediction[i,"forecasted"] <- round(forecasted,2)
  logistic_prediction[i,"ci_low_80"] <- round(ci_low_80,2)
  logistic_prediction[i,"ci_high_80"] <- round(ci_high_80,2)
  logistic_prediction[i,"ci_low_95"] <- round(ci_low_95,2)
  logistic_prediction[i,"ci_high_95"] <- round(ci_high_95,2)
  
  # cat("Forecasted values : Actual forecast - ",forecasted,", Confidence 80 - ",ci_low_80,",",ci_high_80," and "," Confidence 95 - ",ci_low_95,",",ci_high_95)

}

View(logistic_prediction)
