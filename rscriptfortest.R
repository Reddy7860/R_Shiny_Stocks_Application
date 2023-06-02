calc <- function(datai,v){
  # This function is the typical function that is used for testing the upload script window in the Trading Strategy.
  # in this function the name of the function (calc) should not be changed.
  # user could write his/her own R script for thier trading strategy and use it in the upload script window and the application will show the
  # back testing results for that R script.
  #
  # input:
  # ...... datai: The historical data of a specific stock. that contains Open, close, volume and so on.. datai is the historical stock prices.
  # ......     v: v is 1 or 2. when v = 1 in conditional statement in the end of the script, ret should be for Buy prices.
  # ......        when v = 2. the conditional statement in the end of the script, ret should be for sell prices.
  #
  #
  # Output:  ret: return of this function should be a data frame that contain two columns. if v = 1 then ret = ret1. if v = 2 then ret = ret2.
  #               and ret1 includes the buy dates and buy prices. ret2 includes the sell dates and sell prices. like below:
  #                  ret1 <- data.frame(format(time(pob),'%Y-%m-%d'),pcb)
  #                  colnames(ret1) <- c("buy-date","buy price")
  #                  ret2 <- data.frame(format(time(pos),'%Y-%m-%d'),pcs)
  #                  colnames(ret2) <- c("sell-date","sell price")
  #
  #    
  # note: other parts of the code, all could be changed according to the user trading strategy.
  # note: ret3 at the end of the script is for total trades. including all the interval date. even if it has trade or not, ret3 did not used in the 
  #       report of the Rscript upload window. so it could be neglected.
  Po <- datai[,1]
  Pc <- datai[,4] 
  G = matrix(0,nrow=length(Pc),ncol=1)
  stradeopen = 0
  for (i in 1:length(Pc)){
    if (Pc[i] < Po[i]){
      G[i,1] = 1
      stradeopen = stradeopen + 1
    }else{
      G[i,1] = 0
    }
    if ((Pc[i] > Po[i]) & (stradeopen >= 1)){
      G[i,1] = 2
      stradeopen = stradeopen - 1
    }
  }
  ind1 <- G == 1
  ind2 <- G == 2
  diff1 <- c(0,diff(ind1))
  diff2 <- c(0,diff(ind2))
  index1 <- ind1
  index2 <- ind2
  for (i in c(1:length(ind1))){
    if (i == 1){
      index1[i] = 0
    }else{
      if (diff1[i] == -1){
        index1[i] = 1
      }else if(diff1[i] == 0 & ind1[i - 1] == 1){
        index1[i] = 1
      }else{
        index1[i] = 0
      }}
  }
  for (i in c(1:length(ind2))){
    if (i == 1){
      index2[i] = 0
    }else{
      if (diff2[i] == -1){
        index2[i] = 1
      }else if(diff2[i] == 0 & ind2[i - 1] == 1){
        index2[i] = 1
      }else{
        index2[i] = 0
      }}
  }
  atrade <- 0
  pob <- Po[ind1]
  pcb <- Pc[ind1]
  pcbt <- Pc[index1]
  pos <- Po[ind2]
  pcs <- Pc[ind2]
  pcst <- Pc[index2]
  buyc <- sum(pcb)
  sellc <- sum(pcs)
  buyct <- sum(pcbt)
  sellct <- sum(pcst)
  earnc <- sellc - buyc
  earnt <- sellct - buyct
  StrG =  matrix(0,nrow=length(G),ncol=1)
  for (i in 1:length(G)){
    if (G[i] == 1){
      StrG[i] = "B"
    }else if (G[i] == 2){
      StrG[i] = "S"
    }else if (G[i] == 0){
      StrG[i] = "N"
    }
  }
  ret1 <- data.frame(format(time(pob),'%Y-%m-%d'),pcb)
  colnames(ret1) <- c("buy-date","buy price")
  ret2 <- data.frame(format(time(pos),'%Y-%m-%d'),pcs)
  colnames(ret2) <- c("sell-date","sell price")
  ret3 <- data.frame(format(time(Pc),'%Y-%m-%d'),StrG,Pc)
  colnames(ret3) <- c("date","all-trades","price")
  if (v == 1){
    ret <- ret1
  }else if(v == 2){
    ret <- ret2
  }else if(v == 3){
    ret <- ret3
  }
  return(ret)
}