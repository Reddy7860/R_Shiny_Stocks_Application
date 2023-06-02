tradeQuote <- function(datai)
{
  
  predata <- datai[2] - datai[3]
  CR <- c(predata,datai[2])
  
  return(CR)
}


tradeTime <- function(datai,st)
{
  out<-datai[1]
  #attr(out,"tzone")<-"EST"
  min <- as.numeric(format(out, "%M"))
  hour <- as.numeric(format(out, "%H"))
  year <- as.numeric(format(out, "%Y"))
  month <- as.numeric(format(out, "%m"))
  day <- as.numeric(format(out, "%d"))
  if (hour < 9){
    hour <- hour + 12
  }
  currentd <- ISOdate(year,month,day,hour,min,0)
  #attr(currentd,"tzone")<-"EST"
  min <- as.numeric(format(currentd, "%M"))
  hour <- as.numeric(format(currentd, "%H"))
  year <- as.numeric(format(currentd, "%Y"))
  month <- as.numeric(format(currentd, "%m"))
  day <- as.numeric(format(currentd, "%d"))
  orig <- ISOdate(year,month,day,9,30,0)
  Time_in_min <- (hour-9.5) * 60 + min
  ret <- as.numeric(unlist(datai[2]))*matrix(1,nrow=Time_in_min,ncol=1)
  tryCatch({
    rcsv = read.csv(file=sprintf("%s.csv",st), header=FALSE, sep=",")[,1]
    #rcsv = as.matrix(rcsv)
    lec = length(rcsv)
    #dd <- as.POSIXct(rcsv[length(rcsv[,1]),1],origin = "1960-01-01")
    #dd <- as.numeric(format(dd,"%d"))
    if (lec <= Time_in_min){
      print(1)
      for (i in 1:lec) {
        if (i == Time_in_min){
          ret[i,1] <- as.numeric(unlist(datai[2]))
        }else{
          ret[i,1] <- rcsv[i]
        }
      }
    }else{
      print(2)
      print(lec)
      print(Time_in_min)
      ret <- ret
    }
  }, error = function(err) {
    print(3)
    for (i in 1:Time_in_min) {
      ret[i,1] <- as.numeric(unlist(datai[2]))
    }
  })
  write.table(ret, file = sprintf("%s.csv",st), row.names=FALSE,col.names=FALSE,sep=",")
  ret <- cbind(seq(orig,by='min',length=length(ret)),ret)
  return(ret)
}

tradeBuy <- function(P,RSI,CCI,BB,SMA,SMI,SAR,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,cs1,cs2,cs3,cs4,cs5,cs6,cs7,cs8,cs9,cs10,cs11,cs12,cond1,cond2,cond3,cond4,cond5,ss2,GG2)
{
  if (d1 != ""){
    v1 <- as.numeric(d1)}else{v1 <- NULL}
  if (d2 != ""){
    v2 <- as.numeric(d2)}else{v2 <- NULL}
  if (d3  != ""){
    v3 <- as.numeric(d3)}else{v3 <- NULL}
  if (d4  != ""){
    v4 <- as.numeric(d4)}else{v4 <- NULL}
  if (d5  != ""){
    v5 <- as.numeric(d5)}else{v5 <- NULL}
  if (d6  != ""){
    v6 <- as.numeric(d6)}else{v6 <- NULL}
  if (d7  != ""){
    v7 <- as.numeric(d7)}else{v7 <- NULL}
  if (d8  != ""){
    v8 <- as.numeric(d8)}else{v8 <- NULL}
  if (d9  != ""){
    v9 <- as.numeric(d9)}else{v9 <- NULL}
  if (d10  != ""){
    v10 <- as.numeric(d10)}else{v10 <- NULL}
  if (d11  != ""){
    v11 <- as.numeric(d11)}else{v11 <- NULL}
  if (d12  != ""){
    v12 <- as.numeric(d12)}else{v12 <- NULL}
  conditioncheck <- function(cs1,RSI,v1,s,d){
    G1 = matrix(0,nrow=length(RSI),ncol=1)
    lnav <- is.na(RSI)
    print(RSI[lnav[1]])
    for (i in s:length(RSI)){
      if (lnav[i-d] == FALSE & !is.null(v1)){
        if (cs1 == 1 & RSI[i-d] < v1){
          G1[i,1] <- 1
        }else if(cs1 == 2 & RSI[i-d] == v1){
          G1[i,1] <- 1
        }else if(cs1 == 3 & RSI[i-d] > v1){
          G1[i,1] <- 1
        }else if(cs1 == 4 & RSI[i-d] <= v1){
          G1[i,1] <- 1
        }else if(cs1 == 5 & RSI[i-d] >= v1){
          G1[i,1] <- 1
        }else{
          G1[i,1] <- 0
        }}else{
          G1[i,1] <- 0
        }
    }
    return(G1)
  }
  performancondcheck <- function(RSI,cs1,cs4,v1,v4){
    if (!is.null(v1) & !is.null(v4)){
      G1 <- conditioncheck(cs1,RSI,v1,2,1)
      G4 <- conditioncheck(cs4,RSI,v4,1,0)
    }else if(!is.null(v4)){
      G1 <- conditioncheck(cs4,RSI,v4,1,0)
      G4 <- conditioncheck(cs4,RSI,v4,1,0)
    }else if(!is.null(v1)){
      G1 <- conditioncheck(cs1,RSI,v1,2,1)
      G4 <- conditioncheck(cs1,RSI,v1,2,1)
    }else{
      G1 <- conditioncheck(cs1,RSI,v1,2,1)
      G4 <- conditioncheck(cs4,RSI,v4,1,0)
    }
    return(list(G1,G4))
  }
  LG <- performancondcheck(RSI,cs1,cs4,v1,v4)
  LG2 <- performancondcheck(CCI,cs2,cs5,v2,v5)
  LG3 <- performancondcheck(BB,cs3,cs6,v3,v6)
  LG4 <- performancondcheck(SMA,cs7,cs10,v7,v10)
  LG5 <- performancondcheck(SMI,cs8,cs11,v8,v11)
  LG6 <- performancondcheck(SAR,cs9,cs12,v9,v12)
  G1 <- LG[[1]]
  G4 <- LG[[2]]
  G2 <- LG2[[1]]
  G5 <- LG2[[2]]
  G3 <- LG3[[1]]
  G6 <- LG3[[2]]
  G7 <- LG4[[1]]
  G10 <- LG4[[2]]
  G8 <- LG5[[1]]
  G11 <- LG5[[2]]
  G9 <- LG6[[1]]
  G12 <- LG6[[2]]
  con1 <- 3;con2 <- 3;con3 <- 3;con4 <- 3;con5 <- 3;con6 <- 3;
  if (is.null(v1) & is.null(v4)){con1 <- cond1;cond1 <- 2}
  if (is.null(v2) & is.null(v5)){con2 <- cond2;cond2 <- 2}
  if (is.null(v2) & is.null(v5)){cond1 <- 2}
  if (is.null(v3) & is.null(v6)){con3 <- cond3;cond3 <- 2}
  if (is.null(v3) & is.null(v6)){cond2 <- 2}
  if (is.null(v7) & is.null(v10)){con4 <- cond4;cond4 <- 2}
  if (is.null(v7) & is.null(v10)){cond3 <- 2}
  if (is.null(v8) & is.null(v11)){con5 <- cond5;cond5 <- 2}
  if (is.null(v8) & is.null(v11)){cond4 <- 2}
  if (is.null(v9) & is.null(v12)){con6 <- cond5;cond5 <- 2}
  Gtot = matrix(0,nrow=length(G1),ncol=1)
  for (i in 1:length(P)){
    if (cond1 == 1 & cond2 == 1 & cond3 == 1 & cond4 == 1 & cond5 == 1){
      if (G1[i,1] == 1 & G2[i,1] == 1 & G3[i,1] == 1 & G4[i,1] == 1 & G5[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G8[i,1] == 1 & G9[i,1] == 1 & G10[i,1] == 1 & G11[i,1] == 1 & G12[i,1] == 1){
        Gtot[i,1] <- 1
      }
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 1 & cond4 == 1 & cond5 == 1){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1)){
        Gtot[i,1] <- 1
      }
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 1 & cond4 == 1 & cond5 == 1){
      if ((G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1)){
        Gtot[i,1] <- 1
      }
    }else if(cond1 == 1 & cond2 == 1 & cond3 == 2 & cond4 == 1 & cond5 == 1){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1)){
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 1 & cond3 == 1 & cond4 == 2 & cond5 == 1){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1)){
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 1 & cond3 == 1 & cond4 == 1 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1)){
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 1 & cond4 == 1 & cond5 == 1){
      if ((G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1)){
        # v2 maybe null
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 2 & cond4 == 1 & cond5 == 1){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1)){
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 1 & cond4 == 2 & cond5 == 1){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1)){
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 1 & cond4 == 1 & cond5 == 2){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1)){
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 2 & cond4 == 1 & cond5 == 1){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1)){
        # v3 may be null
        Gtot[i,1] <- 1		
      }
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 1 & cond4 == 2 & cond5 == 1){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1)){
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 1 & cond4 == 1 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1)){
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 1 & cond3 == 2 & cond4 == 2 & cond5 == 1){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1)){
        # v7 may be null
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 1 & cond2 == 1 & cond3 == 2 & cond4 == 1 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1)){
        Gtot[i,1] <- 1
      }
    }else if(cond1 == 1 & cond2 == 1 & cond3 == 1 & cond4 == 2 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1)){
        # v8 maybe null
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 2 & cond4 == 1 & cond5 == 1){
      if ((G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1)){
        # v2,v3 maybe null
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 1 & cond4 == 2 & cond5 == 1){
      if ((G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1)){
        # v2 maybe null
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 1 & cond4 == 1 & cond5 == 2){
      if ((G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1)){
        # v2 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 2 & cond4 == 2 & cond5 == 1){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1)){
        # v7 maybe null
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 2 & cond4 == 1 & cond5 == 2){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1)){
        Gtot[i,1] <- 1
      }		
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 1 & cond4 == 2 & cond5 == 2){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1)){
        # v8 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 2 & cond4 == 2 & cond5 == 1){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1)){
        # v3,v7 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 2 & cond4 == 1 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1)){
        # v3 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 1 & cond4 == 2 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1)){
        # v8 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 1 & cond3 == 2 & cond4 == 2 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1)){
        # v7,v8 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 2 & cond4 == 2 & cond5 == 1){
      if ((G8[i,1] == 1 & G11[i,1] == 1 & G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1)){
        # v2,v3,v7 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 2 & cond4 == 1 & cond5 == 2){
      if ((G7[i,1] == 1 & G10[i,1] == 1 & G8[i,1] == 1 & G11[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1)){
        # v2,v3 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 1 & cond4 == 2 & cond5 == 2){
      if ((G3[i,1] == 1 & G6[i,1] == 1 & G7[i,1] == 1 & G10[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1)){
        # v2,v8 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 1 & cond3 == 2 & cond4 == 2 & cond5 == 2){
      if ((G2[i,1] == 1 & G5[i,1] == 1 & G3[i,1] == 1 & G6[i,1] == 1) | (G9[i,1] == 1 & G12[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1)){
        # v7,v8 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 1 & cond2 == 2 & cond3 == 2 & cond4 == 2 & cond5 == 2){
      if ((G1[i,1] == 1 & G4[i,1] == 1 & G2[i,1] == 1 & G5[i,1] == 1) & (G9[i,1] == 1 & G12[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1)){
        # v3,v4,v5 maybe null
        Gtot[i,1] <- 1	
      }	
    }else if(cond1 == 2 & cond2 == 2 & cond3 == 2 & cond4 == 2 & cond5 == 2){
      if ((G9[i,1] == 1 & G12[i,1] == 1) | (G2[i,1] == 1 & G5[i,1] == 1) | (G3[i,1] == 1 & G6[i,1] == 1) | (G7[i,1] == 1 & G10[i,1] == 1) | (G8[i,1] == 1 & G11[i,1] == 1) | (G1[i,1] == 1 & G4[i,1] == 1)){
        # v2,v3,v4,v5 maybe null
        Gtot[i,1] <- 1		
      }
    }else{
      Gtot[i,1] <- 0
    }
  }
  StrG =  matrix(0,nrow=length(Gtot),ncol=1)
  if (ss2 == 1){
    for (i in 1:length(P)){
      if (sum(GG2[1:i]) < sum(Gtot[1:i]) | GG2[i] == 1){
        Gtot[i,1] <- 0
      }
    }
    for (i in 1:length(Gtot)){
      if (Gtot[i] == 1){
        StrG[i] = "S"
      }else{
        StrG[i] = "N"
      }
    }
  }else{
    for (i in 1:length(Gtot)){
      if (Gtot[i] == 1){
        StrG[i] = "B"
      }else{
        StrG[i] = "N"
      }
    }
  }
  ret <- data.frame(StrG,Gtot,format(time(P),'%Y-%m-%d'),P)
  return(ret)
}

autotradeindicator <- function(Gtot,GG2,ss2,P)
{
  if (Gtot[length(Gtot)] == 1){
    atrade <- 1
    if (ss2 == 1 & GG2[length(GG2)] != 0){
      mes <- "Sell now"
    }else if (GG2[length(GG2)] == 1){
      mes <- "Buy now"
    }else{
      mes <- "don't trade"
      atrade <- 0
    }
  }else{
    mes <- "don't trade"
    atrade <- 0
  }
  price <- P[length(P)]
  ret <- list(atrade,mes,price)
  return(ret)
}


# SIMPLE TRADE FUNCTION

simpletrade <- function(Po,Pc,rtd,v,sym,ty){
  Pc[is.na(Pc)] <- 0
  Po[is.na(Po)] <- 0
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
  atrade <- 0
  pob <- Po[ind1]
  pcb <- Pc[ind1]
  pos <- Po[ind2]
  pcs <- Pc[ind2]
  buyc <- sum(pcb)
  sellc <- sum(pcs)
  earnc <- sellc - buyc
  StrG =  matrix(0,nrow=length(G),ncol=1)
  floG =  matrix(0,nrow=length(G),ncol=1)
  for (i in 1:length(G)){
    if (G[i] == 1){
      StrG[i] = "B"
      floG[i] = 1
    }else if (G[i] == 2){
      StrG[i] = "S"
      floG[i] = 1
    }else if (G[i] == 0){
      StrG[i] = "N"
      floG[i] = 0
    }
  }
  ret1 <- data.frame(format(time(pcb),'%Y-%m-%d'),pob,pcb)
  colnames(ret1) <- c("buy-date","open","close")
  ret2 <- data.frame(format(time(pcs),'%Y-%m-%d'),pos,pcs)
  colnames(ret2) <- c("sell-date","open","close")
  ret3 <- data.frame(format(time(Pc),'%Y-%m-%d'),StrG,Po,Pc,floG)
  colnames(ret3) <- c("date","all-trades","open","close","sign")
  
  ddt <- format(time(Pc),'%Y-%m-%d')
  sd <- list(ddt[1])
  ed <- list(ddt[length(Pc)])
  re = NULL;re2 <- NULL;ret11 <- NULL
  if (ty==1){
    fprintf("\nReport of Back Testing the trading strategy:\n",file="report.txt")
    fprintf("\nFrom %s to %s\n",sd,ed,file="report.txt")
    fprintf("\n------------------------------------------------------------------\n",file="report.txt",append = TRUE)
    fprintf("\nbuying trade # = %d - selling trade # = %d\n",length(pcb),length(pcs),file="report.txt",append = TRUE)
    if (length(pcb) > length(pcs)){
      diffb1 <- length(pcb) - length(pcs)
      inii1 <- length(pcb) - diffb1 + 1
      resg1 <- pcb[inii1:length(pcb)]
      re <- earnc + sum(resg1)
    }
    fprintf("\ntotal buy = %6.2f , total sell = %6.2f\n",buyc,sellc,file="report.txt",append = TRUE)
    fprintf("\nwhat is realized earning?  %6.2f\n",(length(pcb)-length(pcs))*Pc[length(Pc)]+sum(pcs)-sum(pcb),file="report.txt",append = TRUE)
    fprintf("\n*************************************************************************************\n",file="report.txt",append = TRUE)
    fprintf("\nAll the trades one after another: buy = B - sell = S - no trade = N\n",file="report.txt",append = TRUE)
    fprintf("\n ____________________________________________________________________________________",file="report.txt",append = TRUE)
    fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20s |","Date","Stock Name","Buy/Sell","How many shares","Transaction Price",file="report.txt",append = TRUE)
    fprintf("\n ====================================================================================",file="report.txt",append = TRUE)
    fprintf("\n| %10s |   %10s |  %10s |  %15d |  %20.2f |",ret3[,1],sym,ret3[,2],abs(ret3[,5]),ret3[,4]*ret3[,5],file="report.txt",append = TRUE)
    fprintf("\n*************************************************************************************\n",file="report.txt",append = TRUE)
    fprintf("\n%10s   %10s","buy-date","Buy Price",file="report.txt",append = TRUE)
    fprintf("\n%10s   %10.2f",ret1[,1],ret1[,3],file="report.txt",append = TRUE)
    fprintf("\n*******************************************************************\n",file="report.txt",append = TRUE)
    fprintf("\n%10s   %10s","sell-date","Sell Price",file="report.txt",append = TRUE)
    fprintf("\n%10s   %10.2f",ret2[,1],ret2[,3],file="report.txt",append = TRUE)
    fprintf("\n*******************************************************************\n",file="report.txt",append = TRUE)
  }
  ret <- list(ret1,ret2,ret3,length(pcb),length(pcs),buyc,sellc,earnc)
  return(ret)
}

weeklyprices <- function(P){
  ind <- gregexpr("Monday+", format(time(P()),'%A, %B %d, %Y %H:%M:%S'), perl=TRUE)
  ret = matrix(0,nrow=length(which(ind %in% c(1))),ncol=1)
  e = matrix(0,nrow=length(which(ind %in% c(1))),ncol=1)
  indh <- which(ind %in% c(1))
  print(indh)
  for (i in c(1:length(indh))) {
    r <- indh[i]
    if (r+6 <= length(ind)){
      ret[i,1] <- mean(P()[r:r+6,4])
      
    }else{
      ret[i,1] <- mean(P()[r:length(P()[,4]),4])}
    e[i,1] <- time(P()[r,4])}
  retm <- xts(ret, order.by=as.POSIXct(as.Date(e)))
  return(retm)
}

analysis_strategy <- function(indicator,P,sign){
  #print(min(indicator[!is.na(indicator)]))
  #print(length(P))
  buy0 <- 0;sell0 <- 0;
  minind <- min(indicator[!is.na(indicator)]);maxind <- max(indicator[!is.na(indicator)]);m = 0;
  s = matrix(0,nrow=floor(maxind)-floor(minind)+1,ncol=length(P))
  b = matrix(0,nrow=floor(maxind)-floor(minind)+1,ncol=length(P))
  buy = matrix(0,nrow=floor(maxind)-floor(minind)+1,ncol=length(P))
  sell = matrix(0,nrow=floor(maxind)-floor(minind)+1,ncol=length(P))
  thresholdb = matrix(0,nrow=floor(maxind)-floor(minind)+1,ncol=1)
  thresholds = matrix(0,nrow=floor(maxind)-floor(minind)+1,ncol=1)
  print(length(sell))
  if (maxind - minind >= 100){
    step = 10;
  }else{
    step = 1;
  }
  print(maxind-minind)
  selected_s <- NULL;selected_b <- NULL;
  for (i in seq(floor(minind), floor(maxind), by=step)){
    thresh <- i
    m = m + 1
    for (j in c(1:length(P))){
      if (!is.na(indicator[j])){
        if (sign == 1){
          if (indicator[j] < thresh){
            buy[m,j] = P[j]
            b[m,j] = 1;
            thresholdb[m,1] = i
          }
          if (indicator[j] > thresh){
            sell[m,j] = P[j]
            s[m,j] = 1;
            thresholds[m,1] = i
          }
        }else{
          if (indicator[j] > thresh){
            buy[m,j] = P[j]
            b[m,j] = 1;
            thresholdb[m,1] = i
          }
          if (indicator[j] < thresh){
            sell[m,j] = P[j]
            s[m,j] = 1;
            thresholds[m,1] = i
          }
        }
      }
    }
  }
  ret0 <- -100000;c1 = 0;selected_ths=NULL;selected_thb=NULL;
  RetG <- array(0,dim=c(0,1));thb <- array(0,dim=c(0,1));ths <- array(0,dim=c(0,1));
  for (i in seq(floor(minind), floor(maxind), by=step)){
    c1 = c1 + 1;c2 = 0;
    for (j in seq(floor(minind), floor(maxind), by=step)){
      c2 = c2 + 1;
      SS = 0;BB = 0;
      for (o in c(1:length(P))){
        if (sum(b[c1,1:o]) >= sum(s[c2,1:o])) {
          SS = SS + sell[c2,o];
        }
        BB = BB + buy[c1,o]
      }
      ret <- SS - BB;
      if (ret > ret0){
        ret0 <- ret;
        selected_ths <- thresholds[c2,1];
        selected_thb <- thresholds[c1,1];
      }
      ths <- c(ths,thresholds[c2,1]);
      thb <- c(thb,thresholds[c1,1]);
      RetG <- c(RetG,ret);
    }	
  }
  my.data = data.frame(thb, ths, RetG)
  colnames(my.data) <- c("buy threshold", "sell threshold", "return")
  print(ret0);print(selected_ths);print(selected_thb);
  RetM <- list(ret0,selected_ths,selected_thb,my.data)
  return(RetM)
}

same_time <- function(P1,P,P2){
  # browser()
  dn <- NULL;mavg <- NULL;up <- NULL;e <- NULL;
  for (i in c(1:length(P1))){
    if (!is.null(P1[i])){
      for (j in c(1:length(P))){
        if (abs(as.Date(time(P1[i])) - as.Date(time(P[j])))<0.5){
          dn = c(dn,P2[i,1])
          mavg = c(mavg,P2[i,2])
          up = c(up,P2[i,3])
          e = c(e,time(P2[i,1]))
        }
      }
    }
  }
  print(paste("retind",mavg))
  if(!isempty(dn)){
    retm <- xts(cbind(dn,mavg,up), order.by=as.POSIXct(as.Date(e)))
    # retm <- xts(cbind(dn,mavg,up), order.by=as.Date.POSIXct(e))
  }else{
    retm <- xts(NA, order.by=as.POSIXct(as.Date(time(P[length(P)]))));
  }
  return(retm)
}

same_time1 <- function(P1,P){
  
  ret <- NULL;e <- NULL;
  for (i in c(1:length(P1))){
    if (!is.null(P1[i])){
      if (sum(abs(as.Date(time(P1[i])) - as.Date(time(P)))<0.5)!= 0){
        ret = c(ret,P1[i])
        e = c(e,time(P1[i]))
      }
    }
  }
  # browser()
  
  print(paste("retind",ret))
  if(!isempty(ret)){
    retm <- xts(ret , order.by=as.POSIXct(as.Date(e)))
    # retm <- xts(ret , order.by=as.Date.POSIXct(e))
    
  }else{
    retm <- xts(NA, order.by=as.POSIXct(as.Date(time(P[length(P)]))));
  }
  return(retm)
}

same_size <- function(P1,P){
  ret <- NULL;e <- NULL;
  for (i in c(1:length(P))){
    if (!is.null(P1)){
      ret = c(ret,P1[time(P1) == time(P[i])])
      e = c(e,time(P1[time(P1) == time(P[i])]))
    }
  }
  print(paste("ret",ret))
  if(!isempty(ret)){
    retm <- xts(ret, order.by=as.POSIXct(as.Date(e)))}else{
      retm <- xts(NA, order.by=as.POSIXct(as.Date(time(P[length(P)]))));
    }
  return(retm)
}


last_point <- function(P){
  P1 <- xts(NA, order.by=as.POSIXct(as.Date(time(P[length(P)])+1)))
  retm <- c(P,P1)
  return(retm)
}
last_point1 <- function(P0,P){
  P1 <- xts(NA, order.by=as.POSIXct(as.Date(time(P[length(P)])+1)))
  retm <- c(P0,P1)
  return(retm)
}
write_report <- function(symb,d1,d2,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,P,RSI,CCI,BB,SMA,SMI,SAR){
  
  two_var <- function(a1,a2){
    if (a1[[1]] > a2[[1]]){
      av = a1
      sign = 1
    }else{
      av = a2
      sign = 3
    }
    Ret = list(av,sign)
    return(Ret)
  }
  av1 <- two_var(a1,a7)
  av2 <- two_var(a2,a8)
  av3 <- two_var(a3,a9)
  av4 <- two_var(a4,a10)
  av5 <- two_var(a5,a11)
  av6 <- two_var(a6,a12)
  print(av1)
  fprintf("\nAnalysis Report:\n",file="analysis_report.txt")
  fprintf("\nStock: %s \nFrom: %s to %s\n",symb,d1,d2,file="analysis_report.txt",append = TRUE)
  fprintf("\n*************************************************************************************\n",file="analysis_report.txt",append = TRUE)
  fprintf("\nSummary: \n",file="analysis_report.txt",append = TRUE)
  fprintf("\n ___________________________________________________________________________________________________________",file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20s |","Indicator","Magic # for buying","Magic # for selling","Buy sign","Sell sign","Max Return",file="analysis_report.txt",append = TRUE)
  fprintf("\n ===========================================================================================================",file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20.2f |","RSI",av1[[1]][[3]],av1[[1]][[2]],if(av1[[2]]==1){"<"}else{">"},if(av1[[2]]==1){">"}else{"<"},av1[[1]][[1]],file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20.2f |","CCI",av2[[1]][[3]],av2[[1]][[2]],if(av2[[2]]==1){"<"}else{">"},if(av2[[2]]==1){">"}else{"<"},av2[[1]][[1]],file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20.2f |","BB",av3[[1]][[3]],av3[[1]][[2]],if(av3[[2]]==1){"<"}else{">"},if(av3[[2]]==1){">"}else{"<"},av3[[1]][[1]],file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20.2f |","SMA",av4[[1]][[3]],av4[[1]][[2]],if(av4[[2]]==1){"<"}else{">"},if(av4[[2]]==1){">"}else{"<"},av4[[1]][[1]],file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20.2f |","SMI",av5[[1]][[3]],av5[[1]][[2]],if(av5[[2]]==1){"<"}else{">"},if(av5[[2]]==1){">"}else{"<"},av5[[1]][[1]],file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20.2f |","SAR",av6[[1]][[3]],av6[[1]][[2]],if(av6[[2]]==1){"<"}else{">"},if(av6[[2]]==1){">"}else{"<"},av6[[1]][[1]],file="analysis_report.txt",append = TRUE)
  fprintf("\n ***********************************************************************************************************\n",file="analysis_report.txt",append = TRUE)
  signsell <- function(av){
    if(av[[2]] == 1){
      as <- 3
    }else{
      as <- 1
    }
    return(as)
  } 
  as1 <- signsell(av1)
  as2 <- signsell(av2)
  as3 <- signsell(av3)
  as4 <- signsell(av4)
  as5 <- signsell(av5)
  as6 <- signsell(av6)
  print(as5)
  print(as6)
  BuyG <- tradeBuy(P,RSI,CCI,BB,SMA,SMI,SAR,"","","",av1[[1]][[3]],av2[[1]][[3]],av3[[1]][[3]],"","","",av4[[1]][[3]],av5[[1]][[3]],av6[[1]][[3]],1,1,1,av1[[2]],av2[[2]],av3[[2]],1,1,1,av4[[2]],av5[[2]],av6[[2]],2,2,2,2,2,0,0)
  sellG <- tradeBuy(P,RSI,CCI,BB,SMA,SMI,SAR,"","","",av1[[1]][[2]],av2[[1]][[2]],av3[[1]][[2]],"","","",av4[[1]][[2]],av5[[1]][[2]],av6[[1]][[2]],1,1,1,as1,as2,as3,1,1,1,as4,as5,as6,2,2,2,2,2,1,BuyG[,2])
  print(BuyG)
  print(sellG)
  print(sum(BuyG[,2]) - sum(sellG[,2]))
  print(sum(sellG[sellG[,2] == 1,4])-sum(BuyG[BuyG[,2] == 1,4]))
  print(round(P[length(P)] * (sum(BuyG[,2]) - sum(sellG[,2]))+sum(sellG[sellG[,2] == 1,4])-sum(BuyG[BuyG[,2] == 1,4]),2))
  
  
  fprintf("\n\nCombining the magic numbers of indicators:\n",file="analysis_report.txt",append = TRUE)
  fprintf("\nThis is the trading strategy:\n",file="analysis_report.txt",append = TRUE)
  fprintf("\nBuy:",file="analysis_report.txt",append = TRUE)
  fprintf("\nRSI:T %s %s",if(av1[[2]]==1){"<"}else{">"},av1[[1]][[3]],file="analysis_report.txt",append = TRUE)
  fprintf("\nCCI:T %s %s",if(av2[[2]]==1){"<"}else{">"},av2[[1]][[3]],file="analysis_report.txt",append = TRUE)
  fprintf("\nBB:T %s %s",if(av3[[2]]==1){"<"}else{">"},av3[[1]][[3]],file="analysis_report.txt",append = TRUE)
  fprintf("\nSMA:T %s %s",if(av4[[2]]==1){"<"}else{">"},av4[[1]][[3]],file="analysis_report.txt",append = TRUE)
  fprintf("\nSMI:T %s %s",if(av5[[2]]==1){"<"}else{">"},av5[[1]][[3]],file="analysis_report.txt",append = TRUE)
  fprintf("\nSAR:T %s %s",if(av6[[2]]==1){"<"}else{">"},av6[[1]][[3]],file="analysis_report.txt",append = TRUE)
  fprintf("\nSell:",file="analysis_report.txt",append = TRUE)
  fprintf("\nRSI:T %s %s",if(av1[[2]]==1){">"}else{"<"},av1[[1]][[2]],file="analysis_report.txt",append = TRUE)
  fprintf("\nCCI:T %s %s",if(av2[[2]]==1){">"}else{"<"},av2[[1]][[2]],file="analysis_report.txt",append = TRUE)
  fprintf("\nBB:T %s %s",if(av3[[2]]==1){">"}else{"<"},av3[[1]][[2]],file="analysis_report.txt",append = TRUE)
  fprintf("\nSMA:T %s %s",if(av4[[2]]==1){">"}else{"<"},av4[[1]][[2]],file="analysis_report.txt",append = TRUE)
  fprintf("\nSMI:T %s %s",if(av5[[2]]==1){">"}else{"<"},av5[[1]][[2]],file="analysis_report.txt",append = TRUE)
  fprintf("\nSAR:T %s %s",if(av6[[2]]==1){">"}else{"<"},av6[[1]][[2]],file="analysis_report.txt",append = TRUE)
  fprintf("\n*************************************************************************************",file="analysis_report.txt",append = TRUE)
  fprintf("\nBuy trades:\n",file="analysis_report.txt",append = TRUE)
  fprintf("\n ____________________________________________________________________________________",file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20s |","Date","Stock Name","Buy","How many shares","Transaction Price",file="analysis_report.txt",append = TRUE)
  fprintf("\n ====================================================================================",file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20.2f |",BuyG[BuyG[,2] == 1,3],symb,BuyG[BuyG[,2] == 1,1],"1",BuyG[BuyG[,2] == 1,4],file="analysis_report.txt",append = TRUE)
  fprintf("\n*************************************************************************************\n",file="analysis_report.txt",append = TRUE)
  fprintf("\nSell trades:\n",file="analysis_report.txt",append = TRUE)
  fprintf("\n ____________________________________________________________________________________",file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20s |","Date","Stock Name","Buy","How many shares","Transaction Price",file="analysis_report.txt",append = TRUE)
  fprintf("\n ====================================================================================",file="analysis_report.txt",append = TRUE)
  fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20.2f |",sellG[sellG[,2] == 1,3],symb,sellG[sellG[,2] == 1,1],"1",sellG[sellG[,2] == 1,4],file="analysis_report.txt",append = TRUE)
  fprintf("\n*************************************************************************************\n",file="analysis_report.txt",append = TRUE)
  fprintf("\ntotal Buy = %6.2f",sum(BuyG[BuyG[,2] == 1,4]),file="analysis_report.txt",append = TRUE)
  fprintf("\ntotal Sell = %6.2f",sum(sellG[sellG[,2] == 1,4]),file="analysis_report.txt",append = TRUE)
  fprintf("\nrealized Earning = %6.2f",round(P[length(P)] * (sum(BuyG[,2]) - sum(sellG[,2]))+sum(sellG[sellG[,2] == 1,4])-sum(BuyG[BuyG[,2] == 1,4]),2),file="analysis_report.txt",append = TRUE)
  
}




individual_report <- function(symb,d1,d2,P,op,sign,ind){
  
  if(ind == 1){indic="RSI"}else if(ind == 2){indic="CCI"}else if(ind == 3){indic="BB"}
  else if(ind == 4){indic="SMA"}else if(ind == 5){indic="SMI"}else if(ind == 6){indic="SAR"}
  
  fprintf("\nAnalysis Report:\n",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic))
  fprintf("\nStock: %s \nFrom: %s to %s\n",symb,d1,d2,file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\nindicator: %s \n",indic,file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n*************************************************************************************\n",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\nBest found results:\n",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n ___________________________________________________________________________________________________________",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20s |","Indicator","Magic # for buying","Magic # for selling","Buy sign","Sell sign","Max Return",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n ===========================================================================================================",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n| %10s | %20s | %20s | %10s | %10s | %20.2f |",indic,op[[3]],op[[2]],if(sign==1){"<"}else{">"},if(sign==1){">"}else{"<"},op[[1]][[1]],file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n ***********************************************************************************************************\n",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  
  fprintf("\nDetails for all trial:\n",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n _________________________________________________________________________________",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n| %10s | %20s | %20s| %20s |","Indicator","Threshold for buying","Threshold for selling","Return",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n =================================================================================",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n| %10s | %20d | %20d | %20.2f |",indic,op[[4]][,1],op[[4]][,2],op[[4]][,3],file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  fprintf("\n *********************************************************************************\n",file=sprintf("stock_%s_indicator_%s_analysis_report.txt",symb,indic),append = TRUE)
  
  
}