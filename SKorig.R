spearmanKarberORIG <- function(inputFile=NULL,trim=0,doPlot=FALSE){
  if(!require(isotone))stop("Install the isotone package")
  outputFile <- paste(c("F",sample(c(letters,LETTERS,0:9),size=11,replace=TRUE),".txt"),collapse='')
  #on.exit(unlink(outputFile))
  
  if(is.null(inputFile)){
    inputFile <- tcltk::tk_choose.files(caption="Select input data file (MUST be plain text format)")
  }
  if(!is.null(inputFile)){
    if(is.character(inputFile))    data2fit <- read.table(inputFile,header=TRUE,fill=TRUE,colClasses="numeric")
    if(!is.character(inputFile))    data2fit <- inputFile
  }
  #input trim is a percent; convert to fraction
  trim <- trim/100.0
  grayCol <- do.call("rgb",as.list(rep(0.5,3)))
  data2fit <- data2fit[order(data2fit$doses),]
  origData <- data2fit
  #plot input data as given, prior to any modification
  if(doPlot)with(origData,{rates <- responses/sizes;plot(y=rates,x=doses,log='x',ylim=c(0,1),type='b')})
  #remove strings of leading zero reponses out to the highest dose in the series
  startRow <- 1
  if(data2fit$responses[1]==0){
    zeroLocs <- which(data2fit$responses==0)
    leadingZeros <- which(zeroLocs==seq(along=zeroLocs))
    startRow <- ifelse(length(leadingZeros)>1,leadingZeros[length(leadingZeros)],1)
  }
  #remove strings of trailing 100% responses in to the lowest such dose in the series
  endRow <- nrow(data2fit)
  if(data2fit$responses[endRow]==data2fit$sizes[endRow]){
    oneLocs <- which(rev(data2fit$responses)==data2fit$sizes)
    trailing100s <- which(oneLocs==seq(along=oneLocs))
    trailing100s <- rev(1:nrow(data2fit))[trailing100s]
    endRow <- ifelse(length(trailing100s)>1,trailing100s[length(trailing100s)],nrow(data2fit))
  }
  #   subset the data
  #   data2fit <- data2fit[startRow:endRow,]
  #print(data2fit)
  #attach so can use just variable names
  attach(data2fit)
  on.exit(detach(2))
  logDoses <- log(doses)
  
  #enforce monotonicity into adjRates variable
  adjRates <- rates <- responses/sizes
  #montonize them using isotone::gpava
  #weights should be n, based on experiments I did with test cases.
  #It amounts to pooling data between two doses to calculate new p.  sqrt(n) will not do that
  adjRates  <- isotone::gpava(y = adjRates,z = logDoses,weights = sizes)$x
  #print(data.frame(data2fit,origP=rates,adjP=adjRates))
  #print(str(gpavaData))
  #stop()
  nDoses <- length(rates)
  if(doPlot)lines(y=adjRates,x=doses,col='blue',lwd=3)

  doseMidpts <- diff(logDoses)/2 + logDoses[-nDoses]
  rateDiffs <- diff(adjRates)
  #this is the usual SK estimator, but it is only valid when the response curve
  #starts with a zero and ends with a 100% (otherwise, must trim)
  SK.vanilla <- sum(diff(rates)*doseMidpts)
  SK <- sum(rateDiffs*doseMidpts)
  #print(c(SK.vanilla=exp(SK.vanilla),SK.montone=exp(SK)))
  minTrim <- max(adjRates[1],1-adjRates[length(adjRates)])
  origTrim <- trim
  #print(c(askTrim=origTrim,minTrim=minTrim))
  if (trim < minTrim){
    trim <- minTrim
    cat("\nRequested trim value is not valid.  Trim value adjusted.\n")
    cat("\nRequested trim=",origTrim,"  Adusted trim=",trim,"\n")
  }
  #implement trim (adjust rates where trim>0)
  if(trim>=.5){
    return(c("SK"=NA,"lower"=NA,"upper"=NA,"trim"=trim))
  }
  trimAdj <- adjRates
  trimDoses <- logDoses
  SKtrimData <<- data.frame(scaleDoses=trimDoses,scaleP=trimAdj)
  
  if (trim>0){
    #linear interpolation
    #print("Trimming Calcs")
    #print(c(trim=trim))
    adjRatesLine <- approxfun(x=logDoses,y=adjRates)
    lowerTrim <- (uniroot(f=function(x){adjRatesLine(x)-trim},interval=range(logDoses))$root)
    upperTrim <- (uniroot(f=function(x){adjRatesLine(x)-(1-trim)},interval=range(logDoses))$root)
    #print(c(lowerTrim=lowerTrim,upperTrim=upperTrim))
    if(doPlot){
      abline(h=c(trim,1-trim),lty=2,col=grayCol)
      lines(x=exp(rep(lowerTrim,2)),y=c(0,trim),lty=2,col=grayCol)
      lines(x=exp(rep(upperTrim,2)),y=c(1,1-trim),lty=2,col=grayCol)
    }
    trimAdj <- (adjRates-trim)/(1-2*trim)
    trimKeep <- which(trimAdj>=0 & trimAdj<=1)
    #print(c(trimKeep=trimKeep))
    trimAdj[trimAdj<0] <- 0
    trimAdj[trimAdj>1] <- 1
    #print(c(trimAdj=trimAdj))
    trimDoses <- logDoses
    #print(c(trimDoses=trimDoses))
    if(trimAdj[trimKeep[1]]>.001 & trimKeep[1]>1){
      trimDoses[trimKeep[1]-1] <- lowerTrim
    }
    if(trimAdj[tail(trimKeep,1)]<.999 & tail(trimKeep,1)<length(trimDoses)){
      trimDoses[tail(trimKeep,1)+1] <- upperTrim
    }
    #print(c(trimDoses=trimDoses))
    #trimAdj <- c(0,trimAdj[trimKeep],1)
    #N.trim <- length(trimAdj)
    #trimDoses <- c(lowerTrim,logDoses[trimKeep],upperTrim)
    trimDiffs <- diff(trimAdj)
    trimDoseMidpts <- diff(trimDoses)/2 + trimDoses[-length(trimDoses)]
    SKtrimData <<- data.frame(scaleDoses=trimDoses,scaleP=trimAdj)
    SK.trim <- sum(trimDiffs*trimDoseMidpts)
    #print(exp(c(SK=SK,SK.trim=SK.trim)))
    if(doPlot)lines(y=trimAdj,x=exp(trimDoses),col='red',type="b")
    if(FALSE){
      adjRates <- trimAdj*(1-2*trim) + trim
      adjRates[adjRates<trim] <- trim
      #adjRates[1] <- 0
      logDoses <- trimDoses
    }
    #adjRates[adjRates<(trim-.001)] <- 0
    #adjRates[adjRates>(1-(trim-.001))] <- 1
  }
  if(doPlot)lines(y=adjRates,x=exp(logDoses),col='magenta',type="b",pch=16)
  SKdata <<- data.frame(data2fit,origP=rates,adjP=adjRates,scaleP=trimAdj)
  #print(SKdata)
  
  #use the notation of Hamilton et al. to calculate s.e. for CI:
  #the Hamilton paper clearly uses the monotonized values
  A <- trim
  #A <- 0
  L <- max(which(adjRates<=(A+.001)))
  U <- min(which(adjRates>=(1-(A+.001))))
  #print(c(A=A,L=L,U=U))
  if(trim==0)SK.trim <- SK
  if((U-L)<=0){
    return(c("SK"=SK.trim,"lower"=NA,"upper"=NA,"trim"=trim))
  }
  x <- logDoses
  p <- adjRates
  n <- sizes
  #print(data.frame(x=x,p=p,n=n))
  #formulae:
  V1 <- ((((x[L+1]-x[L])*(p[L+1]-A)^2)/((p[L+1]-p[L])^2))^2)*p[L]*(1-p[L])/n[L]
  V2 <- (((x[L]-x[L+2]) + (x[L+1]-x[L])*((A-p[L])^2) / (p[L+1]-p[L])^2)^2)*(p[L+1])*(1-p[L+1])/n[L+1]
  V3 <- 0
  if ((U-2)>=(L+2)){
    sumRange <- (L+2):(U-2)
    V3 <- sum((x[sumRange-1]-x[sumRange+1])^2 * p[sumRange]*(1-p[sumRange])/n[sumRange])
  }
  V4 <- ((x[U-2] - x[U]) + (x[U] - x[U-1]) * ((p[U]-1+A)^2) / ((p[U]-p[U-1])^2))^2 * p[U-1]*(1-p[U-1])/n[U-1]
  #TSK package V4 follows -- only diff is parens I'm more careful about re exponents
  V4.tsk <-  ((x[U-2] - x[U]) + (x[U] - x[U-1]) *  (p[U]-1+A)^2  /  (p[U]-p[U-1])^2 )^2 * p[U-1]*(1-p[U-1])/n[U-1]  
  V5 <- (((x[U]-x[U-1])*((1-A-p[U-1])^2) / (p[U]-p[U-1])^2)^2) * p[U]*(1-p[U])/n[U]
  #TSK package V5 follows -- only diff is parens I'm more careful about re exponents
  V5.tsk <- ((x[U]-x[U-1])* (1-A-p[U-1])^2  / (p[U]-p[U-1])^2)^2  * p[U]*(1-p[U])/n[U]
  V6 <- ((((x[U]-x[L+1])*((1-A-p[U])^2)/((p[U]-p[L+1])^2)) - ((x[L+1]-x[L])*((A-p[L])^2)/(p[L+1]-p[L])^2) +
            (x[L]-x[U]))^2) * p[L+1]*(1-p[L+1])/n[L+1]
  #print(c(V1=V1,V2=V2,V3=V3,V4=V4,V5=V5,V6=V6,V4.tsk=V4.tsk,V5.tsk=V5.tsk))
  if ((U-L)==1){
    part1 <-  (((0.5-p[U])^2) / ((p[U]-p[L])^4)) * p[L]*(1-p[L])/n[L] + 
      (((0.5-p[L])^2) / ((p[U]-p[L])^4)) * p[U]*(1-p[U])/n[U]
    var.mu <- ((x[U]-x[L])^2) * part1
  }
  if((U-L)==2) var.mu <- (V1+V5+V6)/((2-4*A)^2)
  if((U-L)==3) var.mu <- (V1+V2+V4+V5)/((2-4*A)^2)
  if((U-L)>=4) var.mu <- (V1+V2+V3+V4+V5)/((2-4*A)^2)
  #print(c(se=sqrt(var.mu)))
  
  results <- c(SK=SK.trim,lower=SK.trim-qnorm(0.975)*sqrt(var.mu),upper=SK.trim+qnorm(0.975)*sqrt(var.mu),trim=trim)
  if(doPlot){
    lines(y=rep(par("usr")[3],2),x=exp(results[2:3]),col='blue',lwd=3)
    lines(y=c(1,1)*0.5,          x=exp(results[2:3]),col='blue',lwd=2)
    points(x=exp(SK.trim),y=0.5,col='blue',pch=16)
    rug(exp(SK.trim),col='blue')
    text(x=10^par("usr")[1],y=1,label=paste("S-K estimate:",format(exp(SK.trim))),adj=0)
    text(x=10^par("usr")[1],y=0.95,label=paste("95% CI: (",format(exp(results["lower"])),",",format(exp(results["upper"])),")"),adj=0)
    text(x=10^par("usr")[1],y=0.90,label=paste("L =",L,"  U =",U),adj=0)
  }
  results <- c(exp(results[c("SK","lower","upper")]),results["trim"])
  #sink(file=NULL)
  #file.show(outputFile)
  #unlink(outputFile)
  results
}

#very similar to above, just used to add a line to the baseplot
#that represents the monotonized SK fit
SKbaseplot <- function(inputFile=NULL,trim=0,doPlot=FALSE,verbose=FALSE){
  if(!require(isotone))stop("Install the isotone package")
  outputFile <- paste(c("F",sample(c(letters,LETTERS,0:9),size=11,replace=TRUE),".txt"),collapse='')
  #on.exit(unlink(outputFile))
  
  if(is.null(inputFile)){
    inputFile <- tcltk::tk_choose.files(caption="Select input data file (MUST be plain text format)")
  }
  if(!is.null(inputFile)){
    if(is.character(inputFile))    data2fit <- read.table(inputFile,header=TRUE,fill=TRUE,colClasses="numeric")
    if(!is.character(inputFile))    data2fit <- inputFile
  }
  #input trim is a percent; convert to fraction
  trim <- trim/100.0
  grayCol <- do.call("rgb",as.list(rep(0.5,3)))
  data2fit <- data2fit[order(data2fit$doses),]
  origData <- data2fit
  #plot input data as given, prior to any modification
  if(doPlot)with(origData,{rates <- responses/sizes;plot(y=rates,x=doses,log='x',ylim=c(0,1),type='b')})
  #remove strings of leading zero reponses out to the highest dose in the series
  startRow <- 1
  if(data2fit$responses[1]==0){
    zeroLocs <- which(data2fit$responses==0)
    leadingZeros <- which(zeroLocs==seq(along=zeroLocs))
    startRow <- ifelse(length(leadingZeros)>1,leadingZeros[length(leadingZeros)],1)
  }
  #remove strings of trailing 100% responses in to the lowest such dose in the series
  endRow <- nrow(data2fit)
  if(data2fit$responses[endRow]==data2fit$sizes[endRow]){
    oneLocs <- which(rev(data2fit$responses)==data2fit$sizes)
    trailing100s <- which(oneLocs==seq(along=oneLocs))
    trailing100s <- rev(1:nrow(data2fit))[trailing100s]
    endRow <- ifelse(length(trailing100s)>1,trailing100s[length(trailing100s)],nrow(data2fit))
  }
  #   subset the data
  #   data2fit <- data2fit[startRow:endRow,]
  if(verbose)print(data2fit)
  #attach so can use just variable names
  attach(data2fit)
  on.exit(detach(2))
  logDoses <- log(doses)
  
  #enforce monotonicity into adjRates variable
  adjRates <- rates <- responses/sizes
  #montonize them using isotone::gpava
  #weights should be n, based on experiments I did with test cases.
  #It amounts to pooling data between two doses to calculate new p.  sqrt(n) will not do that
  adjRates  <- isotone::gpava(y = adjRates,z = logDoses,weights = sizes)$x
  if(verbose)print(data.frame(data2fit,origP=rates,adjP=adjRates))
  #print(str(gpavaData))
  #stop()
  nDoses <- length(rates)
  if(doPlot)lines(y=adjRates,x=doses,col='blue',lwd=3)
  
  doseMidpts <- diff(logDoses)/2 + logDoses[-nDoses]
  rateDiffs <- diff(adjRates)
  #this is the usual SK estimator, but it is only valid when the response curve
  #starts with a zero and ends with a 100% (otherwise, must trim)
  SK.vanilla <- sum(diff(rates)*doseMidpts)
  SK <- sum(rateDiffs*doseMidpts)
  #print(c(SK.vanilla=exp(SK.vanilla),SK.montone=exp(SK)))
  minTrim <- max(adjRates[1],1-adjRates[length(adjRates)])
  origTrim <- trim
  #print(c(askTrim=origTrim,minTrim=minTrim))
  if (trim < minTrim){
    trim <- minTrim
    cat("\nRequested trim value is not valid.  Trim value adjusted.\n")
    cat("\nRequested trim=",origTrim,"  Adusted trim=",trim,"\n")
  }
  #implement trim (adjust rates where trim>0)
  if(trim>=.5){
    return(NULL)
  }
  trimAdj <- adjRates
  trimDoses <- logDoses
  SKtrimData <- data.frame(scaleDoses=trimDoses,scaleP=trimAdj)
  
  if (trim>0){
    #linear interpolation
    #print("Trimming Calcs")
    #print(c(trim=trim))
    adjRatesLine <- approxfun(x=logDoses,y=adjRates)
    lowerTrim <- (uniroot(f=function(x){adjRatesLine(x)-trim},interval=range(logDoses))$root)
    upperTrim <- (uniroot(f=function(x){adjRatesLine(x)-(1-trim)},interval=range(logDoses))$root)
    if(verbose)print(c(lowerTrim=lowerTrim,upperTrim=upperTrim))
    if(doPlot){
      abline(h=c(trim,1-trim),lty=2,col=grayCol)
      lines(x=exp(rep(lowerTrim,2)),y=c(0,trim),lty=2,col=grayCol)
      lines(x=exp(rep(upperTrim,2)),y=c(1,1-trim),lty=2,col=grayCol)
    }
    trimAdj <- (adjRates-trim)/(1-2*trim)
    trimKeep <- which(trimAdj>=0 & trimAdj<=1)
    #print(c(trimKeep=trimKeep))
    trimAdj[trimAdj<0] <- 0
    trimAdj[trimAdj>1] <- 1
    #print(c(trimAdj=trimAdj))
    trimDoses <- logDoses
    #print(c(trimDoses=trimDoses))
    if(trimAdj[trimKeep[1]]>.001 & trimKeep[1]>1){
      trimDoses[trimKeep[1]-1] <- lowerTrim
    }
    if(trimAdj[tail(trimKeep,1)]<.999 & tail(trimKeep,1)<length(trimDoses)){
      trimDoses[tail(trimKeep,1)+1] <- upperTrim
    }
    #print(c(trimDoses=trimDoses))
    #trimAdj <- c(0,trimAdj[trimKeep],1)
    #N.trim <- length(trimAdj)
    #trimDoses <- c(lowerTrim,logDoses[trimKeep],upperTrim)
    trimDiffs <- diff(trimAdj)
    trimDoseMidpts <- diff(trimDoses)/2 + trimDoses[-length(trimDoses)]
    SKtrimData <<- data.frame(scaleDoses=trimDoses,scaleP=trimAdj)
    SK.trim <- sum(trimDiffs*trimDoseMidpts)
    #print(exp(c(SK=SK,SK.trim=SK.trim)))
    if(doPlot)lines(y=trimAdj,x=exp(trimDoses),col='red',type="b")
    if(FALSE){
      adjRates <- trimAdj*(1-2*trim) + trim
      adjRates[adjRates<trim] <- trim
      #adjRates[1] <- 0
      logDoses <- trimDoses
    }
    #adjRates[adjRates<(trim-.001)] <- 0
    #adjRates[adjRates>(1-(trim-.001))] <- 1
  }
  if(doPlot)lines(y=adjRates,x=exp(logDoses),col='magenta',type="b",pch=16)
  return(list(SKdata=data.frame(data2fit,origP=rates,adjP=adjRates,scaleP=trimAdj),
              SKtrim=SKtrimData,
              trimVal=trim))
}


SKgpava <- function(inputFile=NULL,trim=0,doPlot=FALSE,verbose=FALSE){
  if(!require(isotone))stop("Install the isotone package")
  outputFile <- paste(c("F",sample(c(letters,LETTERS,0:9),size=11,replace=TRUE),".txt"),collapse='')
  #on.exit(unlink(outputFile))
  
  if(is.null(inputFile)){
    inputFile <- tcltk::tk_choose.files(caption="Select input data file (MUST be plain text format)")
  }
  if(!is.null(inputFile)){
    if(is.character(inputFile))    data2fit <- read.table(inputFile,header=TRUE,fill=TRUE,colClasses="numeric")
    if(!is.character(inputFile))    data2fit <- inputFile
  }
  #input trim is a percent; convert to fraction
  trim <- trim/100.0
  grayCol <- do.call("rgb",as.list(rep(0.5,3)))
  data2fit <- data2fit[order(data2fit$doses),]
  origData <- data2fit
  #plot input data as given, prior to any modification
  if(doPlot)with(origData,{rates <- responses/sizes;plot(y=rates,x=doses,log='x',ylim=c(0,1),type='b')})
  #remove strings of leading zero reponses out to the highest dose in the series
  startRow <- 1
  if(data2fit$responses[1]==0){
    zeroLocs <- which(data2fit$responses==0)
    leadingZeros <- which(zeroLocs==seq(along=zeroLocs))
    startRow <- ifelse(length(leadingZeros)>1,leadingZeros[length(leadingZeros)],1)
  }
  #remove strings of trailing 100% responses in to the lowest such dose in the series
  endRow <- nrow(data2fit)
  if(data2fit$responses[endRow]==data2fit$sizes[endRow]){
    oneLocs <- which(rev(data2fit$responses)==data2fit$sizes)
    trailing100s <- which(oneLocs==seq(along=oneLocs))
    trailing100s <- rev(1:nrow(data2fit))[trailing100s]
    endRow <- ifelse(length(trailing100s)>1,trailing100s[length(trailing100s)],nrow(data2fit))
  }
  #   subset the data
  #   data2fit <- data2fit[startRow:endRow,]
  if(verbose)print(data2fit)
  #attach so can use just variable names
  attach(data2fit)
  on.exit(detach(2))
  logDoses <- log(doses)
  
  #enforce monotonicity into adjRates variable
  adjRates <- rates <- responses/sizes
  #montonize them using isotone::gpava
  #weights should be n, based on experiments I did with test cases.
  #It amounts to pooling data between two doses to calculate new p.  sqrt(n) will not do that
  trim <- max(adjRates[1],1-tail(adjRates,1))
  adjRates  <- isotone::gpava(y = adjRates,z = logDoses,weights = sizes)$x
  return(list(SKdata=data.frame(data2fit,origP=rates,adjP=adjRates),
              trimVal=trim))
}
