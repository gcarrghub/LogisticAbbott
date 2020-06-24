### LCx functions for Shiny tool
plotSetup.noCI <- function(inputDF,modelType,MLE.ECx=NULL,ECx.target=0.5,genericDoses=FALSE,
                           lineColor="blue",ptColor="darkgray",
                           xlabSTR="Exposure Level",ylabSTR="Probability"){
  #cat("\nplotSetup say modelType = ",modelType,"\n",sep="")
  predictionModelSI <- function(params,logdoses,ECx){
    #DGT0 <- as.numeric(is.finite(logdoses))
    #parameterize background parameter as a logit s.t. no bounds are necessary on the bg rate!
    #p0 goes to zero when BG parm goes large negative, and goes to 1 when BG parm goes large positive
    #could also recenter this so when BG parm is 0, BG prob is 0.05 (BG prob = 0.5 doesn't make any sense)
    #unlikely to make much of a difference, if any.  Most trouble is associated with interval endpoints
    #exactly on dose values (likelihood cliffs)
    p0 <- exp(params["BG"])/(1. + exp(params["BG"]))
    phat <- p0 + (1-p0)/(1+exp(-qlogis(ECx)-(params["alpha"] + params["beta"]*logdoses)))
    phat
  }
  logDoses <- log10(inputDF$doses)
  stepFactor <- median(diff(sort(unique(logDoses))))
  logDoses[!is.finite(logDoses)] <- min(logDoses[is.finite(logDoses)]) - stepFactor
  if(!any(inputDF$doses==0))newX <- seq(min(logDoses)-stepFactor,max(logDoses)+stepFactor,length=1000)
  if( any(inputDF$doses==0))newX <- seq(mean(logDoses[1:2]),max(logDoses)+stepFactor,length=1000)
  plot(x=10^logDoses,y=inputDF$responses/inputDF$sizes,log='x',ylim=c(0,1),xlim=10^range(c(logDoses,newX)),
       xlab=xlabSTR,ylab=ylabSTR,type='n',axes=FALSE,cex.lab=1.5)
  #axis(side=1,at=unique(indata$doses[indata$doses>0]),label=sapply(unique(indata$doses[indata$doses>0]),format))
  labelCodes <- sapply(1:length(unique(inputDF$doses)),FUN=function(i)bquote(C[.(i)]))
  prettyLogs <- axisTicks(usr = par("usr")[1:2],log = TRUE)
  breakAt <- 10^(mean(logDoses[1:2])-.03*diff(par("usr")[1:2]))
  #genericDoses means to choose axis ticks by R defaults, not custom.  Here need to manage
  #if a Dose=0 is part of display
  if(genericDoses & any(inputDF$doses==0)){
    axis(side=1,lwd=2,at=prettyLogs,labels=c("",prettyLogs[-1]),cex.axis=1.5)
    axis(side=1,at=10^logDoses,lwd=2,tck=0,labels = FALSE,cex.axis=1.5)
  }
  if(genericDoses & !any(inputDF$doses==0)){
    axis(side=1,lwd=2,at=prettyLogs,cex.axis=1.5)
  }
  if(!genericDoses)axis(side=1,lwd=2,at=sort(unique(inputDF$doses)),labels=sort(unique(inputDF$doses)),cex.axis=1.5)
  #axis(side=1,at=2^(0:4),label=labelCodes[-c(1,7)],cex.axis=1.5,mgp=c(3,1.4,0))
  #axis(side=1,at=2^c(-1,5),label=labelCodes[c(1,7)],cex.axis=1.0)
  #    axis(side=1,at=axTicks(side=1,usr=c(min(newX),par("usr")[2])))
  #        abline(v=10^(mean(logDoses[1:2])-.03*diff(par("usr")[1:2])))
  if(any(inputDF$doses==0)){
    points(x=10^(logDoses[1]),y=inputDF$responses[1]/inputDF$sizes[1],pch=16,cex=1.5)
    axis(side=1,at=10^min(logDoses),label="Control",las=2,lwd=2)
    axis.break(breakpos=10^(mean(logDoses[1:2])-.03*diff(par("usr")[1:2])))
  }
  points(x=10^logDoses[inputDF$doses>0],y=inputDF$responses[inputDF$doses>0]/inputDF$sizes[inputDF$doses>0],
         pch=16,col=ptColor,cex=2)
  axis(side=2,cex.axis=1.5,lwd=2,las=1)
  if(!(modelType=="lcx"|modelType=="abbott")){
    points(x=10^logDoses[inputDF$doses>0],
           y=inputDF$responses[inputDF$doses>0]/inputDF$sizes[inputDF$doses>0],
           pch=1,col=ptColor,cex=0,lwd=2,type="b")
  }
  #        title(expression(plain(p) == bgroup("(",atop(C,C+(1-C)*over(1,1+plain(e)^{-beta*group("[",log[10](d)-LC[50],"]")})),"")))
  #lines(x=10^newX,y=predict(glmObject,new=data.frame(logDoses=newX),type="response"),col='gray')
  if(modelType=="lcx"|modelType=="abbott"){
    #### if(modelType=="lcx")MLE.ECx <- c(BG=-30,structure(coef(mod),names=c("alpha","beta")))
    #### if(modelType=="abbott")MLE.ECx <- coef(optimxObjectSI)[1,]
    #logDoses <- log10(indata$doses)[is.finite(log10(indata$doses))]
    newY <- predictionModel(params=MLE.ECx,logdoses=newX,ECx=ECx.target)
    #box(lwd=2)
    
    lines(x=10^newX,y=newY,lwd=3,col=lineColor)
    if( any(inputDF$doses==0))rug(side=2,x=predictionModel(params=MLE.ECx,logdoses=c(-Inf),ECx=ECx.target))
    #lines(x=10^newX,y=newY,lwd=2,lty=2,col='gray')
    
    if(any(inputDF$doses==0)){
      BGlevel <- predictionModel(params=MLE.ECx,logdoses=c(-Inf),ECx=ECx.target)
      lines(x=10^c(par("usr")[1],mean(logDoses[1:2])-.06*diff(par("usr")[1:2])),y=rep(BGlevel,2),lwd=3,col=lineColor)
      lines(x=10^c(par("usr")[1],mean(logDoses[1:2])-.06*diff(par("usr")[1:2])),y=rep(BGlevel,2),lwd=2,lty=2,col="gray")
      lines(x=10^c(mean(logDoses[1:2])-.06*diff(par("usr")[1:2]),min(newX)),
            y=c(BGlevel,predictionModel(params=MLE.ECx,logdoses=newX[1],ECx=0.5)),col='gray',lwd=2,lty="dotted")
    }
    #CIlineVals <- results[c("PLL.lower","PLL.upper")]
    #if(CIlineVals[1]<10^par("usr")[1])CIlineVals[1]<-10^par("usr")[1]
    #if(CIlineVals[2]>10^par("usr")[2])CIlineVals[2]<-10^par("usr")[2]
    
    #lines(x=CIlineVals,y=ECxLevel,lwd=3)
    #if(showSTD)lines(x=results[c("STD.lower","STD.upper")],y=ECxLevel,lwd=2,col="cyan")
    #lines(x=CIlineVals,y=rep(par("usr")[3],2),lwd=4)
    #abline(h=ECx.targets,v=10^MLE.ECx["ldXX"])
    if(modelType=="abbott"){
      rug(side=2,x=predictionModel(params=MLE.ECx,logdoses=log(0),ECx=ECx.target),lwd=3)
      rug(side=2,x=predictionModel(params=MLE.ECx,logdoses=log(0),ECx=ECx.target),lwd=2,lty=2,col="gray")
    }
  }
  #if(is.character(fileSTR))mtext(side=1,line=-1,outer=TRUE,text=fileSTR,cex=.5,adj=0.02)
  invisible()
}

LD.Bailer.P<-function(inputDF,confidenceCI=95,debugTF=FALSE,ECx.target=50) {
  # http://www.users.miamioh.edu/baileraj/book/fig7P22.SPLUS
  # calculates confidence interval based upon Fieller's thm.
  # assumes link is linear in dose
  resultNames <- c("x","ECx","FIE.lower","FIE.upper","LR.trend")
  confidenceCI<-confidenceCI/100
  ECx.target<-ECx.target/100
  # add an offset value.  This will adjust estimates for the ECx target, so the LDx is always same ratio of
  # model parameters when fitting the glm() model
  indata <- data.frame(inputDF,offsetVals=rep(log(ECx.target/(1-ECx.target)),nrow(inputDF)))
  indata <- subset(indata,doses>0)
  if(debugTF)print(indata)
  mod <- try(glm(cbind(responses, (sizes-responses)) ~ log10(doses), family = binomial(link=logit),offset = offsetVals,data=indata,control=list(epsilon=1e-12,maxit=100)))
  if(debugTF){
    print(mod)
    mod <<- mod
  }
  #plotSetup.noCI(modelType = "lcx",genericDoses = FALSE)
  #if the model fails, return NAs.  When this happens, can try the more time-consuming PLL approach
  if(class(mod)[1]!="glm")return(structure(c(ECx.target*100,NA,NA,NA,NA),names=resultNames))
  if(class(mod)[1]=="glm"){
    if(debugTF)print(summary(mod))
    LL.glmObject <- c(-sum(dbinom(x=inputDF$responses,size=inputDF$sizes,prob=predict(mod,type="response"),log=TRUE)))
    MLE.NULL <- -sum(dbinom(x=inputDF$responses,size=inputDF$sizes,prob=sum(inputDF$responses)/sum(inputDF$sizes),log=TRUE))
    LR.trend <- 2*abs(LL.glmObject-MLE.NULL)
    coef <- coefficients(mod)
    vcov <- summary.glm(mod)$cov.unscaled
    b0<-coef[1]
    b1<-coef[2]
    var.b0<-vcov[1,1]
    var.b1<-vcov[2,2]
    cov.b0.b1<-vcov[1,2]
    alpha<-1-confidenceCI
    zalpha.2 <- -qnorm(alpha/2)
    gamma <- zalpha.2^2 * var.b1 / (b1^2)
    EC.P <- -b0/b1
    
    const1 <- (gamma/(1-gamma))*(EC.P + cov.b0.b1/var.b1)
    
    const2a <- var.b0 + 2*cov.b0.b1*EC.P + var.b1*EC.P^2 -
      gamma*(var.b0 - cov.b0.b1^2/var.b1)
    
    if(!is.finite(const2a) | b1<0){
      result <- c(100*ECx.target,EC.P,NA,NA,NA)
      names(result) <- resultNames
      return(result)
    }else
    {
      if(const2a>0){
        const2 <- zalpha.2/( (1-gamma)*abs(b1) )*sqrt(const2a)
        if(debugTF)print(c(const1=const1,const2=const2))
        LCL <- EC.P + const1 - const2
        UCL <- EC.P + const1 + const2
      }else
      {
        LCL<- -Inf
        UCL<-  Inf
      }
      if(LCL>=EC.P)LCL <- -Inf
      if(UCL<=EC.P)UCL <- Inf
      result <- c(100*ECx.target,10^c(EC.P,LCL,UCL),LR.trend)
      names(result) <- resultNames
      MLE.ECx <- coefficients(mod)
      logLCX <- EC.P
      #rug(10^logLCX,side=1,lwd=3,lend="butt")
      #predAtLCx <- predictionModelSI(params=MLE.ECx,logdoses=logLCX,ECx=ECx.target)
      #lines(y=ECx.target*c(1,1),x=10^c(LCL,UCL))
      #lines(y=par("usr")[3]*c(1,1),x=10^c(LCL,UCL),lwd=6)
      MLE.ECx <- c(BG=-30,structure(coef(mod),names=c("alpha","beta")))
      return(c(result,beta=unname(coef(mod)[2]),ldXX=unname(logLCX)))
    }
    
  }
}

LD.Bailer.P.abbott <-function(inputDF,confidenceCI=95,debugTF=FALSE,ECx.target=50) {
  if(!require(optimx))stop("Install the optimx package")
  if(!require(plotrix))stop("Install the plotrix package")
  if(!require(isotone,quietly=TRUE))stop("install the isotone package")
  # http://www.users.miamioh.edu/baileraj/book/fig7P22.SPLUS
  # calculates confidence interval based upon Fieller's thm.
  # assumes link is linear in dose
  resultNames <- c("x","ECx","FIE.lower","FIE.upper","LR.trend")
  confidenceCI<-confidenceCI/100
  ECx.target<-ECx.target/100
  
  #set up an offset that adjusts in glm() for the target ECx value
  indata <- data.frame(inputDF,offsetVals=rep(log(ECx.target/(1-ECx.target)),nrow(inputDF)))
  
  SF <- cbind(indata$responses, numalive = indata$sizes - indata$responses)
  logDoses <- log10(indata$doses)
  stepFactor <- (max(10^diff(log10(indata$doses[is.finite(log10(indata$doses))]))))
  #in glm(), can't do zero dose, so adjust it
  logDoses[logDoses==-Inf] <- log10(min(indata$doses[indata$doses>0])/(stepFactor))
  
  newX <- seq(min(logDoses[is.finite(log10(indata$doses))])-log10(stepFactor),max(logDoses)+log10(stepFactor),length=1000)
  glmObject <- try(glm(SF ~ logDoses, family = binomial(link=logit),offset = offsetVals,data=indata,control=list(epsilon=1e-12,maxit=100)))
  LL.glmObject <- c(-sum(dbinom(x=indata$responses,size=indata$sizes,prob=predict(glmObject,type="response"),log=TRUE)))
  
  if(debugTF)cat(paste(rep("-",120),collapse=''),"\nEstimation Results\n",paste(rep("-",120),collapse=''),"\n",sep='')
  
  ###fit the more general model incorporating Abbott's correction
  
  ###confidence intervals under the abbott correction have a different interpretation.  The LC50 is NOT the
  ###dose that causes 50% mortality, it is the dose that causes 50% mortality ABOVE what would naturally occur
  ###anyway.  For example, at an extreme test subjects may experience 50% mortality under control conditions,
  ###s.t. the normal LC50 interpretation would be the control condition (is, no test article toxicity is needed).
  ###Under the correction, the LC50 is the dose that observes 75% mortality (halfway between 50%, in control, and
  ###100% for total toxicity.)
  
  BG.prob.start <- .01
  logdosesBG <- log10(indata$doses)
  logdosesBG[!is.finite(logdosesBG)]<- logdosesBG[is.finite(logdosesBG)][1]-max(diff(logdosesBG[is.finite(logdosesBG)]))
  BG.prob.start <- (isotone::gpava(z=logdosesBG,y=indata$responses/indata$sizes,weights=indata$sizes)$x)[1]
  if(debugTF)print(c(BG.prob.start=BG.prob.start))
  #this shouldn't really happen with data that makes sense, but just in case
  if(BG.prob.start==0)BG.prob.start <- 1e-3
  startValsSI <- c(BG=log(BG.prob.start/(1-BG.prob.start)),alpha=unname(coef(glmObject)[1]),beta=abs(unname(coef(glmObject)[2])))
  if(debugTF)print(startValsSI)
  
  predictionModelSI <- function(params,logdoses,ECx){
    #DGT0 <- as.numeric(is.finite(logdoses))
    #parameterize background parameter as a logit s.t. no bounds are necessary on the bg rate!
    #p0 goes to zero when BG parm goes large negative, and goes to 1 when BG parm goes large positive
    #could also recenter this so when BG parm is 0, BG prob is 0.05 (BG prob = 0.5 doesn't make any sense)
    #unlikely to make much of a difference, if any.  Most trouble is associated with interval endpoints
    #exactly on dose values (likelihood cliffs)
    p0 <- exp(params["BG"])/(1. + exp(params["BG"]))
    phat <- p0 + (1-p0)/(1+exp(-qlogis(ECx)-(params["alpha"] + params["beta"]*logdoses)))
    phat
  }
  minusLL <- function(params,responses,sizes,logdoses,ECx=ECx.target){
    -sum(dbinom(x=responses,size=sizes,prob=predictionModelSI(params,logdoses,ECx),log=TRUE))
  }
  
  optimxObjectSI<-summary(optimx(par=startValsSI,
                                 fn=minusLL,
                                 method=c("BFGS","Nelder-Mead","ucminf","nlminb","CG","spg","L-BFGS-B"),
                                 responses=inputDF$responses,
                                 sizes=inputDF$sizes,
                                 logdoses=log10(inputDF$doses),
                                 control=list(save.failures=FALSE),
                                 hessian=TRUE),order="value")
  #optimxObjectSI<<-optimxObjectSI
  if(debugTF)print(optimxObjectSI)
  #plotSetup.noCI(modelType = "abbott",genericDoses = FALSE)
  
  MLE.NULL <- -sum(dbinom(x=inputDF$responses,size=inputDF$sizes,prob=sum(inputDF$responses)/sum(inputDF$sizes),log=TRUE))
  LR.trend <- 2*abs(optimxObjectSI$value[1]-MLE.NULL)
  #from this point, drop the BG parameter
  coef <- coefficients(optimxObjectSI)[1,-1]
  if(debugTF)print(coef)
  hessMat <- attr(optimxObjectSI,c("details"))[[nrow(optimxObjectSI)*2 + 1]]
  #hessMat[!is.finite(hessMat)] <- 0
  if(debugTF){
    print(hessMat)
    hessMat <<- hessMat
  }
  vcov <- solve(hessMat)[-1,-1]
  b0<-coef[1]
  b1<-coef[2]
  var.b0<-vcov[1,1]
  var.b1<-vcov[2,2]
  cov.b0.b1<-vcov[1,2]
  alpha<-1-confidenceCI
  zalpha.2 <- -qnorm(alpha/2)
  gamma <- zalpha.2^2 * var.b1 / (b1^2)
  EC.P <- -b0/b1
  
  const1 <- (gamma/(1-gamma))*(EC.P + cov.b0.b1/var.b1)
  
  const2a <- var.b0 + 2*cov.b0.b1*EC.P + var.b1*EC.P^2 -
    gamma*(var.b0 - cov.b0.b1^2/var.b1)
  
  if(!is.finite(const2a) | b1<0){
    result <- c(100*ECx.target,EC.P,NA,NA,NA)
    names(result) <- resultNames
    return(result)
  }else
  {
    if(const2a>0){
      const2 <- zalpha.2/( (1-gamma)*abs(b1) )*sqrt(const2a)
      if(debugTF)print(c(const1=const1,const2=const2))
      LCL <- EC.P + const1 - const2
      UCL <- EC.P + const1 + const2
    }else
    {
      LCL<- -Inf
      UCL<-  Inf
    }
    if(LCL>=EC.P)LCL <- -Inf
    if(UCL<=EC.P)UCL <- Inf
    result <- c(100*ECx.target,10^c(EC.P,LCL,UCL),LR.trend)
    names(result) <- resultNames
    
    MLE.ECx <- coefficients(optimxObjectSI)[1,]
    if(debugTF)print(MLE.ECx)
    logLCX <- (-MLE.ECx["alpha"]/MLE.ECx["beta"])
    #rug(10^logLCX,side=1,lwd=3,lend="butt")
    predAtLCx <- predictionModelSI(params=MLE.ECx,logdoses=logLCX,ECx=ECx.target)
    #lines(y=predAtLCx*c(1,1),x=10^c(LCL,UCL))
    #lines(y=par("usr")[3]*c(1,1),x=10^c(LCL,UCL),lwd=6)
    return(c(result,BG=unname(MLE.ECx["BG"]),beta=unname(MLE.ECx["beta"]),ldXX=unname(logLCX)))
  }
  
}

logistic.abbott.PLL <- function(inputFile=NULL,
                                ECx.targets=50,
                                confidenceCI=95,
                                quietTF=TRUE,
                                modelSTR=c("abbott","lcx")[2]
                                #showSTD=FALSE,
                                #xlabSTR="Dose",
                                #ylabSTR="Probability",
                                #modelSTR=c("abbott","lcx")[2],
                                #doTrend=FALSE,
                                #plotTF=FALSE,
                                #finalPlotTF=TRUE,
                                #likePlotTF=FALSE,
                                #detailPlotOnly=FALSE,
                                #interactiveTF=FALSE,
                                #file4plot=NULL
){
  #optimx is the workhorse optimization package.  It may require other optimization packages to be installed,
  #spcifically ucminf
  if(!require(optimx))stop("Install the optimx package")
  #isotone is used to estimate the background rate, when the abbott correction is called for
  if(modelSTR=="abbott"){if(!require(isotone,quietly=TRUE))stop("install the isotone package")}
  #the parent function frame will be used to store objects, so they go away after function exits
  parentENV <- environment()
  
  #always do the trend analysis for general audience
  optimxMethods.ALL <- c("BFGS","Nelder-Mead","ucminf")
  optimxMethods.ucminf <- c("ucminf")
  if(modelSTR == "abbott"){
    nPars<-3
    optimxMethods.PLL <- optimxMethods.ALL
  }
  if(modelSTR == "lcx"){
    nPars<-2
    optimxMethods.PLL <- optimxMethods.ALL #optimxMethods.ucminf
  }
  #PLL calcs for 2-param LCx model are single-parameter optimizations.  Only ucminf is useful in this setting; others for 2 or more.
  
  ECx.targets <- ECx.targets/100
  confidenceCI <- confidenceCI/100
  confidenceAlpha <- 1-confidenceCI
  
  #input data must contain columns named doses, responses, sizes, where doses are untransformed, zero for control
  if(is.null(inputFile)){
    inputFile <- tcltk::tk_choose.files(caption="Select input data file (MUST be plain text format)")
    print(inputFile)
  }
  if(!is.null(inputFile)){
    if(is.character(inputFile))    indata <- read.table(inputFile,header=TRUE,fill=TRUE,colClasses="numeric")
    #this works if the inputFile is actually an object
    if(!is.character(inputFile))    indata <- inputFile
  }
  
  if(ncol(indata)<3)stop("Input data requires 3 columns:  doses, responses, sizes")
  #assure data is in dose order
  indata <- indata[order(indata$doses),]
  #aggregate rows that are from same doses
  indata <- data.frame(doses=indata$doses,
                       as.data.frame(lapply(indata[,c("responses","sizes")],
                                            FUN=function(x,index){tapply(x,INDEX=indata$doses,FUN=sum)}))
  )
  
  nGroups <- nrow(indata)
  hits <- indata[,"responses"]
  total <- indata[,"sizes"]
  
  #trap attempts to force non-count data through these models
  if(any(hits != as.integer(hits)) | any(total != as.integer(total))) {
    print(indata)
    stop("Data are not appropriate.  Only count data are valid in this analysis")
  }
  
  
  
  if(modelSTR=="abbott")predictionModel <- function(params,logdoses,ECx=0.5){
    #DGT0 <- as.numeric(is.finite(logdoses))
    #parameterize background parameter as a logit s.t. no bounds are necessary on the bg rate!
    #p0 goes to zero when BG parm goes large negative, and goes to 1 when BG parm goes large positive
    #could also recenter this so when BG parm is 0, BG prob is 0.05 (BG prob = 0.5 doesn't make any sense)
    #unlikely to make much of a difference, if any.  Most trouble is associated with interval endpoints
    #exactly on dose values (likelihood cliffs)
    p0 <- exp(params["BG"])/(1. + exp(params["BG"]))
    phat <- p0 + (1-p0)/(1+exp(-qlogis(ECx)-(params["beta"])*(logdoses-params["ldXX"])))
    phat
  }
  if(modelSTR=="lcx")predictionModel <- function(params,logdoses,ECx=0.5){
    #DGT0 <- as.numeric(is.finite(logdoses))
    #parameterize background parameter as a logit s.t. no bounds are necessary on the bg rate!
    #p0 goes to zero when BG parm goes large negative, and goes to 1 when BG parm goes large positive
    #could also recenter this so when BG parm is 0, BG prob is 0.05 (BG prob = 0.5 doesn't make any sense)
    #unlikely to make much of a difference, if any.  Most trouble is associated with interval endpoints
    #exactly on dose values (likelihood cliffs)
    #p0 <- exp(params["BG"])/(1. + exp(params["BG"]))
    phat <- 1/(1+exp(-qlogis(ECx)-(params["beta"])*(logdoses-params["ldXX"])))
    phat
  }
  #print(predictionModel)
  
  #can be used (predictionModel() above shows the simple reparameterization):
  ecx.translation <- function(ecx.values,params){
    params["ldXX"]+qlogis(ecx.values)/abs(params["beta"])
  }
  
  
  #LL calculations are simple function of dbinom:
  minusLL <- function(params,responses,sizes,logdoses,ECx=0.5){
    #print(predictionModel(params,logdoses,ECx))
    #probs <- predictionModel(params,logdoses,ECx)
    #-sum(responses*log(probs) + (sizes-responses)*log(1-probs))
    -sum(dbinom(x=responses,size=sizes,prob=predictionModel(params,logdoses,ECx),log=TRUE))
  }
  
  #profiling the likelihood means to optimize over the nuisance parameters, given a value of the
  #parameter of interest (ldXX here)
  minusLL.PLL <- function(params,ldXX,responses,sizes,logdoses,ECx=0.5){
    #input params must be named
    params <- c(params,ldXX=unname(ldXX))
    #probs <- predictionModel(params,logdoses,ECx)
    #-sum(responses*log(probs) + (sizes-responses)*log(1-probs))
    -sum(dbinom(x=responses,size=sizes,prob=predictionModel(params,logdoses,ECx),log=TRUE))
  }
  
  
  #fit the ordinary logistic model, which will be used for starting values
  #!Comment:
  #If ordinary logistic regression is used on log scale, and controls yield zero responses,
  #there is no point to including control group in the model.  The default model predicts zero response
  #at the lower limit of doses, and the data will reflect that, with log-likelihood contribution=0
  #the problem will be that sometimes the remaining data do not reflect where on the dose scale
  #responses drop to zero (though if that is the case, resetting the control dose is very subjective, bias?)
  #...and of course, if there is control mortality, the std logistic model makes no sense
  SF <- cbind(indata$responses, numalive = indata$sizes - indata$responses)
  logDoses <- log10(indata$doses)
  stepFactor <- (max(10^diff(log10(indata$doses[is.finite(log10(indata$doses))]))))
  logDoses[logDoses==-Inf] <- log10(min(indata$doses[indata$doses>0])/(stepFactor))
  
  newX <- seq(min(logDoses[is.finite(log10(indata$doses))])-log10(stepFactor),max(logDoses)+log10(stepFactor),length=1000)
  glmObject <- glm(SF ~  logDoses, family = binomial)
  LL.glmObject <- c(-sum(dbinom(x=indata$responses,size=indata$sizes,prob=predict(glmObject,type="response"),log=TRUE)))
  
  if(!quietTF)cat(paste(rep("-",120),collapse=''),"\nEstimation Results\n",paste(rep("-",120),collapse=''),"\n",sep='')
  
  ###fit the more general model incorporating Abbott's correction
  
  ###confidence intervals under the abbott correction have a different interpretation.  The LC50 is NOT the
  ###dose that causes 50% mortality, it is the dose that causes 50% mortality ABOVE what would naturally occur
  ###anyway.  For example, at an extreme test subjects may experience 50% mortality under control conditions,
  ###s.t. the normal LC50 interpretation would be the control condition (is, no test article toxicity is needed).
  ###Under the correction, the LC50 is the dose that observes 75% mortality (halfway between 50%, in control, and
  ###100% for total toxicity.)
  
  BG.prob.start <- .01
  logdosesBG <- log10(indata$doses)
  logdosesBG[!is.finite(logdosesBG)]<- logdosesBG[is.finite(logdosesBG)][1]-max(diff(logdosesBG[is.finite(logdosesBG)]))
  BG.prob.start <- (isotone::gpava(z=logdosesBG,y=indata$responses/indata$sizes,weights=indata$sizes)$x)[1]
  if(!quietTF)print(c(BG.prob.start=BG.prob.start))
  
  if(BG.prob.start==0)BG.prob.start <- 1e-3
  startVals <- c(BG=log(BG.prob.start/(1-BG.prob.start)),beta=abs(unname(coef(glmObject)[2])),ldXX=-unname(coef(glmObject)[1])/unname(coef(glmObject)[2]))
  lowerBounds <- c(0,-Inf,-Inf)
  
  if(modelSTR=="lcx")startVals <- startVals[-1]
  
  optimxObject<-summary(optimx(par=startVals,
                               fn=minusLL,
                               method=optimxMethods.ALL,
                               responses=indata$responses,
                               sizes=indata$sizes,
                               logdoses=log10(indata$doses),
                               control=list(save.failures=FALSE),
                               hessian=TRUE),order="value")
  if(!quietTF){
    print(str(optimxObject))
    #optimxJUNK <<- optimxObject
  }
  #stop()
  MLE <- coef(optimxObject)[1,]
  #print(MLE)
  
  MLE.ECx <- MLE
  MLE.ECx["ldXX"]<-ecx.translation(ECx.targets,MLE)
  
  if(MLE["beta"]<0)stop("Best fit has mortality decreasing with increasing exposure")
  #print(MLE.ECx)
  #print("CALCULATE LL VALUES")
  MLE.LL <- minusLL(MLE,indata$responses,indata$sizes,log10(indata$doses))
  MLE.NULL <- -sum(dbinom(x=indata$responses,size=indata$sizes,prob=sum(indata$responses)/sum(indata$sizes),log=TRUE))
  #print(c(fittedLL=MLE.LL,nullLL=MLE.NULL))
  #right here, we can check for whether the LL separation is sufficient to calculate an interval.  If NOT, just throw and error with stop()
  #print(c(2*(MLE.NULL-MLE.LL),qchisq(confidenceCI,1),confidenceCI))
  #print((2*(MLE.NULL-MLE.LL)<qchisq(confidenceCI,1)))
  if(2*(MLE.NULL-MLE.LL)<qchisq(confidenceCI,1))return("LL trend criterion not met")
  
  
  LL.values <- c(
    full=MLE.LL,
    reduced=LL.glmObject
  )
  #print(LL.values)
  ###plot data and models.  Control data are the lowest 'dose' value
  
  
  
  #calculate and plot CIs for the requested ECx values
  #basic process is to fix ldXX parameter, fit the remaining parameters, and check if that solution is close to full fit
  #if it is close, it is in the interval.
  CIcalcs.abbott <- function(ECx.val,debugTF=!quietTF,optimxMethods=optimxMethods.PLL){
    ###wald-style CI(this CI will only work if the background param really is away from zero, otherwise, will get error)
    ###have to repeat optim() to get new SE estimates from the hessian matrix (SE is typically smallest for LC50, and largest far from 50)
    ###if the two types of CI are widely different, it should suggest problems with the data/model fit
    ###generalized inverse MASS::ginv() vs. inverse by solve() may give different answers.  Could check this as a diagnostic!
    hessMatrix <- attr(optimxObject,"details")[[1,"nhatend"]]
    covTRY <- try(covMatrix <- MASS::ginv(hessMatrix))
    #covTRY <- try(covMatrix <- MASS::ginv(bestSol$nhatend))
    if(debugTF)print(covTRY)
    if(class(covTRY)!="try-error"){
      parNum <- 2
      if(modelSTR=="abbott") parNum <- 3
      CI.wald <- c(ECx.val,
                   10^c(MLE.ECx["ldXX"],
                        MLE.ECx["ldXX"]+qnorm(1-confidenceAlpha/2)*c(-1,+1)*sqrt(covMatrix[parNum,parNum])),
                   SE=sqrt(covMatrix[parNum,parNum]))
    }
    if(class(covTRY)=="try-error"){
      CI.wald <- c(ECx.val,NA,NA,SE=NA)
    }
    if(debugTF)print(CI.wald)
    #lines(x=CI.wald[3:4],y=rep(predictionModel(MLE.ECx,logdoses=MLE.ECx["ldXX"],ECx=ECx.val),2),lwd=2,col='lightblue')
    #lines(x=CI.wald[3:4],y=rep(par("usr")[3],2),lwd=2,col='lightblue')
    
    #use profile likelihood calc to CI the ECx's
    #as slope/bkgd are held fixed, the ci width is basically the same for all ECx levels
    ###NO, this is not PLL
    unirootFUN <- function(ldxxValue,startValues){
      optimxObject<-summary(optimx(par=startValues,
                                   fn=minusLL.PLL,
                                   method=optimxMethods,
                                   responses=indata$responses,
                                   sizes=indata$sizes,
                                   logdoses=log10(indata$doses),
                                   ldXX=ldxxValue,
                                   ECx=ECx.val,
                                   hessian=FALSE),order="value")
      #print(c(ldxxValue,bestSol$value))
      #given an ldXX value, find best slope, then diff vs MLE.LL
      #optimize()
      assign("unirootSol",coef(optimxObject)[1,],envir=parentENV)
      abs(2*(optimxObject[1,"value"]-MLE.LL))-qchisq(confidenceCI,1)
    }
    
    
    #first, bounding lower CI value (one that the lower CI is above)
    if(debugTF)cat(paste(rep("-",120),collapse=''),"\nLower CI Estimation\n",paste(rep("-",120),collapse=''),"\n",sep='')
    
    #trying A LOT of doses to bracket ECx is expensive, so go very simple first
    #in most cases, going by dose spacing will be sufficient and work without errors
    lowerIntervalFound <- FALSE
    lowerCandidates <- logDoses[is.finite(logDoses)]
    #this range goes well beyond the range of tested concentrations.  If we can get a likelihood number on the furthest point, and still
    #not get a bound, might as well stop.
    lowerCandidates <- rev(sort(c(lowerCandidates[lowerCandidates<MLE.ECx["ldXX"]],lowerCandidates[1]-max(diff(lowerCandidates))*(1:4),MLE.ECx["ldXX"])))
    #recent fix -- not doing this causes issues when ECx is OUTSIDE of tested range
    lowerCandidates <- lowerCandidates[lowerCandidates<=MLE.ECx["ldXX"]]
    previousSolution <- MLE.ECx[-which(names(MLE.ECx)=="ldXX")]
    lowerCandidatesPars <- matrix(NA,ncol=length(MLE.ECx)-1,nrow=length(lowerCandidates),dimnames=list(NULL,names(MLE.ECx[-which(names(MLE.ECx)=="ldXX")])))
    if(debugTF)print(c(lowerCandidates=10^lowerCandidates))
    
    lowerError <- FALSE
    #what if all of these work, but no root in this range...continue, or stop?
    for(ldCandidate in seq(along=lowerCandidates)){
      if(ldCandidate>1)previousSolution <- lowerCandidatesPars[ldCandidate-1,]
      #print(c(ldCandidate,MLE.ECx))
      #print(previousSolution)
      if(debugTF){
        print(paste("Testing ldCandidate #",ldCandidate))
        print(list(par=previousSolution,
                   fn=minusLL.PLL,
                   method=optimxMethods,
                   responses=indata$responses,
                   sizes=indata$sizes,
                   logdoses=log10(indata$doses),
                   ldXX=lowerCandidates[ldCandidate],
                   ECx=ECx.val,
                   hessian=FALSE))
        print(lowerCandidatesPars)
      }
      optimxObjectLI<-try(optimx(par=previousSolution,
                                 fn=minusLL.PLL,
                                 method=optimxMethods,
                                 responses=indata$responses,
                                 sizes=indata$sizes,
                                 logdoses=log10(indata$doses),
                                 ldXX=lowerCandidates[ldCandidate],
                                 ECx=ECx.val,
                                 hessian=FALSE))
      #print(class(optimxObjectLI))
      #if the model cannot be fit, don't try any others
      #this condition should only hold in troublesome cases, like infinite slope
      if(regexpr("error",tolower(class(optimxObjectLI)[1]))>0){
        lowerError <- TRUE
        lowerIntervalFound <- FALSE
        break
      }
      if(debugTF){
        print("lower Search results")
        print(c(doseTested=c(log=lowerCandidates[ldCandidate],std=10^lowerCandidates[ldCandidate])))
        if(!lowerError)print(optimxObjectLI)
        print("end lower Search results")
      }
      #print(c(lowerError=lowerError))
      #only do the rest if the model could be fit
      if(!lowerError){
        if(debugTF){
          print(c(lowerError=lowerError))
          print("inside lowerError IF")
        }
        if(debugTF){
          #print(c(bestSol=bestSol))
          print(optimxObjectLI)
        }
        lowerCandidatesPars[ldCandidate,] <- coef(summary(optimxObjectLI,order="value"))[1,]
        if(2*abs(optimxObjectLI[1,"value"]-MLE.LL)>qchisq(confidenceCI,1)){
          lowerInterval <- sort(lowerCandidates[ldCandidate+c(0,-1)])
          #it is a deliberate choice to take previous solution here.  Once outside CI, param estimates
          #can get extreme, so start at what should be the less extreme choice
          lowerStart.slope <- previousSolution
          lowerIntervalFound <- TRUE
          if(debugTF){print("Lower Interval Found");print(10^lowerInterval)}
          break}
      }
    }
    
    if(exists("lowerInterval") & lowerIntervalFound){
      if(length(lowerInterval)<2){
        cat("\n",paste(rep("#",100),collapse = ""))
        cat("\nLower Interval Issue\n")
        print(c(ldCandidate=ldCandidate))
        print(c(lowerCandidates=lowerCandidates))
        print(optimxObjectLI)
        print(c(MLE.ECx=MLE.ECx["ldXX"],MLE.LL=MLE.LL))
      }
    }
    
    
    #only do this if the first round fails hard (as in, if go through full set of candidates and just liklihood is not different enough, give up,
    #but if it fails hard, as in likelihood is not calculable, we might find solution becuase likelihood is steep near doses)
    didFineGridLI <- FALSE
    if(!lowerIntervalFound & lowerError){
      didFineGridLI <- TRUE
      lowerCandidates <- rev(seq(min(logDoses[is.finite(logDoses)])-diff(range(logDoses[is.finite(logDoses)])),MLE.ECx["ldXX"],length=50))
      #for doses below the LCx, add candidates in a tight window around each dose
      #use the smallest spacing between doses the base delta, then take fractions of it
      addedDoses <- as.vector(subset(data.frame(x=logDoses[is.finite(logDoses)]),x>=min(x) & x<MLE.ECx["ldXX"])[,1])
      if(length(addedDoses)>0){
        addedDeltas <- range(diff(logDoses[is.finite(logDoses)]))*c(.01,.001,.0001)[1]
        addedDeltas <- c(-addedDeltas,addedDeltas)
        #add doeses themselves to it also.  In most cases, when the likelihood is a "cliff" at the dose itself,
        #the dose itself will technically NOT be in the interval, but the likelihood will still be finite
        addedDoses <- c(addedDoses,unlist(sapply(addedDoses,FUN=function(x)x+addedDeltas,simplify=FALSE)))
        #for lower candidates, rev() the sort, so we are checking values nearest the LCx first
        lowerCandidates <- unique(rev(sort(c(lowerCandidates,addedDoses))))
      }
      lowerCandidatesPars <- matrix(NA,ncol=length(MLE.ECx)-1,nrow=length(lowerCandidates),dimnames=list(NULL,names(MLE.ECx[-which(names(MLE.ECx)=="ldXX")])))
      if(debugTF){print("Lower Candidates");print(c(range=range(10^lowerCandidates)));print(lowerCandidates);print(head(10^lowerCandidates))}
      previousSolution <- MLE.ECx[-which(names(MLE.ECx)=="ldXX")]
      lowerIntervalFound<-FALSE
      for(ldCandidate in seq(along=lowerCandidates)){
        if(ldCandidate>1)previousSolution <- lowerCandidatesPars[ldCandidate-1,]
        #print(c(ldCandidate,MLE.ECx))
        #print(previousSolution)
        optimxObjectLI2<-summary(optimx(par=previousSolution,
                                        fn=minusLL.PLL,
                                        method=optimxMethods,
                                        responses=indata$responses,
                                        sizes=indata$sizes,
                                        logdoses=log10(indata$doses),
                                        ldXX=lowerCandidates[ldCandidate],
                                        ECx=ECx.val,
                                        hessian=FALSE),order="value")
        if(debugTF){print(c(doseTested=c(lowerCandidates[ldCandidate],10^lowerCandidates[ldCandidate])));print(optimxObjectLI2)}
        #if(debugTF)print(bestSol)
        lowerCandidatesPars[ldCandidate,] <- coef(optimxObjectLI2)[1,]
        if(2*abs(optimxObjectLI2[1,"value"]-MLE.LL)>qchisq(confidenceCI,1)){
          lowerInterval <- sort(lowerCandidates[ldCandidate+c(0,-1)])
          #it is a deliberate choice to take previous solution here.  Once outside CI, param estimates
          #can get extreme, so start at what should be the less extreme choice
          lowerStart.slope <- previousSolution
          if(debugTF){print("Lower Interval Found");print(10^lowerInterval)}
          lowerIntervalFound <- TRUE
          break}
      }
      
    }
    if(!lowerIntervalFound)lowerInterval=c(-999,-999)
    lowerLines <- as.data.frame(na.omit(cbind(lowerCandidates,lowerCandidatesPars)))
    lowerLines <- lowerLines[-nrow(lowerLines),]
    names(lowerLines)[1] <- "ldXX"
    if(debugTF){
      lowerLines4print <- lowerLines
      if(any(names(lowerLines4print)=="BG")){
        BG.par <- lowerLines4print$BG
        lowerLines4print$P0 <- exp(BG.par)/(1. + exp(BG.par))
      }
      lowerLines4print$LD.Dose <- 10^lowerLines$ldXX
      print(lowerLines4print)
    }
    ##### if(plotTF)assign("lowerLines",lowerLines,envir=parentENV)
    
    
    
    
    
    if(debugTF)cat(paste(rep("-",120),collapse=''),"\nUpper CI Estimation\n",paste(rep("-",120),collapse=''),"\n",sep='')
    #trying A LOT of doses to bracket ECx is expensive, so go very simple first
    upperIntervalFound <- FALSE
    upperCandidates <- logDoses[is.finite(logDoses)]
    upperCandidates <- (sort(c(upperCandidates[upperCandidates>MLE.ECx["ldXX"]],max(upperCandidates)+max(diff(upperCandidates))*(1:4),MLE.ECx["ldXX"])))
    #recent fix -- not doing this causes issues when ECx is OUTSIDE of tested range
    upperCandidates <- upperCandidates[upperCandidates>=MLE.ECx["ldXX"]]
    
    if(debugTF)print(10^upperCandidates)
    previousSolution <- MLE.ECx[-which(names(MLE.ECx)=="ldXX")]
    upperCandidatesPars <- matrix(NA,ncol=length(MLE.ECx)-1,nrow=length(upperCandidates),dimnames=list(NULL,names(MLE.ECx[-which(names(MLE.ECx)=="ldXX")])))
    
    upperError <- FALSE
    for(ldCandidate in seq(along=upperCandidates)){
      if(ldCandidate>1)previousSolution <- upperCandidatesPars[ldCandidate-1,]
      #print(c(ldCandidate,MLE.ECx))
      #print(previousSolution)
      optimxObjectUI<-try(optimx(par=previousSolution,
                                 fn=minusLL.PLL,
                                 method=optimxMethods,
                                 responses=indata$responses,
                                 sizes=indata$sizes,
                                 logdoses=log10(indata$doses),
                                 ldXX=upperCandidates[ldCandidate],
                                 ECx=ECx.val,
                                 hessian=FALSE))
      if(regexpr("error",tolower(class(optimxObjectUI)[1]))>0){
        upperError <- TRUE
        break
      }
      if(debugTF){
        print(c(doseTested=c(upperCandidates[ldCandidate],10^upperCandidates[ldCandidate])))
        if(!upperError)print(optimxObjectUI)
      }
      if(!upperError){
        optimxObjectUI <- summary(optimxObjectUI,order="value")
        #if(debugTF)print(bestSol)
        upperCandidatesPars[ldCandidate,] <- coef(optimxObjectUI)[1,]
        if(2*abs(optimxObjectUI[1,"value"]-MLE.LL)>qchisq(confidenceCI,1)){
          upperInterval <- sort(upperCandidates[ldCandidate+c(0,-1)])
          #it is a deliberate choice to take previous solution here.  Once outside CI, param estimates
          #can get extreme, so start at what should be the less extreme choice
          upperStart.slope <- previousSolution
          if(debugTF){print("upper Interval Found");print(10^upperInterval)}
          upperIntervalFound <- TRUE
          break}
      }
    }
    
    
    
    #again, only do this if we can't quickly bracket above due to likelihood error
    if(!upperIntervalFound & upperError){
      upperCandidates <- rev(seq(max(logDoses)+diff(range(logDoses)),MLE.ECx["ldXX"],length=50))
      #for doses above the LCx, add candidates in a tight window around each dose
      #use the smallest spacing between doses the base delta, then take fractions of it
      addedDoses <- as.vector(subset(data.frame(x=logDoses[is.finite(logDoses)]),x>MLE.ECx["ldXX"] & x<=max(x))[,1])
      if(length(addedDoses)>0){
        addedDeltas <- diff(range(logDoses[is.finite(logDoses)]))*c(.01,.001,.0001)
        addedDeltas <- c(-addedDeltas,addedDeltas)
        #add doeses themselves to it also.  In most cases, when the likelihood is a "cliff" at the dose itself,
        #the dose itself will technically NOT be in the interval, but the likelihood will still be finite
        addedDoses <- c(addedDoses,unlist(sapply(addedDoses,FUN=function(x)x+addedDeltas,simplify=FALSE)))
        #for upper candidates, sort ascending, so we are checking values nearest the LCx first
        upperCandidates <- unique(sort(c(upperCandidates,addedDoses)))
      }
      upperCandidatesPars <- matrix(NA,ncol=length(MLE.ECx)-1,nrow=length(upperCandidates),dimnames=list(NULL,names(MLE.ECx[-which(names(MLE.ECx)=="ldXX")])))
      if(debugTF){print("Upper Candidates");print(c(range=range(10^upperCandidates)));print(upperCandidates);print(head(10^upperCandidates))}
      previousSolution <- MLE.ECx[-which(names(MLE.ECx)=="ldXX")]
      upperIntervalFound<-FALSE
      for(ldCandidate in seq(along=upperCandidates)){
        if(ldCandidate>1)previousSolution <- upperCandidatesPars[ldCandidate-1,]
        optimxObjectUI<-summary(optimx(par=previousSolution,
                                       fn=minusLL.PLL,
                                       method=optimxMethods,
                                       responses=indata$responses,
                                       sizes=indata$sizes,
                                       logdoses=log10(indata$doses),
                                       ldXX=upperCandidates[ldCandidate],
                                       ECx=ECx.val,
                                       hessian=FALSE),order="value")
        if(debugTF){print(c(doseTested=c(upperCandidates[ldCandidate],10^upperCandidates[ldCandidate])));print(optimxObjectUI)}
        upperCandidatesPars[ldCandidate,] <- coef(optimxObjectUI)[1,]
        if(2*abs(optimxObjectUI[1,"value"]-MLE.LL)>qchisq(confidenceCI,1)){
          upperInterval <- sort(upperCandidates[ldCandidate+c(0,-1)])
          upperStart.slope <- previousSolution
          #upperStart.slope <- bestSol$par
          if(debugTF){print("Upper Interval Found");print(10^upperInterval)}
          upperIntervalFound <- TRUE
          break}
      }
    }
    if(debugTF)print(c(upperIntervalFound=upperIntervalFound))
    if(!upperIntervalFound)upperInterval=c(999,999)
    if(debugTF)print(10^upperInterval)
    upperLines <- as.data.frame(na.omit(cbind(upperCandidates,upperCandidatesPars)))
    names(upperLines)[1] <- "ldXX"
    upperLines <- upperLines[-nrow(upperLines),]
    if(debugTF){
      upperLines4print <- upperLines
      if(any(names(upperLines4print)=="BG")){
        BG.par <- upperLines4print$BG
        upperLines4print$P0 <- exp(BG.par)/(1. + exp(BG.par))
      }
      upperLines4print$LD.Dose <- 10^upperLines$ldXX
      print(upperLines4print)
    }
    ###### if(plotTF)assign("upperLines",upperLines,envir=parentENV)
    
    
    
    #############################################################################################
    if(debugTF)print("Find lower CI endpoint")
    if(lowerIntervalFound){
      if(debugTF)print("Doing lower limit uniroot")
      lowerUniroot <- try(uniroot(unirootFUN,interval=lowerInterval,start=lowerStart.slope,extendInt = "downX"))
      if(regexpr("error",tolower(class(lowerUniroot)[1]))<0)lowerUniroot <- lowerUniroot$root
      if(regexpr("error",tolower(class(lowerUniroot)[1]))>0){
        cat("\nUnexpected uniroot error for lower CI\n")
        print(c(lowerIntervalFound=lowerIntervalFound,didFineGridLI=didFineGridLI,lowerInterval=lowerInterval,MLE.ECx=MLE.ECx["ldXX"]))
        print(indata)
        lowerUniroot <- -999
        lowerIntervalFound<-FALSE
        cat("\nEND unexpected uniroot error\n")
        
      }
      #lowerUniroot <- uniroot(unirootFUN,interval=lowerInterval,start=lowerStart.slope)
      #lowerPars <- c(unirootSol,ldXX=lowerUniroot)
      ###if(plotTF & lowerIntervalFound){
      ###  lowerPars <- c(unirootSol,ldXX=lowerUniroot)
      ###  assign("lowerPars",lowerPars,envir=parentENV)
      ###}
    }
    if(!lowerIntervalFound)lowerUniroot<-c(-999)
    
    
    
    
    if(debugTF)print("Find upper CI endpoint")
    if(debugTF)print(upperInterval)
    if(upperIntervalFound){
      if(debugTF)print("Doing upper limit uniroot")
      upperUniroot <- try(uniroot(unirootFUN,interval=upperInterval,start=upperStart.slope,extendInt = "upX"))
      if(regexpr("error",tolower(class(upperUniroot)[1]))<0)upperUniroot <- upperUniroot$root
      if(regexpr("error",tolower(class(upperUniroot)[1]))>0){
        cat("\nUnexpected uniroot error for UPPER CI\n")
        print(c(upperIntervalFound=upperIntervalFound,upperInterval=upperInterval))
        print(indata)
        upperUniroot <- 999
        upperIntervalFound<-FALSE
        cat("\nEND unexpected uniroot error\n")
      }
      #upperUniroot <- uniroot(unirootFUN,interval=upperInterval,start=upperStart.slope)
      #upperPars <- c(unirootSol,ldXX=upperUniroot)
      ###if(plotTF & upperIntervalFound){
      ###  upperPars <- c(unirootSol,ldXX=upperUniroot)
      ###  assign("upperPars",upperPars,envir=parentENV)
      ###}
    }
    if(!upperIntervalFound)upperUniroot<-c(999)
    
    
    CI <- 10^c(MLE=MLE.ECx["ldXX"],lower=lowerUniroot,upper=upperUniroot)
    if(debugTF){
      assign(x="lowerInterval",value=lowerInterval,envir=parentENV)
      assign(x="upperInterval",value=upperInterval,envir=parentENV)
    }
    c(x=100*ECx.val,ECx=unname(CI[1]),PLL=CI[-1],STD=c(lower=CI.wald[3],upper=CI.wald[4]),CIconfidence=100*confidenceCI)
  }#end CIcalcs()
  
  results <- CIcalcs.abbott(ECx.targets)
  
  if(!quietTF)print(MLE.ECx)
  if(!quietTF)print(results)
  #MLE.ECx <- c(MLE.ECx, prob.BG=unname(predictionModel(MLE.ECx,logdose=-Inf)))
  #print(predictionModel(MLE.ECx,MLE.ECx["ldXX"]))
  if(quietTF)return(c(results,LL.fitted=MLE.LL,LL.null=MLE.NULL,prob.BG=unname(predictionModel(MLE.ECx,logdose=-Inf)),MLE.ECx))
  if(!quietTF)return(c(results,LL.fitted=MLE.LL,LL.null=MLE.NULL,lowerINT=lowerInterval,upperINT=upperInterval,MLE.ECx))
}

logisticLCx <- function(inputData,confLevelPct=95,ECxPercent=50,modelType=c("lcx","abbott")[1]){
  if(modelType=="abbott" & FALSE){#if false in here, abbott switch is off
    if(!any(inputData$doses==0)){# | all(inputData$responses[inputData$doses==0]==0)){
      modelType<-"lcx"
      print("Control data (or lack thereof) not consistent with abbott-correction")
    }
  }
  if(modelType=="lcx"){
    if(any(inputData$doses==0))print("Input data with Dose=0 is omitted")
    #if(any(inputData$doses==0) & any(inputData$responses[inputData$doses==0]>0))print("Some non-zero input data with Dose=0 omitted")
    #if(any(inputData$doses==0) & all(inputData$responses[inputData$doses==0]==0))print("Zero reponse input data with Dose=0 omitted (has no effect)")
    inputData <- subset(inputData,doses>0)
    tryFIE <- try(LD.Bailer.P(inputDF=inputData,confidenceCI=confLevelPct,debugTF=FALSE,ECx.target=ECxPercent))
    PLL.set <- FALSE
    if(class(tryFIE)!="try-error"){
      result <- tryFIE
      PLL.set <- with(as.data.frame(rbind(result)),(FIE.lower>=ECx |FIE.upper<=ECx | !is.finite(log(FIE.lower)) | !is.finite(log(FIE.upper))) & LR.trend>qchisq(0.95,1))
    }
    if(class(tryFIE)=="try-error" | PLL.set){
      tryPLL <- logistic.abbott.PLL(inputFile=inputData,
                                    ECx.targets=ECxPercent,
                                    confidenceCI=confLevelPct,
                                    quietTF=TRUE,
                                    modelSTR="lcx")
      if(class(tryPLL)!="try-error"){
        result <- tryPLL
        #lines(x=result[c("PLL.lower","PLL.upper")],y=par("usr")[3]*c(1,1),lwd=6,col="green",lend="butt")
        #lines(x=result[c("PLL.lower","PLL.upper")],y=ECxPercent*c(1,1)/100,lwd=2,col="green",lend="butt")
      }
      if(class(tryPLL)=="try-error")result <- "LCx Estimation Fails"
    }
    return(result)
  }
  if(modelType=="abbott"){
    tryFIE <- try(LD.Bailer.P.abbott(inputDF=inputData,confidenceCI=confLevelPct,debugTF=FALSE,ECx.target=ECxPercent))
    PLL.set <- FALSE
    if(class(tryFIE)!="try-error"){
      result <- tryFIE
      PLL.set <- with(as.data.frame(rbind(result)),(FIE.lower>=ECx |FIE.upper<=ECx | !is.finite(log(FIE.lower)) | !is.finite(log(FIE.upper))) & LR.trend>qchisq(0.95,1))
    }
    if(class(tryFIE)=="try-error" | PLL.set){
      tryPLL <- logistic.abbott.PLL(inputFile=inputData,
                                    ECx.targets=ECxPercent,
                                    confidenceCI=confLevelPct,
                                    quietTF=TRUE,
                                    modelSTR="abbott")
      if(class(tryPLL)!="try-error"){
        result <- tryPLL
        #lines(x=result[c("PLL.lower","PLL.upper")],y=par("usr")[3]*c(1,1),lwd=6,col="green",lend="butt")
        #lines(x=result[c("PLL.lower","PLL.upper")],y=ECxPercent*c(1,1)/100,lwd=2,col="green",lend="butt")
      }
      if(class(tryPLL)=="try-error")result <- "LCx Estimation Fails"
    }
    return(result)
  }
}
