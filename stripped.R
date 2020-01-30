library(shiny)
library(shinyIncubator)
library(XLConnect)
library(optimx)
library(plotrix)

## If debugTF spit everything out to console.
debugTF <- TRUE

## ALWAYS doTrend 
doTrend <- TRUE

exampleDat <- matrix(c(1,0,20,2,0,20,4,10,20,8,20,20,16,20,20),byrow=TRUE,nrow=5)
colnames(exampleDat) <- c("doses","responses","sizes")

shinyServer(function(input,output,session) {


  getFileExt <- reactive({
	# Read in the .csv input data
	  return(fileExt)
  })
  
  output$sheetUI <- renderUI({
	  wb <- loadWorkbook(inFile$datapath)
	  shNames <- getSheets(wb)
	  selectInput('shName', 'Select Sheet Containing Data', shNames)
  })
  
  inputDataFile <- reactive({
	  fileExt <- sub(".*\\.","",inFile[1])
        wb <- loadWorkbook(inFile$datapath)
	    indata <- readWorksheet(wb, sheet=shName, header=TRUE)
      return(list(indata=indata,badDataFlag=badDataFlag))    
	}
  })

  observe({
	if(!is.null(getFileExt()))updateTabsetPanel(session,"tabManager",selected="dataTab")	
  })
  
  
  output$doseColUI <- renderUI({
    namesInp <- names(inputDataFile()[["indata"]])
	  namesInFrame <- names(inputDataFile()[["indata"]])
	  return(selectInput(inputId="nameDoseCol","Select Concentration Variable",namesInFrame,namesInFrame[1]))
	}
  })


  output$respColUI <- renderUI({
	  namesInFrame <- names(inputDataFile()[["indata"]])
      return(selectInput(inputId="nameRespCol","Select Response Variable",namesInFrame,namesInFrame[2]))
    }     
  })

  output$sizeColUI <- renderUI({
	  namesInFrame <- names(inputDataFile()[["indata"]])
      return(selectInput(inputId="nameSizeCol","Select Size Variable",namesInFrame,namesInFrame[3]))
    }     
  })  
  
  dataOrg <- reactive({
    namesInp <- names(inputDataFile()[["indata"]])
	return(list(indata=indata,dose0Flag=dose0Flag))
  })

  performZCA <- function(){
    isolate({
	  inputDataRes <- dataOrg()
	})

    zCA <- function(indata=NULL, fuzzFactor=1e-13, do.fisher=TRUE, totalsColumn=TRUE, n.perm.trials=10000000, verbose=FALSE, last.is.PC=FALSE){
      
      #	#this zCA calculator calculates CA z for a MATRIX of data (one column/case)
      zCA.calculator <- function(permdata,groupSizes){
        sign(b)*sqrt(((b^2)/(pAvg*(1-pAvg)))*denominator)
      }
      #
      zPerm <- function(observedData,trials,observed.zCA,fuzz=fuzzFactor,debugTF=FALSE){
        #	#if trials is greater than 1million, do in sets of 1million, and average the result
        #	#this is set up to do at least 1million; any fraction of 1million is rounded up
        #      #the keys to this working are:
        #	#	1.  rhyper to resample data in batches, rather than case-by-case
        #	#	2.  accounting for rounding errors in computations by the fuzzFactor
        #	#resampling is done under the null, so coding is fairly straightforward
        
        #use the Hahn-Meeker idea that if the largest of 59 independent estimates of the pvalue
        #is p<.05, then it is likely that the p-value really is p<.05, with 95% confidence
        setSize <- 1000
        millionSets <- 59
        groupSizes <- observedData$total
        totalHits <- sum(observedData$hits)
        samplesMatrix <- matrix(0,nrow=nrow(observedData),ncol=setSize)
        mean(sapply(1:millionSets,FUN=function(setNumber){
          samplesMatrix <- samplesMatrix*0.0
          if (verbose) cat(
            paste("\n\nSet",format(c(setNumber,millionSets))[1],"of",millionSets,
                  "  Set size=",format(setSize,big.mark=',',scientific=FALSE),"\n"))
          samplesMatrix[1,] <- rhyper(nn=setSize,m=totalHits,n=sum(groupSizes)-totalHits,k=groupSizes[1])
          if (debugTF) print(samplesMatrix[,1:10])
          if (nrow(observedData)>2){
            for (i in 2:(nrow(observedData)-1)){
              samplesMatrix[i,] <- rhyper(nn=setSize,
                                          #m is number of balls of interest (hits, MN cells,...)
                                          m=( totalHits - colSums(samplesMatrix[1:(i-1),,drop=FALSE]) ),
                                          #n is the rest of the balls (normal cells)
                                          n=sum(groupSizes[i:nrow(observedData)]) - 
                                            ( totalHits - colSums(samplesMatrix[1:(i-1),,drop=FALSE]) ),
                                          #k is number of balls to draw (total cells/group)
                                          k=groupSizes[i])
              if (debugTF) print(samplesMatrix[,1:10])
            }
          }
          samplesMatrix[nrow(observedData),] <- totalHits - colSums(samplesMatrix)
          if (debugTF) print(samplesMatrix[,1:10])
          #		permCounts <- samplesMatrix
          if (debugTF) print(apply(samplesMatrix,1,FUN=function(x)table(x)/setSize))
          zCA.trials <- zCA.calculator(samplesMatrix,groupSizes)
          #this table shows how some zCA sims can arbitrarily close to the observed value
          #and treating them as equal has been shown to closely approximate StatXact output
          if (verbose) print(table(ceiling(log10(abs(zCA.trials - observed.zCA)))))
          result <- mean( zCA.trials >= (observed.zCA-fuzz) )
          result
        }))
      }#zPerm close
      #
      #
      
      
      
      
      #  #technically, the zCA is calculated twice here, simply so the input conforms to the input used
      #  #for the permutation exercise.  It takes a trivial amount of time, and requires a single function
      #  #for all CA calculations, instead of separate ones.
      zCA <- zCA.calculator(cbind(hits,hits),total)[1]
      p.zCA <- 1 - pnorm(zCA)
      if(do.fisher){
        #exact takes too long...
        #zCA.exact <- zPerm(data.frame(hits=hits,total=total),trials=n.perm.trials,observed.zCA=zCA)
        fisherP <- rep(1,nGroups-1)
        for (i in 2:nGroups){
          fisherP[i-1] <- fisher.test(rbind(hits[c(1,i)],total[c(1,i)]-hits[c(1,i)]),alt='less')$p.value
        }
        chisqTestStats <- unlist(suppressWarnings(chisq.test(rbind(hits,total-hits))[c("statistic","parameter")]))
        GOFp <- structure(pchisq(chisqTestStats[1]-zCA^2,chisqTestStats[2]-1,lower.tail=FALSE),names=NULL)
        #    results <- c(CAp.1MM=zCA.exact,CAp=pnorm(zCA,lower=FALSE),trendGOF=GOFp,fisher=fisherP)
        #    names(results) <- c("CAp.exact","CAp.approx","trendGOF",names(results)[-(1:3)])
        results <- c(p.zCA,fisher=fisherP)
        names(results) <- c("trend p-value",names(results)[-1])
      }
      if (!do.fisher) results <- c(CAp=zCA)
      if (last.is.PC){
        PC.matrix <- cbind(c(hits[1],total[1]),c(PC.hits[1],PC.total[1]))
        dimnames(PC.matrix) <- list(c("hits","totals"),c("C","PC"))
        if(debugTF)print(t(PC.matrix))
        PC.p <- fisher.test(rbind(PC.matrix[1,],PC.matrix[2,]-PC.matrix[1,]),alt='less')$p.value
        results <- c(results,PC.FE = structure(PC.p,names=NULL))
      }
      if(debugTF)print(formattedP <- sapply(results,FUN=function(x)ifelse(x<1e-6,format.pval(x,eps=1e-6),format(round(x,6),scientific=FALSE))),quote=FALSE)
      #cat("\n")
      #cat("\n\n",formattedP,"\n",sep="\n")
      invisible(results)
    }
    
	#print( t( as.matrix( zCA(as.matrix(indata[,c("responses","sizes")])) ) ) )
    if(doTrend)return( t( as.matrix( zCA(as.matrix(indata[,c("responses","sizes")])) ) ) ) else return(NULL)

  }
  
  
  analysisReact <- reactive({
	input$updateRes
	
	isolate({
      indata <- dataOrg()[["indata"]]
	  ECx.targets <- input$ECx.targets/100
      confidenceCI <- input$confidenceCI/100
	  bgAdjust <- input$bgAdjust
	})
	#inputDataRes <- dataOrg()
	#indata <- inputDataRes[["indata"]]
	quietTF <- !debugTF
    #always do the trend analysis for general audience
    optimxMethods.ALL <- c("BFGS","Nelder-Mead","ucminf")
    #PLL calcs for 2-param LCx model are single-parameter optimizations.  Only ucminf is useful in this setting; others for 2 or more.
    optimxMethods.ucminf <- c("ucminf")
	
    confidenceAlpha <- 1-confidenceCI
	nPars <- 2 
	if(bgAdjust) nPars <- 3
	modelSTR <- switch(LETTERS[as.numeric(bgAdjust)+1],A="lcx",B="abbott")
	
	#model params must be a named vector with components "BG", "beta", "ldXX"
    #predictionModel returns predicted probabilities for given params values and logdoses
    if(modelSTR!="abbott")  predictionModel <- function(params,logdoses,ECx=0.5){
      1/(1+exp(-qlogis(ECx)-(params["beta"])*(logdoses-params["ldXX"])))
    }
    
	if(modelSTR=="abbott")  predictionModel <- function(params,logdoses,ECx=0.5){
      #DGT0 <- as.numeric(is.finite(logdoses))
      #parameterize background parameter as a logit s.t. no bounds are necessary on the bg rate!
      p0 <- exp(params["BG"])/(1. + exp(params["BG"]))
      phat <- p0 + (1-p0)/(1+exp(-qlogis(ECx)-(params["beta"])*(logdoses-params["ldXX"])))
      phat
    }

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
    
    minusLL.PLL <- function(params,ldXX,responses,sizes,logdoses,ECx=0.5){
      #input params must be named
      params <- c(params,ldXX=unname(ldXX))
      #probs <- predictionModel(params,logdoses,ECx)
      #-sum(responses*log(probs) + (sizes-responses)*log(1-probs))
      -sum(dbinom(x=responses,size=sizes,prob=predictionModel(params,logdoses,ECx),log=TRUE))
    }
    
    #nlm seems to unname args, which I need, so work around...
    minusLL.PLL.nlm <- function(params,ldXX,responses,sizes,logdoses,ECx=0.5){
      #input params must be named
      params <- c(beta=params,ldXX=unname(ldXX))
      #print(predictionModel(params,logdoses,ECx))
      #print(params)
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
    
    # #sink(file=outputFile,type="output")# if(doTrend) {
      # cat("\nTests for increasing response trend",
          # "\n\nCAp.exact should be significant.  If not, the use of this program is\nprobably inappropriate, and may result in errors.\n\n",
          # "pValues labeled 'fisher' compare rates in each higher concentration against\n",
          # "the lowest concentration tested.  Ideally, at least one of the 'fisher' results\nshould also be significant.\n\n",sep='')
      # zCA(as.matrix(indata[,c("responses","sizes")]))
    # }
    # if(!quietTF)cat(paste(rep("-",120),collapse=''),"\nEstimation Results\n",paste(rep("-",120),collapse=''),"\n",sep='')
    
    ###fit the more general model incorporating Abbott's correction
    
    ###confidence intervals under the abbott correction have a different interpretation.  The LC50 is NOT the
    ###dose that causes 50% mortality, it is the dose that causes 50% mortality ABOVE what would naturally occur
    ###anyway.  For example, at an extreme test subjects may experience 50% mortality under control conditions,
    ###s.t. the normal LC50 interpretation would be the control condition (is, no test article toxicity is needed).
    ###Under the correction, the LC50 is the dose that observes 75% mortality (halfway between 50%, in control, and
    ###100% for total toxicity.)
    
    ###should only have to fit this ONCE, then simple algebraic translation can be used to solve ANY ECx
    ###bounds here are mostly used for convenience.  Without them, the fitting algorithm might try
    ###some parameter values that will cause trouble.  Later, optim() will be used without constraints
    ###to get at hessian, which is not returned by nlminb()
    #nlObject <- nlminb(start=c(BG=0.01,beta=unname(coef(glmObject)[2]),ldXX=-unname(coef(glmObject)[1])/unname(coef(glmObject)[2])),
    #                   objective=minusLL,
    #                   responses=indata$responses,
    #                   sizes=indata$sizes,
    #                   logdoses=log10(indata$doses),
    #                   #            ECx=0.5,
    #                   lower=c(0.001,0.00001,min(logDoses)),upper=c(.2,100,max(logDoses))
    #                   )
    #print(nlObject)
    #MLE <- nlObject[["par"]]
    
    startVals <- c(BG=log(0.01/0.99),beta=abs(unname(coef(glmObject)[2])),ldXX=-unname(coef(glmObject)[1])/unname(coef(glmObject)[2]))
    lowerBounds <- c(0,-Inf,-Inf)
    if(modelSTR!="abbott"){
      startVals <- startVals[-1]
      lowerBounds <- lowerBounds[-1]
    }
    
    optimxObject<-optimx(par=startVals,
                         fn=minusLL,
                         method=optimxMethods.ALL,
                         responses=indata$responses,
                         sizes=indata$sizes,
                         logdoses=log10(indata$doses),
                         control=list(save.failures=FALSE),
                         hessian=TRUE)
    if(!quietTF)print(optimxObject)
    #stop()
    likVals <- data.frame(
      method=row.names(optimxObject),            
      fvalues=unlist(optimxObject$value))
    bestSol <- which(likVals$fvalues==min(likVals$fvalues))[1]
    
    
    if(FALSE){  
      detailMethod <- unlist(lapply(attr(optimxObject,"details"),FUN=function(x)x$method))
      #print(detailMethod)
      #print(
      bestSol <- attr(optimxObject,"details")[[match(likVals[bestSol,"method"],detailMethod)]]
    }
    
    MLE <- unlist(optimxObject[bestSol,1:nPars])
    #print(MLE)
    
    MLE.ECx <- MLE
    MLE.ECx["ldXX"]<-ecx.translation(ECx.targets,MLE)
    if(debugTF)print(ecx.translation(ECx.targets,MLE))
    if(debugTF)print(MLE.ECx)
    
    if(MLE["beta"]<0)stop("Best fit has mortality decreasing with increasing exposure")
    #print(MLE.ECx)
    MLE.LL <- minusLL(MLE,indata$responses,indata$sizes,log10(indata$doses))
    LL.values <- c(
      full=MLE.LL,
      reduced=LL.glmObject
      )

    ## Moved plotSetup outside of this reactive expression  
  
    #calculate and plot CIs for the requested ECx values
    #basic process is to fix ldXX parameter, fit the remaining parameters, and check if that solution is close to full fit
    #if it is close, it is in the interval.
    CIfun <- function(ECx.val,debugTF=!quietTF,optimxMethods=optimxMethods.ALL){
      ###wald-style CI(this CI will only work if the background param really is away from zero, otherwise, will get error)
      ###have to repeat optim() to get new SE estimates from the hessian matrix (SE is typically smallest for LC50, and largest far from 50)
      ###if the two types of CI are widely different, it should suggest problems with the data/model fit
      ###generalized inverse MASS::ginv() vs. inverse by solve() may give different answers.  Could check this as a diagnostic!
      covTRY <- try(covMatrix <- MASS::ginv(optim(MLE.ECx,
                                    minusLL,
                                    responses=indata$responses,
                                    sizes=indata$sizes,
                                    logdoses=log10(indata$doses),
                                    ECx=ECx.val,
                                    hessian=TRUE)$hessian))
      #covTRY <- try(covMatrix <- MASS::ginv(bestSol$nhatend))
      if(debugTF)print(covTRY)
      if(class(covTRY)!="try-error"){
        parNum <- 2
        if(modelSTR=="abbott") parNum <- 3
        CI.wald <- c(ECx.val,10^c(MLE.ECx["ldXX"],MLE.ECx["ldXX"]+qnorm(1-confidenceAlpha/2)*c(-1,+1)*sqrt(covMatrix[parNum,parNum])),SE=sqrt(covMatrix[parNum,parNum]))
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
        optimxObject<-optimx(par=startValues,
                             fn=minusLL.PLL,
                             method=optimxMethods,
                             responses=indata$responses,
                             sizes=indata$sizes,
                             logdoses=log10(indata$doses),
                             ldXX=ldxxValue,
                             ECx=ECx.val,
                             hessian=FALSE)
        likVals <- data.frame(
          method=row.names(optimxObject),
          #optimxObject$par,
          fvalues=optimxObject$value)
        if(debugTF){
          print("unirootFUN results")
          print(c(startValues,ldxxValue))
          print(optimxObject)
          print(likVals)
        }
        bestSol <- which(likVals$fvalues==min(likVals$fvalues))[1]
        #detailMethod <- unlist(lapply(attr(optimxObject,"details"),FUN=function(x)x$method))
        #bestSol <- attr(optimxObject,"details")[[match(likVals[bestSol,"method"],detailMethod)]]
        #print(c(ldxxValue,bestSol$value))
        #given an ldXX value, find best slope, then diff vs MLE.LL
        #optimize()
        assign("unirootSol",unlist(optimxObject[bestSol,1:(nPars-1)]),pos=1)
        abs(2*(optimxObject[bestSol,"value"]-MLE.LL))-qchisq(confidenceCI,1)
      }
      #first, bounding lower CI value (one that the lower CI is above)
      lowerCandidates <- rev(seq(min(logDoses[is.finite(logDoses)])-diff(range(logDoses[is.finite(logDoses)])),MLE.ECx["ldXX"],length=50))
      addedDoses <- as.vector(subset(data.frame(x=logDoses[is.finite(logDoses)]),x>=min(x) & x<MLE.ECx["ldXX"])[,1])
      #for doses above the LCx, add candidates in a tight window around each dose
      #use the smallest spacing between doses the base delta, then take fractions of it
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
      if(debugTF){print("Lower Candidates");print(lowerCandidates)}
      previousSolution <- MLE.ECx[-which(names(MLE.ECx)=="ldXX")]
      for(ldCandidate in seq(along=lowerCandidates)){
        if(ldCandidate>1)previousSolution <- lowerCandidatesPars[ldCandidate-1,]
        optimxObject<-optimx(par=previousSolution,
                             fn=minusLL.PLL,
                             method=optimxMethods,
                             responses=indata$responses,
                             sizes=indata$sizes,
                             logdoses=log10(indata$doses),
                             ldXX=lowerCandidates[ldCandidate],
                             ECx=ECx.val,
                             hessian=FALSE)
        if(debugTF)print(optimxObject)
        likVals <- data.frame(
          method=row.names(optimxObject),            
          fvalues=optimxObject$value)
        if(debugTF)print(likVals)
        bestSol <- which(likVals$fvalues==min(likVals$fvalues))[1]
        #detailMethod <- unlist(lapply(attr(optimxObject,"details"),FUN=function(x)x$method))
        #if(debugTF)print(detailMethod)
        #bestSol <- attr(optimxObject,"details")[[match(likVals[bestSol,"method"],detailMethod)]]
        if(debugTF)print(bestSol)
        lowerCandidatesPars[ldCandidate,] <- unlist(optimxObject[bestSol,1:(nPars-1)])
        if(2*abs(optimxObject[bestSol,"value"]-MLE.LL)>qchisq(confidenceCI,1)){
          lowerInterval <- sort(lowerCandidates[ldCandidate+c(0,-1)])
          #it is a deliberate choice to take previous solution here.  Once outside CI, param estimates
          #can get extreme, so start at what should be the less extreme choice
          lowerStart.slope <- previousSolution
          if(debugTF){print("Lower Interval Found");print(10^lowerInterval)}
          break}
      }
      
      if(!exists("lowerInterval",inherits=FALSE))lowerInterval=c(-999,-999)
      lowerLines <- as.data.frame(na.omit(cbind(lowerCandidates,lowerCandidatesPars)))
      lowerLines <- lowerLines[-nrow(lowerLines),]
      names(lowerLines)[1] <- "ldXX"
      if(debugTF)print(lowerLines)
      assign("lowerLines",lowerLines,pos=1)
      
      upperCandidates <- rev(seq(max(logDoses)+diff(range(logDoses)),MLE.ECx["ldXX"],length=50))
      addedDoses <- as.vector(subset(data.frame(x=logDoses[is.finite(logDoses)]),x>MLE.ECx["ldXX"] & x<=max(x))[,1])
      if(length(addedDoses)>0){
        addedDeltas <- diff(range(logDoses[is.finite(logDoses)]))*c(.01,.001,.0001)
        addedDeltas <- c(-addedDeltas,addedDeltas)
        addedDoses <- c(addedDoses,unlist(sapply(addedDoses,FUN=function(x)x+addedDeltas,simplify=FALSE)))
        upperCandidates <- unique(sort(c(upperCandidates,addedDoses)))
      }
      upperCandidatesPars <- matrix(NA,ncol=length(MLE.ECx)-1,nrow=length(upperCandidates),dimnames=list(NULL,names(MLE.ECx[-which(names(MLE.ECx)=="ldXX")])))
      previousSolution <- MLE.ECx[-which(names(MLE.ECx)=="ldXX")]
      for(ldCandidate in seq(along=upperCandidates)){
        if(ldCandidate>1)previousSolution <- upperCandidatesPars[ldCandidate-1,]
        optimxObject<-optimx(par=previousSolution,
                             fn=minusLL.PLL,
                             method=optimxMethods,
                             responses=indata$responses,
                             sizes=indata$sizes,
                             logdoses=log10(indata$doses),
                             ldXX=upperCandidates[ldCandidate],
                             ECx=ECx.val,
                             hessian=FALSE)
        if(debugTF)print(optimxObject)
        likVals <- data.frame(
          method=row.names(optimxObject),            
          fvalues=optimxObject$value)
        bestSol <- which(likVals$fvalues==min(likVals$fvalues))[1]
        if(FALSE){
          detailMethod <- unlist(lapply(attr(optimxObject,"details"),FUN=function(x)x$method))
          bestSol <- attr(optimxObject,"details")[[match(likVals[bestSol,"method"],detailMethod)]]
        }
        upperCandidatesPars[ldCandidate,] <- unlist(optimxObject[bestSol,1:(nPars-1)])
        if(2*abs(optimxObject[bestSol,"value"]-MLE.LL)>qchisq(confidenceCI,1)){
          upperInterval <- sort(upperCandidates[ldCandidate+c(0,-1)])
          upperStart.slope <- previousSolution
          #upperStart.slope <- bestSol$par
          break}
      }
      if(!exists("upperInterval",inherits=FALSE))upperInterval=c(999,999)
      if(debugTF)print(10^upperInterval)
      upperLines <- as.data.frame(na.omit(cbind(upperCandidates,upperCandidatesPars)))
      names(upperLines)[1] <- "ldXX"
      upperLines <- upperLines[-nrow(upperLines),]
      if(debugTF)print(upperLines)
      assign("upperLines",upperLines,pos=1)
      
      if(debugTF)print("Find lower CI endpoint")
      if(lowerInterval[1]!=c(-999)){
        lowerUniroot <- uniroot(unirootFUN,interval=lowerInterval,start=lowerStart.slope)$root
        lowerPars <- c(unirootSol,ldXX=lowerUniroot)
        assign("lowerPars",lowerPars,pos=1)
      }
      if(lowerInterval[1]==c(-999))lowerUniroot<-c(-999)
        
      if(debugTF)print("Find upper CI endpoint")
      if(debugTF)print(upperInterval)
      if(upperInterval[1]!=c(999)){
        upperUniroot <- uniroot(unirootFUN,interval=upperInterval,start=upperStart.slope)$root
        upperPars <- c(unirootSol,ldXX=upperUniroot)
        assign("upperPars",upperPars,pos=1)
      }
      if(upperInterval[1]==c(999))upperUniroot<-c(999)
      
      CI <- 10^c(MLE=MLE.ECx["ldXX"],lower=lowerUniroot,upper=upperUniroot)
    
      c(x=100*ECx.val,ECx=unname(CI[1]),PLL=CI[-1],STD=c(lower=CI.wald[3],upper=CI.wald[4]),CIconfidence=100*confidenceCI)
    }#end CIcalcs()

	
    if(modelSTR=="abbott")results <- CIfun(ECx.targets)
    if(modelSTR!="abbott")results <- CIfun(ECx.targets,optimxMethods = optimxMethods.ucminf)
  
    if(!quietTF)print(MLE.ECx)
    if(!quietTF)print(results)

    list(numRes=t(as.matrix(c(results,MLE.ECx))),
	  plotObs=list(logDoses=logDoses,indata=indata,newX=newX,predictionModel=predictionModel,
	    MLE.ECx=MLE.ECx,ECx.targets=ECx.targets,results=results,lowerPars=lowerPars,upperPars=upperPars,
		lowerLines=lowerLines,upperLines=upperLines,modelSTR=modelSTR,confidenceCI=confidenceCI),
	  resShName=isolate( input$shName )
    )
  })
  
  ## masterClean is just so I can override the clients current plot options if they choose to download a pdf or Excel file.
  plotFun <- function(changeCexTF=FALSE,browserTF=TRUE,masterClean=FALSE){
    
    plotObs <- analysisReact()[["plotObs"]]
   
    bndCurves <- input$bndCurves
    annotate <- input$annotate
    xlabSTR <- input$xlab
    ylabSTR <- input$ylab
    showSTD <- input$showSTD
	
	if(!browserTF){
	  bndCurves <- TRUE
      annotate <- TRUE
      showSTD <- TRUE	  
	  if(masterClean){
	    bndCurves <- FALSE
        annotate <- FALSE
        showSTD <- FALSE	  
	  }
	}
    
	isolate({
      ## Assign the objects from analysisReact that are needed for plotting to this environment
	  assignFun <- function(i){
	    assign(names(plotObs)[i],plotObs[[i]],
	      envir=environment(match.fun(assignFun)) )
        }
        sapply(1:length(plotObs), assignFun)
	    ECxLevel <- rep(predictionModel(MLE.ECx,MLE.ECx["ldXX"],ECx=ECx.targets),2)    
	})
	
	
	plotSetup <- function(){
	
      plot(x=10^logDoses,y=indata$responses/indata$sizes,log='x',ylim=c(0,1),xlim=10^range(c(logDoses,newX)),
           xlab=xlabSTR,ylab=ylabSTR,type='n',axes=FALSE)
      axis(side=1,at=unique(indata$doses[indata$doses>0]),label=sapply(unique(indata$doses[indata$doses>0]),format))
      if(any(indata$doses==0)){
        points(x=10^(logDoses[1]),y=indata$responses[1]/indata$sizes[1],pch=16)
        axis(side=1,at=10^min(logDoses),label="Control",las=2)
        axis.break(breakpos=10^(mean(logDoses[1:2])-.03*diff(par("usr")[1:2])))
      }
      newY <- predictionModel(params=MLE.ECx,logdoses=newX,ECx=ECx.targets)
      points(x=10^logDoses[indata$doses>0],y=indata$responses[indata$doses>0]/indata$sizes[indata$doses>0])
      axis(side=2)
      box(lwd=2) 
      
      lines(x=10^newX,y=newY,lwd=3)
      lines(x=10^newX,y=newY,lwd=2,lty=2,col='gray')
      
      if(any(indata$doses==0)){
        lines(x=10^c(par("usr")[1],mean(logDoses[1:2])-.06*diff(par("usr")[1:2])),y=rep(MLE.ECx["BG"],2),lwd=3)
        lines(x=10^c(par("usr")[1],mean(logDoses[1:2])-.06*diff(par("usr")[1:2])),y=rep(MLE.ECx["BG"],2),lwd=2,lty=2,col="gray")
        lines(x=10^c(mean(logDoses[1:2])-.06*diff(par("usr")[1:2]),min(newX)),
              y=predictionModel(params=MLE.ECx,logdoses=c(-Inf,newX[1]),ECx=0.5),col='gray',lwd=2,lty="dotted")
        
      }
      CIlineVals <- results[c("PLL.lower","PLL.upper")]
      if(CIlineVals[1]<10^par("usr")[1])CIlineVals[1]<-10^par("usr")[1]
      if(CIlineVals[2]>10^par("usr")[2])CIlineVals[2]<-10^par("usr")[2]
      
      lines(x=CIlineVals,y=ECxLevel,lwd=3)
      #if(showSTD)lines(x=results[c("STD.lower","STD.upper")],y=ECxLevel,lwd=2,col="cyan")
      lines(x=CIlineVals,y=rep(par("usr")[3],2),lwd=4)
      rug(results["ECx"],side=1)
      if(any(indata$doses==0)){
        rug(side=2,x=predictionModel(params=MLE.ECx,logdoses=log(0),ECx=ECx.targets),lwd=3)
        rug(side=2,x=predictionModel(params=MLE.ECx,logdoses=log(0),ECx=ECx.targets),lwd=2,lty=2,col="gray")
      }
    }
    
	cexAbForm <- 0.6
	cexResTab <- .75
	cexData <- 0.7
	cexDate <- 0.5
	if(changeCexTF){
	  cexAbForm <- .75
	  cexResTab <- .9
	  cexData <- 0.9	
	  cexDate <- 0.8
	}
    ## Function call and plot add-ons
    plotSetup()
	if(bndCurves){
	  lines(x=10^newX,y=predictionModel(params=lowerPars,logdoses=newX,ECx=ECx.targets),col="gray")
      apply(lowerLines,1,FUN=function(lowerPARS)lines(x=10^newX,y=predictionModel(params=lowerPARS,logdoses=newX,ECx=ECx.targets),col="gray"))
      #somehow when 2-param model, the beta comes back unnamed, so need to work around
      if(modelSTR!="abbott")lines(x=10^newX,y=predictionModel(params=c(beta=lowerPars[1],lowerPars[-1]),logdoses=newX,ECx=ECx.targets),col="red")
      if(modelSTR=="abbott")lines(x=10^newX,y=predictionModel(params=lowerPars,logdoses=newX,ECx=ECx.targets),col="red")
      abline(v=10^lowerPars["ldXX"],col="gray",lty=2)
      lines(x=10^newX,y=predictionModel(params=upperPars,logdoses=newX,ECx=ECx.targets),col="gray")
      apply(upperLines,1,FUN=function(lowerPARS)lines(x=10^newX,y=predictionModel(params=lowerPARS,logdoses=newX,ECx=ECx.targets),col="gray"))
      if(modelSTR!="abbott")lines(x=10^newX,y=predictionModel(params=c(beta=upperPars[1],upperPars[-1]),logdoses=newX,ECx=ECx.targets),col="red")
      if(modelSTR=="abbott")lines(x=10^newX,y=predictionModel(params=upperPars,logdoses=newX,ECx=ECx.targets),col="red")
      abline(v=10^upperPars["ldXX"],col="gray",lty=2)
	}
	if(annotate){
	  #!put formula on lower-right region of figure, depending on type
      #if(modelSTR=="abbott")mtext(line=-2,side=1,adj=1,cex=.6,
      if(modelSTR=="abbott")mtext(line=-2,side=1,adj=.95,cex=cexAbForm,
            text=expression(plain(P)==bgroup("(",
                                             atop(
                                               paste(C,phantom(X(1-C)*over(1,textstyle(1+e)^textstyle({-beta*group("[",log[10](d)-LC[50],"]")}))),phantom(XX),dose==0),
                                               paste(C+(1-C)*over(textstyle(1),textstyle(1+e)^textstyle({-beta*group("[",log[10](d)-LC[50],"]")})),phantom(XX),dose>0)),
                                             "")))
      #if(modelSTR!="abbott")mtext(line=-2,side=1,adj=1,
      if(modelSTR!="abbott")mtext(line=-2,side=1,adj=.95,
            text=expression(plain(P)==
              paste(over(textstyle(1),textstyle(1+e)^textstyle({-beta*group("[",log[10](d)-LC[50],"]")})))))
      
      #lines(x=10^newX,y=predict(glmObject,new=data.frame(logDoses=newX),type="response"),col='gray')
      #lines(x=10^newX,y=predictionModel(params=nlObject[["par"]],logdoses=newX,ECx=0.5),lwd=2)
      stampSTR <- date()
      #if(is.character(inputFile))stampSTR <- paste(stampSTR,inputFile,sep="\n")
      mtext(side=1,line=-1,outer=TRUE,text=stampSTR,cex=cexDate,adj=0.98)
      
      par(new=TRUE)
      plot(1,1,type='n',axes=FALSE,xlab='',ylab='')
      if(browserTF)addtable2plot("topleft",table=indata,xjust=0,yjust=0,cex=cexData)
      addtable2plot(x=par("usr")[2],y=par("usr")[4],
                    table=data.frame(
                      X=100*ECx.targets,
                      ECx=signif(results["ECx"],digits=3),
                      lowerCI=signif(results["PLL.lower"],digits=3),
                      upperCI=signif(results["PLL.upper"],digits=3),
                      confLevel=100*confidenceCI),
                    xjust=1,yjust=1,cex=cexResTab)
	}
	
  }
  
  observe({
    if(input$updateRes!=0)updateTabsetPanel(session,"tabManager",selected="resultsTab")
  })
  

  #### Output 
   output$exampDat <- renderTable({ exampleDat }, include.rownames=FALSE)
  
  ## Data for Analysis tab
  output$dataReadText <- renderText({
    if(is.null(dataOrg())){ NULL
	} else { "Data Read in for Analysis ( and aggregated where appropriate )" }
  })
  output$badDataFlag <- renderText( inputDataFile()[["badDataFlag"]] )
  output$DataTab <- renderTable( dataOrg()[["indata"]] )
  output$dose0Flag <- renderText( inputDataFile()[["dose0Flag"]] )
  
  ## Results tab
  output$noGoodData <- renderText({
    inputDataRes <- inputDataFile()
	if( is.null(inputDataRes) ){
	  return("No data has been uploaded.  Choose an appropriate data file using Choose File button in grey panel")
	} else if( !is.null(inputDataRes[["badDataFlag"]]) ){
	  return("Data uploaded was not in the correct format.  See Data For Analyis Tab.")
	} else NULL
  })
  
  output$zCAWarn <- renderText({
    if((performZCA()[,1]>0.05)){ "NO SIGNIFICANT RESPONSE TREND"
	} else NULL
  })
  
  
  output$zCATrendNote <- renderText({
    if(doTrend){ "Tests..."
	} else NULL
  })
  
  
  output$zCARes <- renderTable({
    inputDataRes <- inputDataFile()  
    if( is.null(inputDataRes) | !is.null(inputDataRes[["badDataFlag"]]) ){ NULL 
	} else performZCA()
  }, include.rownames=FALSE, digits=4)
  output$resTab <- renderTable({
    inputDataRes <- inputDataFile()  
    if( is.null(inputDataRes) | !is.null(inputDataRes[["badDataFlag"]]) ){ NULL
	} else {
	  withProgress(session,{  
	    if(performZCA()[,1]>0.05)setProgress(message="Calculating, please wait",detail="NO SIGNIFICANT TREND")
	    if(!(performZCA()[,1]>0.05))setProgress(message="Calculating, please wait")
		res <- analysisReact()[["numRes"]]
		#print(res[1,c("x","ECx","PLL.lower","PLL.upper","CIconfidence")])
		#print(res[c("x","ECx","PLL.lower","PLL.upper","CIconfidence")])
		signif(res[1,c("x","ECx","PLL.lower","PLL.upper","CIconfidence","beta"), drop=FALSE],5)
	  })
	}
  }, include.rownames=FALSE, digits=4)
  output$plotRes <- renderPlot({
    inputDataRes <- inputDataFile()  
    if( is.null(inputDataRes) | !is.null(inputDataRes[["badDataFlag"]]) ){ NULL
    } else plotFun() 
  })
  # Download Results
    output$downloadResults <- downloadHandler(
    filename= "output.xlsx",
    content = function(file) {
	  numShName <- "Numerical Results"
	  plotShName <- "Plot"
      ##Write out the data to an Excel file
      # If an xls file was provided, write the results to that xlsx file, since writing two images. see ?addimage for explanation
	  #if(grepl("xls",getFileExt())){
	  if(grepl("xlsx",getFileExt())){
	    ### NOTE the gsub, this is necessary to create the named regions later.
		##   Well, it's just the plotShName that can't have spaces since only there are named regions created, however,
		##     it's probably less confusing to client to have the same name format for all sheets.
	    numShName <- make.names( paste0(gsub(" ","_",analysisReact()[["resShName"]]),"_numRes") )
	    plotShName <- make.names( paste0(gsub(" ","_",analysisReact()[["resShName"]]),"_plot") )
	    wb <- loadWorkbook(input$inputFile[["datapath"]])
	    createSheet(wb,c(numShName,plotShName))
	  } else {
	    wb <- loadWorkbook("output template.xlsx")
	    renameSheet(wb,1,numShName)
		createSheet(wb,plotShName)
		createSheet(wb,"Original Data")
		wb$writeWorksheet(dataOrg()[["indata"]], sheet="Original Data", startRow=1, startCol=1, header=TRUE)
	  }
	  print(getSheets(wb))
	  res <- analysisReact()[["numRes"]]
	  res <- res[1,c("x","ECx","PLL.lower","PLL.upper","CIconfidence","beta","ldXX"),drop=FALSE]
	  resPretty <- signif( res, 5 )
      wb$writeWorksheet(resPretty, sheet=numShName, startRow=1, startCol=1, header=TRUE)
      wb$writeWorksheet(date(), sheet=numShName, startRow=3, startCol=length(resPretty)+2, header=FALSE)
	  randomNumber <- trunc(runif(1, 10000000, 99999999))
	  # Although these are the defaults, sometimes the wb has strange cell heights and widths.
	  setColumnWidth(wb,sheet=numShName,1:10,-1)
	  setRowHeight(wb,sheet=numShName,1:2,-1)
	  ## Plots
	  cleanPlotFilename <- "plots/cleanResPlot.png"
	  png(cleanPlotFilename,height=600,width=800)
	  print(plotFun(browserTF=FALSE,masterClean=TRUE,changeCexTF=TRUE))
	  dev.off()
      namesRegions <- c("cleanReg","dirtyReg")
	  createName(wb,name=namesRegions[1],formula=paste0(plotShName,"!$A$1"),overwrite=TRUE)
	  addImage(wb,filename=cleanPlotFilename,name=namesRegions[1],originalSize=TRUE)
	  dirtyPlotFilename <- "plots/dirtyResPlot.png"
	  png(dirtyPlotFilename,height=600,width=800)
	  print(plotFun(browserTF=FALSE,masterClean=FALSE,changeCexTF=TRUE))
	  dev.off()
	  ## I think/hope specifying $N$1 should always work since the size of the png image is fixed.
	  createName(wb,name=namesRegions[2],formula=paste0(plotShName,"!$N$1"),overwrite=TRUE)
	  addImage(wb,filename=dirtyPlotFilename,name=namesRegions[2],originalSize=TRUE)
	  outPath <- paste0("plots/output", randomNumber, ".xlsx")
      saveWorkbook(wb, outPath)
	  file.copy(outPath, file)
	  unlink(c(outPath,cleanPlotFilename,dirtyPlotFilename))
    }
  )

  
  output$downloadPlot <- downloadHandler( 
   filename="output.pdf",
   content=function(file){
     pdf(file)
     print(plotFun(browserTF=FALSE,masterClean=TRUE))
     print(plotFun(browserTF=FALSE,masterClean=FALSE))
     plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="")
	 addtable2plot(.5,1,dataOrg()[["indata"]],xjust=.5,yjust=0)
     dev.off()
   }   
  )

})
