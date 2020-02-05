
### Changes to folder5 : Put a help tab on the user interface.
### Folder 5 allows for an input of .txt, .csv, .xls, or .xlsx.
### Changes to folder 6 : Output plot into excel file.  If excel file 
##   read in, add the results to that workbook, else output with original data
#    into template.

### Changes to folder 7 Setup a progress bar.

### Changes to folder 8 make it non reactive.

### Changes to folder 9 
# For the pdf, instead of superimposing the data on the figure, do a separate plot (no axes, 
# type=“n”, etc, to create a clean page) and then addtable2plot() to put the data on the page.  
# Again, add both the annotated, and clean versions, to the pdf.  Can that be done in the xls file 
# too?
## From ?addImage. May have problems with putting multiple images into an Excel wb.
# There is an known issue in Apache POI with adding images to xls workbooks. The result of adding 
# images to workbooks that already contain shapes or images may be that previous images are removed 
# or that existing images are replaced with newly added ones. It is therefore advised that you use 
# the addImage functionality only with workbooks that have no existing shapes or images. Note that 
# this only holds for xls workbooks (Excel 97-2003) and not for xlsx (Excel 2007+). 
# There should be no issues with xlsx workbooks.
## Make the data read in the same as BV PLL tool.



#### Because of possibility of excel files with multiple sheets of data that needs to be analyzed, when
###   creating a sheet with the numRes and plots, paste the name of the sheet to beginning of new sheet name.
###  Notes :
###  I have to gsub(" ","",sheetNames) since I create named regions and IDK of a way to create a named region
###    when the sheet name has a space.
###  Had to do adjust analysisReact since I need to keep track of what the sheet was active when the user ran the 
###   analysis NOT when user downloads results.

#### Calculate zCA everytime,
##    If no significant trend, alert client using a pop-up dialogue box.

library(shiny)
#library(shinyIncubator)
#library(XLConnect)
library(openxlsx)
library(optimx)
library(plotrix)

source("LAshinyFuns.R")
source("SKorig.R")

reactiveVars <- reactiveValues()
reactiveVars$zeros <- FALSE
reactiveVars$annotatedplot <- NULL
reactiveVars$cleanplot <- NULL
reactiveVars$stamp <- ""
reactiveVars$amsg <- ""

## If debugTF spit everything out to console.
debugTF <- TRUE

## ALWAYS doTrend 
doTrend <- TRUE

exampleDat <- matrix(c(1,0,20,2,0,20,4,10,20,8,20,20,16,20,20),byrow=TRUE,nrow=5)
colnames(exampleDat) <- c("doses","responses","sizes")

shinyServer(function(input,output,session) {

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
  
  
  
  performZCA <- function(){
    input$updateRes
    
    isolate({
      inputDataRes <- dataOrg()
    })
    
    indata <- inputDataRes[["indata"]]
    
    hits <- indata[,"responses"]
    total <- indata[,"sizes"]
    nGroups <- nrow(indata)
    
    
    ###ZCA arg totalsColumn MUST be TRUE
    zCA <- function(
      indata=NULL,
      fuzzFactor=1e-13,
      do.fisher=TRUE,
      totalsColumn=TRUE,
      n.perm.trials=10000000,
      verbose=FALSE,
      last.is.PC=FALSE){
      #	#indata is a two-column matrix of counts. one column is the total number/group, the other is the
      #	#number of events of interest.  By default, the table supplied is numbers of "hits" and "totals".
      #	#and the smaller column total must be the event counts.  If the table is number of "hits"
      #	#and number of "not hits", use totalsColumn=FALSE and put counts of the event of interest in the
      #	#FIRST column
      #
      #	#this analysis assumes that the row order of the input matrix makes sense, and that you are interested
      #	#in an INCREASE in the rate of events as row number increases, for BOTH the CA test, and the Fisher Exact
      #	#tests.
      
      
      #	#this zCA calculator calculates CA z for a MATRIX of data (one column/case)
      zCA.calculator <- function(permdata,groupSizes){
        ###based on Agresti (1990), pp 100-101 (Categorical Data Analysis)
        nGroups <- nrow(permdata)
        #weights are scores.  we use integer scores for equal spacing
        scores <- 1:nrow(permdata)
        xBar <- mean(rep(scores,times=groupSizes))
        pAvg <- colSums(permdata)/sum(groupSizes)
        #convert permdata to props/Group
        #for (i in 1:nrow(permdata))permdata[i,] <- permdata[i,]/groupSizes[i] 
        denominator <- sum(groupSizes*((scores-xBar)^2))
        part1 <- groupSizes*(scores-xBar)
        part2 <- scores-xBar
        b <- (as.vector(part2%*%permdata)-sum(part1)*pAvg)/denominator
        #b <- colSums(part1*permdata-part1*pAvg))/denominator
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
  
  getFileExt <- reactive({
	# Read in the .csv input data
    inFile <- input$inputFile
    if(!is.null(inFile)){
	  fileExt <- sub(".*\\.","",inFile$name)
	  return(fileExt)
    } else return(NULL)
  })
  
  ### If it is xls or xlsx, select the sheet to use
  ### By default, the first (or only if the case) sheet is selected
  output$sheetUI <- renderUI({
    fileExt <- getFileExt()
    #if(debugTF)print(fileExt)
    #print(fileExt)
    inFile <- input$inputFile
    #print(inFile)
    #print(grepl("xls",fileExt))
    #print(!is.null(fileExt) && grepl("xls",fileExt))
    ### XLCONNECT here
    if( !is.null(fileExt) && grepl("xls",fileExt) ){
      ### wb <- loadWorkbook(inFile$datapath)
      ### shNames <- getSheets(wb)
      #print(str(inFile))
      wb <- loadWorkbook(file = inFile$datapath)
      #print(str(wb))
      shNames <- wb$sheet_names
      shNames <- shNames[sapply(shNames,FUN=function(SN){
        testSheet <- readWorkbook(wb, sheet=SN, colNames=TRUE)
        numericCols <- sum(unlist(lapply(testSheet,is.numeric)))
        (numericCols>2)
      })]
      #print(shNames)
      selectInput('shName', 'Select Sheet Containing Data', shNames)
    } else NULL
  })
  
  output$doseColUI <- renderUI({
    shiny::req(inputDataFile())
    namesInp <- names(inputDataFile())
    if (all(c("doses", "responses", "sizes") %in% namesInp)) {
      NULL
    } else {
      namesInFrame <- names(inputDataFile())
      return(
        selectInput(
          inputId = "nameDoseCol",
          "Select Concentration Variable",
          namesInFrame,
          namesInFrame[1]
        )
      )
      #if(debugTF)print(input$nameYCol)
    }
  })
  
  output$respColUI <- renderUI({
    shiny::req(inputDataFile())
    namesInp <- names(inputDataFile())
    if (all(c("doses", "responses", "sizes") %in% namesInp)) {
      NULL
    } else {
      namesInFrame <- names(inputDataFile())
      return(
        selectInput(
          inputId = "nameRespCol",
          "Select Response Variable",
          namesInFrame,
          namesInFrame[2]
        )
      )
      #if(debugTF)print(input$nameDoseCol)
    }
  })
  
  output$sizeColUI <- renderUI({
    shiny::req(inputDataFile())
    namesInp <- names(inputDataFile())
    if (all(c("doses", "responses", "sizes") %in% namesInp)) {
      NULL
    } else {
      namesInFrame <- names(inputDataFile())
      return(
        selectInput(
          inputId = "nameSizeCol",
          "Select Size Variable",
          namesInFrame,
          namesInFrame[3]
        )
      )
      #if(debugTF)print(input$nameDoseCol)
    }
  })
  
  inputDataFile <- reactive({
    shiny::req(input$inputFile)
    inFile <- input$inputFile
    print(inFile)
    if(!is.null(inFile)){
      #reactiveVars$zeros <- FALSE
      fileExt <- sub(".*\\.","",inFile[1])
      
      ## Use grepl because readWorksheet works with both xls and xlsx versions of wb.
      if(grepl("xls",fileExt)){
        ### XLCONNECT here
        wb <- loadWorkbook(inFile$datapath)# same in XLconnect as openxlsx
        shName <- input$shName
        if(is.null(shName)){
          indata <- NULL
        } else {
          ### indata <- readWorksheet(wb, sheet=shName, header=TRUE)### XLCONNECT here
          indata <- readWorkbook(wb, sheet=shName, colNames=TRUE)### XLCONNECT here
        }
      }
      if(fileExt=="txt"){
        indata <- read.table(inFile$datapath,header=TRUE)
      }
      if(fileExt=="csv"){
        indata <- read.csv(inFile$datapath,header=TRUE)
      }
      print(indata)
      return(indata)
    }
    
  })
  
  ## Observe is like a reactive expression. I think it is appropriate to change tabs to the data tab each time the file 
  ##  extension is changed.  Better to switch any time any of the data selection options change
  observe({
    ## Throw dataOrg() in so that anytime a selection is made that has the potential to change the data, redirect the client to the 
	##   data tab.  This is causing shiny to crash IDK why.  I have tried inputDataFile() as well but to no avail.
	if(!is.null(getFileExt()))updateTabsetPanel(session,"tabManager",selected="dataTab")
	
  })
  
  ### (I think) whenever the data change, update the data tab and clear out results tab
  inputChanges <- reactive({list(inputDataFile(),dataOrg(),input$ECx.targets,input$confidenceCI,input$modelType)})
  observeEvent(inputChanges(), {
    if(input$modelType=="abbott")predictionModel <<- function(params,logdoses,ECx=0.5){
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
    if(input$modelType=="lcx")predictionModel <<- function(params,logdoses,ECx=0.5){
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
    
    output$plot <- NULL
    output$resultsTable <- NULL
    output$messages <- renderUI({
      p("Do not download files here unless data table/plot are displayed.
                       Otherwise these files may be for a previously run analysis.")})
    updateTabsetPanel(session,"tabs", "Data For Analysis")
  })
  
  
  
  
  dataOrg <- reactive({
    shiny::req(inputDataFile())
    print("dataOrg")
    namesInp <- names(inputDataFile())
    print(namesInp)
    if (all(c("doses", "responses", "sizes") %in% namesInp)) {
      indata <-
        inputDataFile()[, c("doses", "responses", "sizes")]
    }
    if (!all(c("doses", "responses", "sizes") %in% namesInp)) {
      dataDirty <- inputDataFile()
      ## Have to make sure the UIs are generated before I subset.
      #if (is.null(input$nameSizeCol))
      #  return(NULL)
      indata <-
        dataDirty[, c(input$nameDoseCol, input$nameRespCol, input$nameSizeCol)]
      names(indata) <- c("doses", "responses", "sizes")
    }
    print(indata)
    dose0Flag <- NULL
    #assure data is in dose order
    indata <- indata[order(indata$doses), ]
    #aggregate rows that are from same doses
    indata <- data.frame(doses = indata$doses,
                         as.data.frame(sapply(
                           indata[, c("responses", "sizes")],
                           FUN = function(x, index) {
                             unname(tapply(x, INDEX = indata$doses, FUN = sum))
                           }
                         )))
    if (any(indata$doses <= 0) & input$modelType=="lcx") {
      indata <- subset(indata, doses > 0)
      dose0Flag <-
        "*At least one dose value was 0 or less and was removed from the analysis"
    }
    return(list(indata = indata, dose0Flag = dose0Flag))
    #} else return(list(indata=indata,dose0Flag=dose0Flag,badDataFlag=badDataFlag))
  })
  ### End of data read in (did do a few minor calcs)
  
  

  
  
  
  
  
  
  
  
  
  
  

    observeEvent(input$updateRes, {
      updateTabsetPanel(session, "tabs", "Results")
      reactiveVars$stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      
      withProgress({
        setProgress(message = "Please Wait")
        
        isolate({
          #write(as.character(Sys.time()), file="counter.txt", append=TRUE, sep="\n")
          
          indata <- dataOrg()[["indata"]]
          ECx.targets <- input$ECx.targets / 100
          confidenceCI <- input$confidenceCI / 100
          bgAdjust <- input$modelType=="abbott"
        })
        
        if(FALSE)list(
          numRes = t(as.matrix(c(results, MLE.ECx))),
          plotObs = list(
            logDoses = logDoses,
            indata = indata,
            newX = newX,
            predictionModel = predictionModel,
            MLE.ECx = MLE.ECx,
            ECx.targets = ECx.targets,
            results = results,
            lowerPars = lowerPars,
            upperPars = upperPars,
            lowerLines = lowerLines,
            upperLines = upperLines,
            modelSTR = modelSTR,
            confidenceCI = confidenceCI
          ),
          resShName = isolate(input$shName)
        )
        results <- logisticLCx(
          inputData = dataOrg()[["indata"]],
          ECxPercent = input$ECx.targets,
          confLevelPct = input$confidenceCI,
          modelType = input$modelType)
        if(length(FIEhits <- which(regexpr("FIE",names(results))>0))>0){
          CIbounds <- results[FIEhits]
        }
        #If PLL interval is present, it was done for a reason -- so use it
        if(length(PLLhits <- which(regexpr("PLL",names(results))>0))>0){
          CIbounds <- results[PLLhits]
        }
        modelSTR <- toupper(input$modelType)
        if(modelSTR=="LCX")modelSTR <- "STD"
        resultsDF <- data.frame(method="Logistic",modelType=modelSTR,as.data.frame(rbind(c(results[1:2],CIbounds,NA,input$confidenceCI))))
        names(resultsDF) <- c("Method","ModelType","p","ECp","LowerCL","UpperCL","Trim","ConfLevel")
        output$resultsTableSK <- NULL
        resultsDF.SK <- NULL
        if(input$ECx.targets==50){
          resultsSK <- spearmanKarberORIG(subset(dataOrg()[["indata"]],doses>0),doPlot = FALSE)
          resultsDF.SK <- data.frame(method="TSK",modelType="Nonparametric",as.data.frame(rbind(c(50,resultsSK[1:4],95))))
          names(resultsDF.SK) <- c("Method","ModelType","p","ECp","LowerCL","UpperCL","Trim","ConfLevel")
          #output$resultsTableSK <- shiny::renderTable(expr={resultsDF.SK},bordered = TRUE,na = "N/A")
        }
        finalTable <- rbind(resultsDF,resultsDF.SK)
        output$resultsTable <- shiny::renderTable(expr={finalTable},bordered = TRUE,na = "N/A")
        if(input$modelType=="lcx")MLE.Parms <- tail(results,2)
        if(input$modelType=="abbott")MLE.Parms <- tail(results,3)
        #print(MLE.Parms)
        #print(predictionModel)
        output$plot <- shiny::renderPlot(expr = {
          plotSetup.noCI(inputDF=dataOrg()[["indata"]],
                         modelType=input$modelType,
                         ECx.target=input$ECx.targets/100,
                         MLE.ECx=MLE.Parms,
                         genericDoses = !input$doseTicks,
                         xlabSTR=input$xlab,ylabSTR=input$ylab)
          modelRange <- 1
          BGrate <- 0
          if(input$modelType=="abbott"){
            BGparm <- MLE.Parms[which(regexpr("BG",names(MLE.Parms))>0)]
            BGrate <- exp(BGparm)/(1+exp(BGparm))
            modelRange <- 1-BGrate
          }
          #print(results)
          #print(c(ECx.targets=ECx.targets,ECx.targets=input$ECx.targets/100,BGparm=BGparm,BGrate=BGrate,modelRange=modelRange))
          if(length(FIEhits <- which(regexpr("FIE",names(results))>0))>0){
            lines(x=results[FIEhits],y=BGrate + modelRange*c(1,1)*input$ECx.targets/100,lwd=5,col="magenta")
          }
          if(length(PLLhits <- which(regexpr("PLL",names(results))>0))>0){
            lines(x=results[PLLhits],y=BGrate + modelRange*c(1,1)*input$ECx.targets/100,lwd=5,col="magenta")
          }
        })
      })
    })
      
  
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
  output$DataTab <- renderTable( inputDataFile() )
  output$dose0Flag <- renderText( inputDataFile()[["dose0Flag"]] )
  output$baseplot <- renderPlot(expr={
    plotSetup.noCI(dataOrg()[["indata"]],modelType = "base",genericDoses = !input$doseTicks)
    gpavaData <- subset(dataOrg()[["indata"]])
    logDoses <- log10(gpavaData$doses)
    stepFactor <- median(diff(sort(unique(logDoses))))
    logDoses[!is.finite(logDoses)] <- min(logDoses[is.finite(logDoses)]) - stepFactor
    gpavaData$logDoses <- logDoses
    gpavaData$doses <- 10^gpavaData$logDoses
    SKlineList <- SKgpava(gpavaData)
    
    print("SKline")
    print(SKlineList)
    #with(SKline,lines(x=doses,y=adjP,col="blue",lwd=3))
    #monoSpline <- with(SKline,splinefun(x=log(doses),y=adjP+0.0001*seq(0,1,length=length(doses)),method="hyman"))
    monoSpline <- with(SKlineList[["SKdata"]],splinefun(x=log(doses),y=adjP+0.000*seq(0,1,length=length(doses)),method="monoH.FC"))
    logXvals <- with(SKlineList[["SKdata"]],seq(min(log(doses)),max(log(doses)),length=1000))
    lines(x=exp(logXvals),y=monoSpline(logXvals),col="magenta",lwd=3)
    if(input$modelType=="lcx"){
      abline(h=input$ECx.targets/100,lty=2,col="magenta")
      mtext(side=4,at=(input$ECx.targets/100) - 0.03*diff(par("usr")[3:4]),
            text = paste("EC",format(input$ECx.targets)," Level",sep=""),las=1,adj=1,cex=1.5,col="blue")
    }
    if(input$modelType=="abbott"){
      probLevel <- monoSpline(logXvals)[1] + (1-monoSpline(logXvals)[1])*input$ECx.targets/100
      abline(h=probLevel,lty=2,col="magenta")
      mtext(side=4,at=probLevel - 0.03*diff(par("usr")[3:4]),
            text = paste("EC",format(input$ECx.targets)," Level",sep=""),las=1,adj=1,cex=1.5,col="blue")
    }
    
    
    #monoSpline <- with(SKlineList[["SKtrim"]],splinefun(x=scaleDoses,y=adjP+0.000*seq(0,1,length=length(scaleDoses)),method="monoH.FC"))
    #logXvals <- with(SKlineList[["SKtrim"]],seq(min(scaleDoses),max(scaleDoses),length=1000))
    #lines(x=exp(logXvals),y=monoSpline(logXvals),col="red",lwd=3)
  })
  
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
    if(doTrend){ "Tests for increasing response trend.
        CAp.exact should be significant.  If not, the use of this program is probably inappropriate, and may result in errors.
        pValues labeled 'fisher' compare rates in each higher concentration against
        the lowest concentration tested.  Ideally, at least one of the 'fisher' results should also be significant."
	} else NULL
  })
  output$zCARes <- renderTable({
  	if(input$updateRes==0){
  		return(NULL)
  	}
  	input$updateRes
  	isolate({
  		inputDataRes <- inputDataFile()  
  		if( is.null(inputDataRes) | !is.null(inputDataRes[["badDataFlag"]]) ){ NULL 
  		} else {
  			trend <- performZCA()
  			trend[1,] <- format.pval(round(trend[1,],5), digits=2, eps=0.0001, scientific=FALSE)
  			trend
  		}
  	})
    
  }, include.rownames=FALSE, digits=4)
  output$resTab <- renderTable({
  	if(input$updateRes==0){
  		return(NULL)
  	}
  	input$updateRes
  	isolate({
  		withProgress(session,{  
  		inputDataRes <- inputDataFile()  
  		if( is.null(inputDataRes) | !is.null(inputDataRes[["badDataFlag"]]) ){ NULL
  		} else {
  			
  				if(performZCA()[,1]>0.05)setProgress(message="Calculating, please wait",detail="NO SIGNIFICANT TREND")
  				if(!(performZCA()[,1]>0.05))setProgress(message="Calculating, please wait")
  				res <- analysisReact()[["numRes"]]
  				#print(res[1,c("x","ECx","PLL.lower","PLL.upper","CIconfidence")])
  				#print(res[c("x","ECx","PLL.lower","PLL.upper","CIconfidence")])
  				return(signif(res[1,c("x","ECx","PLL.lower","PLL.upper","CIconfidence"), drop=FALSE],5))
  			
  		}
  		})
  	})
    
  }, include.rownames=FALSE)
  output$plotRes <- renderPlot({
  	if(input$updateRes==0){
  		return(NULL)
  	}
  	input$updateRes
  	isolate({
  		inputDataRes <- inputDataFile()  
  		if( is.null(inputDataRes) | !is.null(inputDataRes[["badDataFlag"]]) ){ NULL
  		} else plotFun()
  	})
     
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
       trend <- performZCA()
	  trend[1,] <- format.pval(round(trend[1,],5), digits=2, eps=0.0001, scientific=FALSE)
       trendPretty <- trend
	  res <- analysisReact()[["numRes"]]
	  #res <- res[1,c("x","ECx","PLL.lower","PLL.upper","CIconfidence","beta","ldXX"),drop=FALSE]
	  res <- res[1,c("x","ECx","PLL.lower","PLL.upper","CIconfidence"),drop=FALSE]
	  resPretty <- signif( res, digits=5 )
	 wb$writeWorksheet(trendPretty, sheet=numShName, startRow=1, startCol=1, header=TRUE) 
      wb$writeWorksheet(resPretty, sheet=numShName, startRow=4, startCol=1, header=TRUE)
	 
      wb$writeWorksheet(date(), sheet=numShName, startRow=6, startCol=length(resPretty)+2, header=FALSE)
	  wb$writeWorksheet(R.version.string, sheet=numShName, startRow=7, startCol=length(resPretty)+2, header=FALSE)
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
