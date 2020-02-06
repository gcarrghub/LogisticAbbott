
#### Calculate zCA everytime,
##    If no significant trend, alert client using a pop-up dialogue box.

library(shiny)
#library(shinyIncubator)
#library(XLConnect)
library(openxlsx)
library(optimx)
library(plotrix)
library(grid)
library(gridExtra)

source("LAshinyFuns.R")
source("SKorig.R")

reactiveVars <- reactiveValues()
reactiveVars$zeros <- FALSE
reactiveVars$annotatedplot <- NULL
reactiveVars$cleanplot <- NULL
reactiveVars$stamp <- ""
reactiveVars$amsg <- ""


## ALWAYS doTrend 
doTrend <- TRUE

exampleDat <- matrix(c(1,0,20,2,0,20,4,10,20,8,20,20,16,20,20),byrow=TRUE,nrow=5)
colnames(exampleDat) <- c("doses","responses","sizes")

if(dir.exists("www")){
  #delete leftover files if they are present from previous runs
  #this only matters when the repository is set up in rstudio
  #and run inside of rstudio in usual way.  When the tool is run
  #through a runGitHub(...) command, it creates temporary space 
  #that is removed on completion (the tool implementation is killed)
  pdfFiles <- list.files(path="www",pattern = "pdf$",full.names = TRUE)
  pngFiles <- list.files(path="www",pattern = "png$",full.names = TRUE)
  xlsxFiles <- list.files(path="www",pattern = "xlsx$",full.names = TRUE)
  files2remove <- c(pdfFiles,pngFiles,xlsxFiles)
  unlink(files2remove)
}
if(!dir.exists("www"))dir.create("www")

shinyServer(function(input,output,session) {

  inputDataFile <- reactive({
    shiny::req(input$inputFile)
    inFile <- input$inputFile
    #print(inFile)
    if(!is.null(inFile)){
      #reactiveVars$zeros <- FALSE
      fileExt <- sub(".*\\.","",inFile[1])
      
      ## Use grepl because readWorksheet works with both xls and xlsx versions of wb.
      if(grepl("xls",fileExt)){
        shiny::req(input$shName)
        ### XLCONNECT here
        wb <- loadWorkbook(inFile$datapath)
        shName <- input$shName
        if(is.null(shName)){
          indata <- NULL
        } else {
          ### indata <- readWorksheet(wb, sheet=shName, header=TRUE)### XLCONNECT here
          indata <- readWorkbook(wb, sheet=shName, colNames=TRUE)
        }
      }
      if(fileExt=="txt"){
        indata <- read.table(inFile$datapath,header=TRUE)
      }
      if(fileExt=="csv"){
        indata <- read.csv(inFile$datapath,header=TRUE)
      }
      #print(indata)
      return(indata)
    }
    
  })

  dataOrg <- reactive({
    #wait for data with correct names
    shiny::req(inputDataFile())
    #print("dataOrg")
    namesInp <- names(inputDataFile())
    #print(namesInp)
    if (all(c("doses", "responses", "sizes") %in% namesInp)) {
      indata <-
        inputDataFile()[, c("doses", "responses", "sizes")]
    }
    if (!all(c("doses", "responses", "sizes") %in% namesInp)) {
      ## Have to make sure the UIs are generated before I subset.
      #if (is.null(input$nameSizeCol))
      #  return(NULL)
      indata <- inputDataFile()
      namesInp <- names(indata)
      #print(namesInp)
      #print("Map names")
      #print(indata)
      shiny::req(c(input$nameDoseCol, input$nameRespCol, input$nameSizeCol))
      indata <-
        indata[, c(input$nameDoseCol, input$nameRespCol, input$nameSizeCol)]
      names(indata) <- c("doses", "responses", "sizes")
      #print(indata)
    }
    
    #print(indata)
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
    integerCheckTF <- TRUE
    output$integerCheck <- NULL
    if(any(indata$sizes!=round(indata$sizes)) | any(indata$responses!=round(indata$responses))  |
       any(indata$sizes<indata$responses) | any(indata$sizes<0) | any(indata$responses<0)){
      integerCheckTF <- FALSE
      output$integerCheck <- renderText({"WARNING:  Non-integer response, total count (sizes), response > size,
      or <0 values detected.  Results may not be valid or may cause the program to fail."})
    }
    
    return(list(indata = indata, dose0Flag = dose0Flag, integerCheck=integerCheckTF))
    #} else return(list(indata=indata,dose0Flag=dose0Flag,badDataFlag=badDataFlag))
  })
  
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
      selectInput('shName', 'Select Sheet Containing Data', shNames,selected=shNames[1])
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
      # if the right names are already in there, no need to give options
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
      #print(c(debugTF=debugTF))
      if(debugTF)print(formattedP <- sapply(results,FUN=function(x)ifelse(x<1e-6,format.pval(x,eps=1e-6),format(round(x,6),scientific=FALSE))),quote=FALSE)
      #cat("\n")
      #cat("\n\n",formattedP,"\n",sep="\n")
      invisible(results)
    }
    
    #print( t( as.matrix( zCA(as.matrix(indata[,c("responses","sizes")])) ) ) )
    if(doTrend)return( t( as.matrix( zCA(as.matrix(indata[,c("responses","sizes")])) ) ) ) else return(NULL)
    
  }
  
  
  ## Observe is like a reactive expression. I think it is appropriate to change tabs to the data tab each time the file 
  ##  extension is changed.  Better to switch any time any of the data selection options change
  
  #
  #### observe({
  ####   ## Throw dataOrg() in so that anytime a selection is made that has the potential to change the data, redirect the client to the 
	####   ##   data tab.  This is causing shiny to crash IDK why.  I have tried inputDataFile() as well but to no avail.
	#### if(!is.null(getFileExt()))updateTabsetPanel(session,"tabManager",selected="dataTab")
  #### })
  
  #### observe({
  ####   if(input$updateRes!=0)updateTabsetPanel(session,"tabManager",selected="resultsTab")
  #### })
  
  
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
  
  
  
  
  ### End of data read in (did do a few minor calcs)
  
  

  
  
  
  
  
  
  
  
  
  
  
  #### Main analysis process.  When the button 'calculate results' is clicked
    observeEvent(input$updateRes, {
      updateTabsetPanel(session, "tabs", "Results")
      stampSTRfile <<-  format(Sys.time(), "%Y%m%d%H%M%S")
      verbose <<- FALSE
      debugTF <<- FALSE
      if(input$debugPrint){
        verbose <<- TRUE
        debugTF <<- TRUE
      }
      withProgress({
        setProgress(message = "Please Wait")
        
        isolate({
          #write(as.character(Sys.time()), file="counter.txt", append=TRUE, sep="\n")
          annotateFUN <- function(eqnCEX=1.5){
            if(input$modelType=="abbott")mtext(line=-2,side=1,adj=.95,cex=eqnCEX,
               text=expression(plain(P)==bgroup("(",
                                                atop(
                                                  paste(C,phantom(X(1-C)*over(1,textstyle(1+e)^textstyle({-beta*group("[",log[10](d)-LC[50],"]")}))),phantom(XX),dose==0),
                                                  paste(C+(1-C)*over(textstyle(1),textstyle(1+e)^textstyle({-beta*group("[",log[10](d)-LC[50],"]")})),phantom(XX),dose>0)),
                                                "")))
            #if(modelSTR!="abbott")mtext(line=-2,side=1,adj=1,
            if(input$modelType!="abbott")mtext(line=-2,side=1,adj=.95,cex=1.5,
                                               text=expression(plain(P)==
                                                                 paste(over(textstyle(1),textstyle(1+e)^textstyle({-beta*group("[",log[10](d)-LC[50],"]")})))))
            
            #lines(x=10^newX,y=predict(glmObject,new=data.frame(logDoses=newX),type="response"),col='gray')
            #lines(x=10^newX,y=predictionModel(params=nlObject[["par"]],logdoses=newX,ECx=0.5),lwd=2)
            stampSTR <- date()
            #if(is.character(inputFile))stampSTR <- paste(stampSTR,inputFile,sep="\n")
            mtext(side=1,line=-1,outer=TRUE,text=stampSTR,cex=0.6,adj=0.98)
            
            # somehow in shiny environment need to overlay new plot region?  I don't understand it
            # but without, get NaN warnings and tables don't appear
            par(new=TRUE)
            plot(1,1,type='n',axes=FALSE,xlab='',ylab='',ylim=c(0,1))
            addtable2plot(x=par("usr")[1]+0.01*diff(par("usr")[1:2]),
                          y=0.9,
                          xjust=0,yjust=0,
                          table=indata,cex=1)
            
            addtable2plot(x=par("usr")[1]+0.01*diff(par("usr")[1:2]),
                          y=par("usr")[4],
                          xjust=0,yjust=1,
                          table=data.frame(
                            P=100*ECx.targets,
                            ECp=signif(results["ECx"],digits=3),
                            lowerCI=signif(CIbounds[1],digits=3),
                            upperCI=signif(CIbounds[2],digits=3),
                            confLevel=100*confidenceCI),
                          cex=1)
          }
          
          finalPlotFUN <- function(){
            plotSetup.noCI(inputDF=dataOrg()[["indata"]],
                           modelType=input$modelType,
                           ECx.target=input$ECx.targets/100,
                           MLE.ECx=MLE.Parms,
                           genericDoses = !input$doseTicks,
                           xlabSTR=input$xlab,ylabSTR=input$ylab)
            #print(results)
            #print(c(ECx.targets=ECx.targets,ECx.targets=input$ECx.targets/100,BGparm=BGparm,BGrate=BGrate,modelRange=modelRange))
            lines(x=CIbounds,y=BGrate + modelRange*c(1,1)*input$ECx.targets/100,lwd=5,col="magenta")
            
          }
          
          indata <- dataOrg()[["indata"]]
          ECx.targets <- input$ECx.targets / 100
          confidenceCI <- input$confidenceCI / 100
          bgAdjust <- input$modelType=="abbott"
        })
        
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
        modelRange <- 1
        BGrate <- 0
        if(input$modelType=="lcx")MLE.Parms <- tail(results,2)
        if(input$modelType=="abbott")MLE.Parms <- tail(results,3)
        if(input$modelType=="abbott"){
          BGparm <- MLE.Parms[which(regexpr("BG",names(MLE.Parms))>0)]
          BGrate <- exp(BGparm)/(1+exp(BGparm))
          modelRange <- 1-BGrate
        }
        
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
        #print(MLE.Parms)
        #print(predictionModel)
        output$plot <- shiny::renderPlot(expr = {
          finalPlotFUN()
          if(input$annotate){annotateFUN()}
          
        })
        pdffilename <- getPDFfilename()
        #print(pdffilename)
        pdf(pdffilename, width = 9)
        par(mai=c(1,1.2,1,0.1))
        finalPlotFUN()
        finalPlotFUN()
        annotateFUN(eqnCEX = 1)
        #!put formula on lower-right region of figure, depending on type
          #if(modelSTR=="abbott")mtext(line=-2,side=1,adj=1,cex=.6,
        grid.newpage()
        grid.table(indata, rows = NULL)
        dev.off()
        
        
        setProgress(detail = "Creating Excel")
        
        numSheetName <- "Numerical Results"
        plotSheetName <- "Plot"
        dataSheetName <- "Analyzed Data"
        xlsxfilename <- getExcelfilename()
        #print(xlsxfilename)
        wb <- createWorkbook(creator = "BV shiny app")
        addWorksheet(wb = wb,sheetName = numSheetName,zoom = 200)
        #options("openxlsx.numFmt" = "#")
        setColWidths(wb = wb, sheet = numSheetName, cols = 1:8, widths = 15)
        writeDataTable(wb = wb,sheet = numSheetName,x = finalTable,withFilter=FALSE)
        #options("openxlsx.numFmt" = NULL)
        #,
        #tableStyle = createStyle(fontSize = 14),
        #headerStyle = createStyle(fontSize = 14))
        writeData(wb = wb,sheet = numSheetName,x = date(), startRow=4, startCol=1)
        #####createSheet(wb, plotSheetName)### XLCONNECT here
        addWorksheet(wb = wb,sheetName = plotSheetName,zoom = 200)
        cleanPlotFilename <- paste0("www/cleanResPlot", stampSTRfile, ".png")
        #print(cleanPlotFilename)
        png(cleanPlotFilename,height=6,width=8,units = "in",res = 200,type = "cairo")
        par(mai=c(1,1.2,1,0.1))
        finalPlotFUN()
        dev.off()
        ### createName(wb, name="cleanPlot", formula=paste0(plotSheetName,"!$A$1"))### XLCONNECT here
        ### addImage(wb, filename=cleanPlotFilename, name="cleanPlot", originalSize=TRUE)### XLCONNECT here
        insertImage(wb = wb,sheet = plotSheetName,file = cleanPlotFilename,
                    height = 6,width = 8)
        
        dirtyPlotFilename <- paste0("www/dirtyResPlot", stampSTRfile, ".png")
        #print(dirtyPlotFilename)
        png(dirtyPlotFilename,height=6,width=8,units = "in",res = 200,type = "cairo")
        par(mai=c(1,1.2,1,0.1))
        finalPlotFUN()
        annotateFUN(eqnCEX = 1)
        dev.off()
        ### createName(wb, name="dirtyPlot", formula=paste0(plotSheetName,"!$N$1"))### XLCONNECT here
        ### addImage(wb, filename=dirtyPlotFilename, name="dirtyPlot", originalSize=TRUE)### XLCONNECT here
        insertImage(wb = wb,sheet = plotSheetName,file = dirtyPlotFilename,startCol = 13,
                    height = 6,width = 8)
        
        ### createSheet(wb, dataSheetName)### XLCONNECT here
        ### writeWorksheet(wb, dataOrgZeroFixed(), dataSheetName)### XLCONNECT here
        addWorksheet(wb = wb,sheetName = dataSheetName,zoom = 200)
        writeDataTable(wb = wb,sheet = dataSheetName,x = indata)
        
        saveWorkbook(wb = wb,file = xlsxfilename,overwrite = TRUE)
        
      })
      reactiveVars$amsg <- "complete"
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
  output$DataTab2 <- renderTable( as.data.frame(lapply(dataOrg()[["indata"]],format)),align = "c")
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
    #print("SKline")
    #print(SKlineList)
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
	  return("Data uploaded was not in the correct format.  See 'Data For Analyis' Tab.")
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
    
  }, include.rownames=FALSE, digits=4,bordered = TRUE)
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
    
  }, include.rownames=FALSE,bordered = TRUE)
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
  getShortFileName <- reactive({
    paste0("LCXoutput", stampSTRfile)
  })
  getPDFfilename <- reactive({
    paste0("www/", getShortFileName(), ".pdf")
  })
  
  getExcelfilename <- reactive({
    paste0("www/", getShortFileName(), ".xlsx")
  })
  
  observeEvent({reactiveVars$amsg},{
    #print(c(rv.amsg=rv$amsg))
    if(reactiveVars$amsg==""){
      output$downloadPlot <- downloadHandler(
        filename=paste0(getShortFileName(), ".pdf"),
        content=function(file){} 
      )
      output$downloadResults <- downloadHandler( 
        filename=paste0(getShortFileName(), ".xlsx"),
        content=function(file){}
      )
    }
    if(reactiveVars$amsg=="complete"){
      output$downloadPlot <- downloadHandler(
        filename=paste0(getShortFileName(), ".pdf"),
        content=function(file){
          file.copy(getPDFfilename(), file)
        }   
      )
      output$downloadResults <- downloadHandler( 
        filename=paste0(getShortFileName(), ".xlsx"),
        content=function(file){
          file.copy(getExcelfilename(), file)
        }   
      )
    }
  })
  
})
