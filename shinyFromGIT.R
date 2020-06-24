####                                              #
#### https://github.com/gcarrghub/LogisticAbbott  # 
####                                              # 


cleanUp <- function(){
  globalVars <- c("debugTF","exampleDat",
                  "LD.Bailer.P","LD.Bailer.P.abbott","logistic.abbott.PLL",
                  "logisticLCx","packages","packageTests",
                  "plotSetup.noCI","predictionModel","SKbaseplot",
                  "SKdata","SKgpava","SKtrimData",
                  "spearmanKarberORIG","stampSTRfile","verbose")
  invisible(sapply(globalVars,FUN = function(objname)if(exists(objname,envir = .GlobalEnv))rm(list=objname,envir = .GlobalEnv)))
}
cleanUp()

####
#### In all cases, should be able to just do the following (as in, the equivalent to sourcing the file)
#### Details given in if(FALSE){} blocks are details details
#### Before running the tool need to check for required packages and install if necessary
#### Typically should only need to do this once per installation of R
#options(repos = "https://cloud.r-project.org/")
packages = c("shiny", "shinydashboardPlus","isotone", "gridExtra", "openxlsx", "optimx", "plotrix","devtools","rstudioapi","colourpicker")
packageTests <- sapply(packages,FUN = require,character.only=TRUE)#,type="binary")
if(all(packageTests)){
  cat("\n",paste(rep("#",100),collapse = ""),
      "\n  All required packages are present.",
      "\n",paste(rep("#",100),collapse = ""),"\n")
}
if(sum(!packageTests)>0){
  cat("\n",paste(rep("#",100),collapse = ""),
      "\n  Please wait while these required packages and their dependencies are installed:",
      "\n   ",paste(names(packageTests[!packageTests]),collapse = " "),
      "\n  Requires internet access and sufficient rights to install R packages on your system.",
      "\n",paste(rep("#",100),collapse = ""),"\n")
  install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/", dependencies=TRUE)
  ### In one case, needed to add this to a users install.packages call:  INSTALL_opts = c('--no-lock')
  # recheck for packages
  packageTests <- sapply(packages,FUN = require,character.only=TRUE)
  if(all(packageTests)){
    cat("\n",paste(rep("#",100),collapse = ""),
        "\n  All required packages were successfully installed.",
        "\n",paste(rep("#",100),collapse = ""),"\n")
  }
  if(!all(packageTests)){
    cat("\n",paste(rep("#",100),collapse = ""),
        "\n  Not all packages were successfully installed:",
        "\n   ",paste(names(packageTests[!packageTests]),collapse = " "),
        "\n",paste(rep("#",100),collapse = ""),"\n")
  }
}

if(FALSE){
  #can use this command to download a copy of the data -- only works inside of rstudio
  if(!require("rstudioapi"))install.packages("rstudioapi")
  downloadName <- "LCx Example Data.xlsx"
  # select the directory where to put it, even if it is the one that opens first
  download.file("https://github.com/gcarrghub/LogisticAbbott/raw/master/BV%20paper%20data.xlsx",
                destfile = paste(rstudioapi::selectDirectory(path = getwd()),downloadName,sep="/"))
  #one-liners that can be shared to launch the tool.  They all source this file, and should work
  #independent of OS, or R session from which it is run (terminal, Rgui, Rstudio).  Links will
  #show contents of this file, when used in a browser
  devtools::source_url("https://raw.github.com/gcarrghub/LogisticAbbott/master/shinyFromGIT.R")
  devtools::source_url("https://github.com/gcarrghub/LogisticAbbott/blob/master/shinyFromGIT.R?raw=TRUE")
  devtools::source_url("https://raw.githubusercontent.com/gcarrghub/LogisticAbbott/master/shinyFromGIT.R?raw=TRUE")
}
#### or in a code-formatted window:
#### https://github.com/gcarrghub/BV-Shiny/blob/master/shinyFromGIT.R
#### From this view, right click on the "Raw" button to save to a local file

#### Example data are at 
#### https://github.com/gcarrghub/BV-Shiny/blob/master/BV%20paper%20data.xlsx
#### Click the download button on the page that opens to use it locally

#### For using git/github with Rstudio, 
#### see https://happygitwithr.com/, https://www.r-bloggers.com/rstudio-and-github/



### once the packages above are installed
### the tool will run with only these two lines
### as long as chrome or firefox are your default browsers
### this should work in Rstudio, the Rgui, or even a terminal window on mac
library(shiny)
runGitHub("LogisticAbbott", "gcarrghub",launch.browser=TRUE)


if(FALSE){
  ### If Firefox or Chrome are not default browser, open tool with
  ### the following two lines, then copy the http web address
  ### from the resulting basic browser window into chrome or firefox address bar
  ###
  ### At least on mac, instead of copy-paste you could alternatively highlight 
  ### the whole address and drag it into an open browser window too
  library(shiny)
  ### NOTE:  This will ONLY work in Rstudio
  runGitHub("LogisticAbbott", "gcarrghub",launch.browser = .rs.invokeShinyWindowViewer)
  
  ### If something other than Chrome or Firefox is your default browser and you 
  ### don't want to change it, another option may be to set a browser option first 
  ### as follows:
  library(shiny)
  ### The location in the following two commands may need to be modified to work on 
  ### your MSwindows system.  It is up to you to find where chrome.exe or firefox.exe 
  ### is located if these don't work.  Use only one of the following two depending on 
  ### your preference on windows
  options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
  options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
  ### Use only one of the following two on mac
  options(browser = "/usr/bin/open -a '/Applications/Google Chrome.app'")
  options(browser = "/usr/bin/open -a '/Applications/Firefox.app'")
  #now the app will open in your above selected browser, independent of defaults
  runGitHub("LogisticAbbott", "gcarrghub",launch.browser=TRUE)
  
  ### it seems that an artifact of messing with browser option is that help pages now 
  ### open in the browser too, instead of internal to Rstudio.  To get back to default 
  ### behavior reset with this command:
  options(browser = function(url){.Call("rs_browseURL", url)})
}


