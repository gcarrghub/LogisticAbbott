####
#### In all cases, should be able to just do the following (as in, the equivalent to sourcing the file)
#### Details given in if(FALSE){} blocks are details details
#### Before running the tool need to check for required packages and install if necessary
#### Typically should only need to do this once per installation of R
#options(repos = "https://cloud.r-project.org/")
packages = c("shiny", "shinydashboardPlus","isotone","grid", "gridExtra", "openxlsx", "optimx", "plotrix","rstudioapi","colourpicker")
packageTests <- sapply(packages,FUN = require,character.only=TRUE,quietly=TRUE)
print(packageTests)
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
  install.packages(packages[!packageTests], repos = "https://cloud.r-project.org/", dependencies=TRUE)
  ### In one case, needed to add this to a users install.packages call:  INSTALL_opts = c('--no-lock')
  # recheck for packages
  packageTests <- sapply(packages,FUN = require,character.only=TRUE)
  print(packageTests)
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

