# 4.2
#
# Richard White
# r.aubrey.white@gmail.com
# github.com/raubreywhite/RLocalSetup
#
# Local setup of skeleton R analysis work

if(!exists("upgradeRLocalSetup")) upgradeRLocalSetup <- FALSE
if(upgradeRLocalSetup){
  try({
    if(!require(httr)){
      install.packages("httr", repos="http://cran.r-project.org")
    }

    l <- readChar("RLocalSetup.R", file.info("RLocalSetup.R")$size)
    lVer <- as.numeric(substr(l,3,5))
    print(paste0("CURRENT LOCAL VERSION ",lVer))

    r <- httr::GET("https://raw.githubusercontent.com/raubreywhite/RLocalSetup/master/RLocalSetup.r")
    r <- httr::content(r)
    rVer <- as.numeric(substr(r,3,5))
    print(paste0("CURRENT REMOTE VERSION ",rVer))

    if(rVer > lVer){
    	print(paste0("UPGRADING FROM ",lVer," to ",rVer))
      write(r, file="RLocalSetup.R")
      stopUpgrade <- TRUE
    }
  },TRUE)
}



# Adding R tools
AddRtools <- function(path="H:/Apps/Rtools"){
  if(path!="" & !devtools::find_rtools()){
    path <- gsub("/","\\\\",path)
    path <- paste0(path,"\\")
    for(i in 1:10) path <- gsub("\\\\\\\\","\\\\",path)
    currentPath <- Sys.getenv("PATH")
    newPath <- paste(currentPath,paste0(path,"\\bin"),paste0(path,"\\gcc-4.6.3\\bin"),sep=";")
    Sys.setenv(PATH=newPath)
  }
  return(devtools::find_rtools())
}


PandocInstalled <- function(){
  suppressWarnings(pandoc.installed <- system('pandoc')==0)
  if(pandoc.installed) return(TRUE)

  rstudio.environment.installed <- Sys.getenv("RSTUDIO_PANDOC")
  if(rstudio.environment.installed!=""){
    rstudio.environment.installed <- paste0('"',rstudio.environment.installed,'/pandoc"')
    suppressWarnings(rstudio.environment.installed <- system(rstudio.environment.installed)==0)
  } else rstudio.environment.installed <- FALSE
  if(rstudio.environment.installed) return(TRUE)

  suppressWarnings(rstudio.pandoc.installed <- system('"C:/Program Files/RStudio/bin/pandoc/pandoc"')==0)
  if(rstudio.pandoc.installed){
    Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
  }
  if(rstudio.pandoc.installed) return(TRUE)

  return(FALSE)
}

CreatePackage <- function(name="test",depends=NULL,imports=NULL){
  if(!suppressWarnings(suppressMessages(require(devtools)))){
    install.packages("devtools", repos="http://cran.r-project.org")
  }

  if(!suppressWarnings(suppressMessages(require(packrat)))){
    install.packages("packrat", repos="http://cran.r-project.org")
  } else packrat::off()

  print("CHECKING SYSTEM VARIABLES FOR RTOOLS LOCATION")
  if(!devtools::find_rtools()){
    stop("ERROR, R TOOLS NOT FOUND IN SYSTEM VARIABLES")
  }
  print("RTOOLS WORKING PROPERLY")

  depends <- unique(c(depends,c("ggplot2")))
  depends <- depends[depends!=""]
  imports <- unique(c(imports,"data.table","raubreywhite/RAWmisc","rstudio/revealjs"))
  imports <- imports[imports!=""]

  devtools::create(name)
  packrat::init(enter=FALSE)
  packrat::on(auto.snapshot=FALSE)
  
  file.remove(paste0(name,"/DESCRIPTION"))

  dependsGithub <- depends[grep("/",depends)]
  dependsCRAN <- depends[!depends %in% dependsGithub]

  importsGithub <- imports[grep("/",imports)]
  importsCRAN <- imports[!imports %in% importsGithub]

  depends <- stringr::str_split(depends,"/")
  for(i in 1:length(depends)) depends[[i]] <- depends[[i]][length(depends[[i]])]
  depends <- unlist(depends)

  imports <- stringr::str_split(imports,"/")
  for(i in 1:length(imports)) imports[[i]] <- imports[[i]][length(imports[[i]])]
  imports <- unlist(imports)

  devtools::create_description(name,extra=list(Package=name,Depends=depends,Imports=imports))
  file.remove(paste0(name,"/NAMESPACE"))
  write("exportPattern(\"^[^\\\\.]\")\nimport(data.table)",file=paste0(name,"/NAMESPACE"))
 
  if(length(dependsCRAN)>0) for(i in dependsCRAN){
    install.packages(i)
  }
  if(length(dependsGithub)>0) for(i in dependsGithub){
    devtools::install_github(i)
  }

  if(length(importsCRAN)>0) for(i in importsCRAN){
    install.packages(i)
  }
  if(length(importsGithub)>0) for(i in importsGithub){
    devtools::install_github(i)
  }

  packrat::snapshot()

  dir.create("data_raw")
  dir.create("data_temp")
  dir.create("data_clean")

  dir.create("results_temp")
  dir.create("results_final")
  dir.create("reports_skeleton")
  dir.create("reports_formatted")
  dir.create("pres_skeleton")
  dir.create("pres_formatted")

  dir.create(paste0(name,"/inst"))
  dir.create(paste0(name,"/inst/extdata"))

  file <- system.file("extdata","report.Rmd",package="RAWmisc")
  file.copy(file, paste0(name,"/inst/extdata/report.Rmd"), overwrite=TRUE)
  file.copy(file, paste0("reports_skeleton/report.Rmd"), overwrite=TRUE)

  file <- system.file("extdata","american-medical-association.csl",package="RAWmisc")
  file.copy(file, paste0(name,"/inst/extdata/american-medical-association.csl"), overwrite=TRUE)
  file.copy(file, paste0("reports_skeleton/american-medical-association.csl"), overwrite=TRUE)

  file <- system.file("extdata","references.bib",package="RAWmisc")
  file.copy(file, paste0(name,"/inst/extdata/references.bib"), overwrite=TRUE)
  file.copy(file, paste0("reports_skeleton/references.bib"), overwrite=TRUE)

  packrat::off()
  file.remove(".Rprofile")
  
  write("Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nAlwaysSaveHistory: No\n\nEnableCodeIndexing: Yes\nUseSpacesForTab: Yes\nNumSpacesForTab: 2\nEncoding: ISO8859-1\n\n\nRnwWeave: Sweave\nLaTeX: pdfLaTeX",file=paste0(name,".Rproj"))
  write(".Rproj.user\n.Rhistory\n.RData\nresults_temp/\nresults_final/\ndata_temp/\ndata_clean/\nreports_formatted/\npres_formatted/\npackrat/",file=".gitignore")

  write("
CleanData <- function(){
  data <- data.frame(x=rnorm(100),y=rnorm(100))
  return(data)
}",file=paste0(name,"/R/CleanData.R"))

  write("
FigureTest <- function(data){
  q <- ggplot(data, aes(x=x, y=y))
  q <- q + geom_point()
  q <- SMAOgraphs::SMAOFormatGGPlot(q)
  SMAOgraphs::SMAOpng(\"results_final/test.png\")
  print(q)
  dev.off()
}",file=paste0(name,"/R/Figures.R"))

  write(
paste0("
setwd(\"",getwd(),"\")

# Change if you want local setup to be pulled from github
upgradeRLocalSetup <- FALSE
source(\"RLocalSetup.R\")

# Packrat
packrat::on(auto.snapshot=FALSE)
#packrat::status()
#packrat::snapshot()

# Load package
devtools::load_all(\"",name,"\")
library(data.table)

# Commit to Git
CommitToGit(paste0(\"Committing while loading at \",Sys.time()))

r <- git2r::repository()
git2r::summary(r)
git2r::contributions(r,by=\"author\")

# Your code starts here

# Self contained HTML file
# - Copying base64 images to word/docx won't work
# - But you can email this to people and it will still work
RAWmisc::RmdToHTMLDOCX(\"reports_skeleton/report.Rmd\",paste0(\"reports_formatted/HTMLReport_\",format(Sys.time(), \"%Y_%m_%d\"),\".html\"), copyFrom=\"reports_skeleton\")


"),file="Run.R")

  repoExists <- FALSE
  try({
    r <- git2r::repository(".")
    repoExists <- TRUE
  },TRUE)
  if(!repoExists){
    r <- git2r::init(".")
    text = paste0('git2r::config(r, user.name="',Sys.info()[["user"]],'", user.email="',Sys.info()[["user"]],'@fhi.no")')
    eval(parse(text=text))
    message("* Adding files and committing")
    paths <- unlist(git2r::status(r, verbose = FALSE))
    git2r::add(r, paths)
    git2r::commit(r, "Initial commit")
  }
}

CommitToGit <- function(message="This is a working version"){
  try({
    #r <- git2r::repository(".")
    r <- git2r::repository()
    text = paste0('git2r::config(r, user.name="',Sys.info()[["user"]],'", user.email="',Sys.info()[["user"]],'@fhi.no")')
    eval(parse(text=text))
    paths <- unlist(git2r::status(r,verbose = FALSE))
    git2r::add(r, paths)
    git2r::commit(r, message)
  },TRUE)
}

# making sure everything is where it shold be

if(!suppressWarnings(suppressMessages(require(packrat)))){
  install.packages("packrat", repos="http://cran.r-project.org")
} else packrat::off()

if(!suppressWarnings(suppressMessages(require(devtools)))){
  install.packages("devtools", repos="http://cran.r-project.org")
}

print("CHECKING SYSTEM VARIABLES FOR RTOOLS LOCATION")
if(!devtools::find_rtools()){
  stop("ERROR, R TOOLS NOT FOUND IN SYSTEM VARIABLES")
}
print("RTOOLS WORKING PROPERLY")

print("CHECKING SYSTEM VARIABLES FOR PANDOC LOCATION")
if(!PandocInstalled()){
    stop("ERROR; PANDOC NOT INSTALLED")
}
print("PANDOC WORKING PROPERLY")
