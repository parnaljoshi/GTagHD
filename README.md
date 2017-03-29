# mmejWebTool
A repository for the MMEJ Web Tool
You can run this shiny web app in RStudio (or R) by opening up an R or RStudio session, and running the following code:


#First time:
install.packages(c("shiny", "shinyjs", "Rcpp", "plyr"))

source("https://bioconductor.org/biocLite.R")

biocLite("biomaRt")


#Subsequent times:

library(shiny)

runGitHub("mmejWebTool", "Dobbs-Lab")


You're all set!
