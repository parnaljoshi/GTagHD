# GTagHD

#A repository for the GTagHD Web Tool

#You can run this shiny web app in RStudio (or R) by opening up an R or RStudio session, and running the following code:


#Run first time you run this tool on a computer:

install.packages(c("shiny", "shinyjs", "Rcpp", "plyr", "stringr", "rlist", "rentrez"))

source("https://bioconductor.org/biocLite.R")

biocLite("biomaRt")



#Run this code whenever you want to use the tool, including the first time:

library(shiny)

runGitHub("GTagHD", "Dobbs-Lab")


#You're all set!
