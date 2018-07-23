#For handling GenBank files that throw errors
#' Title
#'
#' @param gba 
#'
#' @return
#' @export
#'
#' @examples

wonkyGenBankHandler <- function(gba){
  #Fetch the flat file
  gbFile   <- rentrez:::entrez_fetch(db = "nucleotide", gba, rettype = "gb", api_key = "8f945fc5def00d2dadd40ae4bb1b201bb109")
  #Read the flat file line by line
  geneIn   <- unlist(strsplit(gbFile, "\\\n", perl = TRUE))
  #Pass the readLines from flat file to formatApe to create plasmid object
  geneInfo <- formatApe(geneIn)
  return(geneInfo)
}


#For reading uploaded genbank/ape files
readGenBankFile <- function(filename){
  #Read in and format the contents of the file
  genbankContents <- readApe(filename)
  
  return(genbankContents)
}