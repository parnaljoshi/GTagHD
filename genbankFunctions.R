#' getGenbankFile
#'
#' @param accession 
#' @param file 
#' @param deleteTempFile 
#'
#' @return
#' @export
#'
#' @examples

# getGenbankFile <- function(accession, file = 'temp.gb', deleteTempFile = TRUE){
#   require(curl)
#   # Get database from accession format
#   #db <- .getDatabaseFromAccession(accession)
#   db <- 'nuccore'
#   
#   # Construct the URL
#   baseURL <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi'
#   rettype <- 'gbwithparts'
#   retmode <- 'text'
#   tURL    <- paste0(baseURL, "?db=", db, "&id=", accession, "&retmode=", retmode, "&rettype=", rettype)
#   
#   # Get file from URL using curl
#   curl::curl_download(url = tURL, file)
#   
#   # Read in file contents
#   gbContents <- readLines(file)
#   
#   # Delete temporary file
#   if(deleteTempFile){
#     file.remove(file)
#   }
#   
#   # Format the file contents
#   gbContents <- formatApe(gbContents)
#   
#   return(gbContents)
# }


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
  gbFile   <- rentrez:::entrez_fetch(db = "nucleotide", gba, rettype = "gb")

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
