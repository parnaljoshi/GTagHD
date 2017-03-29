#Functions necessary to server mmej calculations

#' doCalculations
#'
#' This function acts as a convenient wrapper for three other functions -
#' getGenomicCutSite, get5Prime, and get3Prime - which together are used
#' to create oligo targeting sequences.
#'
#' @param dnaSeq
#' @param crisprSeq
#' @param gRNA
#' @param mh
#' @param padding
#'
#' @return
#' @export
#'
#' @examples
#'

doCalculations <- function(dnaSeq, crisprSeq, gRNA, mh, padding){
  #' Determine the location of the CRISPR cut site in the cDNA sequence:
  cutSite <- getGenomicCutSite(toupper(dnaSeq), toupper(crisprSeq))
  #' Calculate the 5' oligo targeting domains
  fiveData <- get5Prime(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite, padding)
  #' Calculate the 3' oligo targeting domains
  threeData <- get3Prime(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite)
  return(c(fiveData, threeData))
}

#' getGenomicCutSite
#'
#' This function takes a cDNA/gene sequence and a CRISPR sequence, and determines the CRISPR cut site in the cDNA sequence. It is highly recommended that you check the validity of the cDNA/gene sequence with isDnaSeqValid(), the CRISPR sequence with isCrisprSeqValid(), and that the CRISPR sequence appears only once in the cDNA/gene sequence with crisprTargetAppearances(). The function may throw errors if any of the above checks fail.
#'
#' @param dnaSeq A character sequence containing cDNA or gene that the crisprSeq targets
#' @param crisprSeq A 23-character sequence containing a CRISPR target
#'
#' @return cutIndex The location index of where in dnaSeq the CRISPR target will be cut
#' @export
#'
#' @examples
#' dnaSeq <- "ACTAAAACGATGCAGACTGACGTACGTAGCATGATCGTAGCATGCATGAGGAAAACTGCAACAT"
#' crisprSeq <- "GCATGATCGTAGCATGCATGAGG"
#' getGenomicCutSite(dnaSeq, crisprSeq)

getGenomicCutSite <- function(dnaSeq, crisprSeq){
  #Determine where in the sequence the crispr is located
  crisprLoc <- unlist(str_locate_all(dnaSeq, crisprSeq))
  
  #If the crispr ends with NGG (it is in the forward direction), cut before the PAM sequence:
  if((substring(crisprSeq, nchar(crisprSeq) - 1, nchar(crisprSeq)) == "GG") | (substring(crisprSeq, nchar(crisprSeq) - 1, nchar(crisprSeq)) == "gg")){
    cutIndex <- crisprLoc[2] - 6
    
    #If the crispr begins with CCN (it is in the reverse direction), cut after the PAM sequence:
  } else {
    cutIndex <- crisprLoc[1] + 5
  }
  
  return(cutIndex)
}

#' get5Prime
#'
#' This function takes user inputs and formats them into the 5' forward and reverse targeting oligos for microhomology-mediated end joining
#'
#' @param dnaSeq A character sequence containing only A, C, G, or T
#' @param crisprSeq A 23-character long sequence containing the genomic CRISPR target
#' @param gRNA A character sequence containing only A, C, G, or T; for creation of targeting domains
#' @param mh An integer length of how long the homologous section should be
#' @param cutSite The integer index of where in dnaSeq the Cas9 will cut the CRISPR target
#' @param padding A character sequence (of length 0-2, inclusive) for padding out the targeting oligos so that codons are not broken
#'
#' @return The forward and reverse 5' targeting oligos
#' @export
#'
#' @examples

get5Prime <- function(dnaSeq, crisprSeq, gRNA, mh, cutSite, padding){
  
  #Get the homologous section from the genome
  homology <- substring(toupper(dnaSeq), cutSite - (mh - 1), cutSite)
  #Get the next three nucleotides
  spacer   <- substring(toupper(dnaSeq), cutSite - (mh + 2), cutSite - mh)
  #Generate a three nucleotide-long spacer that is not homologous to spacer
  nhSpacer <- addNonHBP(spacer)
  
  #Create the base five prime oligo
  fivePrimeFBase <- paste0(gRNA, nhSpacer, homology, getPadding(padding))
  
  #If a custom guide RNA is used, add restriction enzyme sites
  if(nchar(gRNA) > 0){
    fivePrimeF <- paste0("aattc", fivePrimeFBase, "g")
    fivePrimeR <- paste0("ggatc", reverseComplement(fivePrimeFBase), "g")
  } else {
    fivePrimeF <- fivePrimeFBase
    fivePrimeR <- reverseComplement(fivePrimeFBase)
  }
  
  return(c(fivePrimeF, fivePrimeR))
}

#' get3Prime
#'
#' This function takes user inputs and formats them into the 3' forward and reverse targeting oligos for microhomology-mediated end joining
#' @param dnaSeq A character sequence containing only A, C, G, or T
#' @param crisprSeq A 23-character long sequence containing the genomic CRISPR target
#' @param gRNA A character sequence containing only A, C, G, or T; for creation of targeting domains
#' @param mh An integer length of how long the homologous section should be
#' @param cutSite The integer index of where in dnaSeq the Cas9 will cut the CRISPR target
#' @param padding A character sequence (of length 0-2, inclusive) for padding out the targeting oligos so that codons are not broken
#'
#' @return The forward and reverse 3' targeting oligos
#' @export
#'
#' @examples

get3Prime <- function(dnaSeq, crisprSeq, passSeq, mh, cutSite){
  
  homology <- substring(dnaSeq, cutSite + 1, cutSite + mh)
  spacer   <- substring(dnaSeq, cutSite + mh + 1, cutSite + mh + 3)
  nhSpacer <- addNonHBP(spacer)
  
  threePrimeFBase <- paste0(homology, nhSpacer, reverseComplement(passSeq))
  
  #Add cloning sites if needed
  if(nchar(passSeq) > 0){
    threePrimeF <- paste0("catgg", threePrimeFBase, "c")
    
    #TODO CHECK THIS SEQ CORRECT
    threePrimeR <- paste0("gccgg", reverseComplement(threePrimeFBase), "c")
  } else {
    threePrimeF <- threePrimeFBase
    threePrimeR <- reverseComplement(threePrimeFBase)
  }
  
  return(c(threePrimeF, threePrimeR))
  
}

#' addNonHBP
#'
#' This method takes a character sequence as input and produces a sequence of identical length that is non-homologous;
#' for instance, the character 'A' can be substituted with 'C', 'G', or 'T' - the character will be randomly chosen.
#'
#' @param seq A character sequence (allowed characters are 'A', 'C', 'G', and 'T')
#'
#' @return NNNseq A character sequence of the same length as seq, containing a sequence non-homologous to seq.
#' @export
#'
#' @examples
#' seq <- "ACTG"

addNonHBP <- function(seq){
  #Empty list to hold NNN non-homologous sequence
  NNNseq <- list()
  
  for(i in 1:nchar(seq)){
    n <- substr(seq, i, i)
    
    if((n == "A")|(n == "a")){
      newNuc <- sample(c("C", "G", "T"), 1)
    } else if ((n == "C")|(n == "c")){
      newNuc <- sample(c("A", "G", "T"), 1)
    } else if ((n == "G")|(n == "g")){
      newNuc <- sample(c("A", "C", "T"), 1)
    } else if ((n == "T")|(n == "t")){
      newNuc <- sample(c("A", "C", "G"), 1)
    } else {
      stop("Error: Unsupported nucleotide type present. Supported nucleotides are A, C, G, and T. Please check to ensure input is a DNA sequence.")
    }
    NNNseq <- c(NNNseq, newNuc)
  }
  
  return(paste(unlist(NNNseq), collapse = ""))
  
}

#' reverse
#'
#' This function takes a string as input and reverses the order of the string
#'
#' @param seq A string to reverse
#'
#' @return revSeq The seq string in reverse
#'
#' @examples
#' reverse("123456")
#'
#'
#' @export

reverse <- function(seq){
  UseMethod("reverse", seq)
}

reverse.default <- function(seq){
  stop("Error: Cannot reverse objects that are not character strings or integers. Please check input sequence.")
}


reverse.character <- function(seq){
  revSeq <- seq
  
  for(i in 1:nchar(seq)){
    curL <- substr(seq, i, i)
    substr(revSeq, (nchar(seq) + 1 - i), (nchar(seq) + 1 - i)) <- curL
  }
  
  return(revSeq)
}

reverse.numeric <- function(seq){
  charSeq <- as.character(seq)
  revSeq <- charSeq
  revSeq <- as.numeric(reverse.character(charSeq))
  return(revSeq)
}

#' complement
#'
#' This function takes a DNA or RNA sequence as input (along with a parameter specifying the type of sequence) and outputs the complement of the input sequence. E.g., "ATTG" will return "TAAC" if type = "DNA" and "UAAC" if type = "RNA"
#'
#' @param seq A DNA or RNA sequence from which to generate a complement string
#' @param type Default is "DNA"; a DNA sequence can only contain "A", "C", "G", or "T" for the purposes of complement(). The other option is "RNA"; an RNA sequence can only contain "A", "C", "G", or "U" for the purposes of complement().
#'
#' @return compSeq The complement of the input sequence
#' @export
#'
#' @examples
#' seq <- "AAAATGGCGAAG"
#' type <- "DNA"
#' complement(seq, type)

complement <- function(seq, type){
  UseMethod("complement", seq)
}

complement.default <- function(seq, type){
  #Prevent attempts to complement objects that are not sequences
  stop("Error: Cannot complement a non-character vector object. Please check input sequence.")
}

complement.character <- function(seq, type){
  compSeq <- seq
  
  fromVal <- c("A", "C", "G", "T", "a", "c", "g", "t")
  toVal   <- c("T", "G", "C", "A", "t", "g", "c", "a")
  
  
  compSeq <- plyr::mapvalues(unlist(strsplit(compSeq, split = "")),
                             from = fromVal,
                             to   = toVal,
                             warn_missing = FALSE)
  compSeq <- paste(compSeq, collapse = "")
  return(compSeq)
}

complement.list <- function(seq, type){
  retList <- list()
  for(i in seq){
    retList <- c(retList, complement(i, type))
  }
  return(retList)
}

#' reverseComplement
#'
#' This function takes a DNA or RNA sequence as input and outputs the reverse complement of the sequence.
#'
#' @param seq A character vector from which to generate a reverse complement.
#' @param type Default is "DNA"; allowed characters are "A", "C", "G", and "T" (case insensitive). Other option is "RNA"; allowed characters are "A", "C", "G", and "U" (case insensitive.)
#'
#' @return seqRevComp The reverse complement of the input sequence
#' @export
#'
#' @examples
#' dnaSeq <- "AATGCC"
#' reverseComplement(dnaSeq)
#' rnaSeq <- "UUAGCC"
#' reverseComplement(rnaSeq, type = "RNA")

reverseComplement <- function(seq, type = "DNA"){
  UseMethod("reverseComplement", seq)
}

reverseComplement.default <- function(seq, type = "DNA"){
  stop("Error: Input sequence is not a character vector. Please check input sequence.")
}

reverseComplement.character <- function(seq, type = "DNA"){
  #Reverse the sequence
  seqRev <- reverse(seq)
  #Get the complement of the reversed sequence
  seqRevComp <- complement(seqRev, type = "DNA")
  return(seqRevComp)
}

reverseComplement.list <- function(seq, type = "DNA"){
  retList <- list()
  for(i in seq){
    retList <- c(retList, reverseComplement.character(i))
  }
  return(unlist(retList))
}


getPadding <- function(padding){
  if(padding == 0){
    pad <- ""
  }
  if(padding == 1){
    pad <- "A"
  }
  if(padding == 2){
    pad <- "AA"
  }
  return(pad)
}

########################
#BLAST cDNA against NCBI


########################
#Ensembl stuff
ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#
getEnsemblSequence <- function(martName, dataSetType, geneId){
  ensembl <- useMart(martName, dataset = dataSetType)
  ensCoords <- biomaRt:::getBM(attributes = c("ensembl_gene_id", "chromosome_name", "start_position", "end_position"), filters = "ensembl_gene_id", values = geneId, mart = ensembl)
  ensSeq <- getSequence(type = "ensembl_gene_id", chromosome = ensCoords[1,2], start = ensCoords[1,3], end = ensCoords[1,4],  seqType = "cdna", mart = ensembl)
  
  seq <- biomaRt:::getSequence(id="ENSG00000146648", type = "ensembl_gene_id", seqType = "cdna", mart = ensembl)
}

