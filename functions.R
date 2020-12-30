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

doCalculations <- function(dnaSeq, crisprSeq, gRNA, mh, padding, revFlag, orientation, progress, toolSeries){
  
  #' Determine the location of the CRISPR cut site in the cDNA sequence:
  cutSite <- getGenomicCutSite(toupper(dnaSeq), toupper(crisprSeq), orientation)
  
  #' Construct oligos correctly if reverse flag has been applied
  #if(revFlag == FALSE){
    progress$set(detail = "generating 5' oligos", value = 0.6)

    # For UFlip ON
    if(toolSeries == 4){
      fiveData  <- get3Prime(toupper(dnaSeq),        toupper(crisprSeq), toupper(gRNA), mh, cutSite, toolSeries)
      threeData <- get5Prime(toupper(dnaSeq),        toupper(crisprSeq), toupper(gRNA), mh, cutSite, padding, orientation, toolSeries)
      
    } else {
      # For everybody else:
      
      #' Calculate the 5' oligo targeting domains
      fiveData  <- get5Prime(toupper(dnaSeq),        toupper(crisprSeq), toupper(gRNA), mh, cutSite, padding, orientation, toolSeries)
      
      progress$set(detail = "generating 3' oligos", value = 0.8)
      #' Calculate the 3' oligo targeting domains
      threeData <- get3Prime(toupper(dnaSeq),        toupper(crisprSeq), toupper(gRNA), mh, cutSite, toolSeries)
      
    }
  #} else {
  #  progress$set(detail = "generating 5' oligos", value = 0.6)
    #' Calculate the 5' oligo targeting domains
  #  fiveData  <- get5PrimeRevFlag(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite, padding, orientation, toolSeries)
    
  #  progress$set(detail = "generating 3' oligos", value = 0.8)
    #' Calculate the 3' oligo targeting domains
  #  threeData <- get3PrimeRevFlag(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite, padding, orientation, toolSeries)
  #}

  return(c(fiveData, threeData))
}

#' getGenomicCutSite
#'
#' This function takes a cDNA/gene sequence and a CRISPR sequence, and determines the CRISPR cut site in the cDNA sequence. It is highly recommended that you check the validity of the cDNA/gene sequence with isDnaSeqValid(), the CRISPR sequence with isCrisprSeqValid(), and that the CRISPR sequence appears only once in the cDNA/gene sequence with crisprTargetAppearances(). The function may throw errors if any of the above checks fail.
#'
#' @param dnaSeq    A character sequence containing cDNA or gene that the crisprSeq targets
#' @param crisprSeq A 20-character sequence without PAM sequence
#'
#' @return          cutIndex The location index of where in dnaSeq the CRISPR target will be cut
#' @export
#'
#' @examples
#' dnaSeq    <- "ACTAAAACGATGCAGACTGACGTACGTAGCATGATCGTAGCATGCATGAGGAAAACTGCAACAT"
#' crisprSeq <- "GCATGATCGTAGCATGCATGAGG"
#' getGenomicCutSite(dnaSeq, crisprSeq)

getGenomicCutSite <- function(dnaSeq, crisprSeq, orientation){
  
  orientation <- as.numeric(orientation)
  #Determine where in the sequence the crispr is located
  crisprLoc <- unlist(str_locate_all(dnaSeq, crisprSeq))
  
  if(orientation == 0){
    #If the CRISPR is in the sense strand, cut between bases 17 and 18 in the matching sequence.
    cutIndex <- crisprLoc[2] - 3
    
  } else {
    #If the CRISPR is in the anti-sense strand, cut between bases 3 and 4 in the matching sequence.
    cutIndex <- crisprLoc[1] + 2
  }
  
  return(cutIndex)
}

#' get5Prime
#'
#' This function takes user inputs and formats them into the 5' forward and reverse targeting oligos for microhomology-mediated end joining
#'
#' @param dnaSeq    A character sequence containing only A, C, G, or T
#' @param crisprSeq A 23-character long sequence containing the genomic CRISPR target
#' @param gRNA      A character sequence containing only A, C, G, or T; for creation of targeting domains
#' @param mh        An integer length of how long the homologous section should be
#' @param cutSite   The integer index of where in dnaSeq the Cas9 will cut the CRISPR target
#' @param padding   A character sequence (of length 0-2, inclusive) for padding out the targeting oligos so that codons are not broken
#'
#' @return          The forward and reverse 5' targeting oligos
#' @export
#'
#' @examples

get5Prime <- function(dnaSeq, crisprSeq, gRNA, mh, cutSite, padding, orientation, toolSeries){
  
  #Get the homologous section from the genome
  homology <- substring(toupper(dnaSeq), cutSite - (mh - 1), cutSite)
  #Get the next three nucleotides
  spacer   <- substring(toupper(dnaSeq), cutSite - (mh + 2), cutSite - mh)
  #Generate a three nucleotide-long spacer that is not homologous to spacer
  nhSpacer <- addNonHBP(spacer)
  
  if(toolSeries == 4){
    #Create the base five prime oligo
    fivePrimeFBase <- paste0(nhSpacer, homology, reverseComplement(gRNA))
    
  } else {
    #Create the base five prime oligo
    fivePrimeFBase <- paste0(gRNA, nhSpacer, homology, getPadding(dnaSeq, cutSite, padding, orientation))
    
  }
  
  #If a custom guide RNA is used, add restriction enzyme sites
  #if(nchar(gRNA) > 0 &&  toolSeries == 0){
   # fivePrimeF <- paste0("aattc", fivePrimeFBase, "g")
    #fivePrimeR <- paste0("ggatc", reverseComplement(fivePrimeFBase), "g")
    
  #} else {
    #fivePrimeF <- paste0("gcgg",  fivePrimeFBase)
    
  # pGTag overhangs
    if(toolSeries == 0){
      fivePrimeF <- paste0("gcgg",  fivePrimeFBase)
      fivePrimeR <- paste0("atcc",  reverseComplement(fivePrimeFBase))
      
  # UFlip ON overhangs
    } else if(toolSeries == 4){
      fivePrimeF <- paste0("gaag",  reverseComplement(fivePrimeFBase))
      fivePrimeR <- paste0("gcgg",  fivePrimeFBase)
  
   # Custom - no overhangs
    } else if(toolSeries == 5){
      fivePrimeF <- fivePrimeFBase
      fivePrimeR <- reverseComplement(fivePrimeFBase)
      
    # Everybody else's overhangs 
    } else {
      fivePrimeF <- paste0("gcgg",  fivePrimeFBase)
      fivePrimeR <- paste0("gaag",  reverseComplement(fivePrimeFBase))
      
    }
  
  return(c(fivePrimeF, fivePrimeR))
}

#' get3PrimeRevFlag
#'
#' @param dnaSeq 
#' @param crisprSeq 
#' @param passSeq 
#' @param mh 
#' @param cutSite 
#' @param padding 
#'
#' @return
#' @export
#'
#' @examples

get3PrimeRevFlag <- function(dnaSeq, crisprSeq, passSeq, mh, cutSite, padding, orientation, toolSeries){
  homology <- substring(dnaSeq, cutSite - 1,      cutSite + mh)
  spacer   <- substring(dnaSeq, cutSite + mh + 1, cutSite + mh + 3)
  nhSpacer <- addNonHBP(spacer)
  
  #fivePrimeRevFBase <- paste0(getPadding(padding), homology, nhSpacer, reverseComplement(passSeq))
  #fivePrimeRevFBase <- paste0(getPadding(dnaSeq, cutSite, padding, orientation), homology, nhSpacer, reverseComplement(passSeq))
  fivePrimeRevFBase <- paste0(homology, nhSpacer, reverseComplement(passSeq))
  
  #Add cloning sites if needed
  #if(nchar(passSeq) > 0 && toolSeries == 0){
  #  fivePrimeRevF <- paste0("catgg", reverseComplement(fivePrimeRevFBase),"g")
  #  fivePrimeRevR <- paste0("ggccg", fivePrimeRevFBase, "g")
    
  fivePrimeRevF <- paste0("aag",  reverseComplement(fivePrimeRevFBase))
  fivePrimeRevR <- paste0("cgg",  fivePrimeRevFBase)
  #} else {
  #  fivePrimeRevF <- paste0("gcgg",  reverseComplement(fivePrimeRevFBase))
  #  if(toolSeries == 1){
  #    fivePrimeRevR <- paste0("gaag",  reverseComplement(fivePrimeRevFBase))
  #  } else {
  #    fivePrimeRevR <- paste0("atcc",  reverseComplement(fivePrimeRevFBase))
  #  }
    
    #fivePrimeRevR <- paste0("atcc",  fivePrimeRevFBase)
  #}
  
  return(c(fivePrimeRevR, fivePrimeRevF))
}

#' get3Prime
#'
#' This function takes user inputs and formats them into the 3' forward and reverse targeting oligos for microhomology-mediated end joining
#' @param dnaSeq    A character sequence containing only A, C, G, or T
#' @param crisprSeq A 23-character long sequence containing the genomic CRISPR target
#' @param gRNA      A character sequence containing only A, C, G, or T; for creation of targeting domains
#' @param mh        An integer length of how long the homologous section should be
#' @param cutSite   The integer index of where in dnaSeq the Cas9 will cut the CRISPR target
#' @param padding   A character sequence (of length 0-2, inclusive) for padding out the targeting oligos so that codons are not broken
#'
#' @return          The forward and reverse 3' targeting oligos
#' @export
#'
#' @examples

get3Prime <- function(dnaSeq, crisprSeq, passSeq, mh, cutSite, toolSeries){
  
  homology <- substring(dnaSeq, cutSite + 1,      cutSite + mh)
  spacer   <- substring(dnaSeq, cutSite + mh + 1, cutSite + mh + 3)
  nhSpacer <- addNonHBP(spacer)
  
  threePrimeFBase <- paste0(homology, nhSpacer, reverseComplement(passSeq))
  
  #Add cloning sites if needed
  #if(nchar(passSeq) > 0){
  #  threePrimeF <- paste0("catgg", threePrimeFBase, "c")
  #  threePrimeR <- paste0("ggccg", reverseComplement(threePrimeFBase), "c")
    
  
  #} else {
  # For UFlip ON, use flip overhangs
  if(toolSeries == 4){
    threePrimeF <- paste0("aag", threePrimeFBase)
    threePrimeR <- paste0("cgg", reverseComplement(threePrimeFBase))
    
    # For custom series with no overhangs
  } else if(toolSeries == 5){
    threePrimeF <- reverseComplement(threePrimeFBase)
    threePrimeR <- threePrimeFBase 
    
    # For everyone else, use normal overhangs
  } else {
    #threePrimeF <- paste0("cgg", reverseComplement(threePrimeFBase))
    #threePrimeR <- paste0("aag", threePrimeFBase) 
    threePrimeF <- paste0("aag", reverseComplement(threePrimeFBase))
    threePrimeR <- paste0("cgg", threePrimeFBase) 
    
  }
    

  #}
  
  return(c(threePrimeF, threePrimeR))
  
}

#' get5PrimeRevFlag
#'
#' @param dnaSeq 
#' @param crisprSeq 
#' @param gRNA 
#' @param mh 
#' @param cutSite 
#'
#' @return
#' @export
#'
#' @examples

get5PrimeRevFlag <- function(dnaSeq, crisprSeq, gRNA, mh, cutSite, orientation, padding, toolSeries){
  #Get the homologous section from the genome
  homology <- substring(toupper(dnaSeq), cutSite - (mh - 1), cutSite)
  #Get the next three nucleotides
  spacer   <- substring(toupper(dnaSeq), cutSite - (mh + 2), cutSite - mh)
  #Generate a three nucleotide-long spacer that is not homologous to spacer
  nhSpacer <- addNonHBP(spacer)
  
  #Create the base five prime oligo
  threePrimeRevFBase <- paste0(gRNA, nhSpacer, homology, getPadding(dnaSeq, cutSite, padding, orientation))
  
  #If a custom guide RNA is used, add restriction enzyme sites
  #if(nchar(gRNA) > 0){
 #   threePrimeRevF <- paste0("aattc", reverseComplement(threePrimeRevFBase), "c")
 #   threePrimeRevR <- paste0("ggatc", threePrimeRevFBase, "c")
 # } else {
    threePrimeRevF <- paste0("gcgg", reverseComplement(threePrimeRevFBase))
    if(toolSeries == 1){
      threePrimeRevR <- paste0("gaag", threePrimeRevFBase)
    } else {
      threePrimeRevR <- paste0("atcc", threePrimeRevFBase)
    }
    
  #}
  
  return(c(threePrimeRevR, threePrimeRevF))
}

#' addNonHBP
#'
#' This method takes a character sequence as input and produces a sequence of identical length that is non-homologous;
#' for instance, the character 'A' can be substituted with 'C', 'G', or 'T' - the character will be randomly chosen.
#'
#' @param seq A character sequence (allowed characters are 'A', 'C', 'G', and 'T')
#'
#' @return    NNNseq A character sequence of the same length as seq, containing a sequence non-homologous to seq.
#' @export
#'
#' @examples
#' seq <- "ACTG"

addNonHBP <- function(seq){
  #Empty list to hold NNN non-homologous sequence
  NNNseq <- list()
  
  #New version
  a <- grepl('a', seq, ignore.case = TRUE)
  c <- grepl('c', seq, ignore.case = TRUE)
  g <- grepl('g', seq, ignore.case = TRUE)
  t <- grepl('t', seq, ignore.case = TRUE)
  nucs <- c('A' = a, 'C' = c, 'G' = g, 'T' = t)
  pool <- nucs[which(nucs != TRUE)]
  keepers <- names(pool)
  
  set.seed(21)
  newNuc <- sample(keepers, 1)
  NNNseq <- rep(newNuc, 3)
  NNNseq <- paste(NNNseq, collapse = "")
  
  return(NNNseq)
  
  #for(i in 1:nchar(seq)){
   #n <- substr(seq, i, i)
    
    #if(        (n == "A") | (n == "a")){
    #  set.seed(21)
    #  newNuc <- sample(c("C", "G", "T"), 1)
    #  
    #} else if ((n == "C") | (n == "c")){
    #  set.seed(21)
    #  newNuc <- sample(c("A", "G", "T"), 1)
      
    #} else if ((n == "G") | (n == "g")){
    #  set.seed(21)
    #  newNuc <- sample(c("A", "C", "T"), 1)
      
    #} else if ((n == "T") | (n == "t")){
    #  set.seed(21)
    #  newNuc <- sample(c("A", "C", "G"), 1)
      
    #} else {
    #  stop("Error: Unsupported nucleotide type present. Supported nucleotides are A, C, G, and T. Please check to ensure input is a DNA sequence.")
    #}
    
    #NNNseq <- c(NNNseq, newNuc)
  #}
  
  #return(paste(unlist(NNNseq), collapse = ""))
}

#' reverse
#'
#' This function takes a string as input and reverses the order of the string
#'
#' @param seq A string to reverse
#'
#' @return    revSeq The seq string in reverse
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
  revSeq  <- charSeq
  revSeq  <- as.numeric(reverse.character(charSeq))
  return(revSeq)
}

#' complement
#'
#' This function takes a DNA or RNA sequence as input (along with a parameter specifying the type of sequence) 
#' and outputs the complement of the input sequence. E.g., "ATTG" will return "TAAC" if type = "DNA" and "UAAC" if type = "RNA"
#'
#' @param seq  A DNA or RNA sequence from which to generate a complement string
#' @param type Default is "DNA"; a DNA sequence can only contain "A", "C", "G", or "T" for the purposes of complement(). 
#'   The other option is "RNA"; an RNA sequence can only contain "A", "C", "G", or "U" for the purposes of complement().
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
                             from         = fromVal,
                             to           = toVal,
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
#' @param seq  A character vector from which to generate a reverse complement.
#' @param type Default is "DNA"; allowed characters are "A", "C", "G", and "T" (case insensitive). 
#'        Other option is "RNA"; allowed characters are "A", "C", "G", and "U" (case insensitive.)
#'
#' @return     seqRevComp The reverse complement of the input sequence
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

#' getPadding
#'
#' @param padding 
#'
#' @return
#' @export
#'
#' @examples

getPadding <- function(seq, cutSite, padding, orientation){
  if(padding == 0){
    pad <- ""
  } else {
    #Get the padding for the sense orientation
    #if(orientation == 0){
      if(padding == 1){
        #Find the nucleotides to fix the genomic frameshift
        pad <- substring(seq, cutSite + 1, cutSite + 1)
      } else {
        #Find the nucleotides to fix the genomic frameshift
        pad <- substring(seq, cutSite + 1, cutSite + 2)
      }
      
      
   # } else {
   #   if(padding == 1){
   #     #Find the nucleotides to fix the genomic frameshift in the anti-sense orientation
   #     pad <- reverse(substring(seq, cutSite + 1, cutSite + 1))
   #   } else {
   #     #Find the nucleotides to fix the genomic frameshift in the anti-sense orientation
    #    pad <- reverse(substring(seq, cutSite + 1, cutSite + 2))
   #   }
      
      
   # }
  }

return(pad)  
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples

pingGeneId <- function(id){
  dset <- getEnsemblSpecies(id)
  if(dset != -1){
    gMart <- useMart("ensembl", dataset = dset[1])
    dummy <- biomaRt:::getBM("description", filters = "ensembl_gene_id", values = id, mart = gMart)
    
    if(nrow(dummy) == 0){
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}



####Misc Functions specific to GTagHD####
#Checks to see if using reverseComplement of cDNA
revCheck <- function(ucDNA, uCS){
  #Count how many times the input CRISPR target sequence appears in the cDNA sequence in the FORWARD direction
  count <- str_count(ucDNA, uCS)
  
  #Count the instance in the reverse complement of the sequence
  revCount <- str_count(reverseComplement(ucDNA), uCS)
  
  if(revCount == 1 && count == 0){
    uDNA <- reverseComplement(ucDNA)
    revFlag <- TRUE
    uCrispr <- reverseComplement(uCS)
  } else {
    uDNA <- ucDNA
    revFlag <- FALSE
    uCrispr <- uCS
  }
  return(c(uDNA, revFlag, uCrispr))
}

#Get padding for codon repairs for GenBank Accessions
getGenBankPadding <- function(uDNA, uCS, orientation){
  #Get the location of the cut site
  cutI <- getGenomicCutSite(uDNA, uCS, orientation)
  
  #Determine how many padding nucleotide 
  if(cutI %% 3 == 0){
    return(0)
    
  } else {
    return(3 - (cutI %% 3))
  }
}

#' stripWhiteSpace
#'
#' This function removes all white space from character vectors. If it is passed a data frame, it will remove all white space from all columns with character data types.
#'
#' @param wsco An object to be stripped of white space
#'
#' @return stripped The object, stripped of white space
#' @export
#'
#' @examples
#' stripWhiteSpace(c("red", " gre en ", "  ", " blue "))
#' V1 <- c("  1", " 2 ", "    3")
#' V2 <- c(1, 2, 3)
#' dummyDF <- data.frame(V1, V2, stringsAsFactors=FALSE)
#' dummyDF
#' stripWhiteSpace(dummyDF)

stripWhiteSpace <- function(wsco) UseMethod("stripWhiteSpace")

#Default method; will not run the method unless it is passed a data frame with at least one column of characters or a character vector
stripWhiteSpace.default <- function(wsco){
  stop("Object is not a character vector")
}

#For handling character vectors
stripWhiteSpace.character <- function(wsco){
  stripped <- gsub('\\s+', '', wsco)
  stripped <- gsub('\\h+', '', stripped)
  stripped <- gsub('\\v+', '', stripped)
  return(stripped)
}

#For handling data frames with at least one character column type
stripWhiteSpace.data.frame <- function(wsco){
  count <- 0
  dDF <- wsco
  for(i in 1:ncol(wsco)){
    if(class(wsco[,i])=="character"){
      dDF[,i] <- stripWhiteSpace(wsco[,i])
      count <- count + 1
    }
  }
  #Determine if any of the columns are character vectors; stop execution if they are not
  if(count == 0){
    stop("Data frame has no character columns")
  }
  return(dDF)
}

#################################For Handling Oligonucleotide insertions###################
getBasePlasmid <- function(plasType){
  switch(plasType,
         "www/plasmids/pGTag-eGFP-B-actin_(071618).ape",
         "www/plasmids/pGTag-eGFP-caax-B-actin_(071618).ape",
         "www/plasmids/pGTag-eGFP-caax-SV40_(071618).ape",
         "www/plasmids/pGTag-eGFP-SV40_(071618).ape",
         "www/plasmids/pGTag-Gal4-VP16-B-actin_(071618).ape",
         "www/plasmids/pGTag-NLS-eGFP-B-actin_(071618).ape",
         "www/plasmids/pGTag-NLS-eGFP-SV40_(071618).ape",
         "www/plasmids/pGTag-NLS-TagRFP-B-actin.ape",
         "www/plasmids/pGTag-NLS-TagRFP-SV40_(071618).ape",
         "www/plasmids/pGTag-TagRFP-B-actin_(071618).ape",
         "www/plasmids/pGTag-TagRFP-caax-B-actin_(071618).ape",
         "www/plasmids/pGTag-TagRFP-caax-SV40_(071618).ape",
         "www/plasmids/pGTag-TagRFP-SV40_(071618).ape")
}

