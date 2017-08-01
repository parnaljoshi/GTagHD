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

doCalculations <- function(dnaSeq, crisprSeq, gRNA, mh, padding, revFlag){
  #' Determine the location of the CRISPR cut site in the cDNA sequence:
  cutSite <- getGenomicCutSite(toupper(dnaSeq), toupper(crisprSeq))
  #' Construct oligos correctly if reverse flag has been applied
  if(revFlag == FALSE){
    #' Calculate the 5' oligo targeting domains
    fiveData <- get5Prime(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite, padding)
    #' Calculate the 3' oligo targeting domains
    threeData <- get3Prime(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite)
  } else {
    #' Calculate the 5' oligo targeting domains
    fiveData <- get5PrimeRevFlag(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite, padding)
    #' Calculate the 3' oligo targeting domains
    threeData <- get3PrimeRevFlag(toupper(dnaSeq), toupper(crisprSeq), toupper(gRNA), mh, cutSite)
  }

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
    fivePrimeF <- paste0("gcgg", fivePrimeFBase)
    fivePrimeR <- paste0("atcc", reverseComplement(fivePrimeFBase))
  }
  
  return(c(fivePrimeF, fivePrimeR))
}

get5PrimeRevFlag <- function(dnaSeq, crisprSeq, passSeq, mh, cutSite, padding){
  homology <- substring(dnaSeq, cutSite + 1, cutSite + mh)
  spacer   <- substring(dnaSeq, cutSite + mh + 1, cutSite + mh + 3)
  nhSpacer <- addNonHBP(spacer)
  
  fivePrimeRevFBase <- paste0(getPadding(padding), homology, nhSpacer, reverseComplement(passSeq))
  
  #Add cloning sites if needed
  if(nchar(passSeq) > 0){
    fivePrimeRevF <- paste0("catgg", reverseComplement(fivePrimeRevFBase),"g")
    fivePrimeRevR <- paste0("ggccg", fivePrimeRevFBase, "g")
    
  } else {
    fivePrimeRevF <- paste0("gcgg", reverseComplement(fivePrimeRevFBase))
    fivePrimeRevR <- paste0("atcc", fivePrimeRevFBase)
    
  }
  
  return(c(fivePrimeRevF, fivePrimeRevR))

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
    threePrimeR <- paste0("ggccg", reverseComplement(threePrimeFBase), "c")
    
  } else {
    threePrimeF <- paste0("aag", threePrimeFBase)
    threePrimeR <- paste0("cgg", reverseComplement(threePrimeFBase))

  }
  
  return(c(threePrimeF, threePrimeR))
  
}

get3PrimeRevFlag <- function(dnaSeq, crisprSeq, gRNA, mh, cutSite){
  #Get the homologous section from the genome
  homology <- substring(toupper(dnaSeq), cutSite - (mh - 1), cutSite)
  #Get the next three nucleotides
  spacer   <- substring(toupper(dnaSeq), cutSite - (mh + 2), cutSite - mh)
  #Generate a three nucleotide-long spacer that is not homologous to spacer
  nhSpacer <- addNonHBP(spacer)
  
  #Create the base five prime oligo
  threePrimeRevFBase <- paste0(gRNA, nhSpacer, homology)
  
  #If a custom guide RNA is used, add restriction enzyme sites
  if(nchar(gRNA) > 0){
    threePrimeRevF <- paste0("aattc", reverseComplement(threePrimeRevFBase), "c")
    threePrimeRevR <- paste0("ggatc", threePrimeRevFBase, "c")
  } else {
    threePrimeRevF <- paste0("aag", reverseComplement(threePrimeRevFBase))
    threePrimeRevR <- paste0("cgg", threePrimeRevFBase)
  }
  
  return(c(threePrimeRevF, threePrimeRevR))
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
      set.seed(21)
      newNuc <- sample(c("C", "G", "T"), 1)
    } else if ((n == "C")|(n == "c")){
      set.seed(21)
      newNuc <- sample(c("A", "G", "T"), 1)
    } else if ((n == "G")|(n == "g")){
      set.seed(21)
      newNuc <- sample(c("A", "C", "T"), 1)
    } else if ((n == "T")|(n == "t")){
      set.seed(21)
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
getEnsemblSeq <- function(dset, geneId, targetSeq, gRNA, mh){
  #Create dataset with specified mart
  gMart <- useMart("ensembl", dataset = dset)
  
  seq <- "Sequence unavailable"
  counter <- 0
  
  while(seq == "Sequence unavailable" && counter < 5){
    #Get the coding sequences corresponding to the given gene ID
    seq <- biomaRt:::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "coding", mart = gMart)
    counter <- counter + 1
  }
  
  #Identify which target sequences have the target
  codingSeqTarget <- seq[grep(targetSeq, seq), 1]
  
  #Identify if the target sequence is in the reverse complement
  codingSeqRevComp <- seq[grep(reverseComplement(targetSeq), seq), 1]
  
  #If the target sequence is not found in either the forward or reverse direction
  if((length(codingSeqTarget) == 0) && (length(codingSeqRevComp) == 0)){
    return(c("Error: Target sequence is not present in ENSEMBL sequence.", "", "", ""))
    
    #If the target sequence appears more than once in the forward or reverse direction
  } else if((length(codingSeqTarget) > 1) || (length(codingSeqRevComp) > 1)){
    return(c("Error: Target sequence appears multiple times in ENSEMBL entry. Please enter a different target sequence.", "", "", ""))
    
    #If the target sequence appears more than once in the forward AND reverse direction.
  } else if(length(codingSeqTarget) > 0 && length(codingSeqRevComp) > 0){
    return(c("Error: Target sequence appears in the forward and reverse direction. Please enter a different target sequence.", "", "", ""))
    
  } else {
    
    #If the target sequence appears exactly once in forward direction
    if(length(codingSeqTarget > 0)){
      #Identify how many times target occurs in coding sequence
      targetLocation <- unlist(str_locate_all(codingSeqTarget, targetSeq))
      
      #If the target sequence appears more than once in this coding sequence
      if(length(targetLocation) > 2){
        return(c("Error: Target sequence appears multiple times in ENSEMBL entry. Please enter a different target sequence.", "", "", ""))
        
        
      } else {
        
        #Determine the # of padding nucleotides needed to fix break
        padding <- if(((targetLocation[1] - 1) %% 3) == 0){
          0
        } else if(((targetLocation[1] - 1) %% 3) == 1){
          2
        } else if(((targetLocation[1] - 1) %% 3) == 2){
          1
        }
        return(doCalculations(codingSeqTarget, targetSeq, gRNA, mh, padding))
      }
    } 
  }
}


#Match Ensembl Gene Id to species
getEnsemblSpecies <- function(inGeneId){
  geneId <- toupper(inGeneId)
  if(nchar(geneId) < 6){
    return(-1)
  }
  if(!grepl("[0-9]+", geneId)){
    return(-1)
  }
  if((substr(geneId, 1, 2) != "FB") && (substr(geneId, 1, 3) != "ENS")){
    return(-1)
  } else {
    if(substr(geneId, 1, 4) == "FBGN"){
      return(c("dmelanogaster_gene_ensembl", "Drosophila melanogaster (Fruitfly)"))
    } else {
      if(substr(geneId, 4, 7) == "CSAV"){
        return(c("csavignyi_gene_ensembl", "Ciona savignyi"))
      } else {
        m3 <- substr(geneId, 4, 6)
        if(m3 == "AME"){
          return(c("amelanoleuca_gene_ensembl", "Ailuropoda melanoleuca (Panda)"))
        } else if(m3 == "APL"){
          return(c("aplatyrhynchos_gene_ensembl", "Anas platyrhynchos (Duck)"))
        } else if(m3 == "ACA"){
          return(c("acarolinensis_gene_ensembl", "Anolis carolinensis (Anole lizard)"))
        } else if(m3 == "AMX"){
          return(c("amexicanus_gene_ensembl", "Astyanax mexicanus (Cave fish)"))
        } else if(m3 == "BTA"){
          return(c("btaurus_gene_ensembl", "Bos taurus (Cow)"))
        } else if(m3 == "CEL"){
          return(c("celegans_gene_ensembl", "Caenorhabditis elegans"))
        } else if(m3 == "CJA"){
          return(c("cjacchus_gene_ensembl", "Callithrix jacchus (Marmoset)"))
        } else if(m3 == "CAF"){
          return(c("cfamiliaris_gene_ensembl", "Canis lupus familiaris (Dog)"))
        } else if(m3 == "CPO"){
          return(c("cporcellus_gene_ensembl", "Cavia porcellus (Guinea Pig)"))
        } else if(m3 == "CSA"){
          return(c("csabaeus_gene_ensembl", "Chlorocebus sabaeus (Vervet-AGM)"))
        } else if(m3 == "CHO"){
          return(c("choffmanni_gene_ensembl", "Choloepus hoffmanni (Sloth)"))
        } else if(m3 == "CIN"){
          return(c("cintestinalis_gene_ensembl", "Ciona intestinalis"))
        } else if(m3 == "DAR"){
          return(c("drerio_gene_ensembl", "Danio rerio (Zebrafish)"))
        } else if(m3 == "DNO"){
          return(c("dnovemcinctus_gene_ensembl", "Dasypus novemcinctus (Armadillo)"))
        } else if(m3 == "DOR"){
          return(c("dordii_gene_ensembl", "Dipodomys ordii (Kangaroo rat)"))
        } else if(m3 == "ETE"){
          return(c("etelfairi_gene_ensembl", "Echinops telfairi (Lesser hedgehog tenrec)"))
        } else if(m3 == "ECA"){
          return(c("ecaballus_gene_ensembl", "Equus caballus (Horse)"))
        } else if(m3 == "EEU"){
          return(c("eeuropaeus_gene_ensembl", "Erinaceus europaeus (Hedgehog)"))
        } else if(m3 == "FCA"){
          return(c("fcatus_gene_ensembl", "Felis catus (Cat)"))
        } else if(m3 == "FAL"){
          return(c("falbicollis_gene_ensembl", "Ficedula albicollis (Flycatcher)"))
        } else if(m3 == "GMO"){
          return(c("gmorhua_gene_ensembl", "Gadus morhua (Cod)"))
        } else if(m3 == "GAL"){
          return(c("ggallus_gene_ensembl", "Gallus gallus (Chicken)"))
        } else if(m3 == "GAC"){
          return(c("gaculeatus_gene_ensembl", "Gasterosteus aculeatus (Stickleback)"))
        } else if(m3 == "GGO"){
          return(c("ggorilla_gene_ensembl", "Gorilla gorilla gorilla (Gorilla)"))
        } else if(m3 == "STO"){
          return(c("itridecemlineatus_gene_ensembl", "Ictidomys tridecemlineatus (Squirrel)"))
        } else if(m3 == "LAC"){
          return(c("lchalumnae_gene_ensembl", "Latimeria chalumnae (Coelacanth)"))
        } else if(m3 == "LOC"){
          return(c("loculatus_gene_ensembl", "Lepisosteus oculatus (Spotted gar)"))
        } else if(m3 == "LAF"){
          return(c("lafricana_gene_ensembl", "Loxodonta africana (Elephant)"))
        } else if(m3 == "MMU"){
          return(c("mmulatta_gene_ensembl", "Macaca mulatta (Macaque)"))
        } else if(m3 == "MEU"){
          return(c("meugenii_gene_ensembl", "Macropus eugenii (Wallaby)"))
        } else if(m3 == "MGA"){
          return(c("mgallopavo_gene_ensembl", "Meleagris gallopavo (Turkey)"))
        } else if(m3 == "MIC"){
          return(c("mmurinus_gene_ensembl", "Microcebus murinus (Mouse Lemur)"))
        } else if(m3 == "MOD"){
          return(c("mdomestica_gene_ensembl", "Monodelphis domestica (Opossum)"))
        } else if(m3 == "MUS"){
          return(c("mmusculus_gene_ensembl", "Mus musculus (Mouse)"))
        } else if(m3 == "MPU"){
          return(c("mfuro_gene_ensembl", "Mustela putorius furo (Ferret)"))
        } else if(m3 == "MLU"){
          return(c("mlucifugus_gene_ensembl", "Myotis lucifugus (Microbat)"))
        } else if(m3 == "NLE"){
          return(c("nleucogenys_gene_ensembl", "Nomascus leucogenys (Gibbon)"))
        } else if(m3 == "OPR"){
          return(c("oprinceps_gene_ensembl", "Ochotona princeps (Pika)"))
        } else if(m3 == "ONI"){
          return(c("oniloticus_gene_ensembl", "Oreochromis niloticus (Tilapia)"))
        } else if(m3 == "OAN"){
          return(c("oanatinus_gene_ensembl", "Ornithorhynchus anatinus (Platypus)"))
        } else if(m3 == "OCU"){
          return(c("ocuniculus_gene_ensembl", "Oryctolagus cuniculus (Rabbit)"))
        } else if(m3 == "ORL"){
          return(c("olatipes_gene_ensembl", "Oryzias latipes (Medaka)"))
        } else if(m3 == "OGA"){
          return(c("ogarnettii_gene_ensembl", "Otolemur garnettii (Bushbaby)"))
        } else if(m3 == "OAR"){
          return(c("oaries_gene_ensembl", "Ovis aries (Sheep)"))
        } else if(m3 == "PTR"){
          return(c("ptroglodytes_gene_ensembl", "Pan troglodytes (Chimpanzee)"))
        } else if(m3 == "PAN"){
          return(c("panubis_gene_ensembl", "Papio anubis (Olive baboon)"))
        } else if(m3 == "PSI"){
          return(c("psinensis_gene_ensembl", "Pelodiscus sinensis (Chinese softshell turtle)"))
        } else if(m3 == "PMA"){
          return(c("pmarinus_gene_ensembl", "Petromyzon marinus (Lamprey)"))
        } else if(m3 == "PFO"){
          return(c("pformosa_gene_ensembl", "Poecilia formosa (Amazon molly)"))
        } else if(m3 == "PPY"){
          return(c("pabelii_gene_ensembl", "Pongo abelii (Orangutan)"))
        } else if(m3 == "PCA"){
          return(c("pcapensis_gene_ensembl", "Procavia capensis (Hyrax)"))
        } else if(m3 == "PVA"){
          return(c("pvampyrus_gene_ensembl", "Pteropus vampyrus (Megabat)"))
        } else if(m3 == "RNO"){
          return(c("rnorvegicus_gene_ensembl", "Rattus norvegicus (Rat)"))
        } else if(m3 == "SCE"){
          return(c("scerevisiae_gene_ensembl", "Saccharomyces cerevisiae (Yeast)"))
        } else if(m3 == "SHA"){
          return(c("sharrisii_gene_ensembl", "Sarcophilus harrisii (Tasmanian devil)"))
        } else if(m3 == "SAR"){
          return(c("saraneus_gene_ensembl", "Sorex araneus (Shrew)"))
        } else if(m3 == "SSC"){
          return(c("sscrofa_gene_ensembl", "Sus scrofa (Pig)"))
        } else if(m3 == "TGU"){
          return(c("tguttata_gene_ensembl", "Taeniopygia guttata (Zebra Finch)"))
        } else if(m3 == "TRU"){
          return(c("trubripes_gene_ensembl", "Takifugu rubripes (Fugu)"))
        } else if(m3 == "TSY"){
          return(c("tsyrichta_gene_ensembl", "Tarsius syrichta (Tarsier)"))
        } else if(m3 == "TNI"){
          return(c("tnigroviridis_gene_ensembl", "Tetraodon nigroviridis (Tetraodon)"))
        } else if(m3 == "TBE"){
          return(c("tbelangeri_gene_ensembl", "Tupaia belangeri (Tree Shrew)"))
        } else if(m3 == "TTR"){
          return(c("ttruncatus_gene_ensembl", "Tursiops truncatus (Dolphin)"))
        } else if(m3 == "VPA"){
          return(c("vpacos_gene_ensembl", "Vicugna pacos (Alpaca)"))
        } else if(m3 == "XET"){
          return(c("xtropicalis_gene_ensembl", "Xenopus tropicalis (Xenopus)"))
        } else if(m3 == "XMA"){
          return(c("xmaculatus_gene_ensembl", "Xiphophorus maculatus (Platyfish)"))
        } else {
          return(c("hsapiens_gene_ensembl", "Homo sapiens (Human)"))
        }
      }
    }
  }
}


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
