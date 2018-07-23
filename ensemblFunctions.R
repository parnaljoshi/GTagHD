########################
#Ensembl stuff
#' getEnsemblSeq
#'
#' @param dset 
#' @param geneId 
#' @param targetSeq 
#' @param gRNA 
#' @param mh 
#'
#' @return
#' @export
#'
#' @examples

#getEnsemblSeq <- function(geneId, targetSeq, gRNA, mh, toolType){
getEnsemblSeq <- function(geneId, targetSeq, gRNA, mh){
  #Identifies the mart and dataset used in biomaRt retrieval
  spec     <- getEnsemblSpecies(geneId)
  seq      <- "Sequence unavailable"
  counter  <- 0
  endOfTry <- FALSE
  
  #Attempt to retrieve the Ensembl ID from Ensembl. Note that biomaRt is wonky, so multiple attempts at retrieval are made.
  gMart  <- tryCatch({
        #Create dataset with mart and dataset identified from accession ID
        biomaRt::useMart(spec[[1]], dataset = spec[[2]])
    
  }, error = function(err){
    return(1)
    
  })
  
  if(gMart != 1){
    #if(toolType == 0){ #pGTag
    #Get the full sequence
    seq   <- biomaRt::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "gene_exon_intron", mart = gMart)
    #Get the exon sequences
    exons <- biomaRt::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "gene_exon",        mart = gMart)

    #} else if(toolType == 1){ #pPRISM
    #  seq <- biomaRt::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "gene_exon_intron", mart = gMart)
    
    #} else if(toolType == 2){ #pPRISM-Fuse
    #  seq <- biomaRt::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "coding",           mart = gMart)
    
    #} else if(toolType == 3){ #pPRISM-Splicer
    #  seq <- biomaRt::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "gene_exon",        mart = gMart)
    
    #} else if(toolType == 4){ #UFlip
    #  seq <- biomaRt::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "coding",           mart = gMart)
    
    #} else if(toolType == 5){ #custom
    #  seq <- biomaRt::getSequence(id = geneId, type = "ensembl_gene_id", seqType = "coding",           mart = gMart)
    #} 
    
    targetExon        <- exons[grep(targetSeq,                    exons), 1]
    targetExonRevComp <- exons[grep(reverseComplement(targetSeq), exons), 1]
    
    #If the target sequence is not found in either the forward or reverse direction while searching the exons
    if((length(targetExon) == 0) && (length(targetExonRevComp) == 0)){
      #Check if the target sequence ran over the edge of the exon
      #Search in the forward direction for the target CRISPR
      codingSeqTarget  <- seq[grep(targetSeq, seq), 1]
      
      #Search the reverse complement for the target CRISPR
      codingSeqRevComp <- seq[grep(reverseComplement(targetSeq), seq), 1]
      
      exonLocs  <- stringr::str_locate_all(seq, exons[, 1])
      starts    <- sapply(exonLocs, "[", 1)
      ends      <- sapply(exonLocs, "[", 2)
      exonTable <- data.frame(starts = starts, end = ends, stringsAsFactors = FALSE)

      
      if((length(codingSeqTarget) == 0) && (length(codingSeqRevComp) == 0)){
      #If the target sequence is not found at all
      return(c("Error: Target sequence is not present in Ensembl sequence.", "", "", ""))
        
      } else {
        #If the target sequence is found in the forward direction, overlapping an exon
        if((length(codingSeqTarget == 1)) && (length(codingSeqRevComp) == 0)){
          orientation <- 0
          locus <- stringr::str_locate_all(seq, targetSeq)
          start <- locus[[1]][1]
          end   <- locus[[1]][2]
          
          overlap1 <- exonTable[which(exonTable$starts < start && exonTable$ends > start), ]
          overlap2 <- exonTable[which(exonTable$starts < end   && exonTable$ends > end),   ]
          
          if(       (nrow(overlap1) == 1) && (nrow(overlap2 == 0))){
            
          } else if((nrow(overlap1) == 0) && (nrow(overlap2) == 1)){
            
          }
          
        } else if((length(codingSeqTarget == 0)) && (length(codingSeqRevComp) == 1)){
          orientation <- 1
          
          locus <- stringr::str_locate_all(seq, targetSeq)
          start <- locus[[1]][1]
          end   <- locus[[1]][2]
          
          overlap1 <- exonTable[which(exonTable$starts < start && exonTable$ends > start), ]
          overlap2 <- exonTable[which(exonTable$starts < end   && exonTable$ends > end),   ]
          
          if(       (nrow(overlap1) == 1) && (nrow(overlap2 == 0))){
            
          } else if((nrow(overlap1) == 0) && (nrow(overlap2) == 1)){
            
          }
          
          
          
        } else {
          
          
        }
      }
      #If the target sequence appears more than once in the forward or reverse direction
    } else if((length(codingSeqTarget) > 1) || (length(codingSeqRevComp) > 1)){
      return(c("Error: Target sequence appears multiple times in Ensembl entry. Please enter a different target sequence.", "", "", ""))
      
      #If the target sequence appears more than once in the forward AND reverse direction.
    } else if((length(codingSeqTarget) > 0) && (length(codingSeqRevComp) > 0)){
      return(c("Error: Target sequence appears in the forward and reverse direction. Please enter a different target sequence.", "", "", ""))
      
    } else {
      
      #If the target sequence appears exactly once in forward OR exactly once in the reverse direction
      #if(length(codingSeqTarget > 0)){
      #Identify how many times target occurs in coding sequence
      targetLocation <- unlist(str_locate_all(codingSeqTarget, codingSeqRevComp))
      
      #If the target sequence appears more than once in this coding sequence
      if(length(targetLocation) > 2){
        return(c("Error: Target sequence appears multiple times in Ensembl entry. Please enter a different target sequence.", "", "", ""))
        
      } else {
        #Determine orientation
        if(length(codingSeqTarget == 1)){
          orientation = 0
        } else if(length(codingSeqRevComp == 1)){
          orientation = 1
        }
        
        #Determine the # of padding nucleotides needed to fix break
        padding <- if(((targetLocation[1] - 1) %% 3) == 0){
          0
        } else if(((    targetLocation[1] - 1) %% 3) == 1){
          2
        } else if(((    targetLocation[1] - 1) %% 3) == 2){
          1
        }
        
        return(doCalculations(targetLocation, targetSeq, gRNA, mh, padding, orientation))
      }
      #} 
    }
  } else {
    return(1)
  }
}

#Match Ensembl Gene Id to species
#' Title
#'
#' @param inGeneId 
#'
#' @return
#' @export
#'
#' @examples

getEnsemblSpecies <- function(inGeneId){
  geneId <- toupper(inGeneId)
  if(nchar(geneId) < 6){
    return(-1)
  }
  if(!grepl("[0-9]+", geneId)){
    return(-1)
  }
  if((substr(geneId, 1, 2) != "FB") && (substr(geneId, 1, 3) != "ENS") && (substr(geneId, 1, 3) != "MGP")){
    return(-1)
    
  } else {
    if(substr(geneId, 1, 2) == "FB"){
      return(c("ENSEMBL_MART_ENSEMBL", "dmelanogaster_gene_ensembl", "Drosophila melanogaster (Fruitfly)"))
      
    } else {
      if(substr(geneId, 4, 7) == "CSAV"){
        return(c("ENSEMBL_MART_ENSEMBL", "csavignyi_gene_ensembl", "Ciona savignyi"))
        
      } else if(substr(geneId, 1, 3) == "MGP"){
        #Mouse datasets
        m3 <- gsub("_", "", substr(geneId, 4, nchar(geneId)))
        
        if(m3 == "PAHARIEIJ"){
          return(c("ENSEMBL_MART_ENSEMBL",   "mpahari_gene_ensembl", "Mus pahari (Shrew Mouse"))
          
        } else if(m3 == "129S1SVLMJ"){
          return(c("ENSEMBL_MART_MOUSE", "m129s1svimj_gene_ensembl", "Mus musculus (Mouse 129S1/SvlmJ"))
          
        } else if(m3 == "WSBEIJ"){
          return(c("ENSEMBL_MART_MOUSE",     "mwsbeij_gene_ensembl", "Mus musculus domesticus (Mouse WSB/EiJ)"))
          
        } else if(m3 == "NODSHILTJ"){
          return(c("ENSEMBL_MART_MOUSE",  "mnodshiltj_gene_ensembl", "Mus musculus (Mouse NOD/ShiLtJ"))
          
        } else if(m3 == "C3HHEJ"){
          return(c("ENSEMBL_MART_MOUSE",     "mc3hhej_gene_ensembl", "Mus musculus (Mouse C3H/HeJ)"))
          
        } else if(m3 == "LPJ"){
          return(c("ENSEMBL_MART_MOUSE",        "mlpj_gene_ensembl", "Mus musculus (Mouse LP/J)"))
          
        } else if(m3 == "NZOHILTJ"){
          return(c("ENSEMBL_MART_MOUSE",   "mnzohlltj_gene_ensembl", "Mus musculus (Mouse NZO/HILtJ)"))
          
        } else if(m3 == "BALBCJ"){
          return(c("ENSEMBL_MART_MOUSE",     "mbalbcj_gene_ensembl", "Mus musculus (Mouse BALB/cJ"))
          
        } else if(m3 == "CASTEIJ"){
          return(c("ENSEMBL_MART_MOUSE",    "mcasteij_gene_ensembl", "Mus musculus castaneus (Mouse CAST/EiJ)"))
          
        } else if(m3 == "CBAJ"){
          return(c("ENSEMBL_MART_MOUSE",       "mcbaj_gene_ensembl", "Mus musculus (Mouse CBA/J)"))
          
        } else if(m3 == "FVBNJ"){
          return(c("ENSEMBL_MART_MOUSE",      "mfvbnj_gene_ensembl", "Mus musculus (Mouse FVB/NJ)"))
          
        } else if(m3 == "PWKPHJ"){
          return(c("ENSEMBL_MART_MOUSE",     "mpwkphj_gene_ensembl", "Mus musculus musculus (Mouse PWK/PhJ)"))
          
        } else if(m3 == "CAROLIEIJ"){
          return(c("ENSEMBL_MART_ENSEMBL",   "mcaroli_gene_ensembl", "Mus caroli (Ryukyu mouse)"))
          
        } else if(m3 == "SPRETEIJ"){
          return(c("ENSEMBL_MART_ENSEMBL", "mspreteij_gene_ensembl", "Mus spretus (Algerian mouse)"))
          
        } else if(m3 == "DBA2J"){
          return(c("ENSEMBL_MART_MOUSE",      "mdba2j_gene_ensembl", "Mus musculus (Mouse DBA/2J)"))
          
        } else if(m3 == "C57BL6NJ"){
          return(c("ENSEMBL_MART_MOUSE",   "mc57bl6nj_gene_ensembl", "Mus musculus (Mouse C57BL/6NJ)"))
          
        } else if(m3 == "AJ"){
          return(c("ENSEMBL_MART_MOUSE",         "maj_gene_ensembl", "Mus musculus (Mouse A/J)"))
          
        } else if(m3 == "AKRJ"){
          return(c("ENSEMBL_MART_MOUSE",       "makrj_gene_ensembl", "Mus musculus (Mouse AKR/J)"))
          
        } else {
          return(-1)
          
        }
        
      } else {
        #ENS prefix datasets
        m3 <- substr(geneId, 4, 6)
        
        if(m3 == "AME"){
          return(c("ENSEMBL_MART_ENSEMBL",      "amelanoleuca_gene_ensembl", "Ailuropoda melanoleuca (Panda)"))
          
        } else if(m3 == "APL"){
          return(c("ENSEMBL_MART_ENSEMBL",    "aplatyrhynchos_gene_ensembl", "Anas platyrhynchos (Duck)"))
          
        } else if(m3 == "ACA"){
          return(c("ENSEMBL_MART_ENSEMBL",     "acarolinensis_gene_ensembl", "Anolis carolinensis (Anole lizard)"))
          
        } else if(m3 == "AMX"){
          return(c("ENSEMBL_MART_ENSEMBL",        "amexicanus_gene_ensembl", "Astyanax mexicanus (Cave fish)"))
          
        } else if(m3 == "ANA"){
          return(c("ENSEMBL_MART_ENSEMBL",             "aotus_gene_ensembl", "Aotus nancymaae (Ma's Night Monkey)"))
          
        }	else if(m3 == "BTA"){
          return(c("ENSEMBL_MART_ENSEMBL",           "btaurus_gene_ensembl", "Bos taurus (Cow)"))
          
        } else if(m3 == "CAN"){
          return(c("ENSEMBL_MART_ENSEMBL",        "cpalliatus_gene_ensembl", "Colobus angolensis palliatus (Angola Colobus)"))
          
        } else if(m3 == "CEL"){
          return(c("ENSEMBL_MART_ENSEMBL",          "celegans_gene_ensembl", "Caenorhabditis elegans"))
          
        } else if(m3 == "CJA"){
          return(c("ENSEMBL_MART_ENSEMBL",          "cjacchus_gene_ensembl", "Callithrix jacchus (Marmoset)"))
          
        } else if(m3 == "CAF"){
          return(c("ENSEMBL_MART_ENSEMBL",       "cfamiliaris_gene_ensembl", "Canis lupus familiaris (Dog)"))
          
        } else if(m3 == "CAP"){
          return(c("ENSEMBL_MART_ENSEMBL",           "caperea_gene_ensembl", "Cavia aperea (Brazilian guinea pig"))
          
        } else if(m3 == "CAT"){
          return(c("ENSEMBL_MART_ENSEMBL",             "catys_gene_ensembl", "Cercoceebus atys (Sooty Mangabey)"))
          
        } else if(m3 == "CCA"){
          return(c("ENSEMBL_MART_ENSEMBL",        "ccapucinus_gene_ensembl", "Cebus capucinus imitator (Capuchin)"))
          
        } else if(m3 == "CGR"){
          return(c("ENSEMBL_MART_ENSEMBL",           "ccrigri_gene_ensembl", "Cricetulus griseus (Chinese hamster)"))
          
        } else if(m3 == "CLA"){
          return(c("ENSEMBL_MART_ENSEMBL",         "clanigera_gene_ensembl", "Chinchilla lanigera (Long-tailed chinchilla)"))
          
        } else if(m3 == "CPO"){
          return(c("ENSEMBL_MART_ENSEMBL",        "cporcellus_gene_ensembl", "Cavia porcellus (Guinea Pig)"))
          
        } else if(m3 == "CSA"){
          return(c("ENSEMBL_MART_ENSEMBL",          "csabaeus_gene_ensembl", "Chlorocebus sabaeus (Vervet-AGM)"))
          
        } else if(m3 == "CHO"){
          return(c("ENSEMBL_MART_ENSEMBL",        "choffmanni_gene_ensembl", "Choloepus hoffmanni (Sloth)"))
          
        } else if(m3 == "CIN"){
          return(c("ENSEMBL_MART_ENSEMBL",     "cintestinalis_gene_ensembl", "Ciona intestinalis"))
          
        } else if(m3 == "DAR"){
          return(c("ENSEMBL_MART_ENSEMBL",            "drerio_gene_ensembl", "Danio rerio (Zebrafish)"))
          
        } else if(m3 == "DNO"){
          return(c("ENSEMBL_MART_ENSEMBL",     "dnovemcinctus_gene_ensembl", "Dasypus novemcinctus (Armadillo)"))
          
        } else if(m3 == "DOR"){
          return(c("ENSEMBL_MART_ENSEMBL",            "dordii_gene_ensembl", "Dipodomys ordii (Kangaroo rat)"))
          
        } else if(m3 == "ETE"){
          return(c("ENSEMBL_MART_ENSEMBL",         "etelfairi_gene_ensembl", "Echinops telfairi (Lesser hedgehog tenrec)"))
          
        } else if(m3 == "ECA"){
          return(c("ENSEMBL_MART_ENSEMBL",         "ecaballus_gene_ensembl", "Equus caballus (Horse)"))
          
        } else if(m3 == "EEU"){
          return(c("ENSEMBL_MART_ENSEMBL",        "eeuropaeus_gene_ensembl", "Erinaceus europaeus (Hedgehog)"))
          
        } else if(m3 == "ETE"){
          return(c("ENSEMBL_MART_ENSEMBL",          "jjaculus_gene_ensembl", "Jaculus jaculus (Lesser Egyptian jerboa"))
          
        } else if(m3 == "FCA"){
          return(c("ENSEMBL_MART_ENSEMBL",            "fcatus_gene_ensembl", "Felis catus (Cat)"))
          
        } else if(m3 == "FAL"){
          return(c("ENSEMBL_MART_ENSEMBL",       "falbicollis_gene_ensembl", "Ficedula albicollis (Flycatcher)"))
          
        } else if(m3 == "FDA"){
          return(c("ENSEMBL_MART_ENSEMBL",       "fdamarensis_gene_ensembl", "Fukomys damarensis (Damara mole rat)"))
          
        }	else if(m3 == "GMO"){
          return(c("ENSEMBL_MART_ENSEMBL",           "gmorhua_gene_ensembl", "Gadus morhua (Cod)"))
          
        } else if(m3 == "GAL"){
          return(c("ENSEMBL_MART_ENSEMBL",           "ggallus_gene_ensembl", "Gallus gallus (Chicken)"))
          
        } else if(m3 == "GAC"){
          return(c("ENSEMBL_MART_ENSEMBL",        "gaculeatus_gene_ensembl", "Gasterosteus aculeatus (Stickleback)"))
          
        } else if(m3 == "GGO"){
          return(c("ENSEMBL_MART_ENSEMBL",          "ggorilla_gene_ensembl", "Gorilla gorilla gorilla (Gorilla)"))
          
        } else if(m3 == "GLF"){
          return(c("ENSEMBL_MART_ENSEMBL",           "hfemale_gene_ensembl", "Heterocephalus glaber (Naked mole-rat female"))
          
        } else if(m3 == "GLM"){
          return(c("ENSEMBL_MART_ENSEMBL",             "hmale_gene_ensembl", "Heterocephalus glaber (Naked mole-rat male"))
          
        } else if(m3 == "STO"){
          return(c("ENSEMBL_MART_ENSEMBL", "itridecemlineatus_gene_ensembl", "Ictidomys tridecemlineatus (Squirrel)"))
          
        } else if(m3 == "LAC"){
          return(c("ENSEMBL_MART_ENSEMBL",        "lchalumnae_gene_ensembl", "Latimeria chalumnae (Coelacanth)"))
          
        } else if(m3 == "LOC"){
          return(c("ENSEMBL_MART_ENSEMBL",         "loculatus_gene_ensembl", "Lepisosteus oculatus (Spotted gar)"))
          
        } else if(m3 == "LAF"){
          return(c("ENSEMBL_MART_ENSEMBL",         "lafricana_gene_ensembl", "Loxodonta africana (Elephant)"))
          
        } else if(m3 == "MAU"){
          return(c("ENSEMBL_MART_ENSEMBL",          "mauratus_gene_ensembl", "Mesocricetus auratus (Golden hamster)"))
          
        } else if(m3 == "MMU"){
          return(c("ENSEMBL_MART_ENSEMBL",          "mmulatta_gene_ensembl", "Macaca mulatta (Macaque)"))
          
        } else if(m3 == "MEU"){
          return(c("ENSEMBL_MART_ENSEMBL",          "neugenii_gene_ensembl", "Macropus eugenii (Wallaby)"))
          
        } else if(m3 == "MFA"){
          return(c("ENSEMBL_MART_ENSEMBL",     "mfascicularis_gene_ensembl", "Macaca fascicularis (Crab-eating macaque"))
          
        }	else if(m3 == "MGA"){
          return(c("ENSEMBL_MART_ENSEMBL",        "mgallopavo_gene_ensembl", "Meleagris gallopavo (Turkey)"))
          
        } else if(m3 == "MIC"){
          return(c("ENSEMBL_MART_ENSEMBL",          "mmurinus_gene_ensembl", "Microcebus murinus (Mouse Lemur)"))
          
        } else if(m3 == "MLE"){
          return(c("ENSEMBL_MART_ENSEMBL",      "mleucophaeus_gene_ensembl", "Mandrillus leucophaeus (Drill)"))
          
        } else if(m3 == "MNE"){
          return(c("ENSEMBL_MART_ENSEMBL",       "mnemestrina_gene_ensembl", "Macaca nemestrina (Pig-tailed macaque"))
          
        } else if(m3 == "MOC"){
          return(c("ENSEMBL_MART_ENSEMBL",      "mochrogaster_gene_ensembl", "Microtus ochrogaster (Prairie vole"))
          
        } else if(m3 == "MOD"){
          return(c("ENSEMBL_MART_ENSEMBL",        "mdomestica_gene_ensembl", "Monodelphis domestica (Opossum)"))
          
        } else if(m3 == "MUS"){
          return(c("ENSEMBL_MART_ENSEMBL",         "mmusculus_gene_ensembl", "Mus musculus (Mouse)"))
          
        } else if(m3 == "MPU"){
          return(c("ENSEMBL_MART_ENSEMBL",             "mfuro_gene_ensembl", "Mustela putorius furo (Ferret)"))
          
        } else if(m3 == "MLU"){
          return(c("ENSEMBL_MART_ENSEMBL",        "mlucifugus_gene_ensembl", "Myotis lucifugus (Microbat)"))
          
        } else if(m3 == "NGA"){
          return(c("ENSEMBL_MART_ENSEMBL",           "ngalili_gene_ensembl", "Nannospalax galili (Upper Galilee mountains blind mole rat"))
          
        } else if(m3 == "NLE"){
          return(c("ENSEMBL_MART_ENSEMBL",       "nleucogenys_gene_ensembl", "Nomascus leucogenys (Gibbon)"))
          
        } else if(m3 == "OPR"){
          return(c("ENSEMBL_MART_ENSEMBL",         "oprinceps_gene_ensembl", "Ochotona princeps (Pika)"))
          
        } else if(m3 == "ONI"){
          return(c("ENSEMBL_MART_ENSEMBL",        "oniloticus_gene_ensembl", "Oreochromis niloticus (Tilapia)"))
          
        } else if(m3 == "OAN"){
          return(c("ENSEMBL_MART_ENSEMBL",         "oanatinus_gene_ensembl", "Ornithorhynchus anatinus (Platypus)"))
          
        } else if(m3 == "ODE"){
          return(c("ENSEMBL_MART_ENSEMBL",            "odegus_gene_ensembl", "Octodon degus (Degu)"))
          
        } else if(m3 == "OCU"){
          return(c("ENSEMBL_MART_ENSEMBL",        "ocuniculus_gene_ensembl", "Oryctolagus cuniculus (Rabbit)"))
          
        } else if(m3 == "ORL"){
          return(c("ENSEMBL_MART_ENSEMBL",          "olatipes_gene_ensembl", "Oryzias latipes (Medaka)"))
          
        } else if(m3 == "OGA"){
          return(c("ENSEMBL_MART_ENSEMBL",        "ogarnettii_gene_ensembl", "Otolemur garnettii (Bushbaby)"))
          
        } else if(m3 == "OAR"){
          return(c("ENSEMBL_MART_ENSEMBL",            "oaries_gene_ensembl", "Ovis aries (Sheep)"))
          
        } else if(m3 == "PEM"){
          return(c("ENSEMBL_MART_ENSEMBL",          "pbairdii_gene_ensembl", "Peromyscus maniculatus bairdii (Northern American deer mouse)"))
          
        } else if(m3 == "PTR"){
          return(c("ENSEMBL_MART_ENSEMBL",      "ptroglodytes_gene_ensembl", "Pan troglodytes (Chimpanzee)"))
          
        } else if(m3 == "PAN"){
          return(c("ENSEMBL_MART_ENSEMBL",           "panubis_gene_ensembl", "Papio anubis (Olive baboon)"))
          
        } else if(m3 == "PCO"){
          return(c("ENSEMBL_MART_ENSEMBL",        "pcoquereli_gene_ensembl", "Propithecus coquereli (Coquerel's sifaka)"))
          
        } else if(m3 == "PSI"){
          return(c("ENSEMBL_MART_ENSEMBL",         "psinensis_gene_ensembl", "Pelodiscus sinensis (Chinese softshell turtle)"))
          
        } else if(m3 == "PMA"){
          return(c("ENSEMBL_MART_ENSEMBL",          "pmarinus_gene_ensembl", "Petromyzon marinus (Lamprey)"))
          
        } else if(m3 == "PFO"){
          return(c("ENSEMBL_MART_ENSEMBL",          "pformosa_gene_ensembl", "Poecilia formosa (Amazon molly)"))
          
        } else if(m3 == "PPA"){
          return(c("ENSEMBL_MART_ENSEMBL",         "ppaniscus_gene_ensembl", "Pan pansicus (Bonobo)"))
          
        }	else if(m3 == "PPY"){
          return(c("ENSEMBL_MART_ENSEMBL",           "pabelii_gene_ensembl", "Pongo abelii (Orangutan)"))
          
        } else if(m3 == "PCA"){
          return(c("ENSEMBL_MART_ENSEMBL",         "pcapensis_gene_ensembl", "Procavia capensis (Hyrax)"))
          
        } else if(m3 == "PVA"){
          return(c("ENSEMBL_MART_ENSEMBL",         "pvampyrus_gene_ensembl", "Pteropus vampyrus (Megabat)"))
          
        } else if(m3 == "RBI"){
          return(c("ENSEMBL_MART_ENSEMBL",            "rbieti_gene_ensembl", "Rhinopithecus bieti (Black snub-nosed monkey)"))
          
        } else if(m3 == "RNO"){
          return(c("ENSEMBL_MART_ENSEMBL",       "rnorvegicus_gene_ensembl", "Rattus norvegicus (Rat)"))
          
        } else if(m3 == "RRO"){
          return(c("ENSEMBL_MART_ENSEMBL",        "rroxellana_gene_ensembl", "Rhinopithecus roxellana (Golden snub-nosed monkey)"))
          
        } else if(m3 == "SCE"){
          return(c("ENSEMBL_MART_ENSEMBL",       "scerevisiae_gene_ensembl", "Saccharomyces cerevisiae (Yeast)"))
          
        } else if(m3 == "SHA"){
          return(c("ENSEMBL_MART_ENSEMBL",         "sharrisii_gene_ensembl", "Sarcophilus harrisii (Tasmanian devil)"))
          
        } else if(m3 == "SAR"){
          return(c("ENSEMBL_MART_ENSEMBL",          "saraneus_gene_ensembl", "Sorex araneus (Shrew)"))
          
        } else if(m3 == "SBO"){
          return(c("ENSEMBL_MART_ENSEMBL",      "sboliviensis_gene_ensembl", "Saimiri boliviensis boliviensis (Bolivian squirrel monkey)"))
          
        }	else if(m3 == "SSC"){
          return(c("ENSEMBL_MART_ENSEMBL",           "sscrofa_gene_ensembl", "Sus scrofa (Pig)"))
          
        } else if(m3 == "TGU"){
          return(c("ENSEMBL_MART_ENSEMBL",          "tguttata_gene_ensembl", "Taeniopygia guttata (Zebra Finch)"))
          
        } else if(m3 == "TRU"){
          return(c("ENSEMBL_MART_ENSEMBL",         "trubripes_gene_ensembl", "Takifugu rubripes (Fugu)"))
          
        } else if(m3 == "TSY"){
          return(c("ENSEMBL_MART_ENSEMBL",         "csyrichta_gene_ensembl", "Carlito syrichta (Tarsier)"))
          
        } else if(m3 == "TNI"){
          return(c("ENSEMBL_MART_ENSEMBL",     "tnigroviridis_gene_ensembl", "Tetraodon nigroviridis (Tetraodon)"))
          
        } else if(m3 == "TBE"){
          return(c("ENSEMBL_MART_ENSEMBL",        "tbelangeri_gene_ensembl", "Tupaia belangeri (Tree Shrew)"))
          
        } else if(m3 == "TTR"){
          return(c("ENSEMBL_MART_ENSEMBL",        "ttruncatus_gene_ensembl", "Tursiops truncatus (Dolphin)"))
          
        } else if(m3 == "VPA"){
          return(c("ENSEMBL_MART_ENSEMBL",            "vpacos_gene_ensembl", "Vicugna pacos (Alpaca)"))
          
        } else if(m3 == "XET"){
          return(c("ENSEMBL_MART_ENSEMBL",       "xtropicalis_gene_ensembl", "Xenopus tropicalis (Xenopus)"))
          
        } else if(m3 == "XMA"){
          return(c("ENSEMBL_MART_ENSEMBL",        "xmaculatus_gene_ensembl", "Xiphophorus maculatus (Platyfish)"))
          
        } else {
          return(c("ENSEMBL_MART_ENSEMBL",          "hsapiens_gene_ensembl", "Homo sapiens (Human)"))
        }
      }
    }
  }
}