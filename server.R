#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Required packages
library(shiny)
library(shinyjs)
library(stringr)
library(plyr)
#library(biomaRt)
library(rentrez)
library(rlist)
#library(genbankr)
#library(biofiles)

#Required files
source("functions.R")
source("apeShiftFunctions.R")

shinyServer(function(input, output, session) {
  
  #Create global variables to minimize calculations and function calls
  dF <- reactiveValues(downloadF = FALSE)
  dFF <- reactiveValues(downloadF = FALSE)
  oligos <<- NULL
  gbFile <<- NULL
  
  ########################################################
  ####################INPUT VALIDATION####################
  ########################################################
  
        ###########GUIDE RNA##############################
  #Check to ensure that the guide RNA is properly formatted, actually a guide RNA, etc.
  validgRNA <- reactive({
    
    #If using custom guide RNA, ensure that only A, C, G, and T are allowed
    if((input$gRNAtype == 2) && (input$gRNA != "")){
      validate(
        need(!str_detect(toupper(input$gRNA), "[^ACGT]"), 
             paste0("Error: Input guide RNA sequence contains non-standard nucleotides. ",
                    "Allowed nucleotides are A, C, G, and T.")
        )
      )
    } else if((input$gRNAtype == 2) && (input$gRNA == "")){
      #Prevent app crashing on empty submit
      validate(
        need(input$gRNA != "", "")
      )
    }
  })
  
        ###########CRISPR TARGET##########################
  #Ensure that CRISPR sequence is a DNA sequence, is 20 nts long, 
  #and is present in forward/reverse direction exactly once
  validCrisprSeq <- reactive({
    
    #Once text is entered, run through validation tests
    if(input$crisprSeq != ""){
      uCS <- toupper(input$crisprSeq)
      validate(
        #Checks that only A, C, G, and T are in sequence
        need(!str_detect(uCS, "[^ACGT]"), 
             paste0("Error: Input DNA sequence contains non-standard nucleotides. ", 
                    "Allowed nucleotides are A, C, G, and T.")),
        
        #Checks that the target is 23 nucleotides long; needs modification for CRISPR agnostic - DEPRECATED DUE TO VARIABLE PAMS ALLOWED
        #need(nchar(uCS) == 23, 
        #     "Error: CRISPR target sequence must include the 20 bp preceding the PAM, and the PAM sequence.")
        
        #Checks that there is a recognizeable PAM sequence in the forward or reverse (or complement) direction
        #need(((substring(uCS, nchar(uCS) - 1, nchar(uCS)) == "GG") | 
        #        (substring(uCS, nchar(uCS) - 1, nchar(uCS)) == "CC") |
        #        (substring(uCS, 1, 2) == "GG") | 
        #        (substring(uCS, 1, 2) == "CC")),
        #     "Error: CRISRPR target sequence does not have an identifiable PAM sequence. Please check input sequence.")
        
        #Checks that the target is 20 nucleotides long
        need(nchar(uCS) == 20,
             "Error: Input should be 20 nucleotides long.")
      )
    } else {
      #Prevent crashing on empty submit
      validate(
        need(input$crisprSeq != "", "")
      )
    }
    
  })
  
        ###########cDNA###################################
  #Test if cDNA sequence is valid - crispr only appears once in either forward or reverse, etc.
  
  validCDNA <- reactive({
    #Don't bother checking valid cDNA unless the CRISPR is also valid
    if(is.null(validCrisprSeq())){
      #Once text has been input
      if(input$cDNA != ""){
        #Convert sequences to uppercase
        ucDNA <- toupper(input$cDNA)
        uCS <- toupper(input$crisprSeq)
        
        #Ensure that the input DNA is plain text, and only contains A, C, G, and T
        validate(
          need(!str_detect(input$cDNA, "[>]"), 
               "Error: Input DNA sequence appears to be in FASTA format. Please paste plain text."),
          
          need(!str_detect(input$cDNA, "[^ACGTacgt]"), 
               paste0("Error: Input DNA sequence contains non-standard nucleotides. ", 
                      "Allowed nucleotides are A, C, G, and T."))
        )
        
        #Count how many times the input CRISPR target sequence appears in the cDNA sequence in the FORWARD direction
        count <- str_count(ucDNA, uCS)
        
        #Count the instance in the reverse complement of the sequence
        revCount <- str_count(reverseComplement(ucDNA), uCS)
        
        #Check to ensure the target only appears once in a single direction
        if(count > 1){
          validate(
            need(count == 1, paste0("Error: CRISPR target sequence appears ", count, " times in the pasted gene/exon sequence.")),
            
            need(revCount == 0, paste0("Error: The CRISPR target sequence appears ", 
                                       revCount, 
                                       " times on the opposite (reverse complement) strand."))
          )
        } else if (count == 1) {
          validate(
            need(revCount == 0, paste0("Error: The CRISPR target sequence also appears ", 
                                       revCount, 
                                       " times on the opposite strand."))
          )
        } else if (count == 0 && revCount == 0){
          validate(
            need(revCount == 1, paste0("Error: The CRISPR target sequence does not appear ", 
                                       "on the given DNA strand, or in its complement."))
          )
        } else if (count == 0 && revCount > 1){
          validate(
            need(revCount == 1, paste0("Error: The CRISPR target sequence appears ", 
                                       revCount, 
                                       " times on the opposite (reverse complement) strand."))
          )
        } 
      } else {
        #Prevents crashing when submitted while empty
        validate(
          need(input$cDNA != "", "")
        )
      }
    } else {
      #Prevent further empty crashing
      validate(
        need(input$cDNA != "", "")
      )
    }
      
      
    })
  

  
  ######################Microhomology######################
  #Test if microhomology is valid, and troubleshoot if not
  validMHCDna <- reactive({
    if((input$crisprSeq != "") && (input$cDNA != "")){
      if(is.null(validCrisprSeq()) && (is.null(validCDNA()))){
        uDNA <- toupper(input$cDNA)
        uCS <- toupper(input$crisprSeq)
        
        #Count how many times the input CRISPR target sequence appears in the cDNA sequence in the FORWARD direction
        count <- str_count(uDNA, uCS)
        
        #Count the instance in the reverse complement of the sequence
        revCount <- str_count(reverseComplement(uDNA), uCS)
        
        #If the target appears in the reverseComplement of cDNA
        if(revCount == 1 && count == 0){
          uDNA <- reverseComplement(input$cDNA)
        } 
        
        crisprLoc <- unlist(str_locate_all(uDNA, input$crisprSeq))
        
        #If the crispr ends with NGG (it is in the forward direction), cut before the PAM sequence:
        #if((substring(input$crisprSeq, nchar(input$crisprSeq) - 1, nchar(input$crisprSeq)) == "GG") | 
        #   (substring(input$crisprSeq, nchar(input$crisprSeq) - 1, nchar(input$crisprSeq)) == "gg")){
        #  cutIndex <- crisprLoc[2] - 6
        
        #If the crispr begins with CCN (it is in the reverse direction), cut after the PAM sequence:
        #} else if((substring(input$crisprSeq, 1, 2) == "CC") | (substring(input$crisprSeq, 1, 2) == "cc")){
        #  cutIndex <- crisprLoc[1] + 5
        #}
        #If the crispr is in the sense direction
        
        if(input$sense == 0){
          #If the CRISPR is in the sense strand, cut between bases 17 and 18 in the matching sequence.
          cutIndex <- crisprLoc[2] - 3
        } else if(input$sense == 1) {
          #If the CRISPR is in the anti-sense strand, cut between bases 3 and 4 in the matching sequence.
          cutIndex <- crisprLoc[1] + 2
        }
        
        
        if((revCount == 0 && count == 1)||(revCount == 1 && count == 0)){
          validate(
            #Check to make sure that the microhomology length won't run off the beginning of the sequence
            need(cutIndex - as.numeric(input$mh) >= 0, 
                 paste0("Error: Microhomology length is too long for this CRISPR target sequence and pasted gene/exon sequence pairing. ",
                        "Choose a smaller homology length, or provide more sequence context starting from the beginning of the sequence.")),
            #Check to make sure that the microhomology length won't run off the end of the sequence
            need(nchar(uDNA) - as.numeric(input$mh) >= cutIndex,
                 paste0("Error: Microhomology length is too long for this CRISPR target sequence and pasted gene/exon sequence pairing. ",
                        "Choose a smaller homology length, or provide more sequence context at the end of the sequence."))
          )
        } 
      }
    }
  })
  
  ######################Gene ID############################
  #Validate ENSEMBL GeneID
  validGeneId <- reactive({
    if(input$geneId != ""){
      if(length(getEnsemblSpecies) > 1){
        validate(
          need(getEnsemblSpecies(input$geneId) != -1, "Error: The gene ID is not recognized as an ENSEMBL gene ID.")
        )
      } #else {
        #if(!pingGeneId(input$geneId)){
        #  need(!pingGeneId(input$geneId) != -1, paste0("Error: ENSEMBL does not have an entry corresponding to your submitted gene ID: ", input$geneId))
        #}
      #}
    } 
  })
  
  ####################GENBANK INPUTS######################
  #Function to output results of genbank validation
  validGenbankId <- function(){
      validate(
        need(1 == 2,
          "Error: This GenBank accession ID does not exist.")
      )
  }
  
  validRefSeqGBGenbankId <- reactive({
    if(input$genbankId != ""){
      if(stringr::str_detect(input$genbankId, regex("^(NM|NR|XM|XR)_[0-9]{6}", ignore_case = TRUE))){
        validate(
          need(1 == 2,
               "Error: This ID matches RefSeq RNA accession format. Please submit an accession corresponding to a DNA sequence."))
        
      } else if(stringr::str_detect(input$genbankId, regex("^(AP|NP|YP|XP|WP)_[0-9]{6}", ignore_case = TRUE))){
        validate(
          need(1 == 2,
               "Error: This ID matches RefSeq protein accession format. Please submit an accession corresponding to a DNA sequence."))
        
      } else if(stringr::str_detect(input$genbankId, regex("^[A-Z]{3}[0-9]{5}", ignore_case = TRUE))){
        validate(
          need(1 == 2,
               "Error: This ID matches GenBank protein accession format. Please submit an accession corresponding to a DNA sequence."))
        
      } else {
        validate(
          need((((stringr::str_detect(input$genbankId, regex("^[a-zA-Z]{2}[0-9]{6}", ignore_case = TRUE))) | 
                   (stringr::str_detect(input$genbankId, regex("^[a-zA-Z]{1}[0-9]{5}", ignore_case = TRUE)))) |
                  (stringr::str_detect(input$genbankId, regex("^(AC|NC|NG|NT|NW|NZ)_[0-9]{6}\\.[0-9]", ignore_case = TRUE)))) |
                 (stringr::str_detect(input$genbankId, regex("^[a-zA-Z]{4}[0-9]{8,10}", ignore_case = TRUE))),
               
               "Error: The entered ID does not match a known Genbank NUCLEOTIDE or RefSeq NUCLEOTIDE ID format. Please check your submitted accession to make sure you are using a NUCLEOTIDE entry."))
      }
    }
  })
  
  
  #Function to determine if Genbank input has valid exon info
  exonWarningFunc <- reactive({
    if(!is.null(gbFile) && input$genbankId != "" && !is.null(input$genbankId)){
      gba = readGenBank(text = gbFile, partial = TRUE)
      if(length(exons(gba)) < 1){
        "Warning: This GenBank record does not have annotated exon information. Automatic padding generation is disabled."
        updateRadioButtons(session, "paddingChoice", selected = 2)
      }
    }
  })
  
  
  #Function to determine if CRISPR target cut will be in an exon
  
  #Function to determine that CRISPR target appears in Genbank sequence exactly once
  
  #Function to determine if MH length is okay for CRISPR target
  
  
  ########################################################
  ##############PRINT VALIDATION RESULTS##################
  ########################################################
  
  #Print out the results of the guide RNA validation
  output$validgrna <- renderText({
    validgRNA()
  })
  
  #Print out the results of the CRISPR sequence validation
  output$validcrisprseq <- renderText({
    validCrisprSeq()
  })
  
  #Print out the results of the cDNA validation
  output$validcdna <- renderText({
    validCDNA()
  })
  
  output$validmhcdna <- renderText({
    validMHCDna()
  })

  #Print out the results of ENSEMBL gene ID validation
  output$validgeneid <- renderText({
    if(is.null(validGeneId())){
      paste0(getEnsemblSpecies(input$geneId)[2], " ENSEMBL gene ID detected.")
    } else {
      validGeneId()
    }
  })
  
  output$exonWarning <- renderText({
    if(!is.null(gbFile)){
      exonWarningFunc()
    }
  })
  
  output$validrefseqgbgenbankid <- renderText({
      validRefSeqGBGenbankId()
  })
  
  ########################################################
  ################PERFORM CALCULATIONS####################
  ########################################################  
  #Do the calculation for the targeting oligos
  
  observeEvent(input$submit, {
    resetOutputs()
    #Check to ensure that all inputs are valid before accepting
    if(is.null(validgRNA()) &&
       is.null(validCrisprSeq()) &&
       is.null(validCDNA()) &&
       is.null(validMHCDna())){
      revFlag <- FALSE
      resetOutputs()
      
      #Convert inputs to uppercase
      guideRNA <- toupper(input$gRNA)
      ucDNA    <- toupper(input$cDNA)
      uCS      <- toupper(input$crisprSeq)
      

      #Determine if the CRISPR is on the forward or reverse strand
      rev <- revCheck(ucDNA, uCS)
      
      uDNA    <- rev[1]
      revFlag <- rev[2]

      #Set up the progress bar
      progress <- Progress$new(session)
      on.exit(progress$close())
      progress$set(message = "Generating Oligos: ")
      
      #Create the oligos
      oligos <<- doCalculations(uDNA, 
                                uCS, 
                                guideRNA, 
                                as.numeric(input$mh),  
                                input$padding,
                                revFlag,
                                input$sense,
                                progress)
      
      progress$set(detail = "Done", value = 1)
      
      #Flag the download button so that it becomes visible
      dF$downloadF  <<- TRUE
      dFF$downloadF <<- TRUE
      
      
      #Output the oligos
      printOutputs(oligos)
    } 
  })

  
  ####GenBank ID submission ####
  observeEvent(input$genBankSubmit, {
    
    resetOutputs()
    #Check to ensure that all other inputs are valid before proceding
    if(is.null(validgRNA()) &&
       is.null(validCrisprSeq()) &&
       is.null(validRefSeqGBGenbankId())){
      revFlag <- FALSE
      
      #Clear Outputs
      resetOutputs()
      
      #Set up a progress object
      progress <- Progress$new(session)
      on.exit(progress$close())
      progress$set(message = "Generating Oligos: ")
      
      progress$set(detail = "Retrieving GenBank entry...", value = 0.1)
      
      #Try to pull genbank entry associated with accession
      #Get the GenBank sequence with exon/intron information
      #Make a genbank acession object
      #gba <- genbankr:::GBAccession(input$genbankId)
      gba <- input$genbankId
      
      endOfTry <<- FALSE
      gbFlag   <<- FALSE
      gbhFlag  <<- FALSE
      
      #Try to get the accession info - working
      #tryCatch({
      #  info <- readGenBank(gba, partial = TRUE, verbose = FALSE)
        
      #  endOfTry <- TRUE
      #}, error = function(err){
        
       # output$validgenbankid <- renderText({
       #   validGenbankId()
        #})
      #})
      
      if(endOfTry == FALSE){
        tryCatch({
          info <- suppressWarnings(wonkyGenBankHandler(gba))
          
          endOfTry <<- TRUE
          gbhFlag  <<- TRUE  #Flag to indicate wonkyGenBankHandler was required
          gbFlag   <<- FALSE #Flag to indicate readGenBank failed
        }, error = function(err){
          output$validgenbankid <- renderText({
            validGenbankId()
          })
        }
        )
      }

      #Only executes if GenBank ID is valid
      if(endOfTry){
        #Convert CRISPR seq to upper case
        uCS <- toupper(input$crisprSeq)
        
        #Convert gRNA to upper case
        guideRNA <- toupper(input$gRNA)
        
        progress$set(detail = "Searching for target in GenBank sequence...", value = 0.2)
        
        if(gbFlag){
          #Search for CRISPR in input sequence
          geneSeq <- toupper(as.character(info@sequence))
        } else {
          
          geneSeq <- toupper(as.character(info$ORIGIN))
        }
        
        #Count how many times the input CRISPR target sequence appears in the sequence in the FORWARD direction
        count <- str_count(geneSeq, uCS)
        
        #Count the instance in the reverse complement of the sequence
        revCount <- str_count(geneSeq, reverseComplement(uCS))
        
        #Kicks an error if CRISPR occurs multiple times in sequence
        if(revCount > 1 || count > 1 || (revCount == 1 && count == 1)){
          output$validgenbankDNA <- renderText({
            validate(
              need(1 == 2,
                   "Error: CRISPR target appears multiple times in the sequence associated with this GenBank Accession ID.")
            )
          })
          
        } else if(revCount == 0 && count == 0){
          #Kicks an error if CRISPR does not occur in sequence
          output$validgenbankDNA <- renderText({
            validate(
              need(1 == 2,
                   "Error: CRISPR target does not appear in the sequence associated with this GenBank Accession ID.")
            )
          })
          
        } else {
          #If CRISPR appears exactly once in sequence:
          ucDNA <- toupper(geneSeq)
          
          #Determine whether CRISPR is in forward or reverse strand
          if(revCount == 1 && count == 0){
            #uDNA <- reverseComplement(ucDNA)
            revFlag <- TRUE
          } else {
            #uDNA <- ucDNA
            revFlag <- FALSE
          } 
          uDNA <- ucDNA
          
          orientation <- input$sense
            
          progress$set(detail = "Identifying exons...", value = 0.3)
          
          if(gbFlag){
            #Get exons from GenBank sequence
            gbExonsLoci <- info@exons@ranges
          } else {
            
            gbExonsLoci <- getExonLocus(info)
          }
          
          cutI <- getGenomicCutSite(ucDNA, uCS, orientation)
          
          if(gbFlag){
            #Find the exon that the cut occurs in
            exon <- IRanges:::findOverlaps(IRanges:::IRanges(cutI, cutI), gbExonsLoci, type = "within", select = "first")
          } else {
            exon <- gbExonsLoci[(cutI >= gbExonsLoci$start) & (gbExonsLoci$stop > cutI),]
          }

          progress$set(detail = "Generating padding...", value = 0.4)
          
          if(gbFlag){
            #Find how many nucleotides are between the start of the exon and the cut site
            nucs <- cutI - info@exons@ranges[exon]@start + 1
          } else {
            nucs <- cutI - as.numeric(exon$start[1]) + 1
          }
          
          if(nucs %% 3 == 0){
            padNum <- 0
          } else {
            padNum <- 3 - (nucs %% 3)
          }
          
          #Determine number of padding nucleotides
          
          #Create the oligos
          if(input$paddingChoice == 1){
            oligos <<- doCalculations(uDNA, 
                                      uCS, 
                                      guideRNA, 
                                      as.numeric(input$mh),  
                                      padNum,
                                      revFlag,
                                      input$sense,
                                      progress)
          } else {
            oligos <<- doCalculations(uDNA, 
                                      uCS, 
                                      guideRNA, 
                                      as.numeric(input$mh),  
                                      0,
                                      revFlag,
                                      input$sense,
                                      progress)
          }
          
          
          
          progress$set(detail = "Done", value = 1)
          dF$downloadF <<- TRUE
          dFF$downloadF <<- TRUE
          
          printOutputs(oligos)
        }
      }
    } 
  })
  
  
  
  #Get cDNA/coding sequence from ENSEMBL
  observeEvent(input$geneIdSubmit, {
    resetOutputs()
    
    withProgress({
    
    
    if(is.null(validgRNA()) &&
       is.null(validCrisprSeq()) &&
       is.null(validGeneId()) &&
       is.null(validRefSeqGBGenbankId())){
      
      setProgress(message = "Identifying Species...")
      species <- getEnsemblSpecies(toupper(input$geneId))
      
      guideRNA <- toupper(input$gRNA)
      
      setProgress(message = "Querying ENSEMBL database...")
      
      
      oligos <<- getEnsemblSeq(species[1],
                              input$geneId, 
                              toupper(input$crisprSeq), 
                              guideRNA, 
                              as.numeric(input$mh))
      
      
      
      setProgress(message = "Done!")
      
      dF$downloadF  <<-TRUE
      dFF$downloadF <<- TRUE
      
      printOutputs(oligos)
    }
    
      
    })
  })

  ####Function to print the oligo output####
  printOutputs <- function(oligos){
    #Print out the 5' forward oligo
    output$fivePF <- renderText({
      oligos[1]
    })
    
    #Print out the 5' reverse oligo
    output$fivePR <- renderText({
      oligos[2]
    })
    
    #Print out the 3' forward oligo
    output$threePF <- renderText({
      oligos[3]
    })
    
    #Print out the 3' reverse oligo
    output$threePR <- renderText({
      oligos[4]
    })
  }
  
  ####Function to render download button for downloading oligos
  output$downOut <- renderUI({
    if(dF$downloadF){
      downloadButton("downOligos", "Download Oligos")
    } else{
      ""
    }
  })
  
  output$downPlasOut <- renderUI({
    if(dFF$downloadF){
      downloadButton("downPlasmid", "Download Plasmid File")
    } else {
      ""
    }
  })
  ####Function to handle file generation for downloading oligos
  output$downOligos <- downloadHandler(
    filename = function(){
      paste0(gsub("CDT", "", gsub(" ", "_", Sys.time())), "_oligos.txt")},
    
    content = function(file){
      cat(paste0("5' Forward Oligo: ", oligos[1]), file = file, sep = "\n", append = TRUE)
      cat(paste0("5' Reverse Oligo: ", oligos[2]), file = file, sep = "\n", append = TRUE)
      cat(paste0("3' Forward Oligo: ", oligos[3]), file = file, sep = "\n", append = TRUE)
      cat(paste0("3' Reverse Oligo: ", oligos[4]), file = file, sep = "\n", append = TRUE)
    }

  )
  
  output$downPlasmid <- downloadHandler(

    filename = function(){
      paste0(gsub("CDT", "", gsub(" ", "_", Sys.time())), "_", basename(getBasePlasmid(as.numeric(input$plasmidName))))
    },
    
    content = function(file){
      inPlas <- readLines(getBasePlasmid(as.numeric(input$plasmidName)))
      readPlas <- formatApe(inPlas)
      incPlas <- apeShift(readPlas, oligos)
      writeApe(incPlas, file)
    }
  )
  
  
  
  #output$downloadFlag <- observeEvent(downloadF, {
  #  downloadF
  #})
  #outputOptions(output, "downloadFlag", suspendWhenHidden = FALSE)
  
  ########################################################
  #####################SIDEBAR STUFF######################
  ########################################################
  
  #Input an example to demonstrate
  observeEvent(input$example, {
    reset()
    
    #Reset the inputs that will not be overwritten to their default values
    updateRadioButtons(session, "gRNAtype", selected = 1)
    updateRadioButtons(session, "cDNAtype", selected = 2)
    updateTextInput(session, "gRNA", value = "")
    updateSelectInput(session, "mh", selected = 24)
    updateSelectInput(session, "padding", selected = 0)
    updateTextInput(session, "crisprSeq", value = "GCGCAGCGAGTCAGTGAGCG")
    updateTextInput(session, "cDNA", value = "CTTTCCTGCGTTATCCCCTGATTCTGTGGATAACCGTATTACCGCCTTTGAGTGAGCTGATACCGCTCGCCGCAGCCGAACGACCGAGCGCAGCGAGTCAGTGAGCGAGGAAGCGGAAGAGCGCCCAATACGCAAACCGCCTCTCCCCGCGCGTTGGCCGATTCATTAATGCAGCTGGCACGACAGGTTTCCCGACTGGAAAGCGGGCAGTGAGCGCAACGCAATTAATACGCGTACCGCTAGCCAGGAAGAGTTTGTAGAAACGCAAAAAGGCCATCCGTCAGGATGGCCTTCTGCTTAGTTTGATGCCTGGCAGTTTATGGCGGGCGTCCTGCCCGCCACCCTCCGGGCCGTTGCTTCACAACGTTCAAATCCGCTCCCGGCGGATTTGTCCTACTCAGGAGAGCGTTCACCGACAAACAACAGATAAAACGAAAGGCCCAGTCTTCCGACTGAGCCTTTCGTTTTATTTGATGCCTGGCAGTTCCCTACTCTCGCGTTAACGCTAGCATGGATGTTTTCCCAGTCACGACGTTGTAAAACGACGGCCAGTCTTAAGCTCGGGCCCTGCAGCTCTAGAGCTCGAATTCGGGAGCGCAGAGCTGGAGACAGGCAAACGCCTGTCGGATCCGGAGCCACGAACTTCTCTCTGTTAAAGCAAGCAGGAGACGTGGAAGAAAACCCCGGTCCTATGGTGTCTAAGGGCGAAGAGCTGATTAAGGAGAACATGCACATGAAGCTGTACATGGAGGGCACCGTGAACAACCACCACTTCAAGTGCACATCCGAGGGCGAAGGCAAGCCCTACGAGGGCACCCAGACCATGAGAATCAAGGTGGTCGAGGGCGGCCCTCTCCCCTTCGCCTTCGACATCCTGGCTACCAGCTTCATGTACGGCAGCAGAACCTTCATCAACCACACCCAGGGCATCCCCGACTTCTTTAAGCAGTCCTTCCCTGAGGGCTTCACATGGGAGAGAGTCACCACATACGAAGACGGGGGCGTGCTGACCGCTACCCAGGACACCAGCCTCCAGGACGGCTGCCTCATCTACAACGTCAAGATCAGAGGGGTGAACTTCCCATCCAACGGCCCTGTGATGCAGAAGAAAACACTCGGCTGGGAGGCCAACACCGAGATGCTGTACCCCGCTGACGGCGGCCTGGAAGGCAGAAGCGACATGGCCCTGAAGCTCGTGGGCGGGGGCCACCTGATCTGCAACTTCAAGACCACATACAGATCCAAGAAACCCGCTAAGAACCTCAAGATGCCCGGCGTCTACTATGTGGACCACAGACTGGAAAGAATCAAGGAGGCCGACAAAGAGACCTACGTCGAGCAGCACGAGGTGGCTGTGGCCAGATACTGCGACCTCCCTAGCAAACTGGGGCACAAACTTAATTGAGGCGCGCCTCTAGAACTATAGTGAGTCGTATTACGTAGATCCAGACATGATAAGATACATTGATGAGTTTGGACAAACCACAACTAGAATGCAGTGAAAAAAATGCTTTATTTGTGAAATTTGTGATGCTATTGCTTTATTTGTAACCATTATAAGCTGCAATAAACAAGTTAACAACAACAATTGCATTCATTTTATGTTTCAGGTTCAGGGGGAGGTGTGGGAGGTTTTTTCCAACTTTATTATACAAAGTTGGCATTATAAAAAAGCATTGCTTATCAATTTGTTGCAACGAACAGGTCACTATCAGTCAAAATAAAATCATTATTTGGAGCTCCATGGTAGCGTTAACGCGGCCGCGATATCCCCTATAGTGAGTCGTATTACATGGTCATAGCTGTTTCCTGGCAGCTCTGGCCCGTGTCTCAAAATCTCTGATGTTACATTGCACAAGATAAAAATATATCATCATGAACAATAAAACTGTCTGCTTACATAAACAGTAATACAAGGGGTGTTATGAGCCATATTCAACGGGAAACGTCGAGGCCGCGATTAAATTCCAACATGGATGCTGATTTATATGGGTATAAATGGGCTCGCGATAATGTCGGGCAATCAGGTGCGACAATCTATCGCTTGTATGGGAAGCCCGATGCGCCAGAGTTGTTTCTGAAACATGGCAAAGGTAGCGTTGCCAATGATGTTACAGATGAGATGGTCAGACTAAACTGGCTGACGGAATTTATGCCTCTTCCGACCATCAAGCATTTTATCCGTACTCCTGATGATGCATGGTTACTCACCACTGCGATCCCCGGAAAAACAGCATTCCAGGTATTAGAAGAATATCCTGATTCAGGTGAAAATATTGTTGATGCGCTGGCAGTGTTCCTGCGCCGGTTGCATTCGATTCCTGTTTGTAATTGTCCTTTTAACAGCGATCGCGTATTTCGTCTCGCTCAGGCGCAATCACGAATGAATAACGGTTTGGTTGATGCGAGTGATTTTGATGACGAGCGTAATGGCTGGCCTGTTGAACAAGTCTGGAAAGAAATGCATAAACTTTTGCCATTCTCACCGGATTCAGTCGTCACTCATGGTGATTTCTCACTTGATAACCTTATTTTTGACGAGGGGAAATTAATAGGTTGTATTGATGTTGGACGAGTCGGAATCGCAGACCGATACCAGGATCTTGCCATCCTATGGAACTGCCTCGGTGAGTTTTCTCCTTCATTACAGAAACGGCTTTTTCAAAAATATGGTATTGATAATCCTGATATGAATAAATTGCAGTTTCATTTGATGCTCGATGAGTTTTTCTAATCAGAATTGGTTAATTGGTTGTAACACTGGCAGAGCATTACGCTGACTTGACGGGACGGCGCAAGCTCATGACCAAAATCCCTTAACGTGAGTTACGCGTCGTTCCACTGAGCGTCAGACCCCGTAGAAAAGATCAAAGGATCTTCTTGAGATCCTTTTTTTCTGCGCGTAATCTGCTGCTTGCAAACAAAAAAACCACCGCTACCAGCGGTGGTTTGTTTGCCGGATCAAGAGCTACCAACTCTTTTTCCGAAGGTAACTGGCTTCAGCAGAGCGCAGATACCAAATACTGTCCTTCTAGTGTAGCCGTAGTTAGGCCACCACTTCAAGAACTCTGTAGCACCGCCTACATACCTCGCTCTGCTAATCCTGTTACCAGTGGCTGCTGCCAGTGGCGATAAGTCGTGTCTTACCGGGTTGGACTCAAGACGATAGTTACCGGATAAGGCGCAGCGGTCGGGCTGAACGGGGGGTTCGTGCACACAGCCCAGCTTGGAGCGAACGACCTACACCGAACTGAGATACCTACAGCGTGAGCATTGAGAAAGCGCCACGCTTCCCGAAGGGAGAAAGGCGGACAGGTATCCGGTAAGCGGCAGGGTCGGAACAGGAGAGCGCACGAGGGAGCTTCCAGGGGGAAACGCCTGGTATCTTTATAGTCCTGTCGGGTTTCGCCACCTCTGACTTGAGCGTCGATTTTTGTGATGCTCGTCAGGGGGGCGGAGCCTATGGAAAAACGCCAGCAACGCGGCCTTTTTACGGTTCCTGGCCTTTTGCTGGCCTTTTGCTCACATGTT")
  })
  
  #Clear the form
  observeEvent(input$reset, {
    reset()
  })
  
  observeEvent(input$exampleEnsembl, {
    reset()
    
    #Reset the inputs to their default values
    updateRadioButtons(session, "gRNAtype", selected = 1)
    updateSelectInput(session, "mh", selected = 24)
    
    #Reset the inputs that will not be overwritten to their default values
    updateRadioButtons(session, "cDNAtype", selected = 1)
    updateTextInput(session, "geneId", value = "ENSG00000146648")
    updateTextInput(session, "crisprSeq", value = "TGCCACAACCAGTGTGCTGC")
  })

  observeEvent(input$exampleGenbank, {
    reset()
    #Reset the inputs to their default values
    updateRadioButtons(session, "gRNAtype", selected = 1)
    updateSelectInput(session, "mh", selected = 24)
    updateTextAreaInput(session, "crisprSeq", value = toupper("ggaagagcttacgaaactta"))
    updateRadioButtons(session, "cDNAtype", selected = 1)
    updateTextInput(session, "genbankId", value = "AY214391.1")
    updateRadioButtons(session, "paddingChoice", selected = 1)
    
    
    
  })
  #Reset function
  reset <- function(){
    
    #Reset the global inputs to their default values
    dF$downloadF <<- FALSE
    dFF$downloadF <<- FALSE
    oligos <<- NULL
    gbFile <<- NULL
    
    #updateRadioButtons(session, "plasmidCond", selected = 0)
    updateRadioButtons(session, "gRNAtype", selected = 1)
    updateTextInput(session, "gRNA", value = NA)
    updateTextInput(session, "crisprSeq", value = NA)
    updateRadioButtons(session, "sense", selected = 0)
    updateRadioButtons(session, "cDNAtype", selected = 1)
    updateTextInput(session, "cDNA", value = NA)
    updateTextInput(session, "genbankId", value = NA)
    updateRadioButtons(session, "paddingChoice", selected = 1)
    #updateTextInput(session, "geneId", value = NA)
    updateSelectInput(session, "mh", selected = 48)
    updateSelectInput(session, "padding", selected = 0)
    updateTextInput(session, "geneId", value = NA)
    updateSelectInput(session, "plasmidName", selected = 1)
    
    resetOutputs()
  }
  
  resetOutputs <- function(){
    
    #Clear the 5' forward oligo output
    output$fivePF <- renderText({
      ""
    })
    #Clear the 5' reverse oligo output
    output$fivePR <- renderText({
      ""
    })
    #Clear the 3' forward oligo output
    output$threePF <- renderText({
      ""
    })
    #Clear the 3' reverse oligo output
    output$threePR <- renderText({
      ""
    })
  }
  
  #Stop session after browser tab is closed
  session$onSessionEnded(stopApp)
})
