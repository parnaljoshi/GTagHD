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
#library(biofiles)

#Required files
source("functions.R")
source("apeShiftFunctions.R")
source("ensemblFunctions.R")
source("genbankFunctions.R")

shinyServer(function(input, output, session) {
  
  #Create global variables to minimize calculations and function calls
  dF  <- reactiveValues(downloadF = FALSE)
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
  #and is present in sense/anti-sense direction exactly once
  validCrisprSeq <- reactive({
    
    #Once text is entered, run through validation tests
    if(input$crisprSeq != ""){
      uCS <- toupper(input$crisprSeq)
      
      validate(
        #Checks that only A, C, G, and T are in sequence
        need(!str_detect(uCS, "[^ACGT]"), 
             paste0("Error: Input DNA sequence contains non-standard nucleotides. ", 
                    "Allowed nucleotides are A, C, G, and T.")),
        
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
  #Test if cDNA sequence is valid - crispr only appears once in either sense or anti-sense, etc.
  
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
        
        #Count how many times the input CRISPR target sequence appears in the cDNA sequence in the sense direction
        count <- str_count(ucDNA, uCS)
        
        #Count the instance in the reverse complement of the sequence
        revCount <- str_count(reverseComplement(ucDNA), uCS)
        
        #Check to ensure the target only appears once in a single direction
        if(count > 1){
          validate(
            need(count      == 1, paste0("Error: CRISPR target sequence appears ", 
                                         count, 
                                         " times in the pasted gene/exon sequence.")),
            
            need(revCount   == 0, paste0("Error: The CRISPR target sequence appears ", 
                                         revCount, 
                                         " times on the opposite (anti-sense) strand."))
          )
        } else if (count    == 1) {
          validate(
            need(revCount   == 0, paste0("Error: The CRISPR target sequence also appears ", 
                                         revCount, 
                                         " times on the opposite strand."))
          )
        } else if (count    == 0 && 
                   revCount == 0){
          validate(
            need(revCount   == 1, paste0("Error: The CRISPR target sequence does not appear ", 
                                         "on the given DNA strand, or in its complement."))
          )
        } else if (count    == 0 && 
                   revCount >  1){
          validate(
            need(revCount   == 1, paste0("Error: The CRISPR target sequence appears ", 
                                         revCount, 
                                         " times on the opposite (anti-sense) strand."))
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
    if((input$crisprSeq != "") && 
       (input$cDNA      != "")){
      if(is.null(validCrisprSeq()) && 
        (is.null(validCDNA()))){
        uDNA <- toupper(input$cDNA)
        uCS  <- toupper(input$crisprSeq)
        
        #Count how many times the input CRISPR target sequence appears in the cDNA sequence in the sense direction
        count    <- str_count(uDNA, uCS)
        
        #Count the instance in the reverse complement of the sequence
        revCount <- str_count(reverseComplement(uDNA), uCS)
        
        #If the target appears in the reverseComplement of cDNA
        if(revCount == 1 && 
           count    == 0){
          uDNA   <- reverseComplement(input$cDNA)
        } 
        
        #Find ALL locations of the guide in the sequence
        crisprLoc <- unlist(str_locate_all(uDNA, input$crisprSeq))

        #If the crispr is in the sense direction
        if(input$sense == 0){
          #If the CRISPR is in the sense strand, cut between bases 17 and 18 in the matching sequence.
          cutIndex <- crisprLoc[2] - 3
          
        } else if(input$sense == 1) {
          #If the CRISPR is in the anti-sense strand, cut between bases 3 and 4 in the matching sequence.
          cutIndex <- crisprLoc[1] + 2
        }
        
        #If the guide only appears once, in either direction
        if((revCount == 0 && count == 1) || 
           (revCount == 1 && count == 0)){
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
  validEnsemblId <- reactive({
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
          paste0("Error: Unable to retrieve information corresponding to this ID from GenBank. ",
                 "Either the accession ID does not exist, or there were connection problems with GenBank."))
      )
  }
  
  validRefSeqGenbankId <- reactive({
    if(input$genbankId != ""){
      if(isRefSeqRnaId(input$genbankId)){
        #This section left blank intentionally - just need to remove RefSeq RNA sequences from being an issue
        
      } else if(isRefSeqProtId(input$genbankId)){
        validate(
          need(1 == 2,
               "Error: This ID matches RefSeq protein accession format. Please submit an accession corresponding to a DNA sequence."))
        
      } else if(isGenBankProtId(input$genbankId)){
        validate(
          need(1 == 2,
               "Error: This ID matches GenBank protein accession format. Please submit an accession corresponding to a DNA sequence."))
        
      } else {
        validate(
          need(isGenBankDnaId(input$genbankId)    ||
               isRefSeqGenomicId(input$genbankId) ||
               isGenBankWGSId(input$genbankId),
               
               paste0("Error: The entered ID does not match a known Genbank NUCLEOTIDE or RefSeq NUCLEOTIDE ID format.", 
                      "Please check your submitted accession to make sure you are using a NUCLEOTIDE entry.")))
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
  
  ##################GENBANK  FILE UPLOAD INPUTS###########
  validExonInfo <- function(){
    validate(
      need(1 == 2,
           paste0(
             "Warning: The exon locus information is either missing or improperly formatted for this GenBank entry. ",
             "Automatic codon repair is not possible. "
           )
      )
    ) 
  }
  
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
  output$validensemblid <- renderText({
    if(is.null(validEnsemblId())){
      paste0(getEnsemblSpecies(input$geneId)[2], " Ensembl gene ID detected.")
      
    } else {
      validEnsemblId()
    }
  })
  
  output$exonWarning <- renderText({
    if(!is.null(gbFile)){
      exonWarningFunc()
    }
  })
  
  #output$validrefseqgenbankid <- renderText({
  #    validRefSeqGenbankId()
  #})
  
  ########################################################
  ################PERFORM CALCULATIONS####################
  ########################################################  
  #Do the calculation for the targeting oligos - COPY/PASTE INPUT
  observeEvent(input$submit, {
    resetOutputs()
    #Check to ensure that all inputs are valid before accepting
    if(is.null(validgRNA())      &&
       is.null(validCrisprSeq()) &&
       is.null(validCDNA())      &&
       is.null(validMHCDna())){
      revFlag <- FALSE
      resetOutputs()
      
      #Convert inputs to uppercase
      guideRNA <- toupper(input$gRNA)
      ucDNA    <- toupper(input$cDNA)
      uCS      <- toupper(input$crisprSeq)

      #Determine if the CRISPR is on the sense or anti-sense strand
      rev     <- revCheck(ucDNA, uCS)
      
      uDNA    <- rev[1]
      revFlag <- rev[2]
      uCrispr <- rev[3]
      
      #Set up the progress bar
      progress <- Progress$new(session)
      on.exit(progress$close())
      progress$set(message = "Generating Oligos: ")
      
      #Create the oligos
      oligos <<- doCalculations(ucDNA, 
                                uCrispr, 
                                guideRNA, 
                                as.numeric(input$mh),  
                                input$padding,
                                revFlag,
                                input$sense,
                                progress,
                                input$toolSeries)
      
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
    
    #Clear output
    resetOutputs()
    
    #Check to ensure that all other inputs are valid before proceding
    if(is.null(     validgRNA()) &&
       is.null(validCrisprSeq())){ 
       #is.null(validRefSeqGenbankId())){
      
      revFlag <- FALSE
      
      #Clear Outputs
      resetOutputs()
      
      #Set up a progress object and intialize
      progress <- Progress$new(session)
      on.exit(    progress$close())
      progress$set(message = "Generating Oligos: ")
      progress$set(detail  = "Retrieving GenBank entry...", value = 0.1)
      
      #Try to pull genbank entry associated with accession
      gba <- input$genbankId
      
      #Flags for indicating problems with getting/processing genbank information
      endOfTry <<- FALSE
      gbFlag   <<- FALSE
      gbhFlag  <<- FALSE
      
      #For dealing with reading in GenBank files
      if(endOfTry == FALSE){
        tryCatch({
          info      <- suppressWarnings(wonkyGenBankHandler(gba))
          
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
    
      #Only executes if GenBank ID is valid/was able to be retrieved from GenBank
      if(endOfTry){
        
        #Convert CRISPR seq to upper case
        uCS      <- toupper(input$crisprSeq)
        
        #Convert gRNA to upper case
        guideRNA <- toupper(input$gRNA)
        
        progress$set(detail = "Searching for target in GenBank sequence...", value = 0.2)
        
        if(gbFlag){
          #Search for CRISPR in input sequence
          geneSeq <- toupper(as.character(info@sequence))
          
        } else {
          geneSeq <- toupper(as.character(info$ORIGIN))
        }
        
        #Count how many times the input CRISPR target sequence appears in the sequence in the sense direction
        count    <- str_count(geneSeq, uCS)
        
        #Count the instance in the reverse complement of the sequence
        revCount <- str_count(geneSeq, reverseComplement(uCS))
        
        #Kicks an error if CRISPR occurs multiple times in sequence
        if(revCount > 1 || 
           count    > 1 || 
           (revCount == 1 && count == 1)){
          
          output$validgenbankDNA <- renderText({
            validate(
              need(1 == 2,
                   "Error: CRISPR target appears multiple times in the sequence associated with this GenBank Accession ID.")
            )
          })
          
        } else if(revCount == 0 && count == 0){
          # Throws an error if CRISPR does not occur in sequence
          output$validgenbankDNA <- renderText({
            validate(
              need(1 == 2,
                   "Error: CRISPR target does not appear in the sequence associated with this GenBank Accession ID.")
            )
          })
          
        } else {
          # If CRISPR appears exactly once in sequence:
          ucDNA <- toupper(geneSeq)

          # Determine if the CRISPR is on the sense or reverse strand
          rev <- revCheck(ucDNA, uCS)
          
          uDNA    <- rev[1]
          revFlag <- rev[2]

          #uDNA <- ucDNA
          
          orientation <- as.numeric(input$sense)
            
          progress$set(detail = "Identifying exons...", value = 0.3)
          
          if(gbFlag){
            #Get exons from GenBank sequence
            gbExonsLoci <- info@exons@ranges
            
          } else {
            gbExonsLoci <- getExonLocus(info)
          }

          cutI <- getGenomicCutSite(uDNA, uCS, orientation)

          if(gbFlag){
            #Find the exon that the cut occurs in
            exon <- IRanges:::findOverlaps(IRanges:::IRanges(cutI, cutI), gbExonsLoci, type = "within", select = "first")
          } else {
            exon <- gbExonsLoci[(cutI >= gbExonsLoci$start) & (gbExonsLoci$stop > cutI),]
          }

          progress$set(detail = "Generating padding...", value = 0.4)
          
          #print('exon: ')
          #print(exon)
          
          if(gbFlag){
            #Find how many nucleotides are between the start of the exon and the cut site
            nucs <- cutI - info@exons@ranges[exon]@start + 1
            
          } else {
            nucs <- cutI - as.numeric(exon$start[1])     + 1
          }
          
          #print('nucs: ')
          #print(nucs)
          
          #Determine number of padding nucleotides
          if(nucs %% 3 == 0){
            padNum <- 0
            
          } else {
            padNum <- 3 - (nucs %% 3)
          }
          
          if(input$paddingChoice == 1){
            padNum <- 0
          }
          
          #Create the oligos
          oligos <<- doCalculations(uDNA, 
                                    uCS, 
                                    guideRNA, 
                                    as.numeric(input$mh),  
                                    padNum,
                                    revFlag,
                                    input$sense,
                                    progress,
                                    input$toolSeries)
          
          progress$set(detail = "Done", value = 1)
          dF$downloadF  <<- TRUE
          dFF$downloadF <<- TRUE
          
          printOutputs(oligos)
        }
      }
    } 
  })
  
  
  # Get cDNA/coding sequence from ENSEMBL
  observeEvent(input$geneIdSubmit, {
    resetOutputs()
    
    withProgress({
      if(is.null(validgRNA())      &&
         is.null(validCrisprSeq()) &&
         is.null(validEnsemblId())){    #&&
         #is.null(validRefSeqGenbankId())){
        
        guideRNA <- toupper(input$gRNA)
        
        setProgress(message = "Querying Ensembl database...")
        
        oligos <<- getEnsemblSeq(input$geneId, 
                                 toupper(input$crisprSeq), 
                                 guideRNA, 
                                 as.numeric(input$mh))
        
        if(oligos == 1){
          setProgress(message = "There was a problem processing your request.")
          output$validensemblid <- renderText({
            validate(
              need(1 == 2,
                   paste0("Unable to retrieve Ensembl entry. Please check to make ", 
                          "sure you've entered your Ensembl ID correctly, OR try ",
                          "again in a few minutes - Ensembl may be having issues.")))
          })
          
        } else {
          setProgress(message = "Done!")
          
          dF$downloadF  <<-TRUE
          dFF$downloadF <<- TRUE
          
          printOutputs(oligos)
        }
        
      }
    })
  })

  # Do the calculation for GenBank file upload
  observeEvent(input$genbankFileUpload, {
    # Clear output
    resetOutputs()
    
    # Check to ensure that all other inputs are valid before proceding
    #if(
    # Check if file exists
    
    #){
      revFlag <- FALSE
      
      # Set up a progress object and intialize
      progress <- Progress$new(session)
      on.exit(    progress$close())
      progress$set(message = "Generating Oligos: ")
      progress$set(detail  = "Processing GenBank file...", value = 0.1)
      
      # Read in the uploaded file's contents
      info     <- readGenBankFile(input$genbankFileUpload)
      
      # Update progress
      progress$set(detail = "Searching for target in GenBank file...", value = 0.2)
      
      # Convert CRISPR seq to upper case
      uCS      <- toupper(input$crisprSeq)
      
      # Convert gRNA to upper case
      guideRNA <- toupper(input$gRNA)
      
      # Get the gene sequence
      geneSeq  <- toupper(as.character(info$ORIGIN))
      
      # Count how many times the input CRISPR target sequence appears in the sequence in the sense direction
      count    <- str_count(geneSeq, uCS)
      
      # Count the instance in the reverse complement of the sequence
      revCount <- str_count(geneSeq, reverseComplement(uCS))
      
      # Throw an error if CRISPR occurs multiple times in sequence
      if(revCount > 1 || # If occurs more than once in the anti-sense strand
         count    > 1 || # If occurs more than once in the sense strand
         (revCount == 1 && count == 1)){ # If occurs once in the anti-sense AND the sense strand
        
        # Report the outcome
        output$validgenbankFileDNA <- renderText({
          validate(
            need(1 == 2,
                 "Error: CRISPR target appears multiple times in the GenBank file DNA sequence.")
          )
        })
        
      } else if(revCount == 0 && count == 0){
        #Kicks an error if CRISPR does not occur in sequence
        output$validgenbankDNA <- renderText({
          validate(
            need(1 == 2,
                 "Error: CRISPR target does not appear in the GenBank file DNA sequence.")
          )
        })
        
      } else {
        # If CRISPR appears exactly once in sequence:
        ucDNA <- toupper(geneSeq)
      
        # Determine if the CRISPR is on the sense or anti-sense strand
        rev     <- revCheck(ucDNA, uCS)
        uDNA    <- rev[1]
        revFlag <- rev[2]
        
        # Get the orientation
        orientation <- as.numeric(input$sense)
        
        # Get the location of the DSB site
        cutI <- getGenomicCutSite(uDNA, uCS, orientation)
        
        # Update progress
        progress$set(detail = "Identifying exons...", value = 0.3)
        
        # Get the exon information from the sequence
        gbExonsLoci  <- getExonLocus(info)
        exonInfoFlag <- TRUE
        
        # Determine the exon the DSB is located in
        exon <- gbExonsLoci[(cutI >= gbExonsLoci$start) & (gbExonsLoci$stop > cutI),]
        
        
        # If the codon is to be repaired, process the exons in the file
        if(input$paddingChoice == 1 && exonInfoFlag){
          progress$set(detail = "Generating padding...", value = 0.4)
          
          # Find the # of nucleotides between the start of the exon and the cut site
          nucs <- cutI - as.numeric(exon$start[1]) + 1
          
          # Determine the number of nucleotides needed to repair a codon break
          if(nucs %% 3 == 0){
            padNum <- 0
          } else {
            padNum <- 3 - (nucs %% 3)
          }
          
        } else if(input$paddingChoice == 1 && !exonInfoFlag){
          # If there is no exon information, set nucs to 0
          nucs <- 0 
          
          # Warn user that there is no exon info
          output$warnNoExonInfo <- renderText({
            validExonInfo()
          })
        }
        
        oligos <<- doCalculations(uDNA,
                                  uCS,
                                  guideRNA,
                                  as.numeric(input$mh),
                                  padNum,
                                  revFlag,
                                  input$sense,
                                  progress)
        
        progress$set(detail = "Done", value = 1)
        dF$downloadF  <<- TRUE
        dFF$downloadF <<- TRUE
        
        printOutputs(oligos)
        
      #if(
      # Check for multiple target sites
      # Check for valid DNA sequence
      # Check for sequence context for microhomology length
      #){
      
      
      
      # 
      
        
      }
      #}
      
    #}
    
    
  })
  
   #### Function to print the oligo output ####
  printOutputs <- function(oligos){
    # Print out the 5' sense oligo
    output$fivePF <- renderText({
      oligos[1]
    })
    
    # Print out the 5' anti-sense oligo
    output$fivePR <- renderText({
      oligos[2]
    })
    
    # Print out the 3' sense oligo
    output$threePF <- renderText({
      oligos[3]
    })
    
    # Print out the 3' anti-sense oligo
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
      cat(paste0("5' Forward Oligo:\t", oligos[1]), file = file, sep = "\n", append = TRUE)
      cat(paste0("5' Reverse Oligo:\t", oligos[2]), file = file, sep = "\n", append = TRUE)
      cat(paste0("3' Forward Oligo:\t", oligos[3]), file = file, sep = "\n", append = TRUE)
      cat(paste0("3' Reverse Oligo:\t", oligos[4]), file = file, sep = "\n", append = TRUE)
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
    updateSelectInput(session,  "toolSeries",  selected = 0)
    updateSelectInput(session,  "plasmidName", selected = 1)
    updateRadioButtons(session, "gRNAtype",  selected = 1)
    updateRadioButtons(session, "cDNAtype",  selected = 2)
    updateTextInput(session,    "gRNA",      value    = "")
    updateSelectInput(session,  "mh",        selected = 24)
    updateSelectInput(session,  "padding",   selected = 0)
    updateTextInput(session,    "crisprSeq", value    = "GCGCAGCGAGTCAGTGAGCG")
    updateTextInput(session,    "cDNA",      value    = paste0("CTTTCCTGCGTTATCCCCTGATTCTGTGGATAACCGTATTACCGCCTTTGAGTGAGCTGATACCGCTCGCCGCA",
                                                               "GCCGAACGACCGAGCGCAGCGAGTCAGTGAGCGAGGAAGCGGAAGAGCGCCCAATACGCAAACCGCCTCTCCCC",
                                                               "GCGCGTTGGCCGATTCATTAATGCAGCTGGCACGACAGGTTTCCCGACTGGAAAGCGGGCAGTGAGCGCAACGC",
                                                               "AATTAATACGCGTACCGCTAGCCAGGAAGAGTTTGTAGAAACGCAAAAAGGCCATCCGTCAGGATGGCCTTCTG",
                                                               "CTTAGTTTGATGCCTGGCAGTTTATGGCGGGCGTCCTGCCCGCCACCCTCCGGGCCGTTGCTTCACAACGTTCA",
                                                               "AATCCGCTCCCGGCGGATTTGTCCTACTCAGGAGAGCGTTCACCGACAAACAACAGATAAAACGAAAGGCCCAG",
                                                               "TCTTCCGACTGAGCCTTTCGTTTTATTTGATGCCTGGCAGTTCCCTACTCTCGCGTTAACGCTAGCATGGATGT",
                                                               "TTTCCCAGTCACGACGTTGTAAAACGACGGCCAGTCTTAAGCTCGGGCCCTGCAGCTCTAGAGCTCGAATTCGG",
                                                               "GAGCGCAGAGCTGGAGACAGGCAAACGCCTGTCGGATCCGGAGCCACGAACTTCTCTCTGTTAAAGCAAGCAGG",
                                                               "AGACGTGGAAGAAAACCCCGGTCCTATGGTGTCTAAGGGCGAAGAGCTGATTAAGGAGAACATGCACATGAAGC",
                                                               "TGTACATGGAGGGCACCGTGAACAACCACCACTTCAAGTGCACATCCGAGGGCGAAGGCAAGCCCTACGAGGGC",
                                                               "ACCCAGACCATGAGAATCAAGGTGGTCGAGGGCGGCCCTCTCCCCTTCGCCTTCGACATCCTGGCTACCAGCTT",
                                                               "CATGTACGGCAGCAGAACCTTCATCAACCACACCCAGGGCATCCCCGACTTCTTTAAGCAGTCCTTCCCTGAGG",
                                                               "GCTTCACATGGGAGAGAGTCACCACATACGAAGACGGGGGCGTGCTGACCGCTACCCAGGACACCAGCCTCCAG",
                                                               "GACGGCTGCCTCATCTACAACGTCAAGATCAGAGGGGTGAACTTCCCATCCAACGGCCCTGTGATGCAGAAGAA",
                                                               "AACACTCGGCTGGGAGGCCAACACCGAGATGCTGTACCCCGCTGACGGCGGCCTGGAAGGCAGAAGCGACATGG",
                                                               "CCCTGAAGCTCGTGGGCGGGGGCCACCTGATCTGCAACTTCAAGACCACATACAGATCCAAGAAACCCGCTAAG",
                                                               "AACCTCAAGATGCCCGGCGTCTACTATGTGGACCACAGACTGGAAAGAATCAAGGAGGCCGACAAAGAGACCTA",
                                                               "CGTCGAGCAGCACGAGGTGGCTGTGGCCAGATACTGCGACCTCCCTAGCAAACTGGGGCACAAACTTAATTGAG",
                                                               "GCGCGCCTCTAGAACTATAGTGAGTCGTATTACGTAGATCCAGACATGATAAGATACATTGATGAGTTTGGACA",
                                                               "AACCACAACTAGAATGCAGTGAAAAAAATGCTTTATTTGTGAAATTTGTGATGCTATTGCTTTATTTGTAACCA",
                                                               "TTATAAGCTGCAATAAACAAGTTAACAACAACAATTGCATTCATTTTATGTTTCAGGTTCAGGGGGAGGTGTGG",
                                                               "GAGGTTTTTTCCAACTTTATTATACAAAGTTGGCATTATAAAAAAGCATTGCTTATCAATTTGTTGCAACGAAC",
                                                               "AGGTCACTATCAGTCAAAATAAAATCATTATTTGGAGCTCCATGGTAGCGTTAACGCGGCCGCGATATCCCCTA",
                                                               "TAGTGAGTCGTATTACATGGTCATAGCTGTTTCCTGGCAGCTCTGGCCCGTGTCTCAAAATCTCTGATGTTACA",
                                                               "TTGCACAAGATAAAAATATATCATCATGAACAATAAAACTGTCTGCTTACATAAACAGTAATACAAGGGGTGTT",
                                                               "ATGAGCCATATTCAACGGGAAACGTCGAGGCCGCGATTAAATTCCAACATGGATGCTGATTTATATGGGTATAA",
                                                               "ATGGGCTCGCGATAATGTCGGGCAATCAGGTGCGACAATCTATCGCTTGTATGGGAAGCCCGATGCGCCAGAGT",
                                                               "TGTTTCTGAAACATGGCAAAGGTAGCGTTGCCAATGATGTTACAGATGAGATGGTCAGACTAAACTGGCTGACG",
                                                               "GAATTTATGCCTCTTCCGACCATCAAGCATTTTATCCGTACTCCTGATGATGCATGGTTACTCACCACTGCGAT",
                                                               "CCCCGGAAAAACAGCATTCCAGGTATTAGAAGAATATCCTGATTCAGGTGAAAATATTGTTGATGCGCTGGCAG",
                                                               "TGTTCCTGCGCCGGTTGCATTCGATTCCTGTTTGTAATTGTCCTTTTAACAGCGATCGCGTATTTCGTCTCGCT",
                                                               "CAGGCGCAATCACGAATGAATAACGGTTTGGTTGATGCGAGTGATTTTGATGACGAGCGTAATGGCTGGCCTGT",
                                                               "TGAACAAGTCTGGAAAGAAATGCATAAACTTTTGCCATTCTCACCGGATTCAGTCGTCACTCATGGTGATTTCT",
                                                               "CACTTGATAACCTTATTTTTGACGAGGGGAAATTAATAGGTTGTATTGATGTTGGACGAGTCGGAATCGCAGAC",
                                                               "CGATACCAGGATCTTGCCATCCTATGGAACTGCCTCGGTGAGTTTTCTCCTTCATTACAGAAACGGCTTTTTCA",
                                                               "AAAATATGGTATTGATAATCCTGATATGAATAAATTGCAGTTTCATTTGATGCTCGATGAGTTTTTCTAATCAG",
                                                               "AATTGGTTAATTGGTTGTAACACTGGCAGAGCATTACGCTGACTTGACGGGACGGCGCAAGCTCATGACCAAAA",
                                                               "TCCCTTAACGTGAGTTACGCGTCGTTCCACTGAGCGTCAGACCCCGTAGAAAAGATCAAAGGATCTTCTTGAGA",
                                                               "TCCTTTTTTTCTGCGCGTAATCTGCTGCTTGCAAACAAAAAAACCACCGCTACCAGCGGTGGTTTGTTTGCCGG",
                                                               "ATCAAGAGCTACCAACTCTTTTTCCGAAGGTAACTGGCTTCAGCAGAGCGCAGATACCAAATACTGTCCTTCTA",
                                                               "GTGTAGCCGTAGTTAGGCCACCACTTCAAGAACTCTGTAGCACCGCCTACATACCTCGCTCTGCTAATCCTGTT",
                                                               "ACCAGTGGCTGCTGCCAGTGGCGATAAGTCGTGTCTTACCGGGTTGGACTCAAGACGATAGTTACCGGATAAGG",
                                                               "CGCAGCGGTCGGGCTGAACGGGGGGTTCGTGCACACAGCCCAGCTTGGAGCGAACGACCTACACCGAACTGAGA",
                                                               "TACCTACAGCGTGAGCATTGAGAAAGCGCCACGCTTCCCGAAGGGAGAAAGGCGGACAGGTATCCGGTAAGCGG",
                                                               "CAGGGTCGGAACAGGAGAGCGCACGAGGGAGCTTCCAGGGGGAAACGCCTGGTATCTTTATAGTCCTGTCGGGT",
                                                               "TTCGCCACCTCTGACTTGAGCGTCGATTTTTGTGATGCTCGTCAGGGGGGCGGAGCCTATGGAAAAACGCCAGC",
                                                               "AACGCGGCCTTTTTACGGTTCCTGGCCTTTTGCTGGCCTTTTGCTCACATGTT"))
  })
  
  #Clear the form
  observeEvent(input$reset, {
    reset()
  })
  
  observeEvent(input$exampleEnsembl, {
    reset()
    
    #Reset the inputs to their default values
    updateSelectInput(session, "toolSeries", selected = 0)
    updateSelectInput(session, "")
    updateRadioButtons(session, "gRNAtype",  selected = 1)
    updateSelectInput(session,  "mh",        selected = 24)
    
    #Reset the inputs that will not be overwritten to their default values
    updateRadioButtons(session, "cDNAtype",  selected = 3)
    updateTextInput(session,    "geneId",    value    = "ENSG00000146648")
    updateTextInput(session,    "crisprSeq", value    = "TGCCACAACCAGTGTGCTGC")
  })

  observeEvent(input$exampleGenbank, {
    reset()

    #Reset the inputs to their default values
    updateSelectInput(session, "toolSeries",      selected = 0)
    updateRadioButtons(session,  "gRNAtype",      selected = 1)
    updateSelectInput(session,   "mh",            selected = 24)
    updateTextAreaInput(session, "crisprSeq",     value    = toupper("ggaagagcttacgaaactta"))
    updateRadioButtons(session,  "cDNAtype",      selected = 1)
    updateTextInput(session,     "genbankId",     value    = "AY214391.1")
    updateRadioButtons(session,  "paddingChoice", selected = 1)
    updateSelectInput(session,   "plasmidName",   selected = 1)
  })
  
  #Reset function
  reset <- function(){
    
    #Reset the global inputs to their default values
    dF$downloadF  <<- FALSE
    dFF$downloadF <<- FALSE
    oligos <<- NULL
    gbFile <<- NULL
    
    #updateRadioButtons(session, "plasmidCond", selected = 0)
    updateSelectInput(session,  "toolSeries",    selected = 0)
    updateRadioButtons(session, "gRNAtype",      selected = 1 )
    updateTextInput(session,    "gRNA",          value    = NA)
    updateTextInput(session,    "crisprSeq",     value    = NA)
    updateRadioButtons(session, "sense",         selected = 0 )
    updateRadioButtons(session, "cDNAtype",      selected = 1 )
    updateTextInput(session,    "cDNA",          value    = NA)
    updateTextInput(session,    "genbankId",     value    = NA)
    updateRadioButtons(session, "paddingChoice", selected = 1 )
    #updateTextInput(session, "geneId", value = NA)
    updateSelectInput(session,  "mh",            selected = 48)
    updateSelectInput(session,  "padding",       selected = 0 )
    updateTextInput(session,    "geneId",        value    = NA)
    updateSelectInput(session,  "plasmidName",   selected = 1 )
    
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
  
})
