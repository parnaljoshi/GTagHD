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

#Required files
source("functions.R")

shinyServer(function(input, output, session) {
  
  dF <- reactiveValues(downloadF = FALSE)
  oligos <<- NULL
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
  #Ensure that CRISPR sequence is a DNA sequence, is 23 nts long, 
  #has a PAM sequence in either forward or reverse direction 
  
  validCrisprSeq <- reactive({
    
    #Once text is entered, run through validation tests
    if(input$crisprSeq != ""){
      uCS <- toupper(input$crisprSeq)
      validate(
        #Checks that only A, C, G, and T are in sequence
        need(!str_detect(uCS, "[^ACGT]"), 
             paste0("Error: Input DNA sequence contains non-standard nucleotides. ", 
                    "Allowed nucleotides are A, C, G, and T.")),
        
        #Checks that the target is 23 nucleotides long
        need(nchar(uCS) == 23, 
             "Error: CRISPR target sequence is not 23 nucleotides long. Please check input sequence."),
        
        #Checks that there is a recognizeable PAM sequence in the forward or reverse (or complement) direction
        need(((substring(uCS, nchar(uCS) - 1, nchar(uCS)) == "GG") | 
                (substring(uCS, nchar(uCS) - 1, nchar(uCS)) == "CC") |
                (substring(uCS, 1, 2) == "GG") | 
                (substring(uCS, 1, 2) == "CC")),
             "Error: CRISRPR target sequence does not have an identifiable PAM sequence. Please check input sequence.")
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
            need(count == 1, paste0("Error: CRISPR target sequence appears ", count, " times in the cDNA.")),
            
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
    }
      
      
    })
  
  #Function to validate the input GenBank ID
  validGenbankId <- reactive({
    tryCatch({
      gbFile = entrez_fetch("nucleotide", input$genbankId, rettype = "gb")
    }, error = function(err){
      validate(
        need(1 == 2, "Error: That GenBank ID does not exist.")
      )
    })
    if(length(gbFile) > 0){
      ""
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
        if((substring(input$crisprSeq, nchar(input$crisprSeq) - 1, nchar(input$crisprSeq)) == "GG") | 
           (substring(input$crisprSeq, nchar(input$crisprSeq) - 1, nchar(input$crisprSeq)) == "gg")){
          cutIndex <- crisprLoc[2] - 6
          
          #If the crispr begins with CCN (it is in the reverse direction), cut after the PAM sequence:
        } else if((substring(input$crisprSeq, 1, 2) == "CC") | (substring(input$crisprSeq, 1, 2) == "cc")){
          cutIndex <- crisprLoc[1] + 5
        }
        
        validate(
          if(revCount == 0 && count == 1){
            need(cutIndex - as.numeric(input$mh) >= 0, 
                 paste0("Error: Microhomology length is too long for this CRISPR target sequence and cDNA pairing. ",
                        "Choose a smaller homology length. Maximum allowed homology ", 
                        "length for this cDNA/CRISPR pairing is ", 
                        cutIndex - 1, " nucleotides."))
          }
        )
        
        }
      }
  })
  
  ######################Gene ID############################
  #Validate GeneID
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
  
  output$validgenbankid <- renderText({
    validGenbankId()
  })

  #Print out the results of gene ID validation
  output$validgeneid <- renderText({
    if(is.null(validGeneId())){
      paste0(getEnsemblSpecies(input$geneId)[2], " ENSEMBL gene ID detected.")
    } else {
      validGeneId()
    }
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
       is.null(validCDNA())){
      revFlag <- FALSE
      resetOutputs()
      #Convert inputs to uppercase
      if(input$gRNAtype == 1){
        guideRNA <- ""
      } else {
        guideRNA <- toupper(input$gRNA)
      }
      
      ucDNA <- toupper(input$cDNA)
      uCS <- toupper(input$crisprSeq)
      
      #Count how many times the input CRISPR target sequence appears in the cDNA sequence in the FORWARD direction
      count <- str_count(ucDNA, uCS)
      
      #Count the instance in the reverse complement of the sequence
      revCount <- str_count(reverseComplement(ucDNA), uCS)
      
      #Checks to see if using reverseComplement of cDNA
      if(revCount == 1 && count == 0){
        uDNA <- reverseComplement(input$cDNA)
        revFlag <- TRUE
      } else {
        uDNA <- input$cDNA
        revFlag <- FALSE
      }
      
      #Create the oligos
      oligos <<- doCalculations(toupper(uDNA), 
                               toupper(input$crisprSeq), 
                               guideRNA, 
                               as.numeric(input$mh),  
                               input$padding,
                               revFlag)
      dF$downloadF <<-TRUE
      printOutputs(oligos)
    } 
  })

  
  #Get cDNA/coding sequence from ENSEMBL
  observeEvent(input$geneIdSubmit, {
    resetOutputs()
    
    withProgress({
    
    
    if(is.null(validgRNA()) &&
       is.null(validCrisprSeq()) &&
       is.null(validGeneId())){
      
      setProgress(message = "Identifying Species...")
      species <- getEnsemblSpecies(toupper(input$geneId))
      
      if(input$gRNAtype == 1){
        guideRNA <- ""
      } else {
        guideRNA <- toupper(input$gRNA)
      }
      
      setProgress(message = "Querying ENSEMBL database...")
      
      
      oligos <<- getEnsemblSeq(species[1],
                              input$geneId, 
                              toupper(input$crisprSeq), 
                              guideRNA, 
                              as.numeric(input$mh))
      
      
      
      setProgress(message = "Done!")
      
      dF$downloadF <<-TRUE
      
      printOutputs(oligos)
    }
    
      
    })
  })

  #Function to print the output
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
  
  output$downOut <- renderUI({
    if(dF$downloadF){
        downloadButton("downOligos", "Download Oligos")
    } else{
  ""
}
  })
  
  
  output$downOligos <- downloadHandler(
    filename = function(){
      paste(gsub("CDT", "", gsub(" ", "_", Sys.time())), "_oligos.txt")},
    
    content = function(file){
      cat(paste0("5' Forward Oligo: ", oligos[1]), file = file, sep = "\n", append = TRUE)
      cat(paste0("5' Reverse Oligo: ", oligos[2]), file = file, sep = "\n", append = TRUE)
      cat(paste0("3' Forward Oligo: ", oligos[3]), file = file, sep = "\n", append = TRUE)
      cat(paste0("3' Reverse Oligo: ", oligos[4]), file = file, sep = "\n", append = TRUE)
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
    updateTextInput(session, "crisprSeq", value = "GCGCAGCGAGTCAGTGAGCGAGG")
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
    updateTextInput(session, "crisprSeq", value = "TGCCACAACCAGTGTGCTGCAGG")
  })

  
  #Reset function
  reset <- function(){
    
    #Reset the inputs to their default values
    dF$downloadF <<- FALSE
    oligos <<- NULL
    updateRadioButtons(session, "plasmidCond", selected = 0)
    updateRadioButtons(session, "gRNAtype", selected = 1)
    updateTextInput(session, "gRNA", value = NA)
    updateTextInput(session, "crisprSeq", value = NA)
    updateRadioButtons(session, "cDNAtype", selected = 2)
    updateTextInput(session, "cDNA", value = NA)
    updateSelectInput(session, "mh", selected = 48)
    updateSelectInput(session, "padding", selected = 0)
    updateTextInput(session, "geneId", value = NA)
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
