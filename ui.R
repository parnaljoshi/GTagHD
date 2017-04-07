#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

shinyUI(
  
  #Creates the navbar set-up
  navbarPage(id = 'mainPage',
             #Stylesheet
             theme = "ogtheme.css", 
             
             #Page title box
             tags$div("MMEJ Tool (Better Name Suggestions Welcome)", 
                      style = "color:white"),
             
             
             ########ABOUT TAB#################################################
             tabPanel(tags$div("About", style = "color:white"),
                      titlePanel(""),
                      sidebarLayout(
                        
                        #Sidebar panel with links
                        sidebarPanel(
                          width = 2,
                          tags$html(tags$div(tags$span("We can put links to", 
                                                       a(href = "http://www.iastate.edu", "other"), 
                                                       tags$a(href = "http://www.mayoclinic.org", "websites"), 
                                                       "here.")),
                                    tags$div(tags$span("Or ", 
                                                       a(href = "examplepdf.pdf", "papers"), 
                                                       ". Or anything, really.")))
                        ),
                        
                        #Text area in center of page
                        mainPanel(
                          #ISU Logo
                          tags$img(src = "isulogo.jpg",
                                   height = "75px"), 
                          p(""),
                          #Mayo Clinic Logo
                          tags$img(src = "mayoclinic.jpeg", 
                                   height = "218px", 
                                   width = "200px"),
                          #Display page instructions
                          includeHTML("www/instructions.html")
                        )
                      )
             ),
             
             ##########SUBMIT SINGLE JOB TAB###################################
             tabPanel(id = "single",
                      tags$div("Submit Single Job", style = "color:white"),
                      titlePanel(""),
                      
                      ##Sidebar############################################################
                      #Adds a sidebar for users to pre-populate fields with an example, and reset the form
                      column(2, wellPanel(
                        
                        #Cut/Paste cDNA example; input$example
                        actionLink("example", 
                                   label = "Example"),
                        
                        p(""),
                        
                        #ENSEMBL gene ID example; input$exampleEnsembl
                        actionLink("exampleEnsembl", 
                                   label = "Example For ENSEMBL Function"),
                        
                        p(""),
                        
                        #Reset Button; input$reset
                        actionLink("reset", 
                                   label = "Reset Form")
                      )),
                      
                      
                      ####Main Bar#########################################################
                      #Main panel for entering information and submitting job
                      column(9, wellPanel(
                        
                        ############Guide RNA selection section############################ 
                        #Guide RNA instructions
                        p(paste0("1. Select the guide RNA type. ",
                                 "You can use the universal guide RNA, or paste a custom ", 
                                 "guide sequence in PLAIN TEXT format (no FASTA header).")),
                        
                        #Buttons to select input$gRNAtype
                        radioButtons("gRNAtype", 
                                     label = "", 
                                     choices = list("universal guide RNA" = 1, 
                                                    "custom guide RNA" = 2), 
                                     selected = 1),
                        
                        #This panel displays if the user wants to put in a custom guide RNA
                        conditionalPanel(
                          condition = "input.gRNAtype == 2",
                          
                          #Space to output validation results; output$validgrna
                          textOutput("validgrna"),
                          
                          #Area to input custom guide RNA; input$gRNA
                          textAreaInput("gRNA", 
                                        label = "", 
                                        value = "", 
                                        placeholder = "Paste custom gRNA here...")
                        ),
                        
                        ############CRISPR target sequence section#########################
                        #CRISPR target Instructions
                        p(paste0("2. Enter the CRISPR target sequence ", 
                                 "(23 bases total - 20 from target, 3 from PAM):")),
                        
                        #Space to output validation results; output$validcrisprseq
                        textOutput("validcrisprseq"),
                        
                        #Box to paste crispr sequence; input$crisprSeq
                        textAreaInput("crisprSeq", 
                                      label = "", 
                                      value = "", 
                                      placeholder = "Paste CRISPR target sequence here..."),
                        
                        
                        ###########cDNA/Gene ID Section####################################
                        #Gene ID/cDNA instructions
                        p(paste0("3. Paste an ENSEMBL gene ID or a cDNA sequence into ", 
                                 "the box in PLAIN TEXT format (no FASTA header).")),
                        
                        #Buttons to choose cDNAtype; input$cDNAtype
                        radioButtons("cDNAtype", 
                                     label = "", 
                                     choices = list("ENSEMBL gene ID" = 1, 
                                                    "Pasted cDNA" = 2), 
                                     selected = 2),
                        
                        #####cDNAtype == GENE ID#####
                        #This panel displays if the user wants to use an ENSEMBL gene ID
                        conditionalPanel(
                          condition = "input.cDNAtype == 1",
                          
                          #Space to output validated gene ID; output$validgeneid
                          textOutput("validgeneid"),
                          
                          #Box to paste gene ID; input$geneId
                          textInput("geneId", 
                                    label = "", 
                                    value = "", 
                                    placeholder = "Paste ENSEMBL gene ID here...")
                        ),
                        
                        #####cDNAtype == CDNA SEQUENCE#####
                        
                        ###This panel displays if the user is putting in their own cDNA sequence##
                        conditionalPanel(
                          condition = "input.cDNAtype == 2",
                          
                          #Space to output validated cDNA sequence; output$validcdna
                          textOutput("validcdna"),
                          
                          #Box to paste cDNA sequence; input$cDNA
                          textAreaInput("cDNA", 
                                        label = "", 
                                        value = "", 
                                        placeholder = "Paste cDNA sequence here...", 
                                        rows = 6),
                          
                          #Padding Selection ################
                          #Padding instructions
                          p(paste0("Do you want to attempt to automatically generate the ", 
                                   "'padding' nucleotides to repair potential codon breaks?")),
                          
                          #Padding option selection; input$paddingSelection
                          radioButtons("paddingSelection", 
                                       label="", 
                                       choices = list("No" = 1, 
                                                      "Yes" = 2), 
                                       selected = 1),
                          
                          ###Auto Padding ###################
                          #This panel displays if the user wants to try to automatically generate padding codons
                          conditionalPanel(
                            
                            condition = "input.paddingSelection == 2",
                            
                            #Instructions for BLAST
                            p("Specify parameters for BLAST search:"),
                            p("How many hits to display (default is up to 5):"),
                            
                            #Numeric drop-down to select the number of hits; input$hitNumber
                            numericInput("hitNumber", 
                                         label="", 
                                         value = 5, 
                                         min = 1, 
                                         max = 20, 
                                         step = 1),
                            
                            #E-value instructions
                            p(paste0("E-Value cutoff power; only hits better than this ", 
                                     "value will be displayed (default is 3, which corresponds to 1E-03):")),
                            
                            #Numeric drop-down to select the e-value cutoff; input$eCutoff
                            numericInput("eCutoff", 
                                         label="", 
                                         value = 3, 
                                         min = 1, 
                                         step = 1),
                            
                            #BLAST submission; input$blastSubmit
                            actionButton("blastSubmit", label = "Submit BLAST")
                          ),
                          
                          ###Manual Padding ###############
                          #This panel displays if the user knows how many padding nucleotides to put in
                          conditionalPanel(
                            condition = "input.paddingSelection == 1",
                            
                            #Instructions
                            p("Please select the number of 'padding' nucleotides to repair codon changes."),
                            
                            #Selection drop-down box; input$padding
                            selectInput("padding", 
                                        label = "", 
                                        choices = list("0" = 0, 
                                                       "1" = 1, 
                                                       "2" = 2), 
                                        selected = "", 
                                        width = '100px')
                          )
                        ),
                        
                        
                        ############Microhomology Selection section########################
                        #Determine the length of microhomology
                        #Instructions
                        p("4. Select the length of microhomology you wish to use (recommended value = 24):"),
                        
                        #Slider bar to select mh length; input$mh
                        sliderInput("mh", 
                                    label = "", 
                                    value = 24, 
                                    min = 12, 
                                    max = 48, 
                                    step = 1, 
                                    ticks = TRUE),
                        
                        #Space to output microhomology validation resutls; output$validmh
                        textOutput("validmh"), 
                        
                        
                        ############SUBMIT####################
                        #Conditional display of submit buttons, depending on cDNA type
                        
                        #If pasting cDNA ####
                        conditionalPanel(
                          condition = "input.cDNAtype == 1",
                          actionButton("geneIdSubmit", label = "Submit to ENSEMBL")
                        ),
                        
                        #If using gene ID ####
                        conditionalPanel(
                          condition = "input.cDNAtype == 2",
                          actionButton("submit", label = "Submit")
                        ),
                        
                        
                        p(""),
                        p(""),
                        p(""),
                        
                        
                        #Output
                        p(""),
                        p(""),
                        p("5' forward oligo:"),
                        textOutput("fivePF"),
                        p(""),
                        p(""),
                        p("5' reverse oligo:"),
                        textOutput("fivePR"),
                        p(""),
                        p(""),
                        p("3' forward oligo:"),
                        textOutput("threePF"),
                        p(""),
                        p(""),
                        p("3' reverse oligo:"),
                        textOutput("threePR")
                        
                      )
                      )
             ),
             
             
             ##########SUMBIT MULTIPLE JOBS Tab################################
             tabPanel(tags$div("Submit Several Jobs", style = "color:white"),
                      titlePanel(""),
                      p("We could do this, but it might be better to have people use offline version instead...")
                      
             ),
             
             ##########HOW TO CITE Tab#########################################
             tabPanel(
               tags$div("How to Cite", style = "color:white"),
               titlePanel(""),
               p("When the paper is published, citation goes here...")
             ),
             
             ##########CONTACT US Tab##########################################
             tabPanel(
               tags$div("Contact", style = "color:white"),
               titlePanel(""),
               p("We can put links to lab websites or Genome Engineering stuff here.")
             ),
             
             ##########FUNDING Tab#############################################
             tabPanel(
               tags$div("Funding", style = "color:white"),
               titlePanel(""),
               p("R24 Grant Stuff Here!")
             ),
             
             
             ######STATUS and CHANGELOG Tab####################################
             tabPanel(
               tags$div("Change Log", style = "color:white"),
               titlePanel(""),
               includeHTML("www/changelog.html")
             )
             
  ))
#



