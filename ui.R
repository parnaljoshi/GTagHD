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
             windowTitle = "GTagHD",
             
             #Stylesheet
             theme = "ogtheme.css", 
             
             #Page title box
             tags$div("GTagHD v1.0.10", 
                      style = "color:white"),
             
             ########ABOUT TAB#################################################
             tabPanel(tags$div("About", style = "color:white"),
                      titlePanel(""),
                      
                      #Sidebar panel with links
                      column(2, wellPanel(
                        tags$div(tags$span(a(href = "http://ll-g2f.gdcb.iastate.edu/gss/", target = "_blank", tags$img(src = "GSS logo small.png", width = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isu-logo-alt.png",     width = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "MC_stack_4c_DAC.png", width = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", 
                                             target = "_blank", 
                                             tags$img(src = "genome-writers-guild-logo_DAC.png", width = "100%"))))
                      )),
                      
                      #Text area in center of page
                      column(9, wellPanel(
                        includeHTML("www/about.html")
                      ))
                      
             ),
             
             ##########INSTRUCTIONS############################################
             tabPanel(tags$div("Instructions and FAQs", style = "color:white"),
                      titlePanel(""),
                      
                      #Sidebar panel with links
                      column(2, wellPanel(
                        tags$div(tags$span(a(href = "http://ll-g2f.gdcb.iastate.edu/gss/", target = "_blank", tags$img(src = "GSS logo small.png", width = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isu-logo-alt.png",     width = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "MC_stack_4c_DAC.png", width = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", 
                                             target = "_blank", 
                                             tags$img(src = "genome-writers-guild-logo_DAC.png", width = "100%"))))
                      )),
                      
                      #Text area in center of page
                      column(9, wellPanel(
                        
                        #Display page instructions
                        includeHTML("www/instructions.html")
                      ))
                      
             ),
             
             ##########SUBMIT SINGLE JOB TAB###################################
             tabPanel(id = "single",
                      tags$div("Submit Single Job", style = "color:white"),
                      titlePanel(""),
                      
                      ##Sidebar############################################################
                      #Adds a sidebar for users to pre-populate fields with an example, and reset the form
                      column(2, wellPanel(
                        
                        #Cut/Paste gene/exon example; input$example
                        actionLink("example", 
                                   label = "Pasted Sequence Example"),
                        
                        p(""),
                        
                        #Genbank Example
                        actionLink("exampleGenbank", 
                                   label = "GenBank Example"),
                        p(""),
                        
                        #ENSEMBL gene ID example; input$exampleEnsembl
                        #actionLink("exampleEnsembl", 
                        #           label = "Example For ENSEMBL Function"),
                        
                        #p(""),
                        
                        #Reset Button; input$reset
                        actionLink("reset", 
                                   label = "Reset Form")
                      )),
                      
                      
                      ####Main Bar#########################################################
                      #Main panel for entering information and submitting job
                      column(9, wellPanel(
                        
                        ############Guide RNA selection section############################ 
                        #Guide RNA instructions
                        #Buttons to select input$gRNAtype
                        radioButtons("gRNAtype", 
                                     label = "1. Select the guide RNA type. You can use the universal guide RNA (for use with pGTag Series plasmids), or paste a custom guide sequence in PLAIN TEXT format (no FASTA header.)", 
                                     choices = list("Universal Guide RNA" = 1, 
                                                    "Custom Guide RNA" = 2), 
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
                        )),
                        wellPanel(
                          
                          ############CRISPR target sequence section#########################
                          #CRISPR target Instructions
                          p(tags$div(tags$b("2. Enter the CRISPR target sequence. Please only include the 20 nt target sequence; DO NOT include the PAM sequence."))),
                          
                          #conditionalPanel(
                          #  condition = "input.sense == 0",
                          #  p("Please include the 20 nucleotides preceding the PAM AND the PAM sequence."),
                          #  p("For example, a target for SpCas9 should have a sequence 23 bp long - 20 nucleotides, plus the 3 bp 5'-NGG-3' PAM sequence."),
                          #  p("GTagHD assumes that the CRISPR will cut 3bp upstream of your PAM sequence.")
                          #),
                          #conditionalPanel(
                          #  condition = "input.sense == 1",
                          #  p("Please include the PAM sequence and the 20 nucleotides succeding the PAM."),
                          #  p("For example, a target for SaCas9 should have a sequence 26 bp long - the reverse complement of the 5'-NNNRRT-3' PAM, followed by 20 nucleotides."),
                          #  p("GTagHD assumes that the CRISPR will cut 3bp downstream of the end of the PAM sequence.")
                          #),
                          p("GTagHD assumes that your CRISPR will cut between bases 17 and 18 in the case of a 5'-3' targeting sequence, and between bases 3 and 4 in the case of a 3'-5' targeting sequence, which is consistent with most Cas9 species. See 'Instructions and FAQ' tab for further clarification."),
                          #Space to output validation results; output$validcrisprseq
                          textOutput("validcrisprseq"),
                          
                          #Box to paste crispr sequence; input$crisprSeq
                          textAreaInput("crisprSeq", 
                                        label = "", 
                                        value = "", 
                                        placeholder = "Paste CRISPR target sequence here..."),
                          
                          radioButtons("sense",
                                       label = "Is your CRISPR target sequence in the sense (5'-target-PAM-3') or anti-sense (5'-PAM-target-3') orientation?",
                                       choices = c("sense" = 0,
                                                   "anti-sense" = 1),
                                       selected = 0,
                                       inline = TRUE)
                        ),
                        
                        
                        ###########gene/Gene ID Section####################################
                        #Gene ID/geneinstructions
                        wellPanel(
                          #Buttons to choose cDNAtype; input$cDNAtype
                          radioButtons("cDNAtype", 
                                       label = "3. Specify the target gene of interest in the form of a GenBank accession number (ENSEMBL support coming soon), or copy/paste the gene sequence.", 
                                       choices = list("GenBank Gene ID" = 1,
                                                      #"ENSEMBL Gene ID" = 3,
                                                      "Pasted gene/exon sequence" = 2), 
                                       selected = 1),
                          
                          #####cDNAtype == GENBANK ID####
                          conditionalPanel(
                            condition = "input.cDNAtype == 1",
                            
                            p("Paste GenBank Accession here. If the entry matching your accession number does not have exon location information, automatic codon repair is not possible."),
                            #p("This feature is still under development. Certain GenBank sequences will cause the application to crash due to a formatting conflict. A fix is in development."),
                            #Space to output validated gene ID; output$validgeneid
                            textOutput("validgenbankid"),
                            
                            textOutput("validgenbankDNA"),
                            textOutput("validrefseqgbgenbankid"),
                            
                            #Box to paste gene ID; input$geneId
                            textInput("genbankId", 
                                      label = "", 
                                      value = "", 
                                      placeholder = "Paste GenBank nucleotide gene accession here..."),
                            
                            textOutput("exonWarning"),
                            
                            p("Do you want to generate nucleotides to repair a codon break?"),
                            radioButtons("paddingChoice",
                                         label    = "",
                                         choices  = list("Yes" = 1,
                                                        "No"  = 2),
                                         selected = 1,
                                         inline = TRUE)
                            
                          ),
                          
                          #####cDNAtype == ENSEMBL ID#####
                          #This panel displays if the user wants to use an ENSEMBL gene ID
                          conditionalPanel(
                            condition = "input.cDNAtype == 3",
                            
                            
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
                            
                            p("Please paste your sequence in PLAIN TEXT format. (No FASTA headers.)"),
                            
                            #Space to output validated cDNA sequence; output$validcdna
                            textOutput("validcdna"),
                            
                            #Box to paste cDNA sequence; input$cDNA
                            textAreaInput("cDNA", 
                                          label = "", 
                                          value = "", 
                                          placeholder = "Paste gene/exon sequence here...", 
                                          rows = 6),
                            
                            #Padding Selection ################
                            #This panel displays if the user knows how many padding nucleotides to put in
                            #Instructions
                            p("Please select the number of additional nucleotides required to maintain the reading frame."),
                            
                            #Selection drop-down box; input$padding
                            selectInput("padding", 
                                        label = "", 
                                        choices = list("0" = 0, 
                                                       "1" = 1, 
                                                       "2" = 2), 
                                        selected = "", 
                                        width = '100px')
                          )),
                        
                        
                        wellPanel(
                          ############Microhomology Selection section########################
                          #Determine the length of microhomology
                          
                          #Slider bar to select mh length; input$mh
                          selectInput("mh", 
                                      label = "4. Select the length of microhomology you wish to use (recommended value = 48):", 
                                      choices = c("12" = 12,
                                                  "24" = 24, 
                                                  "36" = 36,
                                                  "48" = 48),
                                      selected = 48),
                          
                          #Conditionally output microhomology validation for gene ID
                          conditionalPanel(
                            condition = "input.cDNAtype == 1",
                            #Space to output microhomology validation resutls; output$validmhgeneid
                            textOutput("validmhgeneid")
                          ),
                          
                          #Conditionally output microhomology validation for gene ID
                          conditionalPanel(
                            condition = "input.cDNAtype == 2",
                            
                            #Space to output microhomology validation resutls; output$validmhcdna
                            textOutput("validmhcdna")
                          )),
                        
                        ######################Plasmid Selection section#######################
                        wellPanel(
                          
                            selectInput("plasmidName",
                                        label = "5. If you are using a pGTag series plasmid, you can specify the plasmid below and download a copy of the finished pGTag donor vector, including homology arms.",
                                        choices = c("pGTag-eGFP-B-actin" = 1,
                                                    "pGTag-eGFP-caax-B-actin" = 2,
                                                    "pGTag-eGFP-caax-SV40" = 3,
                                                    "pGTag-eGFP-B-actin" = 4,
                                                    "pGTag-Gal4-VP16-B-actin" = 5,
                                                    "pGTag-NLS-eGFP-B-actin" = 6,
                                                    "pGTag-NLS-eGFP-SV40" = 7,
                                                    "pGTag-NLS-TagRFP-B-actin" = 8,
                                                    "pGTag-NLS-TagRFP-SV40" = 9,
                                                    "pGTag-TagRFP-B-actin" = 10,
                                                    "pGTag-TagRFP-caax-B-actin" = 11,
                                                    "pGTag-TagRFP-caax-SV40" = 12,
                                                    "pGTag-TagRFP-SV40" = 13
                                        ),
                                        selected = 1)
                          ),
                        
                        
                        ############SUBMIT####################
                        #Conditional display of submit buttons, depending on cDNA type
                        wellPanel(
                          #If pasting cDNA ####
                          conditionalPanel(
                            condition = "input.cDNAtype == 3",
                            p("After pushing the Submit button, please wait a few seconds for your results to appear."),
                            actionButton("geneIdSubmit", label = "Submit to ENSEMBL")
                          ),
                          
                          #If using GenBank ID ####
                          conditionalPanel(
                            condition = "input.cDNAtype == 1",
                            p("After pushing the Submit button, please wait a few seconds for your results to appear."),
                            actionButton("genBankSubmit", label = "Submit to GenBank")
                          ),
                          
                          #If using ENSEMBL ID ####
                          conditionalPanel(
                            condition = "input.cDNAtype == 2",
                            textOutput("geneProg"),
                            actionButton("submit", label = "Submit")
                          )),
                        
                        wellPanel(
                          
                          
                          ####Output####
                          h3("Oligonucleotides:"),
                          tags$br(),
                          p("5' forward oligo: "),
                          textOutput("fivePF"),
                          tags$br(),
                          p("5' reverse oligo:"),
                          textOutput("fivePR"),
                          tags$br(),
                          p("3' forward oligo:"),
                          textOutput("threePF"),
                          tags$br(),
                          p("3' reverse oligo:"),
                          textOutput("threePR"),
                          
                          tags$br(),
                          
                          uiOutput("downOut"),
                          tags$br(),
                          uiOutput("downPlasOut")
                          
                        )
                      )
             ),
             
             ##########TOOLS AND DOWNLOADS TAB#################################
             
             tabPanel(
               tags$div("Tools and Protocols", style = "color:white"),
               titlePanel(""),
               
               #Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href = "http://ll-g2f.gdcb.iastate.edu/gss/", target = "_blank", tags$img(src = "GSS logo small.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isu-logo-alt.png",     width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "MC_stack_4c_DAC.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", 
                                      target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png", width = "100%")))),
                 tags$br(),
                 p(tags$a(href = "https://github.com/Dobbs-Lab/GTagHD", target = "_blank", "Download GTagHD at GitHub"))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 h3("Download Stand-alone GTagHD"),
                 p("A standalone version of this code may be downloaded from", tags$a(href = "https://github.com/Dobbs-Lab/GTagHD", target = "_blank", " GitHub."), " The R code is provided as-is, and may not be used in commercial applications. Please be aware that you modify the code at your own risk; we are unable to provide support for modified versions.")
               ),
               wellPanel(
                 #p("Download standalone version, or other stuff."),
                 
                 h3("Download G-Series Plasmid Sequences"),
                 
                 tags$p("Download all plasmids in ", 
                        tags$a(href     = 'plasmids/062917_gseries_plasmids_gb.zip', 
                               target   = 'blank', 
                               'GenBank', 
                               download = '062917_gseries_plasmids_gb.zip'),
                        " format or ",
                        tags$a(href     = 'plasmids/062917_gseries_plasmids_ape.zip', 
                               target   = 'blank', 
                               'ApE', 
                               download = '062917_gseries_plasmids_ape.zip'),
                        " format"),
                 tags$p("Or download individual plasmids: "),
                 tags$p("pGTag-eGFP-B-actin_(062917): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-B-actin_(062917).gb',
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-B-actin_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-B-actin_(062917).ape',
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-B-actin_(062917).ape")
                        
                 ),
                 
                 tags$p("pGTag-eGFP-caax-B-actin_(062917): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-B-actin_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-caax-B-actin_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-B-actin_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-caax-B-actin_(062917).ape")
                        
                 ),
                 
                 tags$p("pGTag-eGFP-caax-SV40_(062917): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-SV40_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-caax-SV40_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-SV40_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-caax-SV40_(062917).ape")
                        
                 ),
                 
                 tags$p("pGTag-eGFP-B-actin_(062917): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-SV40_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-B-actin_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-SV40_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-B-actin_(062917).ape")
                 ),
                 
                 tags$p("pGTag-Gal4-VP16-B-actin_(062917)",
                        tags$a(href     = 'plasmids/pGTag-Gal4-VP16-B-actin_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-Gal4-VP16-B-actin_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-Gal4-VP16-B-actin_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-Gal4-VP16-B-actin_(062917).ape")
                 ),
                 
                 tags$p("pGTag-NLS-eGFP-B-actin_(062917)",
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-B-actin_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-eGFP-B-actin_(062917)).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-B-actin_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-eGFP-B-actin_(062917)).ape")
                 ),
                 
                 tags$p("pGTag-NLS-eGFP-SV40_(062917)",
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-SV40_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-eGFP-SV40_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-SV40_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-eGFP-SV40_(062917).ape")
                 ),
                 tags$p("pGTag-NLS-TagRFP-B-actin_(062917)",
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-B-actin_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-TagRFP-B-actin_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-B-actin_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-TagRFP-B-actin_(062917).ape")
                 ),
                 
                 tags$p("pGTag-NLS-TagRFP-SV40_(062917)",
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-SV40_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-TagRFP-SV40_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-SV40_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-TagRFP-SV40_(062917).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-B-actin_(062917)",
                        tags$a(href     = 'plasmids/pGTag-TagRFP-B-actin_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-B-actin_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-TagRFP-B-actin_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-B-actin_(062917).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-caax-B-actin_(062917)",
                        tags$a(href = 'plasmids/pGTag-TagRFP-caax-B-actin_(062917).gb', 
                               target = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-caax-B-actin_(062917).gb"),
                        tags$a(href = 'plasmids/pGTag-TagRFP-caax-B-actin_(062917).ape', 
                               target = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-caax-B-actin_(062917).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-caax-SV40_(062917)",
                        tags$a(href     = 'plasmids/pGTag-TagRFP-caax-SV40_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-caax-SV40_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-TagRFP-caax-SV40_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-caax-SV40_(062917).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-SV40_(062917)",
                        tags$a(href     = 'plasmids/pGTag-TagRFP-SV40_(062917).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-SV40_(062917).gb"),
                        tags$a(href     = 'plasmids/pGTag-TagRFP-SV40_(062917).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-SV40_(062917).ape")
                 )
                 
               ))
               
             ),
             
             ##########FUNDING Tab#############################################
             tabPanel(
               tags$div("Funding", style = "color:white"),
               titlePanel(""),
               #Sidebar panel with links
               column(2, wellPanel(
                 #Sidebar panel with links
                 tags$div(tags$span(a(href = "http://ll-g2f.gdcb.iastate.edu/gss/", target = "_blank", tags$img(src = "GSS logo small.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isu-logo-alt.png",     width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "MC_stack_4c_DAC.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", 
                                      target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.nih.gov/", target = "_blank", tags$img(src = "nihlogo.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://dill-picl.org", target = "_blank", tags$img(src = "lawlab_web_wiki_header.png", width = "100%"))))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 tags$p("This webtool was created and is maintained by funding through ", a(href = "https://projectreporter.nih.gov/project_info_description.cfm?aid=9276155&icde=37715852", target = "_blank", 'NIH R24 OD020166: Development of Tools for Site-directed Analysis of Gene Function'), " and is a joint effort by ", a(href = "https://www.iastate.edu/", target = "_blank", 'Iowa State University'), " and ", a(href = "https://www.mayoclinic.org/", target = "_blank", " The Mayo Clinic.")),
                 tags$p("This server is generously hosted by the", a(href = "https://dill-picl.org/", target = "_blank", 'Lawrence-Dill Plant Informatics and Computation (Dill-PICL) Lab'), "at Iowa State University.")
               ),
               
               wellPanel(
                 p("NIH R24 OD020166 Project Abstract: "),
                 p("The overarching goal of this application is to create tools and efficient methods to define genes that can promote human health. While a tremendous amount of data has been cataloged on gene mutation and changes in gene expression associated with complex human disease, our understanding of those genes that could be co-opted to restore patient health is lacking. To address this need and test for genes that when restored to wild type function promote health, we propose develop mutagenic, revertible and conditional alleles that provide spatial and temporal control of gene expression. The ability to make site-specific, untagged mutant alleles in zebrafish and other models has been greatly advanced by custom nucleases that include TALENs and CRISPR/Cas9 systems. These systems operate on the same principle: they are designed to bind to specific sequences in the genome and create a double strand break. The goals of this proposal leverage the activities of TALEN and CRISPR/Cas9 technologies to make site-specific double strand breaks. First, we propose to develop a suite of vectors to make integration alleles that are highly mutagenic and allow production of conditional and revertible alleles. Second, we propose to develop methods to generate predictable alleles in zebrafish at TALEN- and CRISPR/Cas9-induced double strand break sites by invoking the microhomology mediated end-joining pathway. Third, leveraging our preliminary data, we propose to improve methods for homology directed repair with oligonucleotides to create disease associated alleles in zebrafish and for site-specific integration using homologous recombination at TALENs and CRISPR/Cas9 cut sites. Fourth, we propose use single-strand annealing at TALENs and CRISPR/Cas9 cut sites to promote precise transgene integration to make tagged and highly mutagenic allele. These tools and techniques will have direct implications for providing precise gene editing techniques to assess the roles of genes in disease and their ability to promote health following disease progression. While we will develop these methodologies in zebrafish due to their ease of gene delivery, we anticipate these methodologies will not only enhance the efficiency of gene editing but will be readily adaptable for use in other model organisms and large animals. In our opinion, this will have important implications for modeling human disease and health in animal systems by greatly enhancing the ability to make predictible alleles, small nucleotide polymorphisms similar to those associated with human disease, and conditional alleles to test for the ability of a gene to restore health.")
               ))
             ),
             
             ##########HOW TO CITE Tab#########################################
             tabPanel(
               tags$div("How to Cite", style = "color:white"),
               titlePanel(""),
               
               #Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href = "http://ll-g2f.gdcb.iastate.edu/gss/", target = "_blank", tags$img(src = "GSS logo small.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isu-logo-alt.png",     width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "MC_stack_4c_DAC.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", 
                                      target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png", width = "100%"))))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 p("Manuscript is in prep; citation will be available shortly.")
               ))
               
             ),
             
             ##########CONTACT US Tab##########################################
             tabPanel(
               tags$div("Report Bugs or Contact Us", style = "color:white"),
               titlePanel(""),
               #Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href = "http://ll-g2f.gdcb.iastate.edu/gss/", target = "_blank", tags$img(src = "GSS logo small.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isu-logo-alt.png",     width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "MC_stack_4c_DAC.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", 
                                      target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png", width = "100%"))))
               )),
               
               #Text area in center of page
               column(9, 
                      wellPanel(
                        p("Please use the form below, or email us directly at GeneSculptSuite@gmail.com, to report issues and request support."),
                        p("Please note that GTagHD requires the latest version of Javascript to be enabled for full functionality.")
                      ),
                 
                   tags$iframe(id = "googleform", 
                               src = "https://docs.google.com/forms/d/e/1FAIpQLSeq9aDRj6EOCskBwPsA2PFQ2LsKxT4v85-rGTlYQOk0n8X2Gw/viewform?usp=pp_url&entry.358268393&entry.1646278736=GTagHD&entry.1934309806&entry.565411344&entry.754537383&entry.826100992",
                               width = 760,
                               height = 2000,
                               frameborder = 0,
                               marginheight = 0)
                 
             )
               
             ),
             
             
            
             
             
             ######STATUS and CHANGELOG Tab####################################
             tabPanel(
               tags$div("Change Log", style = "color:white"),
               titlePanel(""),
               
               column(2, wellPanel(
                 tags$div(tags$span(a(href = "http://ll-g2f.gdcb.iastate.edu/gss/", target = "_blank", tags$img(src = "GSS logo small.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isu-logo-alt.png",     width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "MC_stack_4c_DAC.png", width = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", 
                                      target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png", width = "100%"))))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 includeHTML("www/changelog.html")
               ))
             )
  ))
#



