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
             windowTitle = "Epic Title Fight",
             
             #Stylesheet
             theme = "ogtheme.css", 
             
             #Page title box
             tags$div("Epic Title Fight v0.0.5", 
                      style = "color:white"),
             
             ########ABOUT TAB#################################################
             tabPanel(tags$div("About", style = "color:white"),
                      titlePanel(""),
                      
                      #Sidebar panel with links
                      column(2, wellPanel(
                        tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isulogo.jpg",     width = "90%")))),
                        p(""),
                        tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "mayoclinic.jpeg", width = "50%"))))
                      )),
                      
                      #Text area in center of page
                      column(9, wellPanel(
                      ))
                      
             ),
             
             ##########INSTRUCTIONS############################################
             tabPanel(tags$div("Instructions", style = "color:white"),
                      titlePanel(""),
                      
                      #Sidebar panel with links
                      column(2, wellPanel(
                        tags$div(tags$span(a(href = "https://www.iastate.edu/",   target = "_blank", tags$img(src = "isulogo.jpg",     width = "90%")))),
                        p(""),
                        tags$div(tags$span(a(href = "https://www.mayoclinic.org", target = "_blank", tags$img(src = "mayoclinic.jpeg", width = "50%"))))
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
                        
                        ############Plasmid selection section##############################
                        #Plasmid Instructions
                        #p(paste0("0. Do you want to use a pGTag Series plasmid?")),
                        #radioButtons("plasmidCond",
                        #             label = "",
                        #             choices = list("Yes" = 1,
                        #                            "No"  = 0),
                        #             selected = 0),
                        #
                        #conditionalPanel(condition = "input.plasmidCond == '1'",
                        #                 selectInput("plasmidType",
                        #                             label = "",
                        #                             choices = list("None" = 0,
                        #                                            "pGTag-eGFP-B-actin_(062917)" = 1,
                        #                                            "pGTag-eGFP-caax-B-actin_(062917)" = 2,
                        #                                            "pGTag-eGFP-caax-SV40_(062917)" = 3,
                        #                                            "pGTag-eGFP-SV40_(062917)" = 4,
                        #                                            "pGTag-Gal4-VP16-B-actin_(062917)" = 5,
                        #                                            "pGTag-NLS-eGFP-B-actin_(062917)" = 6,
                        #                                            "pGTag-NLS-eGFP-SV40_(062917)" = 7,
                        #                                            "pGTag-NLS-TagRFP-B-actin_(062917)" = 8,
                        #                                            "pGTag-NLS-TagRFP-SV40_(062917)" = 9,
                        #                                            "pGTag-TagRFP-B-actin_(062917)" = 10,
                        #                                            "pGTag-TagRFP-caax-B-actin_(062917)" = 11,
                        #                                            "pGTag-TagRFP-caax-SV40_(062917)" = 12,
                        #                                            "pGTag-TagRFP-SV40_(062917)" = 13))
                        #                 
                        #),
                        ############Guide RNA selection section############################ 
                        #Guide RNA instructions
                        p(paste0("1. Select the guide RNA type. ",
                                 "You can use the universal guide RNA, or paste a custom ", 
                                 "guide sequence in PLAIN TEXT format (no FASTA header).")),
                        
                        #Buttons to select input$gRNAtype
                        radioButtons("gRNAtype", 
                                     label = "", 
                                     choices = list("Universal Guide RNA (for use with pGTag Series plasmids)" = 1, 
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
                          #This panel displays if the user knows how many padding nucleotides to put in
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
                        ),
                        
                        
                        
                        ############Microhomology Selection section########################
                        #Determine the length of microhomology
                        #Instructions
                        p("4. Select the length of microhomology you wish to use (recommended value = 48):"),
                        
                        #Slider bar to select mh length; input$mh
                        selectInput("mh", 
                                    label = "", 
                                    choices = c("12" = 12, 
                                                   "24" = 24, 
                                                   "48" = 48),
                                    selected = 48,
                                    width = '100px'),
                        
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
                        ),

                        
                        
                        ############SUBMIT####################
                        #Conditional display of submit buttons, depending on cDNA type
                        
                        #If pasting cDNA ####
                        conditionalPanel(
                          condition = "input.cDNAtype == 1",
                          p("After pushing the Submit button, please wait a few seconds for your results to appear."),
                          actionButton("geneIdSubmit", label = "Submit to ENSEMBL")
                        ),
                        
                        #If using gene ID ####
                        conditionalPanel(
                          condition = "input.cDNAtype == 2",
                          textOutput("geneProg"),
                          actionButton("submit", label = "Submit")
                        ),
                        
                        
                        p(""),
                        p(""),
                        p(""),
                        
                        
                        ####Output####
                        p(""),
                        p(""),
                        p("5' forward oligo: "),
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
                        textOutput("threePR"),
                        
                        p(""),
                        p(""),
                        
                        uiOutput("downOut")
                      )
                      )
             ),
             
             
             
             ##########HOW TO CITE Tab#########################################
             tabPanel(
               tags$div("How to Cite", style = "color:white"),
               titlePanel(""),
               #Sidebar panel with links
               column(2, wellPanel(
                 p("Paper link here?")
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 p("When the paper is published, citation goes here...")
               ))
               
             ),
             
             ##########CONTACT US Tab##########################################
             tabPanel(
               tags$div("Contact", style = "color:white"),
               titlePanel(""),
               #Sidebar panel with links
               column(2, wellPanel(
                 p("Lab website links here.")
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 p("Please contact <epictitlefightsupport@iastate.edu> to report issues and request support."),
                 p("A standalone version of this code may be downloaded from", tags$a(href = "https://github.com/Dobbs-Lab/mmejWebTool", target = "_blank", " GitHub."), " The R code is provided as-is under <license>. Please be aware that you modify the code at your own risk; we are unable to provide support for modified versions.")
               ))
               
             ),
             ##########TOOLS AND DOWNLOADS TAB#################################
             
             tabPanel(
               tags$div("Tools and Downloads", style = "color:white"),
               titlePanel(""),
               #Sidebar panel with links
               column(2, wellPanel(
                 p(tags$a(href = "https://github.com/Dobbs-Lab/mmejWebTool", target = "_blank", "Download Epic Title Fight at GitHub"))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 #p("Download standalone version, or other stuff."),
                 
                 h1("Download G-Series Plasmid Sequences"),
                 
                 p(tags$a(href     = 'plasmids/allplasmids.zip', 
                          target   = 'blank', 
                          'Download all plasmids', 
                          download = '062917_gseries_plasmids.zip')),
                 p(tags$a(href     = 'plasmids/pGTag-eGFP-B-actin_(062917).ape',
                          target   = 'blank', 
                          "pGTag-eGFP-B-actin_(062917)", 
                          download = "pGTag-eGFP-B-actin_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-eGFP-caax-B-actin_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-eGFP-caax-B-actin_(062917)", 
                          download = "pGTag-eGFP-caax-B-actin_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-eGFP-caax-SV40_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-eGFP-caax-SV40_(062917)", 
                          download = "pGTag-eGFP-caax-SV40_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-eGFP-SV40_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-eGFP-B-actin_(062917)", 
                          download = "pGTag-eGFP-B-actin_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-Gal4-VP16-B-actin_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-Gal4-VP16-B-actin_(062917)", 
                          download = "pGTag-Gal4-VP16-B-actin_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-NLS-eGFP-B-actin_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-NLS-eGFP-B-actin_(062917)", 
                          download = "pGTag-NLS-eGFP-B-actin_(062917)).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-NLS-eGFP-SV40_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-NLS-eGFP-SV40_(062917)", 
                          download = "pGTag-NLS-eGFP-SV40_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-B-actin_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-NLS-TagRFP-B-actin_(062917)", 
                          download = "pGTag-NLS-TagRFP-B-actin_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-SV40_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-NLS-TagRFP-SV40_(062917)", 
                          download = "pGTag-NLS-TagRFP-SV40_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-TagRFP-B-actin_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-TagRFP-B-actin_(062917)", 
                          download = "pGTag-TagRFP-B-actin_(062917).ape")),
                 p(tags$a(href = 'plasmids/pGTag-TagRFP-caax-B-actin_(062917).ape', 
                          target = 'blank', 
                          "pGTag-TagRFP-caax-B-actin_(062917)", 
                          download = "pGTag-TagRFP-caax-B-actin_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-TagRFP-caax-SV40_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-TagRFP-caax-SV40_(062917)", 
                          download = "pGTag-TagRFP-caax-SV40_(062917).ape")),
                 p(tags$a(href     = 'plasmids/pGTag-TagRFP-SV40_(062917).ape', 
                          target   = 'blank', 
                          "pGTag-TagRFP-SV40_(062917)", 
                          download = "pGTag-TagRFP-SV40_(062917).ape"))
               ))
               
             ),
             
             ##########FUNDING Tab#############################################
             tabPanel(
               tags$div("Funding", style = "color:white"),
               titlePanel(""),
               #Sidebar panel with links
               column(2, wellPanel(
                 tags$html(tags$div(tags$span(a(href = "https://www.iastate.edu/", target = "_blank", "Iowa State University"))),
                           tags$div(tags$span(a(href = "https://www.mayoclinic.org/", target = "_blank", "The Mayo Clinic"))),
                           tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", target = "_blank", "Genome Writers Guild"))),
                           tags$div(tags$span(a(href = "https://dill-picl.org", target = "_blank", "Dill-PICL Lab"))),
                           tags$div(tags$span(a(href = "https://www.nih.gov/", target = "_blank", "National Institutes of Health (NIH)")))
                 ))),
               
               #Text area in center of page
               column(9, wellPanel(
                 tags$p("This project was funded through NIH R24 OD020166 and is a joint effort by Iowa State University and The Mayo Clinic."),
                 tags$p("This server is generously hosted by the", a(href = "https://dill-picl.org/", target = "_blank", 'Dill-PICL Lab'), "at Iowa State University.")
               ))
             ),
             
             
             ######STATUS and CHANGELOG Tab####################################
             tabPanel(
               tags$div("Change Log", style = "color:white"),
               titlePanel(""),
               
               column(2, wellPanel(
                 tags$html(tags$div(tags$span(a(href = "https://www.iastate.edu/", target = "_blank", "Iowa State University"))),
                           tags$div(tags$span(a(href = "https://www.mayoclinic.org/", target = "_blank", "The Mayo Clinic"))),
                           tags$div(tags$span(a(href = "https://www.genomewritersguild.org/", target = "_blank", "Genome Writers Guild"))),
                           tags$div(tags$span(a(href = "https://dill-picl.org", target = "_blank", "Dill-PICL Lab"))),
                           tags$div(tags$span(a(href = "https://www.nih.gov/", target = "_blank", "National Institutes of Health (NIH)")))
                 ))),
               
               #Text area in center of page
               column(9, wellPanel(
                 includeHTML("www/changelog.html")
               ))
             )
  ))
#



