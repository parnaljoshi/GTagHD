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
             tags$div(""),
             
             ########ABOUT TAB#################################################
             tabPanel(tags$div("GTagHD v1.3.1"),
                      titlePanel(""),
                      
                      # Sidebar panel with links
                      column(2, wellPanel(
                        tags$div(tags$span(a(href         = "http://www.genesculpt.org/",               target = "_blank", 
                                             tags$img(src = "GSS logo small.png",                      width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
                                             tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
                                             tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
                                             tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://github.com/FriedbergLab/GTagHD",     target = "_blank", 
                                             tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://hub.docker.com/r/parnaljoshi/gtaghd",  target = "_blank", 
                                             tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
                                             tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
                                                      width = "100%", rel = "noopener noreferrer"))))
                      )),
                      
                      #Text area in center of page
                      column(9, wellPanel(
                        includeHTML("www/about.html")
                      ))
             ),
             
             ##########INSTRUCTIONS############################################
             tabPanel(tags$div("Instructions and FAQs"),
                      titlePanel(""),
                      
                      # Sidebar panel with links
                      column(2, wellPanel(
                        tags$div(tags$span(a(href         = "http://www.genesculpt.org/",               target = "_blank", 
                                             tags$img(src = "GSS logo small.png",                      width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
                                             tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
                                             tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
                                             tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://github.com/FriedbergLab/GTagHD",     target = "_blank", 
                                             tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://hub.docker.com/r/parnaljoshi/gtaghd",  target = "_blank", 
                                             tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
                        tags$br(),
                        tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
                                             tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
                                                      width = "100%", rel = "noopener noreferrer"))))
                      )),
                      
                      #Text area in center of page
                      column(9, wellPanel(
                        
                        #Display page instructions
                        includeHTML("www/instructions.html")
                      ))
             ),
             
             ##########SUBMIT SINGLE JOB TAB###################################
             tabPanel(id = "single",
                      tags$div("Submit Job"),
                      titlePanel(""),
                      
                      ##Sidebar############################################################
                      #Adds a sidebar for users to pre-populate fields with an example, and reset the form
                      column(2, wellPanel(
                        class = "examplePanel",
                        p(tags$b(tags$u("Example Inputs"))),
                        
                        #Cut/Paste gene/exon example; input$example
                        actionLink("example", 
                                   label = "[Pasted Sequence Example]"),
                        tags$br(),
                        tags$br(),
                        
                        #Genbank Example
                        actionLink("exampleGenbank", 
                                   label = "[GenBank Example (zebrafish noto gene)]"),
                        tags$br(),
                        tags$br(),
                        
                        #ENSEMBL gene ID example; input$exampleEnsembl
                        actionLink("exampleEnsembl", 
                                   label = "[Ensembl Example (zebrafish noto gene)]"),
                        
                        tags$br(),
                        tags$br(),
                        
                        #Reset Button; input$reset
                        actionLink("reset", 
                                   label = "Reset Form")
                      )),
                      
                      column(9, wellPanel(tags$span(tags$b(tags$p("NOTE: GTagHD will only process a single job at a time, regardless of the number of browser windows open. You should download the results of a job before submitting another. See FAQs for more information.", style="color:blue;"))),
                                          tags$span(tags$b("Please see the ", 
                                                           tags$span(tags$a(href="New_GeneWeldProtocol_102720.pdf", target="_blank", "GeneWeld protocol", style="color:black;")), 
                                                           "for detailed instructions on how to use the oligonucleotides generated by this tool."), style="color:blue;"),
                        tags$span(tags$b(tags$p("Please see \"Instructions and FAQs\" for detailed instructions on how to use GTagHD."), style="color:blue;")))),
                      
                      ####Main Bar#########################################################
                      #Main panel for entering information and submitting job
                      column(9, wellPanel(
                        
                        ############Tool Selection Section#################################
                        selectInput("toolSeries",
                                    label    = "0. Choose your GeneWeld plasmid series (use 'Custom' for non-GeneWeld plasmids):",
                                    choices  = list("pGTag"          = 0,
                                                    "pPRISM"         = 1,
                                                    #"pPRISM-Fuse"    = 2,
                                                    #"UFlip-Off"      = 3,
                                                    #"UFlip-On"       = 4
                                                    #"pPRISM-Fuse"    = 2,
                                                    #"pPRISM-Splicer" = 3,
                                                    #"UFlip"          = 4,
                                                    "Custom"  = 5
                                    ),
                                    selected = 0
                        )#,
                        
                        # conditionalPanel(condition = "input.toolSeries == 2",
                        #                  radioButtons("uFlipOp",
                        #                               label   = paste0("Select whether to use the 'on' or 'off' UFlip orientation: "),
                        #                               choices = list("Off" = 0,
                        #                                              "On"  = 1),
                        #                               selected = 0),
                        #                  inline   = TRUE
                        #                  )
                        # 
                        ),

                        ############Guide RNA selection section############################ 
                        #Guide RNA instructions
                        #Buttons to select input$gRNAtype
                        wellPanel(
                        radioButtons("gRNAtype", 
                                     label    = paste0("1. Select the guide RNA for liberating the plasmid cargo. Use the \"Universal Guide RNA\" ", 
                                                       "for any GeneWeld plasmid. Use \"Custom Guide RNA\" for non-GeneWeld plasmids."), 
                                     choices  = list("Universal Guide RNA" = 1, 
                                                     "Custom Guide RNA"    = 2), 
                                     selected = 1),
                        
                        #This panel displays if the user wants to put in a custom guide RNA
                        conditionalPanel(
                          condition = "input.gRNAtype == 2",
                          
                          #Space to output validation results; output$validgrna
                          textOutput("validgrna"),
                          
                          p("Leave this box blank if you DO NOT need to clone your custom gRNA into the plasmid."),
                          
                          #Area to input custom guide RNA; input$gRNA
                          textAreaInput("gRNA", 
                                        label       = "", 
                                        value       = "", 
                                        placeholder = "Paste custom gRNA here...")
                        )),
                        
                        wellPanel(
                          
                          ############CRISPR target sequence section#########################
                          #CRISPR target Instructions
                          p(tags$div(tags$b(paste0("2. Enter the guide RNA sequence for your genomic insertion site. ", 
                                                   "Please only include the 20 nt target sequence; ", 
                                                   "DO NOT include the PAM sequence.")))),
                          
                          #Space to output validation results; output$validcrisprseq
                          textOutput("validcrisprseq"),
                          
                          #Box to paste crispr sequence; input$crisprSeq
                          textAreaInput("crisprSeq", 
                                        label       = "", 
                                        value       = "", 
                                        placeholder = "Paste CRISPR guide target sequence here..."),
                          
                          radioButtons("sense",
                                       label = paste0("Is your PAM sequence 5' (upstream) or 3' (downstream) of your pasted gRNA sequence?"),
                                       choices = c("5' (upstream: PAM-sequence)"    = 1,
                                                   "3' (downstream: sequence-PAM)" = 0),
                                       selected = 0,
                                       inline = TRUE)
                        ),
                        
                        ###########gene/Gene ID Section####################################
                        #Gene ID/geneinstructions
                        wellPanel(
                          #Buttons to choose cDNAtype; input$cDNAtype
                          radioButtons("cDNAtype", 
                                       label    = paste0("3. Specify the genomic DNA surrounding the insertion site in the form ", 
                                                         "of a GenBank/RefSeq accession, Ensembl accession, or pasted plain text " , 
                                                         "gene sequence (no FASTA header)."),
                                       choices  = list("GenBank or RefSeq Gene ID"               = 1,
                                                       "ENSEMBL transcript, exon, or protein ID" = 3,
                                                       "Copy/paste sequence"                     = 2),
                                                       #"Upload a GenBank (or APE) format file"  = 4,
                                                       #"Upload a FASTA or plain text file"      = 5), 
                                       selected = 1),
                          
                          textOutput("uploadType"),
                          
                        #wellPanel(
                          #####cDNAtype == GENBANK ID OR FILE####
                          conditionalPanel(
                            condition = "input.cDNAtype == 1 || input.cDNAtype == 4",
                            
                            conditionalPanel(
                              condition = "input.cDNAtype == 1",

                              #Space to output validated gene ID; output$validgenbankid
                              textOutput("validgenbankid"),
                              #Space to output if the genbank file is valid
                              textOutput("validgenbankDNA"),
                              textOutput("validrefseqgenbankid"),
                              
                              #Box to paste gene ID; input$geneId
                              textInput("genbankId", 
                                        label       = paste0("Paste GenBank Accession here. If the entry matching your accession number does not have ", 
                                                             "exon location information, automatic codon repair is not possible."), 
                                        value       = "", 
                                        placeholder = "Paste GenBank nucleotide gene accession here..."),
                              
                              # Warning (not error) if target is RefSeq mRNA
                              #textOutput("validnotrefseqmrna"),
                              
                              # Warning (not error) if target is outside of an exon
                              #textOutput("exonWarning"),
                            #),
                            
                            #For uploading GenBank/ApE format files
                            #conditionalPanel(
                            #  condition = "input.cDNAtype == 4",
                              
                            #  fileInput("genbankFileUpload",
                            #    label       = paste0("Please upload your gene sequence in GenBank (.gb or .gbk) ", 
                            #                         "or A Plasmid Editor (.ape) format (there is a 5MB max file size limit): "),
                            #    multiple    = FALSE,
                            #    buttonLabel = "Select file for upload: ",
                            #    placeholder = "No file selected...",
                            #    accept      = "chemical/seq-na-genbank",
                            #    width       = "70%")
                            #),
                            
                            #Warn users about automatic codon repair requirements
                            p(paste0("Do you want to automatically repair frameshifts at the genomic DSB site? ",
                                     "(If your GenBank/RefSeq ID does not have adequate coding sequence annotation OR ",
                                     "you are targeting a non-coding/intronic section, automatic codon repair is not possible.)")),
                            
                            radioButtons("paddingChoice",
                                         label    = "",
                                         choices  = list("Yes" = 1,
                                                         "No"  = 0),
                                         selected = 1,
                                         inline   = TRUE)
                            
                          )),
                          
                          #####cDNAtype == ENSEMBL ID#####
                          #This panel displays if the user wants to use an ENSEMBL gene ID
                          conditionalPanel(
                            condition = "input.cDNAtype == 3",

                            #Space to output validated gene ID; output$validgeneid
                            textOutput("validensemblid"),

                            #Box to paste gene ID; input$geneId
                            textInput("ensemblId",
                                      label = paste0("Paste Ensembl transcript, protein, or exon ID here. ",
                                                     "Since Ensembl genes may have multiple associated transcripts, ",
                                                     "we currently do not support Ensembl gene ID inputs. "),
                                      value = "",
                                      placeholder = "Paste Ensembl transcript, protein, or exon ID here..."),

                            #Warn users about automatic codon repair requirements
                            p("Do you want to automatically repair frameshifts at the genomic DSB site? "),

                            radioButtons("paddingChoiceEns",
                                         label    = "",
                                         choices  = list("Yes" = 1,
                                                         "No"  = 0),
                                         selected = 1,
                                         inline   = TRUE)
                          ),

                          #####cDNAtype == CDNA SEQUENCE#####
                          ###This panel displays if the user is putting in their own cDNA sequence##
                          conditionalPanel(
                            #If they are uploading or pasting a plain-text or FASTA file
                            condition = "input.cDNAtype == 2 | input.cDNAtype == 5",

                            conditionalPanel(condition = "input.cDNAtype == 2",

                                             #Space to output validated cDNA sequence; output$validcdna
                                             textOutput("validcdna"),

                                             #Box to paste cDNA sequence; input$cDNA
                                             textAreaInput("cDNA",
                                                           label       = "Please paste your sequence in plain text (no FASTA header):",
                                                           value       = "",
                                                           placeholder = "Paste gene/exon sequence here...",
                                                           rows        = 6)
                            ),

                            #conditionalPanel(condition = "input.cDNAtype == 5",
                            #                 fileInput("fptFileUpload",
                            #                           label = "Please upload your gene sequence in FASTA or plain text format (there is a 5MB max file size limit): ",
                            #                           multiple = FALSE,
                            #                           buttonLabel = "Select file for upload: ",
                            #                           placeholder = "No file selected...",
                            #                           accept = "application/fasta,chemical/seq-na-fasta,text/plain",
                            #                           width = "70%")
                            #),

                            #Padding Selection ################
                            #This panel displays if the user knows how many padding nucleotides to put in
                            #Instructions
                            p("Please select the number of additional nucleotides required to maintain the reading frame:"),

                            #Selection drop-down box; input$padding
                            selectInput("padding",
                                        label    = "",
                                        choices  = list("0" = 0,
                                                        "1" = 1,
                                                        "2" = 2),
                                        selected = "",
                                        width    = '100px')
                          )),
                        
                        wellPanel(
                          ############Microhomology Selection section########################
                          #Determine the length of microhomology
                          #Slider bar to select mh length; input$mh
                          selectInput("mh", 
                                      label    = "4. Select the length of microhomology you wish to use (recommended value = 48):", 
                                      choices  = c("12" = 12,
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
                          p(tags$b(paste0("5. (OPTIONAL) If you are using a GeneWeld plasmid, you can specify the plasmid below ", 
                                          "and download a copy of the plasmid map with your (labeled) incorporated oligonucleotide sequences.", 
                                          "This step has NO EFFECT on the oligonucleotide sequence, and is provided merely as a convenience."))),
                          p("Please see the the 'Downloads' tab for more information on available plasmids."),
                          
                            selectInput("plasmidName",
                                        label    = paste0("Select a plasmid series for oligo incorporation and ",
                                                          "download. After you hit \"Submit\" for the first time, ",
                                                          "you can change the plasmid and hit the \"Download Plasmid File\" ", 
                                                          "button to download your incorporated sequences in the currently ", 
                                                          "selected plasmid vector; you do not need to re-calculate your oligos ", 
                                                          "unless you want to change the GeneWeld plasmid series"),
                                        choices  = c("pGTag-eGFP-B-actin"        = 1,
                                                     "pGTag-eGFP-caax-B-actin"   = 2,
                                                     "pGTag-eGFP-caax-SV40"      = 3,
                                                     "pGTag-eGFP-B-actin"        = 4,
                                                     "pGTag-Gal4-VP16-B-actin"   = 5,
                                                     "pGTag-NLS-eGFP-B-actin"    = 6,
                                                     "pGTag-NLS-eGFP-SV40"       = 7,
                                                     "pGTag-NLS-TagRFP-B-actin"  = 8,
                                                     "pGTag-NLS-TagRFP-SV40"     = 9,
                                                     "pGTag-TagRFP-B-actin"      = 10,
                                                     "pGTag-TagRFP-caax-B-actin" = 11,
                                                     "pGTag-TagRFP-caax-SV40"    = 12,
                                                     "pGTag-TagRFP-SV40"         = 13),
                                        selected = 1)
                          ),
                        
                        ############SUBMIT####################
                        #Conditional display of submit buttons, depending on cDNA type
                        wellPanel(
                          #If using GenBank ID ####
                          conditionalPanel(
                            condition = "input.cDNAtype == 1 || input.cDNAtype == 4",
                            p("After pushing the Submit button, please wait a few seconds for your results to appear."),
                            actionButton("genBankSubmit",        label = "Submit")
                          ),
                          
                          #If using pasted CDNA ####
                          conditionalPanel(
                            condition = "input.cDNAtype == 2 || input.cDNAtype == 5",
                            textOutput("geneProg"),
                            actionButton("submit",               label = "Submit")
                          ),
                          
                          # #If using ENSEMBL gene ID ####
                          conditionalPanel(
                            condition = "input.cDNAtype == 3",
                            p("After pushing the Submit button, please wait a few seconds for your results to appear."),
                            actionButton("ensemblSubmit",         label = "Submit"),

                            textOutput("validensemblDNA")
                          )#,
                          
                          #If using GenBank/ApE file upload####
                         #conditionalPanel(
                        #    condition = "input.cDNAtype == 4",
                         #   p("After pushing the Submit button, please wait a few seconds for your results to appear."),
                        #  actionButton("genbankFileSubmit",    label = "Submit")
                         # ),   
                          
                          #If using plain text upload####
                          #conditionalPanel(
                          #  condition = "input.cDNAtype == 5",
                          #  p("After pushing the Submit button, please wait a few seconds for your results to appear."),
                          #  actionButton("plainTextFileSubmit",  label = "Submit")
                          #)
                        ),
                        
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
             
             ########## PROTOCOLS #############################################
             # tabPanel(
             #   tags$div("Protocol"),
             #   titlePanel(""),
             #   
             #   # Sidebar panel with links
             #   column(2, wellPanel(
             #     tags$div(tags$span(a(href         = "http://www.genesculpt.org",               target = "_blank", 
             #                          tags$img(src = "GSS logo small.png",                      width  = "100%")))),
             #     tags$br(),
             #     tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
             #                          tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
             #     tags$br(),
             #     tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
             #                          tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
             #     tags$br(),
             #     tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
             #                          tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
             #     tags$br(),
             #     tags$div(tags$span(a(href         = "https://github.com/Dobbs-Lab/GTagHD",     target = "_blank", 
             #                          tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
             #     tags$br(),
             #     tags$div(tags$span(a(href         = "https://hub.docker.com/r/cmmann/gtaghd",  target = "_blank", 
             #                          tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
             #     tags$br(),
             #     tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
             #                          tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
             #                                   width = "100%", rel = "noopener noreferrer"))))
             #   )),
             #   
             #   column(9, wellPanel(
             #     includeHTML("www/protocol.html")
             #   ))
             # ),
             
             ##########TOOLS AND DOWNLOADS TAB#################################
             
             tabPanel(
               tags$div("Downloads"),
               titlePanel(""),
               
               # Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href         = "http://www.genesculpt.org/",               target = "_blank", 
                                      tags$img(src = "GSS logo small.png",                      width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
                                      tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
                                      tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://github.com/FriedbergLab/GTagHD",     target = "_blank", 
                                      tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://hub.docker.com/r/parnaljoshi/gtaghd",  target = "_blank", 
                                      tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
                                      tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
                                               width = "100%", rel = "noopener noreferrer"))))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 h3("Download GTagHD"),
                 tags$p(HTML(paste0("A standalone version of this code can be freely downloaded from our ", 
                                    tags$a(href = "https://github.com/FriedbergLab/GTagHD", target = "_blank", "GitHub repository"),
                                    "."))),
                 
                 tags$p(HTML(paste0("There are extensive installation/usage instructions available in the GitHub ", 
                                    tags$a(href = "https://github.com/FriedbergLab/GTagHD#how-to-run-gtaghd-locally", target = "_blank", "README"), 
                                    " file."))),
                 tags$p("You can clone the repository with the following git command:"),
                 tags$p(tags$code("git clone https://github.com/FriedbergLab/GTagHD.git"), style = "text-align:center;"),
                 tags$p(HTML(paste0("GTagHD is available under the GNU General Public License v3 (GPL 3.0). You can read the license ",
                                    tags$a(href = "https://github.com/FriedbergLab/GTagHD/blob/master/LICENSE", target = "_blank", "here"),
                                    "."))),
                 tags$p(HTML(paste0("The GTagHD R code is provided as-is; please be aware that you modify the code at your own risk. ",
                                    "We are unable to provide technical support for modified versions.")))
               ),
               
               wellPanel(
                 h3("Run GTagHD Locally"),
                 
                 tags$p(HTML(paste0("If you have R installed on your system, you can also follow the instructions ",
                                    tags$a(href = "https://github.com/FriedbergLab/GTagHD#3-run-gtaghd-locally", target = "_blank", "here"),
                                    " to easily run the GTagHD RShiny app from R, without dealing with Git."))),
                 p("GTagHD is also available as a Docker container image. You can clone the Docker image using the following command:"),
                 tags$p(tags$code("sudo docker pull parnaljoshi/gtaghd"), style = "text-align:center;")
               ),
                 
               wellPanel(
                 h3("Download GeneWeld Protocol"),
                 tags$p("Click", tags$a(href="New_GeneWeldProtocol_102720.pdf", target="_blank", "here"), "to download the GeneWeld protocol.")
               ),  
                 
               
               wellPanel(
                 h3("Download G-Series Plasmid Sequences"),
                 tags$p("You can download the plasmid maps for GeneWeld compatible plasmids below."),
                 
                 tags$p("You can download all plasmid maps in ", 
                        tags$a(href     = 'plasmids/071618_pGTag_series_plasmids_gb.zip', 
                               target   = 'blank', 
                               'GenBank', 
                               download = '071618_pGTag_series_plasmids_gb.zip'),
                        " format or ",
                        tags$a(href     = 'plasmids/071618_pGTag_series_plasmids_ape.zip', 
                               target   = 'blank', 
                               'ApE', 
                               download = '071618_pGTag_series_plasmids_ape.zip'),
                        " format."),
                 
                 tags$p("You can also download the individual plasmid sequences: "),
                 tags$p("pGTag-eGFP-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-B-actin_(071618).gb',
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-B-actin_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-B-actin_(071618).ape',
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-B-actin_(071618).ape")
                 ),
                 
                 tags$p("pGTag-eGFP-caax-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-B-actin_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-caax-B-actin_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-B-actin_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-caax-B-actin_(071618).ape")
                 ),
                 
                 tags$p("pGTag-eGFP-caax-SV40_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-SV40_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-caax-SV40_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-caax-SV40_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-caax-SV40_(071618).ape")
                 ),
                 
                 tags$p("pGTag-eGFP-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-eGFP-SV40_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-eGFP-B-actin_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-eGFP-SV40_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-eGFP-B-actin_(071618).ape")
                 ),
                 
                 tags$p("pGTag-Gal4-VP16-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-Gal4-VP16-B-actin_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-Gal4-VP16-B-actin_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-Gal4-VP16-B-actin_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-Gal4-VP16-B-actin_(071618).ape")
                 ),
                 
                 tags$p("pGTag-NLS-eGFP-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-B-actin_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-eGFP-B-actin_(071618)).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-B-actin_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-eGFP-B-actin_(071618)).ape")
                 ),
                 
                 tags$p("pGTag-NLS-eGFP-SV40_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-SV40_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-eGFP-SV40_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-eGFP-SV40_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-eGFP-SV40_(071618).ape")
                 ),
                 
                 tags$p("pGTag-NLS-TagRFP-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-B-actin_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-TagRFP-B-actin_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-B-actin_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-TagRFP-B-actin_(071618).ape")
                 ),
                 
                 tags$p("pGTag-NLS-TagRFP-SV40_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-SV40_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-NLS-TagRFP-SV40_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-NLS-TagRFP-SV40_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-NLS-TagRFP-SV40_(071618).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-TagRFP-B-actin_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-B-actin_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-TagRFP-B-actin_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-B-actin_(071618).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-caax-B-actin_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-TagRFP-caax-B-actin_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-caax-B-actin_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-TagRFP-caax-B-actin_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-caax-B-actin_(071618).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-caax-SV40_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-TagRFP-caax-SV40_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-caax-SV40_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-TagRFP-caax-SV40_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-caax-SV40_(071618).ape")
                 ),
                 
                 tags$p("pGTag-TagRFP-SV40_(071618): ",
                        tags$a(href     = 'plasmids/pGTag-TagRFP-SV40_(071618).gb', 
                               target   = 'blank', 
                               "GenBank", 
                               download = "pGTag-TagRFP-SV40_(071618).gb"),
                        tags$a(href     = 'plasmids/pGTag-TagRFP-SV40_(071618).ape', 
                               target   = 'blank', 
                               "ApE", 
                               download = "pGTag-TagRFP-SV40_(071618).ape")
                 )
               ))
             ),
             
             ##########FUNDING Tab#############################################
             tabPanel(
               tags$div("Funding"),
               titlePanel(""),
               # Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href         = "http://www.genesculpt.org/",               target = "_blank", 
                                      tags$img(src = "GSS logo small.png",                      width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
                                      tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
                                      tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://github.com/FriedbergLab/GTagHD",     target = "_blank", 
                                      tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://hub.docker.com/r/parnaljoshi/gtaghd",  target = "_blank", 
                                      tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
                                      tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
                                               width = "100%", rel = "noopener noreferrer")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://dill-picl.org",                   target = "_blank", 
                                      tags$img(src = "lawlab_web_wiki_header.png",              width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href = "https://www.nih.gov/",                            target = "_blank", 
                                      tags$img(src = "nihlogo.png",                             width  = "100%"))))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
               includeHTML("www/funding.html")  
             ))),
             
             ##########HOW TO CITE Tab#########################################
             tabPanel(
               tags$div("How to Cite"),
               titlePanel(""),
               
               # Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href         = "http://www.genesculpt.org/",               target = "_blank", 
                                      tags$img(src = "GSS logo small.png",                      width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
                                      tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
                                      tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://github.com/FriedbergLab/GTagHD",     target = "_blank", 
                                      tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://hub.docker.com/r/parnaljoshi/gtaghd",  target = "_blank", 
                                      tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
                                      tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
                                               width = "100%", rel = "noopener noreferrer"))))
               )),
               
               #Text area in center of page
               column(9, 
                 wellPanel(
                   includeHTML("www/citation.html")
                 ),
                 
                 wellPanel(
                   h3("Licensing"),
                   p("GTagHD is available under the GNU GPL v3.0 license.")
                   
                 )
                 
               )
             ),
             
             ##########CONTACT US Tab##########################################
             tabPanel(
               tags$div("Report Bugs or Contact Us"),
               titlePanel(""),
               
               # Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href         = "http://www.genesculpt.org/",               target = "_blank", 
                                      tags$img(src = "GSS logo small.png",                      width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
                                      tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
                                      tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://github.com/FriedbergLab/GTagHD",     target = "_blank", 
                                      tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://hub.docker.com/r/parnaljoshi/gtaghd",  target = "_blank", 
                                      tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
                                      tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
                                               width = "100%", rel = "noopener noreferrer"))))
               )),
               
               #Text area in center of page
               column(9, 
                      wellPanel(
                        p("Please use the form below, or email us directly at GeneSculptSuite@gmail.com, to report issues and request support."),
                        p("Please note that GTagHD requires the latest version of Javascript to be enabled for full functionality.")
                      ),
                 
                   tags$iframe(id           = "googleform", 
                               src          = paste0("https://docs.google.com/forms/d/e/1FAIpQLSeq9aDRj6EOCskBwPsA2PFQ2LsKxT4v85",
                                                     "-rGTlYQOk0n8X2Gw/viewform?usp=pp_url&entry.358268393&entry.1646278736=", 
                                                     "GTagHD&entry.1934309806&entry.565411344&entry.754537383&entry.826100992"),
                               width        = 760,
                               height       = 2000,
                               frameborder  = 0,
                               marginheight = 0)
                 
             )),
             
             ######STATUS and CHANGELOG Tab####################################
             tabPanel(
               tags$div("Change Log"),
               titlePanel(""),
               
               # Sidebar panel with links
               column(2, wellPanel(
                 tags$div(tags$span(a(href         = "http://www.genesculpt.org/",               target = "_blank", 
                                      tags$img(src = "GSS logo small.png",                      width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.iastate.edu/",                target = "_blank", 
                                      tags$img(src = "isu-logo-alt.png",                        width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.mayoclinic.org",              target = "_blank", 
                                      tags$img(src = "MC_stack_4c_DAC.png",                     width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.genomewritersguild.org/",     target = "_blank", 
                                      tags$img(src = "genome-writers-guild-logo_DAC.png",       width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://github.com/FriedbergLab/GTagHD",     target = "_blank", 
                                      tags$img(src = "GitHub_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://hub.docker.com/r/parnaljoshi/gtaghd",  target = "_blank", 
                                      tags$img(src = "Docker_Logo.png",                         width  = "100%")))),
                 tags$br(),
                 tags$div(tags$span(a(href         = "https://www.addgene.org/Jeffrey_Essner/", target = "_blank",
                                      tags$img(src = "https://www.addgene.org/headers/images/plasmid_request_button_no_pi.png", 
                                               width = "100%", rel = "noopener noreferrer"))))
               )),
               
               #Text area in center of page
               column(9, wellPanel(
                 includeHTML("www/changelog.html")
               ))
             )
  ))
#



