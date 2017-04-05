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
             theme = "ogtheme.css", 
             tags$div("MMEJ Tool (Better Name Suggestions Welcome)", 
                      style = "color:white"),
             
             
    ########ABOUT#######################
    tabPanel(tags$div("About", style = "color:white"),
             titlePanel(""),
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                
                 tags$html(tags$div(tags$span("We can put links to", 
                                              a(href = "http://www.iastate.edu", "other"), 
                                              tags$a(href = "http://www.mayoclinic.org", "websites"), 
                                              "here.")),
                 tags$div(tags$span("Or ", 
                                    a(href = "examplepdf.pdf", "papers"), 
                                    ". Or anything, really.")
               ))),
               
               mainPanel(
                 #tags$div("We can put information about labs, collaborations, etc."),
                 #tags$div("And/or information about how the tool works."),
                 #tags$img(src = "Iowa_State_Cyclones_logo.svg.png", 
                          #height = "200px", 
                          #width = "290px"),
                 tags$img(src = "isulogo.jpg",
                          height = "75px"), 
                          #width = "290px"),
                 p(""),
                 tags$img(src = "mayoclinic.jpeg", 
                          height = "218px", 
                          width = "200px"),
                 p("The MMEJ Web Tool can automatically create CRISPR targeting oligos designed to support microhomology mediated end joining."),
                 p(""),
                 p('To use the tool, choose the "Submit Single Job" tab.'),
                 p("In 1, you can choose to use a universal guide RNA designed to function with the universal series plasmid, or submit a custom guide RNA for use with a plasmid of your choice."),
                 p("In 2, you must enter your CRISPR target sequence. This squence should be 23 bp long - 20 bases as the target, and a PAM sequence in the form of either 'CCN' or 'GGN' at the beginning of the sequence, or 'NCC' or 'NGG' at the end of the sequence."),
                 p("In 3, you can paste a section of cDNA containing your CRISPR target in (2). Please note that your CRISPR target should appear exactly once in either the forward or reverse complement direction of the cDNA sequence."),
                 p("We will support automatically downloading a cDNA sequence, based on a gene ID, shortly."),
                 p("You can specify 0-2 'padding' nucleotides be added to the targeting oligos such that the CRISPR cut will not shift the reading frame."),
                 p("Automatic generation of padding nucleotides will be added in conjunction with gene ID support."),
                 p("In 4, you can select the length of microhomology to be used. We recommend using the default value of 24 nucleotides."),
                 p("If there are any problems with the sequences and values you have entered, the web page will inform you of the errors and give you advice on how to fix them."),
                 p("If no errors are present, you can press the 'Submit' button at the bottom of the page to generate your targeting oligos."),
                 p(""),
                 p("You can also see an example of the inputs using the 'Example' link on the left hand column of the 'Submit Single Job' page.")

               )
             )
             
    ),
    
    ##########SUBMIT SINGLE JOB###############
    tabPanel(id = "single",
             tags$div("Submit Single Job", style = "color:white"),
             titlePanel(""),
             
             #Adds a sidebar for users to pre-populate fields with an example, and reset the form
             column(2, wellPanel(
               actionLink("example", 
                          label = "Example"),
               
               p(""),
               
               actionLink("exampleEnsembl", 
                          label = "Example For ENSEMBL Function"),
               
               p(""),
               actionLink("reset", 
                          label = "Reset Form")
             )),
             
             #Main panel for entering information and submitting job
             column(9, wellPanel(
               
               ############Guide RNA selection section############################ 
               p(paste0("1. Select the guide RNA type. ",
                        "You can use the universal guide RNA, or paste a custom ", 
                        "guide sequence in PLAIN TEXT format (no FASTA header).")),
               
               radioButtons("gRNAtype", 
                            label = "", 
                            choices = list("universal guide RNA" = 1, 
                                           "custom guide RNA" = 2), 
                            selected = 1),
               
               #This panel displays if the user wants to put in a custom guide RNA
               conditionalPanel(
                 condition = "input.gRNAtype == 2",
                 textOutput("validgrna"),
                 textAreaInput("gRNA", 
                               label = "", 
                               value = "", 
                               placeholder = "Paste custom gRNA here...")
               ),
               
               
               ############CRISPR target sequence###############################
               #Input the sequence of your CRISPR target
               p("2. Enter the CRISPR target sequence (23 bases total - 20 from target, 3 from PAM):"),
               textOutput("validcrisprseq"),
               textAreaInput("crisprSeq", 
                             label = "", 
                             value = "", 
                             placeholder = "Paste CRISPR target sequence here..."),
               
               #cDNA/gene ID selection section
               p(paste0("3. Paste an ENSEMBL gene ID or a cDNA sequence into ", 
                        "the box in PLAIN TEXT format (no FASTA header).")),
               
               radioButtons("cDNAtype", 
                            label = "", 
                            choices = list("ENSEMBL gene ID" = 1, 
                                           "Pasted cDNA" = 2), 
                            selected = 2),
               
               #This panel displays if the user wants to use an ENSEMBL gene ID
               conditionalPanel(
                  condition = "input.cDNAtype == 1",
                  
                  
                  #Choose the species - required for bioMart
                  p("Please select the species:"),
                  selectInput("species", 
                              label = "", 
                              choices = list("Human" = 0, 
                                             "Zebrafish" = 1,
                                             "Fruitfly" = 2),
                                            # "Maize" = 3),
                              selected = 0),
                  textOutput("validgeneid"),
                  textInput("geneId", 
                            label = "", 
                            value = "", 
                            placeholder = "Paste ENSEMBL gene ID here...")

                  
                  
               ),
               
               #This panel displays if the user is putting in their own cDNA sequence
               conditionalPanel(
                 condition = "input.cDNAtype == 2",
                 textOutput("validcdna"),
                 textAreaInput("cDNA", 
                               label = "", 
                               value = "", 
                               placeholder = "Paste cDNA sequence here...", 
                               rows = 6),
                 
                 p(paste0("Do you want to attempt to automatically generate the ", 
                          "'padding' nucleotides to repair potential codon breaks?")),
                 radioButtons("paddingSelection", 
                              label="", 
                              choices = list("No" = 1, 
                                             "Yes" = 2), 
                              selected = 1),
                 
                 #This panel displays if the user wants to try to automatically generate padding codons
                 conditionalPanel(
                   condition = "input.paddingSelection == 2",
                   p("Specify parameters for BLAST search:"),
                   p("How many hits to display (default is up to 5):"),
                   numericInput("hitNumber", 
                                label="", 
                                value = 5, 
                                min = 1, 
                                max = 20, 
                                step = 1),
                   p(paste0("E-Value cutoff power; only hits better than this ", 
                            "value will be displayed (default is 3, which corresponds to 1E-03):")),
                   
                   numericInput("eCutoff", 
                                label="", 
                                value = 3, 
                                min = 1, 
                                step = 1),
                   
                   actionButton("blastButton", label = "Submit BLAST")
                 ),
                 
                 #This panel displays if the user knows how many padding nucleotides to put in
                 conditionalPanel(
                   condition = "input.paddingSelection == 1",
                   p("Please select the number of 'padding' nucleotides to repair codon changes."),
                   selectInput("padding", 
                               label = "", 
                               choices = list("0" = 0, 
                                              "1" = 1, 
                                              "2" = 2), 
                               selected = "", 
                               width = '100px')
                 )
                 
               ),
               
               
               ############Microhomology Selection#############
               p("4. Select the length of microhomology you wish to use (recommended value = 24):"),
               sliderInput("mh", 
                           label = "", 
                           value = 24, 
                           min = 12, 
                           max = 48, 
                           step = 1, 
                           ticks = TRUE),
               textOutput("validmh"), 
               
               
               
               ############SUBMIT####################
               conditionalPanel(
                 condition = "input.cDNAtype == 2",
                 actionButton("submit", label = "Submit")
               ),
               conditionalPanel(
                 condition = "input.cDNAtype == 1",
                 actionButton("geneIdSubmit", label = "Submit to ENSEMBL")
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
    
    
    ##########SUMBIT MULTIPLE JOBS#################
    tabPanel(tags$div("Submit Several Jobs", style = "color:white"),
             titlePanel(""),
             p("We could do this, but it might be better to have people use offline version instead...")
             
    ),
    
    ##########HOW TO CITE#################
    tabPanel(
      tags$div("How to Cite", style = "color:white"),
      titlePanel(""),
      p("When the paper is published, citation goes here...")
    ),
    
    ##########CONTACT US################
    tabPanel(
      tags$div("Contact", style = "color:white"),
      titlePanel(""),
      p("We can put links to lab websites or Genome Engineering stuff here.")
    ),
    
    ##########FUNDING###############
    tabPanel(
      tags$div("Funding", style = "color:white"),
      titlePanel(""),
      p("R24 Grant Stuff Here!")
    ),
    
    
    ######STATUS and CHANGELOG#####
    tabPanel(
      tags$div("Change Log", style = "color:white"),
      titlePanel(""),
      includeHTML("www/changelog.html")
    )
  
))
#



