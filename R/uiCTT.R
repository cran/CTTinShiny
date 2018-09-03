# Defines the user interface for this package
uiCTT <- fluidPage(
  
  mainPanel(h3("Classical Test Theory Analyses and Reporting"),
            
            tabsetPanel(
              
              tabPanel("Intro",
                       br(),       
                       p("This interface provides an easy way to conduct the most common 
                         classical test theory analyses."),
                       br(),
                       p("Read in new data on the \"Data\" tab."),
                       p("Conduct analyses on the \"Analysis\" tab."),
                       p("Create figures on the \"Figures\" tab."),
                       p("CTT utility functions can be found on the \"Tools\" tab.")
                       ), #End Intro            
              
              ################
              # Start Data Tab
              ################
              tabPanel("Data",
                       
                       fluidRow(column(4,
                                       radioButtons('isScoredFlag', 
                                                    "Is the data scored?", 
                                                    choices = NULL,
                                                    selected = NULL,
                                                    inline = FALSE, width = NULL, 
                                                    choiceNames = c("Yes, it is already numeric.",
                                                                    "No, it has an associated key file."), 
                                                    choiceValues = c("scored","raw"))       
                       ),
                       column(4,
                              radioButtons('fileType', 
                                           "Choose File Type", 
                                           choices = NULL,
                                           selected = NULL,
                                           inline = FALSE, width = NULL, 
                                           choiceNames = c("CSV","SAV (SPSS)","FWF"), 
                                           choiceValues = c("csv","sav","fwf"))
                       )   
                       ),
                       
                       
                       uiOutput("fileControls"),
                       
                       uiOutput("fileDisplay")
                       
              ), 
              #############
              # end Data
              #############
              
              
              tabPanel("Analysis",
                       tabsetPanel(
                         tabPanel("Reliability",
                                  br(),       
                                  uiOutput("reliabilityControl"),       
                                  uiOutput("reliabilityDisplay")
                         ),
                         tabPanel("Distractor Analysis",
                                  uiOutput("distractorControls")
                         )
                       )    
              ), # end Analysis
              
              tabPanel("Figures",
                       br(),       
                       uiOutput("figureUI")
              ),#End About
              
              tabPanel("Tools",
                       br(),
                       p("Use this Spearman-Brown tool to estimate reliability under different testing assumptions."),
                       p("You can input any value or use the \"Use Current Form\" button to populate the form with a current analysis."),
                       actionButton(inputId = "currentFormInfo", 
                                    label = "Use Current Form", icon = NULL, width = NULL),
                       br(),br(),
                       numericInput(inputId = "currentRel", 
                                    label = "Enter current reliability:", value=NULL, min = 0, max = 1, step = NA,
                                    width = NULL),  
                       numericInput(inputId = "currentN", 
                                    label = "Enter current number of items:", value=NULL, min = 2, max = NA, step = 1,
                                    width = NULL),
                       p("You can enter a target reliability and/or target number of items. They perform separate analyses."),
                       numericInput(inputId = "targetRel", 
                                    label = "Enter target reliability:", value=NULL, min = 0, max = 1, step = NA,
                                    width = NULL),
                       numericInput(inputId = "targetN", 
                                    label = "Enter target number of items:", value=NULL, min = 2, max = NA, step = NA,
                                    width = NULL),
                       p(textOutput('targetN')),
                       p(textOutput('targetR'))
              ), # end tools
              
              tabPanel("About",
                       br(),       
                       p("Developed by John Willse <jtwillse@uncg.edu>."),
                       p("This interface uses the package CTT. CTT can perform all of these analyses 
                         (and more!) in a standard R environment."),
                       a("CTT documentation on CRAN",target="_blank",href="https://CRAN.R-project.org/package=CTT" )
                       ) #End About
            ) # end tabsetpanel
            
  ) # end mainPanel
  
  ) # end UI
