serverCTT <- function(input, output, session){
  
  ##########################
  # Data Code
  ##########################
  
  
  ##
  # Dynamic File Controls, raw or scored?
  ##
  output$fileControls <- renderUI({
    
    if(input$isScoredFlag=="raw"){
      tagList(
        fileInput(paste0('dataFile',toupper(input$fileType),input$isScoredFlag), 'Choose a raw data file'
        ),
        
        fileInput(paste0('keyFile',toupper(input$fileType),input$isScoredFlag), 'Choose a key file'
        )
      ) # end tagList
    } else if(input$isScoredFlag=="scored"){
      fileInput(paste0('dataFile',toupper(input$fileType),input$isScoredFlag), 'Choose a scored data file'
      )
    } # end isScoredFlag  
  })
  
  
  #################
  # Handles displaying data
  #################
  
  output$fileDisplay <- renderUI({
    
    if(input$isScoredFlag=="raw"){
      tabsetPanel(
        tabPanel("Scored",
                 p(textOutput('dataTableText')),
                 dataTableOutput('outScoredTable')
        ), #End Scored  
        tabPanel("Raw",
                 p(textOutput('rawTableText')),
                 dataTableOutput('outRawTable')
        ),
        tabPanel("Key",
                 p(textOutput('keyTableText')),
                 dataTableOutput('outKeyTable')
        ),
        tabPanel("Select",
                 br(),
                 p("If you remove (uncheck) items, you will 
                   need to re-run analyses for them to update."),
                 checkboxGroupInput("selectedItems", label = "Included Items", 
                                    choices = dataVariableNames(),
                                    selected = dataVariableNames())
                 ) 
  )  
    } else if(input$isScoredFlag=="scored"){
      
      tabsetPanel(
        tabPanel("Scored",
                 p(textOutput('dataTableText')),
                 dataTableOutput('outScoredTable')
        ),
        tabPanel("Select",
                 br(),
                 p("If you remove (uncheck) items, you will 
                   need to re-run analyses for them to update."),
                 checkboxGroupInput("selectedItems", label = "Included Items", 
                                    choices = dataVariableNames(),
                                    selected = dataVariableNames())
                 )
      )
    } # end isScoredFlag  
  })
  
  
  
  ##
  # Gets File data file.
  ##
  
  fileData <- reactive({
    infile <- input[[paste0('dataFile',toupper(input$fileType),input$isScoredFlag)]]
    if (is.null(infile)) {
      return(NULL) # returns Null until file selected
    }
    if(input$fileType == "csv"){
      fileD <- read.csv(infile$datapath, as.is=TRUE)
    } else if(input$fileType == "sav"){
      fileD <- foreign::read.spss(infile$datapath,
                         to.data.frame=TRUE,
                         use.value.labels=FALSE)
    } else if(input$fileType == "fwf"){
      
    }
    return(fileD)
  })
  
  ##
  # Gets key file.
  ##
  
  
  fileKey <- reactive({
    infile <- input[[paste0('keyFile',toupper(input$fileType),input$isScoredFlag)]]
    if (is.null(infile)) {
      return(NULL) # returns Null until file selected
    }
    if(input$fileType == "csv"){
      fileK <- read.csv(infile$datapath, as.is=TRUE)
    } else if(input$fileType == "sav"){
      fileK <- foreign::read.spss(infile$datapath,
                         to.data.frame=TRUE,
                         use.value.labels=FALSE)
    } else if(input$fileType == "fwf"){
      
    }
    return(fileK)
  })
  
  
  scoredFileData <- reactive({
    if(input$isScoredFlag=="scored"){
      return(fileData())
    } else{
      if(! is.null(fileKey())){
        scoredData <- as.data.frame(score(fileData(), fileKey(),
                                          output.scored=TRUE)$scored)
        colnames(scoredData) <- colnames(fileData())
        return(scoredData)
      }  
    }
  })  
  
  # Just getting some things for later use
  personScores <- reactive(rowSums(scoredFileData()[,input$selectedItems]))
  dataVariableNames <- reactive(colnames(scoredFileData()))
  
  ##
  # Modal Dialog, FWF not implimented...
  ##
  
  observeEvent(input$fileType, {
    if(input$fileType == "fwf"){
      showModal(modalDialog(
        title = "Sorry!",
        "Fixed width format file entry is not available yet.",
        easyClose = TRUE
      ))
      updateRadioButtons(session, "fileType",
                         selected = "csv"
      )
    }
  })
  
  
  ##
  # Gets basic data stats
  ##
  
  #output$nItem <- ncol(scoredFileData())
  #output$nPeople <- nrow(scoredFileData())
  
  ##
  # Deals with displaying the data
  ##
  output$outScoredTable <- renderDataTable(scoredFileData(), 
                                           options=list(pageLength=10, 
                                                        searching=FALSE))
  
  output$outRawTable <- renderDataTable(
    fileData(),  
    options=list(pageLength=10, 
                 searching=FALSE)
  )
  
  output$outKeyTable <- renderDataTable(
    fileKey(),  
    options=list(pageLength=10, 
                 searching=FALSE)
  )
  
  output$rawTableText <- renderText(
    if(is.null(fileKey())|is.null(fileData())){
      return("Raw data will display below once a raw data file is selected.")
    } else{
      return("Your raw data:")
    }
  )
  
  
  output$keyTableText <- renderText(
    if(is.null(fileKey())|is.null(fileData())){
      return("Your key will display below once a key file is selected.")
    } else{
      return("Your key:")
    }
  )
  
  output$dataTableText <- renderText(
    if(is.null(scoredFileData())){
      return("Data will display below once a file is selected.")
    } else{
      return("Your data:")
    }
  )  
  
  
  ###################################################
  # Analysis Code
  ###################################################
  
  
  ####################
  # Distractor UI
  ####################
  output$distractorControls <- renderUI({
    
    if(input$isScoredFlag=="raw"){
      tagList(
        br(),
        p("Run distractor analysis:"),
        numericInput("nGroupsDistractor", "Number of quantiles",value=4,min=2,max=8),
        actionButton(inputId = "distractorButton", label = "Run"),
        distractorOutUI)
      
    } else if(input$isScoredFlag=="scored"){
      tagList(
        br(),
        p("Distractor analysis is only available with unscored data.")
      )
    } # end isScoredFlag  
  })
  
  observeEvent(input$distractorButton, {
    if(is.null(fileKey())|is.null(fileData())){
      showModal(modalDialog(title = "Unscored Data Needed!", 
                            "Make sure you have specified a raw data file and key file.",
                            footer = modalButton("Dismiss"))
      )
    }
  })
  
  ################
  # Distractor Calculation and Rendering
  ################
  
  completedDistractor <- eventReactive(input$distractorButton, {
    if(!(is.null(fileKey())|is.null(fileData()))){
      disOut <- distractorAnalysis(fileData()[,input$selectedItems],
                                   fileKey()[input$selectedItems],
                                   nGroups = input$nGroupsDistractor)
    } else disOut <- NULL
    disOut}
  )
  
  
  distractorOutUI <- renderUI({
    tagList(
      hr(),
      downloadButton("downloadDistractor"),
      disList <- lapply(1:length(completedDistractor()), function(tableIter){
        tagList(strong(hr()),
                strong(renderText(paste0("Item: ",names(completedDistractor()[tableIter])))),
                renderTable(completedDistractor()[[tableIter]])
        )
      })
    )
  })
  
  output$downloadDistractor <- downloadHandler(
    filename = function() {
      paste("distractorAnalysis-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(scaleAnalysis()$itemReport, file,row.names = FALSE)
      
      for (i in 1:length(completedDistractor())) {
        appendIt <- !i == 1
        tmpItem <- completedDistractor()[[i]]
        write.table(t(c("Item", names(completedDistractor())[i])), file, 
                    row.names = FALSE, col.names = FALSE, na = " ", 
                    append = appendIt, sep = ",")
        write.table(t(colnames(tmpItem)), file, row.names = FALSE, 
                    col.names = FALSE, na = " ", append = TRUE, sep = ",")
        write.table(tmpItem, file, row.names = FALSE, 
                    col.names = FALSE, na = " ", append = TRUE, sep = ",")
        write.table(t(c("", "")), file, row.names = FALSE, 
                    col.names = FALSE, na = " ", append = TRUE, sep = ",")
        write.table(t(c("", "")), file, row.names = FALSE, 
                    col.names = FALSE, na = " ", append = TRUE, sep = ",")
      }
      
    }
  )
  
  #############
  # reliability
  #############
  
  output$reliabilityControl <- renderUI({
    if(is.null(scoredFileData())){
      p(textOutput('dataAnalysisText'))
    } else{
      tagList(
        p("Run a reliability and item analysis. The fields below allow for optional
          item flags. You may leave any or all blank."),
        actionButton(inputId = 'runReliabilityButton', label='Estimate Reliability'),
        fluidRow(column(3, br()),
                 column(3, br())
        ),
        fluidRow(column(3, numericInput('hardFlag','Too Hard',value=NULL, step=.05)),
                 column(3, numericInput('easyFlag','Too Easy',value=NULL, step=.05))
        ),
        fluidRow(column(3, numericInput('lowPBis', 'Low pBis',value=NULL, step=.05)),
                 column(3, numericInput('lowBis', 'Low Bis',value=NULL, step=.05))
        )
        )
    }
  })
  
  output$reliabilityDisplay <- renderUI({
    if(! is.null(scaleAnalysis())) tagList(
      p(textOutput('dataAnalysisText')),
      tableOutput('alpha'),
      p(div(style="display: inline-block;vertical-align:middle;",textOutput('itemAnalysisText')),
        div(style="display: inline-block;vertical-align:middle;",downloadButton("downloadItemAnalysis"))
      ),   
      dataTableOutput('itemTable')
    )
  }  
  )
  
  output$dataAnalysisText <- renderText(
    if(is.null(scoredFileData())){
      return("You must load a file in the \"Data\" tab.")
    } else #if(! is.null(scaleAnalysis())){
      return("Scale Summary:")
    #} else return("taco")
  )  
  
  output$itemAnalysisText <- renderText(
    if(is.null(scoredFileData())){
      return("")
    } else{
      if(! is.null(scaleAnalysis())) return("Item Summary:")
    }
  )  
  
  scaleAnalysis <-  eventReactive(input$runReliabilityButton,{
    if(is.null(scoredFileData())){
      return(NULL)
    } else{
      itAn <- do.call("itemAnalysis",
                      c(list(scoredFileData()[,input$selectedItems]),
                        if(!is.na(input$easyFlag)) list(easyFlag = input$easyFlag),
                        if(!is.na(input$hardFlag)) list(hardFlag = input$hardFlag),
                        if(!is.na(input$lowPBis)) list(pBisFlag = input$lowPBis),
                        if(!is.na(input$lowBis)) list(bisFlag = input$lowBis)))
      return(itAn)
    }
  })
  
  
  output$alpha <- renderTable({
    if(! is.null(scoredFileData())){
      return(
        data.frame( 
          Stat = c("Items", "People", "Mean", "SD", "Alpha"),
          Value = as.character(c(scaleAnalysis()$nItem,
                                 scaleAnalysis()$nPerson,
                                 round(scaleAnalysis()$scaleMean,3),
                                 round(scaleAnalysis()$scaleSD,3),
                                 round(scaleAnalysis()$alpha,3)))
        )
      )
    } else return(NULL)  
  },colnames=FALSE)
  
  output$itemTable <- renderDataTable(scaleAnalysis()$itemReport, 
                                      options=list(searching=FALSE,
                                                   buttons=c('copy')))
  
  output$downloadItemAnalysis <- downloadHandler(
    filename = function() {
      paste("itemAnalysis-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(scaleAnalysis()$itemReport, file,row.names = FALSE)
    }
  )
  
  ####################
  # Figure UI
  ####################
  output$figureUI <- renderUI(
    if(is.null(scoredFileData())){
      tagList( 
        p("Use this form to create empirical ICCs."),
        p("Scored data needed before a figure can be created.")
      )
    } else{
      sidebarLayout(
        sidebarPanel(
          tagList(  
            p("Use this form to create empirical ICCs. 
              Click the render button after making changes to
              any graphing parameters."),
            selectizeInput("selectItemForICC", "Select Item (you can also type item ID): ", 
                           choices = dataVariableNames(),
                           options = list(
                             placeholder = 'Please select an option below',
                             onInitialize = I('function() { this.setValue(""); }')
                           )),
            selectizeInput("selectColorThemeICC", "Select color scheme: ", 
                           choices = c("greys","spartans","dukes","cavaliers")
            ),
            textInput("textMainTitleICC","Figure Title:","Default"),
            checkboxInput("addItemIDCheck","Add item ID to Title?", TRUE),
            sliderInput("sliderLineW","Line Width: ", 1,3,1,1),
            sliderInput("sliderCEX","Dot Size: ", .5,2.5,1.5,.5),
            actionButton("buttonRenderICC", "Render")
            ), width=5), # end sideBarPanel
        mainPanel(
          plotOutput("cttICC",height="400px", width="400px")
          , width=7) 
      )
    }
      )
  
  output$cttICC <- renderPlot({
    input$buttonRenderICC
    
    # This keeps reactive code from executing on change
    isolate({
      if(input$textMainTitleICC == "Default"){
        thisPlotTitle <- "ICC for Item"
      } else{
        thisPlotTitle <- input$textMainTitleICC
      }
      if(input$addItemIDCheck) thisPlotTitle <- paste0(thisPlotTitle," ",
                                                       input$selectItemForICC)
      
      thisColorTheme <- input$selectColorThemeICC
      
      thisLWD <- input$sliderLineW
      thisCEX <- input$sliderCEX
      
    })
    
    if(! input$selectItemForICC == ""){
      cttICC(personScores(),
             scoredFileData()[,input$selectItemForICC],
             colTheme = thisColorTheme,
             plotTitle=thisPlotTitle,
             lwd=thisLWD,cex=thisCEX)
    }  
  })
  
  
  
  ####################
  # Spearman Brown Code
  ####################
  
  observeEvent(input$currentFormInfo, {
    if(! is.null(scaleAnalysis())){
      updateNumericInput(session, inputId="currentRel", label = NULL, 
                         value = scaleAnalysis()$alpha,
                         min = NULL, max = NULL, step = NULL)
      updateNumericInput(session, inputId="currentN", label = NULL, 
                         value = scaleAnalysis()$nItem,
                         min = NULL, max = NULL, step = NULL)
    } else{
      showModal(modalDialog(title = "Run Analysis First", 
                            "This button won't work until you run an analysis. 
                            You can manually enter values below if you don't have test data.",
                            footer = modalButton("Dismiss"))
      )
    }  
  }, once = FALSE)
  
  
  estTargetN <- reactive({
    if((! is.na(input$currentRel)) & 
       (! is.na(input$targetRel))
    ){
      sbNFactor <- spearman.brown(input$currentRel,input$targetRel,n.or.r = "r")
      sbNText <-paste0("To achieve a target reliability of ",
                       input$targetRel, 
                       " your test length must be changed by a factor of ",
                       round(sbNFactor$n.new,2),".")
      if(is.na(input$currentN)){
        sbNText <- paste0(sbNText," If you provide the current number of items, a new test length will be provided.")
      } else{
        sbNText <- paste0(sbNText," This result implies a new test length of ", ceiling(sbNFactor$n.new*input$currentN),".")  
      }
      return(sbNText)
    } else return(NULL)
    
  })
  
  estTargetR <- reactive({
    if((! is.na(input$currentRel)) & 
       (! is.na(input$currentN)) &
       (! is.na(input$targetN))
    ){
      sbREst <- spearman.brown(input$currentRel,input$targetN/input$currentN,n.or.r = "n")
      sbNText <-paste0("If your test length is changed from ", round(input$currentN,2),
                       " to ", input$targetN, 
                       ", your new reliability will be approximately ", 
                       round(sbREst$r.new,2),".")
      return(sbNText)
    } else return(NULL)
    
  })
  
  output$targetN <-renderText(estTargetN())
  output$targetR <-renderText(estTargetR())
  
  }

