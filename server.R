library(protr)
library(seqinr)
library(randomForest)


shinyServer(function(input, output, session) {
  
  mod <- readRDS("Model.rds")
  
  observe({
    
    shinyjs::hide("downloadData") # Hide download button before input submission
    if(input$submitbutton>0)
      shinyjs::show("downloadData") # Show download button after input submission
  })
  
  observe({  
    FASTADATA <- ''
    fastaexample <- '>pos-pep1 
AAEWLDAFFVRHVDR
>pos-pep2 
ADCRQKPCL
>pos-pep3 
ADGAPRPGAPLA
>pos-pep4 
AEGEFGHWCDQHWLQYWYEGDPAK
>pos-pep5 
AEGEFGWWGDSHWLQYEGDPAK
>neg-pep1 
RAERDG
>neg-pep2 
GKDAAAASV
>neg-pep3 
LAGIGLG
>neg-pep4 
CAQPREPGV
>neg-pep5 
LATATLSK
    '
    
    if(input$addlink>0) {
      isolate({
        FASTADATA <- fastaexample
        updateTextInput(session, inputId = "Sequence", value = FASTADATA)
      })
    }
  })
  
  datasetInput <- reactive({
    
    inFile <- input$file1 
    inTextbox <- input$Sequence
    
    if (is.null(inTextbox)) {
      return("Please insert/upload sequence in FASTA format")
    } else {
      if (is.null(inFile)) {
        # Read data from text box
        x <- inTextbox
        write.fasta(sequence = x, names = names(x),
                    nbchar = 80, file.out = "text.fasta")
        xtest <- readFASTA("text.fasta")
        
        # Feature extraction for Testing set
        #xtest <- read.fasta('example.fasta', seqtype="AA", as.string = TRUE)###read data
        xtest2 <- xtest[(sapply(xtest, protcheck))]###check special symbol
        
        aactest <- t(sapply(xtest2, extractAAC))
        data <- data.frame(aactest)
        
        # Predicting unknown sequences
        results <- data.frame(Prediction= predict(mod,data))
        
        print(results)
      } 
      else {  
        # Read data from uploaded file
        xtest <- readFASTA(inFile$datapath)
        
        # Feature extraction for Testing set
        #xtest <- read.fasta('example.fasta', seqtype="AA", as.string = TRUE)###read data
        xtest2 <- xtest[(sapply(xtest, protcheck))]###check special symbol
        
        aactest <- t(sapply(xtest2, extractAAC))
        data <- data.frame(aactest)
        
        # Predicting unknown sequences
        results <- data.frame(Prediction= predict(mod,data))
        
        print(results)
      }
    }
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } else {
      return("Server is ready for prediction.")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('predicted_results', '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file, row.names=FALSE)
    })
  
})
