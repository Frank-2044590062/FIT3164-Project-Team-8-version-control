# data1 file  aims for call
# 8/10/2019 
# author: YinanKang, Ken Lin 
# approved by: YinanKang, Ken Lin 
# maintenance by:YinanKang, Ken Lin 
# version: 5 


library(shiny)

shinyApp(
  
  ui=shinyUI(bootstrapPage(
    fileInput("upload", "Upload", multiple = FALSE)
  )),
  
  server=shinyServer(function(input, output, session){               
    observe({
      if (is.null(input$upload)) return()
      file.copy(input$upload$datapath, "C:/Users/new/Desktop/3164_project_R/common Beard-heath")
    })
  })
)