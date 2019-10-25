# this file is for displaying the result in the matched content in ui.R 
# data1 file  
# 8/10/2019 
# author: YinanKang, Ken Lin 
# approved by: YinanKang, Ken Lin 
# maintenance by:YinanKang, Ken Lin 
# version: 5 

# aim: The aim of the file is to output the result into design section 
# We design the user interface to output these different sections
# User can check the result they want to see by click the button
shinyServer(function(input, output) {
  

  file <- reactive({
  
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    df<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
   })
  # This function connect the ui design with the output when user click the button for a particualr species name
    observeEvent(input$radio, {
    if (input$radio=='Agile Antechinus') {
   
      if (input$file1$name != 'test2.csv'){ renderPrint(
        {"Wrong Input File"}
          
        )
        output$contents2= renderPrint(
          {"Wrong Input File"}
          
        )
        output$contents3= renderPrint(
          {"Wrong Input File"}
          
        )
      } else{
      source("Brown Treecreeper_data.R")
      output$contents2<-renderTable({
        rf_test_1
        
      })
      output$contents<-renderTable({
        importance_1
        
      })
      output$contents3<-renderPlot({
        qplot(x = rf_test_1$Longitude,y = rf_test_1$Lantituded,data = rf_test_1,colour = rf_test_1$Reliability,geom = 'point')
      })}
      
    }# the output for the second species
      else if (input$radio=='common Beard-heath'){
        # check the name of the file
        if (input$file1$name != 'test3.csv'){
        
          output$contents= renderPrint(
            {"Wrong Input File"}
            
          )
          output$contents2= renderPrint(
            {"Wrong Input File"}
            
          )
          output$contents3= renderPrint(
            {"Wrong Input File"}
            
          )
        } 
        else{
        source("Common Beard-heat_data.R")
          # output the relability of the new observation
        output$contents2<-renderTable({
          rf_test_2
          
        })
        # output the iportance rank of the predictor
        output$contents<-renderTable({
          importance_2
          
        })
        # plot the r=dritribution map
        output$contents3<-renderPlot({
          qplot(x = rf_test_2$Longitude,y = rf_test_2$Lantituded,data = rf_test_2,colour = rf_test_2$Reliability,geom = 'point')
        })}
        
      }
      else if (input$radio=='southern brown tree frog'){
        # check the input file name
        if (input$file1$name != 'test4.csv'){
          output$contents= renderPrint(
            {"Wrong Input File"}
            
          )
          output$contents2= renderPrint(
            {"Wrong Input File"}
            
          )
          output$contents3= renderPrint(
            {"Wrong Input File"}
            
          )
        } 
        else{
        source("Southern Brown Tree Frog data.R")
          # output the reliablity of the new observation
        output$contents2<-renderTable({
          rf_test_3
          
        })
        # output the importance rank of the predictor
        output$contents<-renderTable({
          importance_3
          
        })
        # plot the distribution map
        output$contents3<-renderPlot({
          qplot(x = rf_test_3$Longitude,y = rf_test_3$Lantituded,data = rf_test_3,colour = rf_test_3$Reliability,geom = 'point')
        })}
        
      }
      else if (input$radio=='white-browed treecreeper'){
        # check if the file name is valid
        if (input$file1$name != 'test5.csv'){
          output$contents= renderPrint(
            {"Wrong Input File"}
            
          )
          output$contents2= renderPrint(
            {"Wrong Input File"}
            
          )
          output$contents3= renderPrint(
            {"Wrong Input File"}
            
          )
        } else{
        source("White-browed Treecreeper data.R")
          #output the reliability of the new observation
        output$contents2<-renderTable({
          rf_test_4
          
        })
        #output the importance rank of the predictor
        output$contents<-renderTable({
          importance_4
          
        })
        #plot the distribution
        output$contents3<-renderPlot({
          qplot(x = rf_test_4$Longitude,y = rf_test_4$Lantituded,data = rf_test_4,colour = rf_test_4$Reliability,geom = 'point')
        })}
        
      }
    })    
   
})