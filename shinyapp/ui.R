# this file is for design the user face 
# data1 file  
# 8/10/2019 
# author: YinanKang, Ken Lin 
# approved by: YinanKang, Ken Lin 
# maintenance by:YinanKang, Ken Lin 
# version: 5 

# aim: The aim of the file is to design the content of the user interface  
# We design the user interface into two sections, one for the user input the csv file and select the initial conditions for the csv file 
# The second section is to diaplay the expect output 
# The user interface is easy to understand and esay to use
shinyUI(pageWithSidebar(
  # set the tile of the user interface into two sections
  headerPanel("DELWP Species Overview"),
  # set the content for user input the format should be csv file
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    # check if the user needs a header for the input file
    checkboxInput('header', 'Header', TRUE),
    # select the seperation method of the input file
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),
    #select how to process the file
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote'),
    # select which species you want to observe
    
    radioButtons("radio", label = h3("Species"),
                 choices = list("Agile Antechinus" = "Agile Antechinus", "Brown Treecreeper" = "Brown Treecreeper", "common Beard-heath" = "common Beard-heath",
                                "southern brown tree frog"="southern brown tree frog","white-browed treecreeper"="white-browed treecreeper"), 
                 selected = 1),
    
    hr()
  ),
  # this is the content that content the output
  mainPanel(
    tabsetPanel(
      tabPanel('Importance', tableOutput('contents')),
      tabPanel('Reliability', tableOutput('contents2')),
      tabPanel('Map distribution', plotOutput('contents3'))
    )
    
  )
  
))
