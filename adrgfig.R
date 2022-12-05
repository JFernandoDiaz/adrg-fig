###
# Shiny - Creates graph for ADRG
# Uses Diagrammer 
###

library(tidyverse)
library(DiagrammeR)
library(DiagrammeRsvg)
library(magick)
library(glue)
library(dplyr)
library(rsvg)-
library(shinydashboard)

ui <- dashboardPage (
  
  dashboardHeader (title = "ADRG Graphs"),
  
  dashboardSidebar(),
  
  dashboardBody(

  textAreaInput("char", label = "Creating graphs for Submission-ADRG", value = " ") ,
  actionButton("update" ,"Update Graph"),
  helpText("When you click the button above, you should see",
           "the output below update to reflect the value you",
           "entered at the top:"),
  verbatimTextOutput("value"),

# Outputs Graph

  grVizOutput("dg"),

# download button

  downloadButton ('downloadPlot','Download png')
  
  )
  )


server <- function(input, output, session) {
  
  output$value <- renderPrint(input$char)
  

  output$dg <- renderGrViz({
    
    req(input$char)

    dmodel <- eventReactive(input$update,{ input$char })

    my_model <- dmodel()
 

    diagram <-  glue("digraph boxes {{
      graph [layout = dot,
             overlap = true]
      node [shape = rectangle,
            fixedsize = true,
            fontname = Helvetica,
            width = 1]
      edge [color = grey]      
        {my_model}

      }}
       ")
    
    
 DiagrammeR::grViz({diagram})  


  })
  

    output$downloadPlot <- downloadHandler(
    
      filename = "graph.png",
      
      content = function (file){
      
      req(input$char)
        
      dmodel <- eventReactive(input$update,{ input$char })
        
      my_model <- dmodel()
        
        
      diagram <-  glue("digraph boxes {{
      graph [layout = dot,
             overlap = true]
      node [shape = rectangle,
            fixedsize = true,
            fontname = Helvetica,
            width = 1]
      edge [color = grey]      
        {my_model}

      }}
       ")
        
       DiagrammeR::grViz({diagram}) %>%
         export_svg %>% charToRaw %>% rsvg_png(file)
      },
      contentType = "image/png"
    )  
  
}

shinyApp(ui, server)