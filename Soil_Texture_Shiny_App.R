#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Soil Textural Classes"),
   
   # Sidebar with a slider input for number of bins 
   titlePanel("Clasificación de texturas del suelo"),
   
   sidebarLayout(
     
     sidebarPanel(
       
       textInput(inputId = "arena", label = "Introduce el porcentaje de arena:", 0.0),
       helpText("Note: while the data view will show only",
                "the specified number of observations, the",
                "summary will be based on the full dataset."),
       textInput(inputId = "arcilla", label = "Introduce el porcentaje de arcilla:", 0.0),
       helpText("Note: while the data view will show only",
                "the specified number of observations, the",
                "summary will be based on the full dataset."),
       textInput(inputId = "limo", label = "Introduce el porcentaje de limo:", 0.0),
       helpText("Nota: La suma de los porcentajes de arena, arcilla y limo deben
                sumar exactamente el 100%.")
     ),
     
     mainPanel(
       
       
       tabsetPanel(
         tabPanel("Instrucciones", textOutput("inst")),
         tabPanel("Gráfico", plotOutput("distPlot")),
         tabPanel("Interpretación", textOutput("texto")),
         tabPanel("Tabla", dataTableOutput("tabla"))
       )
       
     )
     
   )
)

# Define server logic required to draw a histogram

library(ggtern)



# not_num <- function(input) {
# 
#   as.numeric(input)
#   
#   if (input == "") {
#     FALSE
#   } else {
#     NULL
#   }
# }


server <- function(input, output) {
  
  
  # data <- reactive({
  #   
  #   # are <- as.numeric(input$arena)
  #   # arc <- as.numeric(input$arcilla)
  #   # lim <- as.numeric(input$limo)
  #   
  #   
  #   validate(
  #     need(not_num(input$lim), 'El porcentaje de los tres componentes debde
  #             sumar exactamente el 100%.')
  #   )
  # })
  
  


  
  
  
  output$inst <- renderText("Note: while the data view will show only,
           the specified number of observations, the,
                            summary will be based on the full dataset.")
  
  output$texto <- renderText({
    
    print(paste("El porcentaje de arena es ", input$arena, " el porcentaje de arcilla es ", input$arcilla,
                " y el porcentaje de limo es ", input$limo))
    
  })

  
  
   output$distPlot <- renderPlot({
     
     are <- reactive({as.numeric(input$arena)
       
       validate(

         need(as.numeric(input$arena) < 0, "Por favor inserte un valor no negativo")

       )

       are()
       
       })
     
     
     arc <- reactive({as.numeric(input$arcilla)
                     
                     validate(
                       
                       need(as.numeric(input$arcilla) < 0, "Por favor inserte un valor no negativo")
                       
                     )
                     
                     arc()
                     
                     })
     
     
     lim <- reactive({as.numeric(input$limo)})
     

     
     ##-----------------------------------------------
     ## Basic Usage
     ##-----------------------------------------------
     df = data.frame(x = runif(50),
                     y = runif(50),
                     z = runif(50),
                     Value = runif(50,1,10),
                     Group = as.factor(round(runif(50,1,2))))
     p <- ggtern(data=df,aes(x,y,z,color=Group)) + 
       theme_bw()  + theme(tern.axis.arrow.show = T, tern.axis.clockwise = T) +
       geom_point() +  
       labs(x="% de arena",y="% de arcilla",z="% de limo",title="Title") + 
       geom_Lline(Lintercept=are()/100,color="red") +
       geom_Tline(Tintercept=arc()/100,color="blue") +
       geom_Rline(Rintercept=lim()/100,color="green")
     
     print(p)
     
     
     out <- are() + arc() + lim()
     validate(
       need(out <= 100, "La suma de los tres componetes debe ser exactamente 100%")
     )
     
   })
   
   
   output$tabla <- renderDataTable(iris,
                                   options = list(pageLength = 5)
   )
   
}

# Run the application 
shinyApp(ui = ui, server = server)

