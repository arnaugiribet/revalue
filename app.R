library(shiny)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(OutDec = ',', digits=8)
source('r/utils.R')
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Inversió en Habitatge"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          strong(p("Habitatge", style = "font-size:20px")),
          numericInput("preu", "Preu de l'habitatge", value=300000, min=0,step=10000),
          numericInput("taxacio", "Taxació", value=425, min=0),
          numericInput("notaria", "Notaria", value=936, min=0),
          numericInput("registre", "Registre de la propietat", value=478, min=0),
          numericInput("gestoria", "Gestoria", value=300, min=0),
          shinyWidgets::currencyInput("itp", "Impost de Transmisió de Patrimoni", value = 0.05, format = "percentageEU2dec"),
          br(),
          strong(p("Hipoteca", style = "font-size:20px")),
          shinyWidgets::currencyInput("interes", "Interès", value = 0.027, format = "percentageEU2dec"),
          numericInput("anys", "Anys", value=30, min=0),
          numericInput("entrada", "Entrada", value=60000, min=0, step=10000),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          strong(p("Hipoteca Resultant", style = "font-size:20px")),
          textOutput('hipoteca'),
          textOutput('interesHipoteca'),
          textOutput('quotaMensual'),
          br(),
          strong(p("Evolució del patrimoni", style = "font-size:20px")),
          plotOutput("valuePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  hipoteca<-reactive({
    cost <- reactive(input$preu+input$taxacio+input$notaria+input$registre+input$gestoria+input$itp*input$preu)
    f_hipoteca(cost(),input$entrada)
  })
  quotaMensual<-reactive({
    f_quotaMensual(hipoteca(),input$interes,input$anys)
  })
  interesHipoteca<-reactive({
    round(quotaMensual()*12*input$anys-hipoteca(),2)
  })

  output$hipoteca<-renderText(paste0("La hipoteca sense interès és de: ",hipoteca(),"€"))
  output$interesHipoteca<-renderText(paste0("L'interès és de: ",interesHipoteca(), "€"))
  output$quotaMensual<-renderText(paste0("Quota mensual resultant: ",quotaMensual(), "€"))
  
  
  estalvi_abans_anys<-c(0, 9780, 19560, 29340, 39120, 48900, 58680, 68460, 78240, 88020, 
                        97800, 107580, 117360, 127140, 136920, 146700, 156480, 166260, 
                        176040, 185820, 195600, 205380, 215160, 224940, 234720, 244500, 
                        254280, 264060, 273840, 283620, 293400)
  nou_estalvi_anys<-c(-100000, -95799.83, -91599.67, -87399.5, -83199.34, -78999.17, 
                      -74799, -70598.84, -66398.67, -62198.51, -57998.34, -53798.18, 
                      -49598.01, -45397.84, -41197.68, -36997.51, -32797.35, -28597.18, 
                      -24397.01, -20196.85, -15996.68, -11796.52, -7596.35, -3396.19, 
                      803.98, 5004.15, 9204.31, 13404.48, 17604.64, 21804.81, 26004.98)
  nou_patrimoni_anys<-c(-22375.39, -13683.67, -4878.36, 4043.4, 13084.57, 22248.16, 
                        31537.26, 40955.06, 50504.8, 60189.82, 70013.55, 79979.49, 90091.23, 
                        100352.48, 110766.99, 121338.66, 132071.45, 142969.45, 154036.82, 
                        165277.85, 176696.93, 188298.57, 200087.38, 212068.1, 224245.58, 
                        236624.79, 249210.83, 262008.94, 275024.48, 288262.94, 301729.98)
                      
  anys_t<-0:30
  estalvi<-100000
  t<-15
  
    output$valuePlot <- renderPlot({
      t<-15
      plot(estalvi_abans_anys~anys_t, ylim=c(min(-estalvi,0),400000), pch='.', xlim=c(0,t), xaxt='n', yaxt='n', ylab='')
      grid()
      axis(1, at = seq(0, t, by = 1), las=1)
      axis(2, at = seq(0, 10^6, by = 10^5), las=2)
      lines(estalvi_abans_anys~anys_t)
      #lines(cost_lloguer_actual_anys~anys_t, col='red', lty=3)
      lines(nou_estalvi_anys~anys_t,col='green', lty=3)
      lines(nou_patrimoni_anys~anys_t,col='blue', lty=3)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
