library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(OutDec = ',', digits=8)
source('r/utils.R')

ui <- fluidPage(

    # Application title
    titlePanel("Inversió en Habitatge"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          strong(p("Ingressos i despeses actuals", style = "font-size:20px")),
          fluidRow(
            column(4,
                   shinyWidgets::currencyInput("ingressos", "Ingressos anuals nets", value=30000, align='left')),
            column(4,
                   shinyWidgets::currencyInput("lloguer", "Cost mensual del lloguer", value=408, align='left')),
            column(4,
                   shinyWidgets::currencyInput("costVida", "Altres despeses anuals", value=11000, align='left'))
          ),
          br(),
          strong(p("Compra d'habitatge", style = "font-size:20px")),
          fluidRow(
            column(5,
                   shinyWidgets::currencyInput("preu", "Preu de l'habitatge", value=280000, align='left'))
          ),
          fluidRow(
            column(5,
                   shinyWidgets::currencyInput("taxacio", "Taxació", value=425, align='left')),
            column(5,
                   shinyWidgets::currencyInput("notaria", "Notaria", value=936, align='left'))
          ),
          fluidRow(
            column(5,
                   shinyWidgets::currencyInput("registre", "Registre de la propietat", value=478, align='left')),
            column(5,
                   shinyWidgets::currencyInput("gestoria", "Gestoria", value=300, align='left'))
          ),
          fluidRow(
            column(5,
                   shinyWidgets::currencyInput("itp", "Impost sobre Transmissions Patrimonials", value = 0.05, 
                                               format = "percentageEU2dec", align='left'))
          ),
          br(),
          strong(p("Hipoteca", style = "font-size:20px")),
          fluidRow(
            column(4,
                   shinyWidgets::currencyInput("entrada", "Entrada", value=60000, align='left')),
            column(4,
                   shinyWidgets::currencyInput("interes", "Interès", value = 0.03, format = "percentageEU2dec", align='left')),
            column(4,
                   numericInput("anys", "Anys", value=30, min=0))
          ),
          br(),
          strong(p("Despeses del nou habitatge", style = "font-size:20px")),
          fluidRow(
            column(4,
                   shinyWidgets::currencyInput("upfrontImprovements", "Inversió inicial", value=0, align='left')),
            column(4,
                   shinyWidgets::currencyInput("manteniment", "Manteniment anual", value=1500, align='left'))
          ),
          br(),
          strong(p("Venda del futur habitatge", style = "font-size:20px")),
          fluidRow(
            column(5,
                   shinyWidgets::currencyInput("cancelacioHipoteca", "Penalització per cancel·lació anticipada d'hipoteca", 
                                               value = 0.01, format = "percentageEU2dec", align='left')),
            column(5,
                   shinyWidgets::currencyInput("honoraris", "Honoraris de l'agència immobiliària", 
                                               value = 0.05, format = "percentageEU2dec", align='left'))
          ),
          fluidRow(
            column(5,
                   shinyWidgets::currencyInput("incrementValor", "Increment anual de valor de l'habitatge", 
                                               value = 0.02, format = "percentageEU2dec", align='left'))
          )
        ),
        
        mainPanel(
          strong(p("Hipoteca Resultant", style = "font-size:20px")),
          textOutput('costTotal'),
          textOutput('hipoteca'),
          textOutput('interesHipoteca'),
          textOutput('quotaMensual'),
          br(),
          highchartOutput("hipotecaPlot", height = "500px"),
          br(),
          strong(p("Evolució del patrimoni i els estalvis", style = "font-size:20px")),
          highchartOutput("estalvisPlot", height = "500px")
        )
    )
)

server <- function(input, output) {

  cost <- reactive(input$preu+input$taxacio+input$notaria+input$registre+input$gestoria+input$itp*input$preu)
  hipoteca<-reactive({
    f_hipoteca(cost(),input$entrada)
  })
  quotaMensual<-reactive({
    f_quotaMensual(hipoteca(),input$interes,input$anys)
  })
  interesHipoteca<-reactive({
    round(quotaMensual()*12*input$anys-hipoteca(),2)
  })

  output$costTotal<-renderText(paste0("El cost total de la compra és de: ",f(cost())," €"))
  output$hipoteca<-renderText(paste0("La hipoteca sense interès és de: ",f(hipoteca())," €"))
  output$interesHipoteca<-renderText(paste0("L'interès és de: ",f(interesHipoteca()), " €"))
  output$quotaMensual<-renderText(paste0("Quota mensual resultant: ",f(quotaMensual()), " €"))

  hipotecaRestantAnual<-reactive({
    f_hipotecaRestant(hipoteca(),input$anys,input$interes,quotaMensual())
  })
  
  dades<-reactive({
    f_estalvi(input$ingressos,input$lloguer,input$costVida,
              input$anys,input$entrada,quotaMensual(),hipotecaRestantAnual(),
              input$upfrontImprovements,input$manteniment,
              input$cancelacioHipoteca,input$honoraris,input$incrementValor,
              input$preu,cost())
  })
  
  output$hipotecaPlot <- renderHighchart({
    dades() %>% 
      select(Any,`Hipoteca Restant`,`Valor Amortitzat Hipoteca`, `Quota Pagada`) %>% 
      pivot_longer(cols      = -Any, # works similar to using select()
                   names_to  = 'Grup', # the name of the column that will have column names as labels
                   values_to = 'Quantitat'  # the name of the column for the values
                   ) %>% 
    hchart('line', hcaes(x=Any,y=Quantitat,group=Grup)) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  output$estalvisPlot <- renderHighchart({
    dades() %>% 
      select(Any,`Estalvi Previ`,`Nou Estalvi`,`Valor Liquidat Habitatge (menys despeses de venda)`,`Nou Patrimoni Total`) %>% 
      pivot_longer(cols      = -Any, # works similar to using select()
                   names_to  = 'Grup', # the name of the column that will have column names as labels
                   values_to = 'Quantitat'  # the name of the column for the values
      ) %>% 
      hchart('line', hcaes(x=Any,y=Quantitat,group=Grup)) %>%
      hc_add_theme(hc_theme_elementary())
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
