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
                   shinyWidgets::currencyInput("upfrontImprovements", "Inversió per reformes", value=0, align='left')),
            column(4,
                   shinyWidgets::currencyInput("valueIncreaseImprovements", "Increment de valor per reformes", value=0, align='left')),
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
                                               value = 0.02, format = "percentageEU2dec", align='left')),
            column(5,
                   checkboxGroupInput('impostosVenda','Impostos associats a la venda',
                                      choiceNames=list('IRPF','Plusvàlua Municipal','IBI'),
                                      choiceValues=list('irpf','plusvaluaMunicipal','ibi'),
                                      selected=list('irpf','plusvaluaMunicipal','ibi')))
          )
        ),
        
        mainPanel(
          strong(p("Hipoteca Resultant", style = "font-size:20px")),
          htmlOutput('costTotal'),
          textOutput('hipoteca'),
          textOutput('interesHipoteca'),
          textOutput('hipotecaAmbInteres'),
          textOutput('quotaMensual'),
          br(),
          highchartOutput("hipotecaPlot", height = "500px"),
          br(),
          strong(p("Evolució del patrimoni i els estalvis", style = "font-size:20px")),
          textOutput('estalviAnterior'),
          textOutput('estalviNou'),
          textOutput('tempsNecessariRendible'),
          br(),
          textOutput('textAclarimentsPatrimoniAmortitzat'),
          textOutput('textAclariments'),
          br(),
          highchartOutput("estalvisPlot", height = "500px")
        )
    )
)

server <- function(input, output) {

  observeEvent(input$upfrontImprovements, {
    updateNumericInput(inputId = "valueIncreaseImprovements",
                       value = input$upfrontImprovements)
  })
  
  costDespeses <- reactive(label="Despeses de compra",{
    input$taxacio+input$notaria+input$registre+input$gestoria
  })
  cost <- reactive(label="Cost total de compra",{
    input$preu+costDespeses()+input$itp*input$preu
  })
  hipoteca<-reactive(label="Hipoteca necessària",{
    f_hipoteca(cost(),input$entrada)
  })
  quotaMensual<-reactive(label="Quota Mensual",{
    f_quotaMensual(hipoteca(),input$interes,input$anys)
  })
  interesHipoteca<-reactive(label="Interès de l'hipoteca",{
    quotaMensual()*12*input$anys-hipoteca()
  })
  
  estalviPrevi<-reactive({
    f_estalviPrevi(input$ingressos,input$lloguer,input$costVida)
  })
  estalviNou<-reactive({
    f_estalviNou(input$ingressos,quotaMensual(),input$costVida,input$manteniment)
  })
  anysGuanyEconomic<-reactive({
    f_anysGuanyEconomic(dadesEstalviHipoteca()$`Estalvi Previ`,dadesEstalviHipoteca()$`Nou Patrimoni Total`)
  })
  
  output$costTotal<-renderText(HTML(paste0("El cost total de la compra és de: ",f(cost())," €<br/>",
                                           f(input$preu)," € de l'habitatge, ",f(input$preu*input$itp)," € d'ITP i ",
                                           f(costDespeses())," € de despeses associades.")))
  output$hipoteca<-renderText(paste0("La hipoteca sense interès és de: ",f(hipoteca())," €"))
  output$interesHipoteca<-renderText(paste0("L'interès és de: ",f(interesHipoteca()), " €"))
  output$interesHipoteca<-renderText(paste0("La quota total (hipoteca + interès) serà de: ",f(hipoteca()+interesHipoteca()), " €"))
  output$quotaMensual<-renderText(paste0("Quota mensual resultant: ",f(quotaMensual()), " €"))
  
  output$estalviAnterior<-renderText(paste0("L'estalvi previ anual (amb lloguer) sense hipoteca era de: ",f(estalviPrevi())," €"))
  output$estalviNou<-renderText(paste0("El nou estalvi (sense lloguer) amb hipoteca seria de: ",f(estalviNou()), " €"))
  output$tempsNecessariRendible<-renderText(paste0("Anys per a que la venda de l'habitatge suposi un guany econòmic: ",anysGuanyEconomic()))
  output$textAclariments<-renderText(paste0("Per calcular el valor liquidat net de l'habitatge es resta al valor de l'immoble en el seu 
                                     moment de venda les despeses associades: cancel·lació d'hipoteca i honoraris. El nou patrimoni és la suma dels nous estalvis i el líquid net obtingut de la venda de l'habitatge."))
  output$textAclarimentsPatrimoniAmortitzat<-renderText(paste0("Dels ",f(input$entrada)," € d'entrada ",f(costDespeses()+input$itp*input$preu),
                                                               " es destinen a ITP+despeses i ",f(input$entrada-costDespeses()-input$itp*input$preu),
                                                               " és el valor liquidat inicial de l'habitatge."))
  
  hipotecaRestantAnual<-reactive(label="Hipoteca restant anual",{
    f_hipotecaRestant(hipoteca(),input$anys,input$interes,quotaMensual())
  })

  dadesEvolucioHipoteca<-reactive({
    f_dadesEvolucioHipoteca(input$anys,quotaMensual(),hipotecaRestantAnual())
  })
  
  dadesEstalviHipoteca<-reactive({
    f_dadesEstalviHipoteca(estalviPrevi(),estalviNou(),
              input$anys,input$entrada,quotaMensual(),hipotecaRestantAnual(),
              input$upfrontImprovements,input$valueIncreaseImprovements,
              input$cancelacioHipoteca,input$honoraris,input$incrementValor,
              input$preu,cost())
  })
 
  
  output$hipotecaPlot <- renderHighchart({
    dadesEvolucioHipoteca() %>% 
      select(Any,`Hipoteca Restant`,`Valor Amortitzat Hipoteca`, `Quota Pagada`) %>% 
      pivot_longer(cols      = -Any, # works similar to using select()
                   names_to  = 'Grup', # the name of the column that will have column names as labels
                   values_to = 'Quantitat'  # the name of the column for the values
                   ) %>% 
      hchart('line', hcaes(x=Any,y=Quantitat,group=Grup)) %>%
      hc_tooltip(split=T) %>% 
      hc_add_theme(hc_theme_hcrt())
  })
  
  output$estalvisPlot <- renderHighchart({
    dadesEstalviHipoteca() %>% 
      select(Any,`Estalvi Previ`,`Nou Estalvi`,`Valor Liquidat Habitatge (menys despeses de venda)`,`Nou Patrimoni Total`) %>% 
      pivot_longer(cols      = -Any, # works similar to using select()
                   names_to  = 'Grup', # the name of the column that will have column names as labels
                   values_to = 'Quantitat'  # the name of the column for the values
      ) %>% 
      hchart('line', hcaes(x=Any,y=Quantitat,group=Grup)) %>%
      hc_tooltip(split=T) %>% 
      hc_add_theme(hc_theme_hcrt())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
