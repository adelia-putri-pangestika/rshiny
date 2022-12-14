library(shiny)
library(shinydashboard)
library(stringr)
library(agricolae)
library(lmtest)
library(car)
library(randtests)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Experimental Design"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Rancangan Acak Lengkap", tabName = "RAL", icon = icon("th-large"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "RAL",
                                fluidPage(
                                  box(title = "Data",
                                      status = "primary",
                                      solidHeader = T,
                                      fileInput(inputId = "file", label = "Masukkan File", multiple = FALSE,
                                                accept = c("text/csv", ".csv", 
                                                           "text/comma-separated-values,text/plain",
                                                           ".xlsx",".xls"), 
                                                width = NULL, buttonLabel = "Cari File", 
                                                placeholder = "Tidak ada file yang dipilih"),
                                      checkboxInput(inputId = "header", label = "Baris pertama merupakan nama kolom", 
                                                    value = T, width = NULL),
                                      selectInput(inputId = "pemisah",
                                                  label = "Pilih Pemisah",
                                                  choices = c("Semicolon (;)" = ";",
                                                              "Comma (,)" =",",
                                                              "Tab" = "\t",
                                                              "Pipe (|)" = "|",
                                                              "Spasi" = " "),
                                                  selected = ";")),
                                  box(title = "Peubah",
                                      status = "primary",
                                      solidHeader = T,
                                      selectInput(inputId = "var1",
                                                  label = "Pilih Respon",
                                                  choices = NULL),
                                      selectInput(inputId = "var2",
                                                  label = "Pilih Perlakuan",
                                                  choices = NULL))
                                  ),
                                fluidPage(
                                  tabBox(
                                    id = "tabset1",
                                    height = "1000px",
                                    width = 12,
                                    tabPanel("Data",
                                             dataTableOutput(outputId = "tabel_ral")),
                                    tabPanel("Data Summary",
                                             verbatimTextOutput(outputId = "summary_ral")),
                                    tabPanel("Anova",
                                             verbatimTextOutput(outputId = "anova_ral")),
                                    tabPanel("Uji Lanjut",
                                             box(title = "Uji LSD",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_rallsd")),
                                             box(title = "Uji Tukey",
                                                 collapsible = TRUE,width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_raltukey")),
                                             box(title = "Uji Duncan",
                                                 collapsible = TRUE,width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_ralduncan"))),
                                    tabPanel("Uji Asumsi",
                                             box(title = "Uji Kesamaan Ragam Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "homoskedastis_ral")),
                                             box(title = "Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "autokor_ral")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "normalitas_ral"))),
                                  )
                                )
                                    
                                  )
                                )
                          
                        )
                      )
server <- function(input, output, session){
  inData <- reactive({file <- input$file
  ext <- tools::file_ext(file$datapath)
  req(file)
  
  if(ext == "txt" | ext == "csv"){
    
    dataIn <- read.table(file$datapath, sep = input$pemisah, header = input$header)
    
    if(input$header == T) {
      main <- colnames(dataIn)
      
    } else {
      main <- NULL
    }
    
    return(dataIn)
  }
  
  else{
    dataIn <- readxl::read_excel(file$datapath, col_names = input$header)
  }
  
  })
  
  
  
  output$tabel_ral <- renderDataTable(inData(), options = list(pageLength = 10))
  
  output$summary_ral <- renderPrint(summary(inData()))
  
  observe(
    updateSelectInput(session = session, inputId = "var1", 
                      label = "Pilih Variabel Respon", choices = colnames(inData())[sapply(inData(), is.numeric)])
  )
  
  observeEvent(input$var1,{
    updateSelectInput(session = session, inputId = "var2",label = "Pilih Variabel Perlakuan",
                      choices = colnames(inData())[!(colnames(inData()) %in% input$var1)])})
  
  datanew <- reactive({
    y <- as.numeric(inData()[[input$var1]])
    x <- as.factor(inData()[[input$var2]])
    datafix <- data.frame(x,y)
    return(datafix)
  })
  
  modelral <- reactive({
    modelral1 <- lm(y~x,datanew())
    return(modelral1)
  })
  
  anovaral <- reactive({
    if(is.null(input$var2)){
      return(NULL)
    }
    else{
      return(aov(y~x,datanew()))
    }
  })
  
  output$anova_ral <- renderPrint(summary(anovaral()))
  
  lsdral <- reactive({
     lsdral1 <- LSD.test(anovaral(),"x", p.adj="none")
     return(lsdral1)
  })
  
  output$ujilanjut_rallsd <- renderPrint(lsdral())
  
  tukeyral <- reactive({
    tukeyral1 <- TukeyHSD(anovaral(),"x")
    return(tukeyral1)
  })
  
  output$ujilanjut_raltukey <- renderPrint(tukeyral())
  
  duncanral <- reactive({
    duncanral1 <- duncan.test(anovaral(),"x",group = T, console = T)
    return(duncanral1)
  })
  
  output$ujilanjut_ralduncan <- renderPrint(duncanral())
  
  heteroral <- reactive({
    heteroral1 <- bptest(modelral())
    return(heteroral1)
  })
  
  output$homoskedastis_ral <- renderPrint(heteroral())
  
  autokorral <- reactive({
    autocorral1 <- runs.test(modelral()$residuals)
    return(autocorral1)
  })
  
  output$autokor_ral <- renderPrint(autokorral())
  
  normalral <- reactive({
    normalral1 <- shapiro.test(modelral()$residuals)
    return(normalral1)
  })
  
  output$normalitas_ral <- renderPrint(normalral())

}

shinyApp(ui, server)