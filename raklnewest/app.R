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
                        menuItem("Rancangan Acak Kelompok Lengkap", tabName = "RAKL", icon = icon("th-large"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "RAKL",
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
                                      selectInput(inputId = "varr1",
                                                  label = "Pilih Respon",
                                                  choices = NULL),
                                      selectInput(inputId = "varr2",
                                                  label = "Pilih Perlakuan",
                                                  choices = NULL),
                                      selectInput(inputId = "varr3",
                                                  label = "Pilih Kelompok",
                                                  choices = NULL)),
                                  box(title = "Apakah terdapat minimal perlakuan atau kelompok yang signifikan terhadap p-value?",
                                      status = "primary",
                                      solidHeader = T,
                                      selectInput(inputId = "butuh_ujilanjut",
                                                  label = "Pilih...",
                                                  choices = c("Ya","Tidak"))),
                                  box(title = "Manakah yang signifikan terhadap p-value?",
                                      status = "primary",
                                      solidHeader = T,
                                      selectInput(inputId = "peubah_ujilanjut",
                                                  label = "Pilih...",
                                                  choices = c("Perlakuan","Kelompok"))),
                                ),
                                fluidPage(
                                  tabBox(
                                    id = "tabset1",
                                    height = "1000px",
                                    width = 12,
                                    tabPanel("Data",
                                             dataTableOutput(outputId = "tabel_rakl")),
                                    tabPanel("Data Summary",
                                             box(title = "Summary Respon",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "summary_respon")),
                                             box(title = "Summary Perlakuan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "summary_perlakuan")),
                                             box(title = "Summary Kelompok",
                                                 collapsible = TRUE, width = 12,
                                                 verbatimTextOutput(outputId = "summary_kelompok"))),
                                    tabPanel("Anova",
                                             verbatimTextOutput(outputId = "anova_rakl")),
                                    tabPanel("Uji Lanjut",
                                             box(title = "Uji LSD",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_rakllsd")),
                                             box(title = "Plot Uji LSD",
                                                 height = "480px", collapsible = TRUE, width = 12,
                                                 plotOutput(outputId = "ujilanjut_lsd_plot")),
                                             box(title = "Uji Tukey",
                                                 collapsible = TRUE,width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_rakltukey")),
                                             box(title = "Plot Uji Tukey",
                                                 height = "480px", collapsible=TRUE, width = 12,
                                                 plotOutput(outputId = "ujilanjut_rakltukey_plot")),
                                             box(title = "Uji Duncan",
                                                 collapsible = TRUE,width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_raklduncan")),
                                             box(title = "Plot Uji Duncan",
                                                 height = "480px", collapsible = TRUE, width = 12,
                                                 plotOutput(outputId = "ujilanjut_duncan_plot"))),
                                    tabPanel("Uji Asumsi",
                                             box(title = "Uji Kesamaan Ragam Sisaan",
                                                 collapsible = TRUE, width = 12,
                                                 verbatimTextOutput(outputId = "homoskedastis_rakl")),
                                             box(title = "Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width = 12,
                                                 verbatimTextOutput(outputId = "autkor_rakl")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width = 12,
                                                 verbatimTextOutput(outputId = "normalitas_rakl"))),
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
  
  output$tabel_rakl <- renderDataTable(inData(), options = list(pageLength = 10))
  
  
  observe(
    updateSelectInput(session = session, inputId = "varr1", 
                      label = "Pilih Respon", choices = colnames(inData())[sapply(inData(), is.numeric)])
  )
  
  observeEvent(input$varr1,{
    updateSelectInput(session = session, inputId = "varr2",label = "Pilih Perlakuan",
                      choices = colnames(inData())[!(colnames(inData()) %in% input$varr1)])})
  
  observeEvent(input$varr2,{
    updateSelectInput(session = session, inputId = "varr3",label = "Pilih Kelompok",
                      choices = colnames(inData())[!(colnames(inData()) %in% input$varr1)])})
  
  
  summaryrespon <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    summary(Respon)
  })
  
  output$summary_respon <- renderPrint(summaryrespon())
  
  summaryperlakuan <- reactive({
    Perlakuan <- as.factor(inData()[[input$varr2]])
    summary(Perlakuan)
  })
  
  output$summary_perlakuan <- renderPrint(summaryperlakuan())
  
  summarykelompok <- reactive({
    Kelompok <- as.factor(inData()[[input$varr3]])
    summary(Kelompok)
  })
  
  output$summary_kelompok <- renderPrint(summarykelompok())
  
  anovarakl <- reactive({
    if(is.null(input$varr1)){
      return(NULL)
    }
    else{
      Respon <- as.numeric(inData()[[input$varr1]])
      Perlakuan <- as.factor(inData()[[input$varr2]])
      Kelompok <- as.factor(inData()[[input$varr3]])
      return(aov(as.formula(Respon ~ Perlakuan + Kelompok)))
    }
  })
  
  
  output$anova_rakl <- renderPrint(summary(anovarakl()))
  
  lsdrakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    lsdrakl1 <- LSD.test(anovarakl(),"Perlakuan", p.adj="none")
    return(lsdrakl1)
  })
  
  lsdrakl2 <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    lsdrakl1 <- LSD.test(anovarakl(),"Perlakuan", p.adj="none")
    plot(lsdrakl1)
  })
  
  output$ujilanjut_rakllsd <- renderPrint(lsdrakl())
  
  output$ujilanjut_lsd_plot <-  renderPlot((lsdrakl2()))
  
  tukeyrakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    tukeyrakl1 <- TukeyHSD(anovarakl(),"Perlakuan")
    return(tukeyrakl1)
  })
  
  tukeyrakl2 <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    tukeyrakl1 <- TukeyHSD(anovarakl(),"Perlakuan")
    plot(tukeyrakl1)
  })
  
  output$ujilanjut_rakltukey <- renderPrint(tukeyrakl())
  
  output$ujilanjut_rakltukey_plot <-  renderPlot((tukeyrakl2()))
  
  duncanrakl <-reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    duncanrakl1 <- duncan.test(anovarakl(), "Perlakuan")
    return(duncanrakl1)
  })
  
  duncanrakl2 <-reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    duncanrakl1 <- duncan.test(anovarakl(), "Perlakuan")
    plot(duncanrakl1)
  })
  
  output$ujilanjut_raklduncan <- renderPrint(duncanrakl())
  
  output$ujilanjut_duncan_plot <-  renderPlot((duncanrakl2()))
  
  heterorakl <- reactive({
    heterorakl1 <- bptest(anovarakl())
    return(heterorakl1)
  })
  
  output$homoskedastis_rakl <- renderPrint(heterorakl())
  
  autokorrakl <- reactive({
    autokorrakl1 <- runs.test(anovarakl()$residuals)
    return(autokorrakl1)
  })
  
  output$autokor_rakl <- renderPrint(autokorrakl())
  
  normalrakl <- reactive({
    normalrakl1 <- shapiro.test(anovarakl()$residuals)
    return(normalrakl1)
  })
  
  output$normalitas_rakl <- renderPrint(normalrakl())
  
}

shinyApp(ui, server)