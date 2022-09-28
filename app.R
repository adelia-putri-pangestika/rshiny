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
                        menuItem("Rancangan Bujur Sangkar Latin", tabName = "RBSL", icon = icon("th-large"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "RBSL",
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
                                                  label = "Pilih Variabel Respon",
                                                  choices = NULL),
                                      selectInput(inputId = "var2",
                                                  label = "Pilih Variabel Perlakuan",
                                                  choices = NULL),
                                      selectInput(inputId = "var3",
                                                  label = "Pilih Variabel Baris",
                                                  choices = NULL),
                                      selectInput(inputId = "var4",
                                                  label = "Pilih Variabel KOlom",
                                                  choices = NULL))
                                  
                                ),
                                fluidPage(
                                  tabBox(
                                    id = "tabset1",
                                    height = "1000px",
                                    width = 12,
                                    tabPanel("Data",
                                             dataTableOutput(outputId = "tabel_rbsl")),
                                    tabPanel("Data Summary",
                                             box(title = "Summary Respon",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "summary_responrbsl")),
                                             box(title = "Summary Perlakuan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "summary_perlakuanrbsl")),
                                             box(title = "Summary Baris",
                                                 collapsible = TRUE, width = 12,
                                                 verbatimTextOutput(outputId = "summary_barisrbsl")),
                                             box(title = "Summary Kolom",
                                                 collapsible = TRUE, width = 12,
                                                 verbatimTextOutput(outputId = "summary_kolomrbsl"))),
                                    
                                    tabPanel("Anova",
                                             verbatimTextOutput(outputId = "anova_rbsl"),
                                             verbatimTextOutput(outputId = "anova.result")),
                                    tabPanel("Uji Lanjut",
                                             box(title = "Uji LSD",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "sel.lsdrbsl",
                                                             label = "Pilih Variabel Signifikan",
                                                             choices = c("Tidak Ada" = "Tidak Ada",
                                                                         "Perlakuan" = "Perlakuan",
                                                                         "Baris" = "Baris",
                                                                         "Kolom" = "Kolom"),
                                                             selected = NULL),
                                                 actionButton("kliklsdrbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "ujilanjut_rbsllsd")),
                                             box(title = "Uji Tukey",
                                                 collapsible = TRUE,width=12,
                                                 selectInput(inputId = "sel.tukeyrbsl",
                                                             label = "Pilih Variabel Signifikan",
                                                             choices = c("Tidak Ada" = "Tidak Ada",
                                                                         "Perlakuan" = "Perlakuan",
                                                                         "Baris" = "Baris",
                                                                         "Kolom" = "Kolom"),
                                                             selected = "Tidak Ada"),
                                                 actionButton("kliktukeyrbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "ujilanjut_rbsltukey")),
                                             box(title = "Uji Duncan",
                                                 collapsible = TRUE,width=12,
                                                 selectInput(inputId = "sel.duncanrbsl",
                                                             label = "Pilih Variabel Signifikan",
                                                             choices = c("Tidak Ada" = "Tidak Ada",
                                                                         "Perlakuan" = "Perlakuan",
                                                                         "Baris" = "Baris",
                                                                         "Kolom" = "Kolom"),
                                                             selected = "Tidak Ada"),
                                                 actionButton("klikduncanrbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "ujilanjut_rbslduncan"))),
                                    tabPanel("Uji Asumsi",
                                             box(title = "Uji Kesamaan Ragam Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "homoskedastis_rbsl")),
                                             box(title = "Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "autokor_rbsl")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "normalitas_rbsl"))),
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
  
  
  output$tabel_rbsl <- renderDataTable(inData(), options = list(pageLength = 10))
  
  
  observe(
    updateSelectInput(session = session, inputId = "var1", 
                      label = "Pilih Variabel Respon", choices = colnames(inData()))
  )
  
  observeEvent(input$var1,{
    updateSelectInput(session = session, inputId = "var2",label = "Pilih Variabel Perlakuan",
                      choices = colnames(inData())[!(colnames(inData()) %in% input$var1)])})
  
  observeEvent(input$var2,{
    updateSelectInput(session = session, inputId = "var3",label = "Pilih Variabel Baris",
                      choices = colnames(inData())[!(colnames(inData()) %in% input$var2 )])})
  
  observeEvent(input$var3,{
    updateSelectInput(session = session, inputId = "var4",label = "Pilih Variabel Kolom",
                      choices = colnames(inData())[!(colnames(inData()) %in% input$var3)])})
  
  summaryresponrbsl <- reactive({
    Respon <- as.numeric(inData()[[input$var1]])
    summary(Respon)
  })
  
  output$summary_responrbsl <- renderPrint(summaryresponrbsl())
  
  summaryperlakuanrbsl <- reactive({
    Perlakuan <- as.factor(inData()[[input$var2]])
    summary(Perlakuan)
  })
  
  output$summary_perlakuanrbsl <- renderPrint(summaryperlakuanrbsl())
  
  summarybarisrbsl <- reactive({
    Baris <- as.factor(inData()[[input$var3]])
    summary(Baris)
  })
  
  output$summary_barisrbsl <- renderPrint(summarybarisrbsl())
  
  summarykolomrbsl <- reactive({
    Kolom <- as.factor(inData()[[input$var3]])
    summary(Kolom)
  })
  
  output$summary_kolomrbsl <- renderPrint(summarykolomrbsl())
  
  datarbsl <- reactive({
    Respon <- as.numeric(inData()[[input$var1]])
    Perlakuan <- as.factor(inData()[[input$var2]])
    Baris <- as.factor(inData()[[input$var3]])
    Kolom <- as.factor(inData()[[input$var4]])
    data.rbsl <- data.frame(Respon, Perlakuan, Baris, Kolom)
    return(data.rbsl)
  })
  
  modelrbsl <- reactive({
    modelrbsl1 <- lm(Respon~Perlakuan+Baris+Kolom, data = datarbsl())
    return(modelrbsl1)
  })
  
  anovarbsl <- reactive({
    if(is.null(input$var1)){
      return(NULL)
    }
    else{
      Respon <- as.numeric(inData()[[input$var1]])
      Perlakuan <- as.factor(inData()[[input$var2]])
      Baris <- as.factor(inData()[[input$var3]])
      Kolom <- as.factor(inData()[[input$var4]])
      return(aov(as.formula(Respon ~ Perlakuan + Baris + Kolom)))
    }
  })
  
  output$anova_rbsl <- renderPrint(summary(anovarbsl()))
  
  
  lsdrbsl <- reactive({
    req(anovarbsl())
    if(input$sel.lsdrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lsdrbsl == "Perlakuan"){
      lsdrbsl1 <- LSD.test(anovarbsl(),"Perlakuan", p.adj="none")
      return(lsdrbsl1)
    }
  })
  
  output$ujilanjut_rbsllsd <- renderPrint({
    input$kliklsdrbsl
    isolate(lsdrbsl())
  })
  
  tukeyrbsl <- reactive({
    req(anovarbsl())
    if(input$sel.tukeyrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.tukeyrbsl == "Perlakuan"){
      tukeyrbsl1 <- TukeyHSD(anovarbsl(),"Perlakuan")
      return(tukeyrbsl1)
    }
  })
  
  output$ujilanjut_rbsltukey <- renderPrint({
    input$kliktukeyrbsl
    isolate(tukeyrbsl())
  })
  
  duncanrbsl <- reactive({
    req(anovarbsl())
    if(input$sel.duncanrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.duncanrbsl == "Perlakuan"){
      duncanrbsl1 <- duncan.test(anovarbsl(),"Perlakuan",group = T, console = T)
      return(duncanrbsl1)
    }
  })
  
  output$ujilanjut_rbslduncan <- renderPrint({
    input$klikduncanrbsl
    isolate(duncanrbsl())
  })
  
  heterorbsl <- reactive({
    heterorbsl1 <- bptest(modelrbsl())
    return(heterorbsl1)
  })
  
  output$homoskedastis_rbsl <- renderPrint(heterorbsl())
  
  autokorrbsl <- reactive({
    autocorrbsl1 <- runs.test(modelrbsl()$residuals)
    return(autocorrbsl1)
  })
  
  output$autokor_rbsl <- renderPrint(autokorrbsl())
  
  normalrbsl <- reactive({
    normalrbsl1 <- shapiro.test(modelrbsl()$residuals)
    return(normalrbsl1)
  })
  
  output$normalitas_rbsl <- renderPrint(normalrbsl())
  
  
  
  
}

shinyApp(ui, server)
