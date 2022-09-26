library(shiny)
library(shinydashboard)
library(stringr)
library(agricolae)

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
                                ),
                                fluidPage(
                                  tabBox(
                                    id = "tabset1",
                                    height = "1000px",
                                    width = 12,
                                    tabPanel("Data",
                                             dataTableOutput(outputId = "tabel_rakl")),
                                    tabPanel("Data Summary",
                                             verbatimTextOutput(outputId = "summary_rakl")),
                                    tabPanel("Anova",
                                             verbatimTextOutput(outputId = "anova_rakl")),
                                    tabPanel("Uji Lanjut",
                                             box(title = "Uji LSD",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_rakllsd")),
                                             box(title = "Uji Tukey",
                                                 collapsible = TRUE,width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_rakltukey")),
                                             box(title = "Plot Uji Tukey",
                                                height = "480px", solidHeader = TRUE,
                                                plotOutput(outputId = "ujilanjut_rakltukey_plot")),
                                             box(title = "Uji Duncan",
                                                 collapsible = TRUE,width=12,
                                                 verbatimTextOutput(outputId = "ujilanjut_raklduncan"))),
                                    
                                    
                                    
                                    tabPanel("Uji Asumsi",
                                             box(title = "Uji Asumsi Normalitas",
                                                 height = "280px",
                                                 selectInput(inputId = "sel.norm",
                                                             label = "Pilih Jenis Uji",
                                                             choices = c("Shapiro-Wilk"="Shapiro-Wilk",
                                                                         "Kolmogorov-Smirnov"= "Kolmogorov-Smirnov",
                                                                         "Anderson-Darling" = "Anderson-Darling"
                                                                         ),
                                                             selected = "Shapiro-Wilk"),
                                                 verbatimTextOutput(outputId = "norm"),
                                                 verbatimTextOutput(outputId = "norm.result")),
                                             box(title = "Uji Asumsi Heteroskedastisitas",
                                                 height = "280px",
                                                 selectInput(inputId = "sel.hetero",
                                                             label = "Pilih Jenis Uji",
                                                             choices = c("Breusch-Pagan" = "Breusch-Pagan",
                                                                         "Glesjer" = "Glesjer"
                                                             ),
                                                             selected = "Breusch-Pagan"),
                                                 verbatimTextOutput(outputId = "hetero"),
                                                 verbatimTextOutput(outputId = "hetero.result")),
                                             box(title = "Uji Asumsi Autokorelasi",
                                                 height = "280px",
                                                 selectInput(inputId = "sel.auto",
                                                             label = "Pilih Jenis Uji",
                                                             choices = c("Durbin-Watson" = "Durbin-Watson",
                                                                         "Breusch-Godfrey" = "Breusch-Godfrey"
                                                             ),
                                                             selected = "Durbin-Watson"),
                                                 verbatimTextOutput(outputId = "auto"),
                                                 verbatimTextOutput(outputId = "auto.result")),
                                             )
                                               
                                             ),
                          
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
  
  output$summary_rakl <- renderPrint(summary(inData()))
  
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
  
  
  
  anovarakl <- reactive({
    if(is.null(input$varr2)){
      return(NULL)
    }
    
    if(is.null(input$varr3)){
      return(NULL)
    }
    
    else{
      return(aov(as.formula(paste(input$varr1," ~ ",paste(input$varr2, "+", input$varr3, collapse="+"))),data=inData()))
    }
  })
  

  output$anova_rakl <- renderPrint(summary(anovarakl()))
  
  lsdrakl <- reactive({
    lsdrakl1 <- LSD.test(anovarakl(),paste(input$varr2), p.adj="none")
    return(lsdrakl1)
  })
  
  output$ujilanjut_rakllsd <- renderPrint(lsdrakl())

  tukeyrakl <- reactive({
    tukeyrakl1 <- TukeyHSD(anovarakl(),paste(input$varr2))
    return(tukeyrakl1)
  })
  
  tukeyrakl2 <- reactive({
    tukeyrakl1 <- TukeyHSD(anovarakl(),paste(input$varr2))
    plot(tukeyrakl1)
  })
  
  output$ujilanjut_rakltukey <- renderPrint(tukeyrakl())
  
  output$ujilanjut_rakltukey_plot <-  renderPlot((tukeyrakl2()))
  
  duncanrakl <-reactive({
    duncanrakl1 <- duncan.test(anovarakl(), paste(input$varr2))
    return(duncanrakl1)
  })
  
  output$ujilanjut_raklduncan <- renderPrint(duncanrakl())
  
  norm.result1<-reactive({
    norm.result1<-shapiro.test(anovarakl())
  })
  
  output$norm.result <- renderPrint(norm.result1())
  
}

shinyApp(ui, server)