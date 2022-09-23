library(shiny)
library(shinydashboard)
library(stringr)
library(agricolae)

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
                                             verbatimTextOutput(outputId = "asumsi_ral")),
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
  
  anovaral <- reactive({
    if(is.null(input$var2)){
      return(NULL)
    }
    else{
      return(aov(as.formula(paste(input$var1," ~ ",paste(input$var2,collapse="+"))),data=inData()))
    }
  })
  
  output$anova_ral <- renderPrint(summary(anovaral()))
  
  lsdral <- reactive({
     lsdral1 <- LSD.test(anovaral(),paste(input$var2), p.adj="none")
     return(lsdral1)
  })
  
  output$ujilanjut_rallsd <- renderPrint(lsdral())
  
  tukeyral <- reactive({
    tukeyral1 <- TukeyHSD(anovaral(),paste(input$var2))
    return(tukeyral1)
  })
  
  output$ujilanjut_raltukey <- renderPrint(tukeyral())
  

}

shinyApp(ui, server)