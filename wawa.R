
library(shiny)
library(shinydashboard)
library(stringr)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Experimental Design"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Rancangan Acak Lengkap", tabName = "RAL", icon = icon("th-large")),
                        menuItem("Rancangan Acak Kelompok Lengkap", tabName = "RAKL", icon = icon("th-large"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "RAL",
                                fluidPage(
                                  box(title = "Data",
                                      status = "primary",
                                      height = "360px",
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
                                                  choices = NULL)
                                  )
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
                                             verbatimTextOutput(outputId = "ujilanjut_ral")),
                                    tabPanel("Uji Asumsi",
                                             verbatimTextOutput(outputId = "asumsi_ral")),
                                  )
                                )
                                
                        ),
                        tabItem(tabName = "RAKL",
                                fluidPage(
                                  box(title = "Data",
                                      status = "primary",
                                      height = "360px",
                                      solidHeader = T,
                                      fileInput(inputId = "file12", label = "Masukkan File", multiple = FALSE,
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
                                                  label = "Pilih Ulangan",
                                                  choices = NULL)
                                  )
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
                                    tabPanel(
                                      "Uji Lanjut",
                                      conditionalPanel(
                                        condition = "output.anova",
                                        box(title = "Uji Lanjut",
                                            height = "280px",
                                            selectInput(inputId = "sel.norm",
                                                        label = "Pilih Jenis Uji",
                                                        choices = c("Shapiro-Wilk"="Shapiro-Wilk",
                                                                    "Kolmogorov-Smirnov"= "Kolmogorov-Smirnov",
                                                                    "Anderson-Darling" = "Anderson-Darling"
                                                        ),
                                                        selected = "Shapiro-Wilk"),
                                            verbatimTextOutput(outputId = "norm"),
                                    tabPanel(
                                      "Uji Asumsi",
                                      conditionalPanel(
                                        condition = "output.anova",
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
                                                                    "Glesjer" = "Glesjer"),
                                                        selected = "Breusch-Pagan"),
                                            verbatimTextOutput(outputId = "hetero"),
                                            verbatimTextOutput(outputId = "hetero.result")),
                                        
                                        box(title = "Uji Asumsi Autokorelasi",
                                            height = "280px",
                                            selectInput(inputId = "sel.auto",
                                                        label = "Pilih Jenis Uji",
                                                        choices = c("Durbin-Watson" = "Durbin-Watson",
                                                                    "Breusch-Godfrey" = "Breusch-Godfrey"),
                                                        selected = "Durbin-Watson"),
                                            verbatimTextOutput(outputId = "auto"),
                                            verbatimTextOutput(outputId = "auto.result")),
                                        
                                        box(title = "Ringkasan",
                                            height = "280px",
                                            tableOutput(outputId = "ringkas_uji"))
                                      )
                                    ),
                                  )
                                )
                                
                        )
                        
                        
                      ),
                        
                
                    ),
                    
))))
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
}
shinyApp(ui = ui, server = server)
