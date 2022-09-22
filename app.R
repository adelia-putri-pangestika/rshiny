library(shiny)
library(shinydashboard)
library(stringr)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Rancangan Percobaan"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Rancangan Acak Lengkap", tabName = "RAL", icon = icon("file")),
      menuItem("Rancangan Acak Kelompok Lengkap", tabName = "RAKL", icon = icon("file")),
      menuItem("Rancangan Bujur Sangkar Latin", tabName = "RBSL", icon = icon("file"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("RBSL",
              fluidPage(
                box(title = "Data", 
                    status = "primary",
                    height = "302px",
                    solidHeader = T,
                    
                    fileInput(inputId = "file", label = "Masukkan File", multiple = FALSE,
                              accept = c("text/csv", ".csv", 
                                         "text/comma-separated-values,text/plain",
                                         ".xlsx",".xls"), 
                              width = NULL, buttonLabel = "Cari File", 
                              placeholder = "Tidak ada file yang dipilih"),
                    
                    checkboxInput(inputId = "header", 
                                  label = "Baris pertama merupakan nama kolom", 
                                  value = TRUE),
                    
                    selectInput(inputId = "delimiter", label = "Pilih Pemisah",
                                choices = c("semicolon (;)"=";",
                                            "comma (,)"=",",
                                            "pipe (|)"="|",
                                            "space ( )"=" "))
                ),
                box(title = "Pilih Peubah", status = "primary", solidHeader = T,
                    selectInput(inputId = "baris",
                                label = "Peubah Baris:",
                                choices = NULL),
                    selectInput(inputId = "kolom",
                                label = "Peubah KOlom:",
                                choices = NULL),
                    selectInput(inputId = "perlakuan",
                                label = "Perlakuan:",
                                choices = NULL),
                    selectInput(inputId = "respon",
                                label = "Respon:",
                                choices = NULL)
                ),
                fluidPage(
                  tabBox(
                    id = "tabset1",
                    height = "1000px",
                    width = 12,
                    
                    tabPanel("Data",
                             dataTableOutput(outputId = "tabel")),
                    
                    tabPanel(
                      "Data Summary",
                      verbatimTextOutput(outputId = "summary")),
                    
                    tabPanel(
                      "ANOVA",
                      verbatimTextOutput(outputId = "anova")),
                    
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
                        )
                      )
                    )
                    
                  )
                )
              )
      )
    )
  )
)
                      
                        

server <- function(input, output, session){
  file <- reactive({
    if (input$select == "df") {
      return(df1)
    } else {
      req(input$file)
      ext <- tools::file_ext(input$file$datapath)
      validate(need(ext == c("txt","csv"), "Harap unggah file .txt atau .csv"))
      file <- read.csv(input$file$datapath, 
                       header = input$header,
                       sep = input$delimiter)
      return(file)
    }
  })
  
}

shinyApp(ui,server)