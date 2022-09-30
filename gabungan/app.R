library(shiny)
library(shinydashboard)
library(stringr)
library(agricolae)
library(lmtest)
library(car)
library(randtests)
library(nortest)
library(skedastic)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Experimental Design"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("RAL", tabName = "RAL", icon = icon("th-large")),
                        menuItem("RAKL", tabName = "RAKL", icon = icon("th-large")),
                        menuItem("RBSL", tabName = "RBSL", icon = icon("th-large"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "RAL",
                                fluidPage(
                                  box(title = "Data",
                                      status = "primary",
                                      solidHeader = T,
                                      fileInput(inputId = "fileral", label = "Masukkan File", multiple = FALSE,
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
                                             box(title = "Summary Respon",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "summary_responral")),
                                             box(title = "Summary Perlakuan",
                                                 collapsible = TRUE, width=12,
                                                 verbatimTextOutput(outputId = "summary_perlakuanral"))),
                                    tabPanel("Anova",
                                             verbatimTextOutput(outputId = "anova_ral"),
                                             verbatimTextOutput(outputId = "resultanova_ral")),
                                    tabPanel("Uji Lanjut",
                                             selectInput(inputId = "sel.lanjutral",
                                                         label = "Pilih Variabel Signifikan",
                                                         choices = c("Tidak Ada" = "Tidak Ada",
                                                                     "Perlakuan" = "Perlakuan"),
                                                         selected = NULL),
                                             actionButton("kliklanjutral","Pilih"),
                                             selectInput(inputId = "ujilanjutral",
                                                         label = "Pilih Uji Lanjut",
                                                         choices = c("Uji LSD" = "Ujilsdral",
                                                                     "Uji Tukey" = "Ujitukeyral",
                                                                     "Uji Duncan" = "Ujiduncanral"),
                                                         selected = NULL),
                                             conditionalPanel(condition = "input.ujilanjutral == 'Ujilsdral'",
                                                              box(title = "Uji LSD",
                                                                  collapsible = TRUE, width=12,
                                                                  verbatimTextOutput(outputId = "ujilanjut_rallsd"),
                                                                  plotOutput(outputId = "plotlsdral"))),
                                             conditionalPanel(condition = "input.ujilanjutral == 'Ujitukeyral'",
                                                              box(title = "Uji Tukey",
                                                                  collapsible = TRUE,width=12,
                                                                  verbatimTextOutput(outputId = "ujilanjut_raltukey"),
                                                                  plotOutput(outputId = "plottukeyral"))),
                                             conditionalPanel(condition = "input.ujilanjutral == 'Ujiduncanral'",
                                                              box(title = "Uji Duncan",
                                                                  collapsible = TRUE,width=12,
                                                                  verbatimTextOutput(outputId = "ujilanjut_ralduncan"),
                                                                  plotOutput(outputId = "plotduncanral")))),
                                    tabPanel("Uji Asumsi",
                                             box(title = "Uji Kesamaan Ragam Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "Heteroskedastisral",
                                                             label = "Pilih Jenis Uji Kehomogenan Ragam Sisaan",
                                                             choices = c("Breush-Pagan Test" = "bptestral",
                                                                         "Non-constant Variance Score Test" = "nonconstantral",
                                                                         "Glejser" = "glejser_ral"),
                                                             selected = NULL),
                                                 actionButton("klikheteroral","Pilih"),
                                                 verbatimTextOutput(outputId = "ujiheteroral"),
                                                 verbatimTextOutput(outputId = "result.ujiheteroral")),
                                             box(title = "Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "tabautokorral",
                                                             label = "Pilih Jenis Uji Kebebasan Sisaan",
                                                             choices = c("Durbin-Watson Test" = "dwtestral",
                                                                         "Runs Test" = "runtestral",
                                                                         "Breusch-Godfrey" = "breusch_ral"),
                                                             selected = NULL),
                                                 actionButton("klikautokorral","Pilih"),
                                                 verbatimTextOutput(outputId = "ujiautokorral"),
                                                 verbatimTextOutput(outputId = "result.ujiautokorral")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "normalitasral",
                                                             label = "Pilih Jenis Uji Kenormalan Sisaan",
                                                             choices = c("Kolmogorov-Smirnov Test" = "kstestral",
                                                                         "Shapiro-Wilk Test" = "shapiroral",
                                                                         "Anderson-Darling" = "adarling_ral"),
                                                             selected = NULL),
                                                 actionButton("kliknormalral","Pilih"),
                                                 verbatimTextOutput(outputId = "ujinormalitasral"),
                                                 verbatimTextOutput(outputId = "result.ujinormalitasral")))
                                  )
                                )
                        ),
                        
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
                                             verbatimTextOutput(outputId = "anova_rakl"),
                                             verbatimTextOutput(outputId = "anova_rakl.result1"),
                                             verbatimTextOutput(outputId = "anova_rakl.result2")),
                                    tabPanel("Uji Lanjut",
                                             selectInput(inputId = "sel.lanjutrakl",
                                                         label = "Pilih Variabel Signifikan",
                                                         choices = c("Tidak Ada" = "Tidak Ada",
                                                                     "Perlakuan" = "Perlakuan",
                                                                     "Kelompok" = "Kelompok"
                                                         ),
                                                         selected = NULL),
                                             actionButton("kliklanjutrakl","Pilih"),
                                             selectInput(inputId = "ujilanjutrakl",
                                                         label = "Pilih Uji Lanjut",
                                                         choices = c("Uji LSD" = "Ujilsdrakl",
                                                                     "Uji Tukey" = "Ujitukeyrakl",
                                                                     "Uji Duncan" = "Ujiduncanrakl"),
                                                         selected = NULL),
                                             conditionalPanel(condition = "input.ujilanjutrakl == 'Ujilsdrakl'",
                                                              box(title = "Uji LSD",
                                                                  collapsible = TRUE, width = 12, 
                                                                  verbatimTextOutput(outputId = "ujilanjut_rakllsd"),
                                                                  plotOutput(outputId = "ujilanjut_rakllsd_plot"))),
                                             conditionalPanel(condition = "input.ujilanjutrakl == 'Ujitukeyrakl'",
                                                              box(title = "Uji Tukey",
                                                                  collapsible = TRUE, width = 12,
                                                                  verbatimTextOutput(outputId = "ujilanjut_rakltukey"),
                                                                  plotOutput(outputId = "ujilanjut_rakltukey_plot"))),
                                             conditionalPanel(condition = "input.ujilanjutrakl == 'Ujiduncanrakl'",
                                                              box(title = "Uji Duncan",
                                                                  collapsible = TRUE, width = 12, 
                                                                  verbatimTextOutput(outputId = "ujilanjut_raklduncan"),
                                                                  plotOutput(outputId = "ujilanjut_raklduncan_plot")))),
                                    tabPanel("Uji Asumsi",
                                             box(title = "Uji Kesamaan Ragam Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "ujihomogenragamsisaan",
                                                             label = "Pilih Jenis Uji Kehomogenan Ragam Sisaan",
                                                             choices = c("Breusch-Pagan Test" = "Breusch-Pagan Test",
                                                                         "Non-constant Variance Score Test" = "Non-constant Variance Score Test",
                                                                         "Glejser" = "Glejser"),
                                                             selected = NULL),
                                                 actionButton("klikhomogensisaanrakl","Pilih"),
                                                 verbatimTextOutput(outputId = "ujihomogenrakl"),
                                                 verbatimTextOutput(outputId = "result.ujihomogenrakl")),
                                             box(title="Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "ujikebebasansisaanrakl",
                                                             label = "Pilih Jenis Uji Kebebasan Sisaan",
                                                             choices = c("Durbin-Watson Test" = "Durbin-Watson Test",
                                                                         "Runs Test" = "Runs Test",
                                                                         "Breusch-Godfrey" = "Breusch-Godfrey"),
                                                             selected = NULL),
                                                 actionButton("klikbebassisaanrakl","Pilih"),
                                                 verbatimTextOutput(outputId = "ujikebebasanrakl"),
                                                 verbatimTextOutput(outputId = "result.ujikebebasanrakl")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "ujikenormalansisaanrakl",
                                                             label = "Pilih Jenis Uji Kenormalan Sisaan",
                                                             choices = c("Kolmogorov-Smirnov Test" = "Kolmogorov-Smirnov Test",
                                                                         "Shapiro-Wilk Test" = "Shapiro-Wilk Test",
                                                                         "Anderson-Darling" = "Anderson-Darling"),
                                                             selected = NULL),
                                                 actionButton("kliknormalsisaanrakl","Pilih"),
                                                 verbatimTextOutput(outputId = "ujikenormalanrakl"),
                                                 verbatimTextOutput(outputId = "result.ujikenormalanrakl"))),
                                  )
                                  
                                )
                                
                        ),
                        
                        tabItem(tabName = "RBSL",
                                fluidPage(
                                  box(title = "Data",
                                      status = "primary",
                                      solidHeader = T,
                                      fileInput(inputId = "filerbsl", label = "Masukkan File", multiple = FALSE,
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
                                      selectInput(inputId = "variabel1",
                                                  label = "Pilih Variabel Respon",
                                                  choices = NULL),
                                      selectInput(inputId = "variabel2",
                                                  label = "Pilih Variabel Perlakuan",
                                                  choices = NULL),
                                      selectInput(inputId = "variabel3",
                                                  label = "Pilih Variabel Baris",
                                                  choices = NULL),
                                      selectInput(inputId = "variabel4",
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
                                             verbatimTextOutput(outputId = "anova_rbsl.result1"),
                                             verbatimTextOutput(outputId = "anova_rbsl.result2"),
                                             verbatimTextOutput(outputId = "anova_rbsl.result3")),
                                    
                                    tabPanel("Uji Lanjut",
                                             selectInput(inputId = "sel.lanjutrbsl",
                                                         label = "Pilih Variabel Signifikan",
                                                         choices = c("Tidak Ada" = "Tidak Ada",
                                                                     "Perlakuan" = "Perlakuan",
                                                                     "Baris" = "Baris",
                                                                     "Kolom" = "Kolom"),
                                                         selected = NULL),
                                             actionButton("kliklanjutrbsl","Pilih"),
                                             selectInput(inputId = "ujilanjutrbsl",
                                                         label = "Pilih Uji Lanjut",
                                                         choices = c("Uji LSD" = "Ujilsdrbsl",
                                                                     "Uji Tukey" = "Ujitukeyrbsl",
                                                                     "Uji Duncan" = "Ujiduncanrbsl"),
                                                         selected = NULL),
                                             conditionalPanel(condition = "input.ujilanjutrbsl == 'Ujilsdrbsl'",
                                                              box(title = "Uji LSD",
                                                                  collapsible = TRUE, width=12,
                                                                  verbatimTextOutput(outputId = "ujilanjut_rbsllsd"),
                                                                  plotOutput(outputId = "plotlsdrbsl"))),
                                             conditionalPanel(condition = "input.ujilanjutrbsl == 'Ujitukeyrbsl'",
                                                              box(title = "Uji Tukey",
                                                                  collapsible = TRUE,width=12,
                                                                  verbatimTextOutput(outputId = "ujilanjut_rbsltukey"),
                                                                  plotOutput(outputId = "plottukeyrbsl"))),
                                             conditionalPanel(condition = "input.ujilanjutrbsl == 'Ujiduncanrbsl'",
                                                              box(title = "Uji Duncan",
                                                                  collapsible = TRUE,width=12,
                                                                  verbatimTextOutput(outputId = "ujilanjut_rbslduncan"),
                                                                  plotOutput(outputId = "plotduncanrbsl")))),
                                    tabPanel("Uji Asumsi",
                                             box(title = "Uji Kesamaan Ragam Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "sel.heterorbsl",
                                                             label = "Pilih Jenis Uji Kehomogenan Ragam Sisaan",
                                                             choices = c("Breusch-Pagan Test" = "bptestrbsl",
                                                                         "Non-constant Variance Score Test" = "nonconstantrbsl",
                                                                         "Glejser" = "glejser_rbsl"),
                                                             selected = "NULL"),
                                                 actionButton("klikheterorbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "homoskedastis_rbsl"),
                                                 verbatimTextOutput(outputId = "homoskedastis_rbsl.result")),
                                             box(title = "Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "sel.autorbsl",
                                                             label = "Pilih Jenis Uji Kebebasan Sisaan",
                                                             choices = c("Durbin-Watson Test" = "dwtestrbsl",
                                                                         "Runs Test" = "runtestrbsl",
                                                                         "Breusch-Godfrey" = "breusch_rbsl"),
                                                             selected = "NULL"),
                                                 actionButton("klikautokorrbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "autokor_rbsl"),
                                                 verbatimTextOutput(outputId = "autokor_rbsl.result")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "sel.normrbsl",
                                                             label = "Pilih Jenis Uji Kenormalan Sisaan",
                                                             choices = c("Kolmogorov-Smirnov Test"= "kstestrbsl",
                                                                         "Shapiro-Wilk Test"="shapirorbsl",
                                                                         "Anderson-Darling" = "adarling_rbsl"
                                                             ),
                                                             selected = "Shapiro-Wilk"),
                                                 actionButton("kliknormalrbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "normalitas_rbsl"),
                                                 verbatimTextOutput(outputId = "normalitas_rbsl.result"))),
                                  )
                                )
                                
                        )
                      )))




server <- function(input, output, session){
  inDataral <- reactive({fileral <- input$fileral
  ext <- tools::file_ext(fileral$datapath)
  req(fileral)
  
  if(ext == "txt" | ext == "csv"){
    
    dataIn <- read.table(fileral$datapath, sep = input$pemisah, header = input$header)
    
    if(input$header == T) {
      main <- colnames(dataIn)
      
    } else {
      main <- NULL
    }
    
    return(dataIn)
  }
  
  else{
    dataIn <- readxl::read_excel(fileral$datapath, col_names = input$header)
  }
  
  })
  
  
  
  output$tabel_ral <- renderDataTable(inDataral(), options = list(pageLength = 10))
  
  observe(
    updateSelectInput(session = session, inputId = "var1", 
                      label = "Pilih Variabel Respon", choices = colnames(inDataral())[sapply(inDataral(), is.numeric)])
  )
  
  observeEvent(input$var1,{
    updateSelectInput(session = session, inputId = "var2",label = "Pilih Variabel Perlakuan",
                      choices = colnames(inDataral())[!(colnames(inDataral()) %in% input$var1)])})
  
  summaryresponral <- reactive({
    Respon <- as.numeric(inDataral()[[input$var1]])
    summary(Respon)
  })
  
  output$summary_responral <- renderPrint(summaryresponral())
  
  summaryperlakuanral <- reactive({
    Perlakuan <- as.factor(inDataral()[[input$var2]])
    summary(Perlakuan)
  })
  
  output$summary_perlakuanral <- renderPrint(summaryperlakuanral())
  
  datanew <- reactive({
    y <- as.numeric(inDataral()[[input$var1]])
    x <- as.factor(inDataral()[[input$var2]])
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
  
  anovaral2 <- reactive({
    anovaral3 <- anova(modelral())
    resultral <- data.frame(anovaral3$`Pr(>F)`)
    anovafix <- resultral[1,]
    return(anovafix)
  })
  
  resultanovaral <- reactive({
    req(anovaral2())
    if (anovaral2() > 0.05){
      return("Perlakuan Tidak Berpengaruh Signifikan Terhadap Respon")
    }
    else{
      return("Perlakuan Berpengaruh Signifikan Terhadap Respon")
    }
  })
  
  output$anova_ral <- renderPrint(summary(anovaral()))
  
  output$resultanova_ral <- renderPrint(resultanovaral())
  
  lsdral <- reactive({
    req(anovaral())
    if(input$sel.lanjutral == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutral == "Perlakuan"){
      lsdral1 <- LSD.test(anovaral(),"x", p.adj="none")
      return(lsdral1)
    }
  })
  
  output$ujilanjut_rallsd <- renderPrint({
    input$kliklanjutral
    isolate(lsdral())
  })
  
  lsdralplot <- reactive({
    req(anovaral())
    if(input$sel.lanjutral == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutral == "Perlakuan"){
      lsdral1 <- LSD.test(anovaral(),"x", p.adj="none")
      return(plot(lsdral1))
    }
  })
  
  output$plotlsdral <- renderPlot({
    input$kliklanjutral
    isolate(lsdralplot())
  })
  
  tukeyral <- reactive({
    req(anovaral())
    if(input$sel.lanjutral == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutral == "Perlakuan"){
      tukeyral1 <- TukeyHSD(anovaral(),"x")
      return(tukeyral1)
    }
  })
  
  output$ujilanjut_raltukey <- renderPrint({
    input$kliklanjutral
    isolate(tukeyral())
  })
  
  tukeyralplot <- reactive({
    req(anovaral())
    if(input$sel.lanjutral == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutral == "Perlakuan"){
      tukeyral1 <- TukeyHSD(anovaral(),"x")
      return(plot(tukeyral1))
    }
  })
  
  output$plottukeyral <- renderPlot({
    input$kliklanjutral
    isolate(tukeyralplot())
  })
  
  duncanral <- reactive({
    req(anovaral())
    if(input$sel.lanjutral == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutral == "Perlakuan"){
      duncanral1 <- duncan.test(anovaral(),"x",group = T, console = T)
      return(duncanral1)
    }
  })
  
  output$ujilanjut_ralduncan <- renderPrint({
    input$kliklanjutral
    isolate(duncanral())
  })
  
  duncanralplot <- reactive({
    req(anovaral())
    if(input$sel.lanjutral == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutral == "Perlakuan"){
      duncanral1 <- duncan.test(anovaral(),"x",group = T, console = T)
      return(plot(duncanral1))
    }
  })
  
  output$plotduncanral <- renderPlot({
    input$kliklanjutral
    isolate(duncanralplot())
  })
  
  heteroral <- reactive({
    if (input$Heteroskedastisral == "bptestral"){
      heteroral1 <- bptest(modelral())
      return(heteroral1)}
    else if (input$Heteroskedastisral == "nonconstantral"){
      heteroral2 <- ncvTest(modelral())
      return(heteroral2)
    }
    else if (input$Heteroskedastisral == "glejser_ral"){
      heteroral3 <- glejser(modelral())
      return(heteroral3)
    }
  })
  
  output$ujiheteroral <- renderPrint({
    input$klikheteroral
    isolate(heteroral())
  })
  
  result_heteroral <- reactive({
    req(heteroral())
    if (input$Heteroskedastisral == "bptestral"){
      if(heteroral()$p.value > 0.05){
        return("Ragam sisaan homogen")
      }
      else if(heteroral()$p.value <= 0.05){
        return("Ragam sisaan tidak homogen")
      }}
    else if (input$Heteroskedastisral == "nonconstantral"){
      if(heteroral()$p > 0.05){
        return("Ragam sisaan homogen")
      }
      else if(heteroral()$p <= 0.05){
        return("Ragam sisaan tidak homogen")
      }}
    else if(input$Heteroskedastisral == "glejser_ral"){
      if(heteroral()$p.value > 0.05){
        return("Ragam sisaan homogen")}
      else{
        return("Ragam sisaan tidak homogen")
      }}
  })
  
  output$result.ujiheteroral <- renderPrint(({
    input$klikheteroral
    isolate(result_heteroral())
  }))
  
  autokorral <- reactive({
    if (input$tabautokorral == "dwtestral"){
      autocorral1 <- dwt(modelral())
      return(autocorral1)}
    else if (input$tabautokorral == "runtestral"){
      autocorral2 <- runs.test(modelral()$residuals)
      return(autocorral2)}
    else if(input$tabautokorral == "breusch_ral"){
      autocorral3 <- bgtest(modelral())
      return(autocorral3)}
  })
  
  output$ujiautokorral <- renderPrint({
    input$klikautokorral
    isolate(autokorral())
  })
  
  result_autokorral <- reactive({
    req(autokorral())
    if (input$tabautokorral == "dwtestral"){
      if(autokorral()$p > 0.05){
        return("Antar sisaan saling bebas")
      }
      else{
        return("Antar sisaan tidak saling bebas")
      }}
    else if (input$tabautokorral == "runtestral"){
      if(autokorral()$p.value > 0.05){
        return("Antar sisaan saling bebas")
      }
      else{
        return("Antar sisaan tidak saling bebas")
      }}
    else if(input$tabautokorral == "breusch_ral"){
      if (autokorral()$p.value > 0.05){
        return("Antar sisaan saling bebas")}
      else{
        return("Antar sisaan tidak saling bebas")
      }
      }
  })
  
  output$result.ujiautokorral <- renderPrint({
    input$klikautokorral
    isolate(result_autokorral())
  })
  
  normalral <- reactive({
    if (input$normalitasral == "kstestral"){
      normalral1 <- ks.test(modelral()$residuals, "pnorm", mean=mean(modelral()$residuals), sd=sd(modelral()$residuals))
      return(normalral1)}
    else if (input$normalitasral == "shapiroral"){
      normalral2 <- shapiro.test(modelral()$residuals)
      return(normalral2)}
    else if(input$normalitasral == "adarling_ral"){
      normalral3 <- ad.test(modelral()$residuals)
      return(normalral3)}
  })
  
  output$ujinormalitasral <- renderPrint({
    input$kliknormalral
    isolate(normalral())})
  
  result_normalitasral <- reactive({
    req(normalral())
    if(normalral()$p.value > 0.05){
      return("Sisaan menyebar normal")
    }
    else{
      return("Sisaan tidak menyebar normal")
    }
  })
  
  output$result.ujinormalitasral <- renderPrint({
    input$kliknormalral
    isolate(result_normalitasral())
  })
  
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
  
  datarsetrakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    data.rakl <- data.frame(Respon, Perlakuan, Kelompok)
    return(data.rakl)
  })
  
  modelrakl <- reactive({
    modelrakl1 <- lm(Respon~Perlakuan+Kelompok, data = datarsetrakl())
    return(modelrakl1)
  })
  
  anovaraklres1 <- reactive({
    anovarakl3 <- anova(modelrakl())
    resultrakl <- data.frame(anovarakl3$`Pr(>F)`)
    anovafixrakl <- resultrakl[1,]
    return(anovafixrakl)
  })
  
  resultanovarakl1 <- reactive({
    req(anovaraklres1())
    if (anovaraklres1() > 0.05){
      return("Perlakuan Tidak Berpengaruh Signifikan Terhadap Respon")
    }
    else{
      return("Perlakuan Berpengaruh Signifikan Terhadap Respon")
    }
  })
  
  anovaraklres2 <- reactive({
    anovarakl4 <- anova(modelrakl())
    resultrakl1 <- data.frame(anovarakl4$`Pr(>F)`)
    anovafixrakl1 <- resultrakl1[2,]
    return(anovafixrakl1)
  })
  
  resultanovarakl2 <- reactive({
    req(anovaraklres2())
    if (anovaraklres2() > 0.05){
      return("Kelompok Tidak Berpengaruh Signifikan Terhadap Respon")
    }
    else{
      return("Kelompok Berpengaruh Signifikan Terhadap Respon")
    }
  })
  
  output$anova_rakl <- renderPrint(summary(anovarakl()))
  
  output$anova_rakl.result1 <- renderPrint(resultanovarakl1())
  
  output$anova_rakl.result2 <- renderPrint(resultanovarakl2())
  
  lsdrakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    if(input$sel.lanjutrakl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrakl == "Perlakuan"){
      lsdrakl1 <- LSD.test(anovarakl(),"Perlakuan", p.adj="none")
      return(lsdrakl1)
    }
    else if(input$sel.lanjutrakl == "Kelompok"){
      lsdrakl2 <- LSD.test(anovarakl(),"Kelompok", p.adj="none")
      return(lsdrakl2)
    }
  })
  
  lsdrakl4 <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    if(input$sel.lanjutrakl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan, Tidak Dapat Melanjutkan Uji Lanjut")
    }
    else if(input$sel.lanjutrakl == "Perlakuan"){
      lsdrakl1 <- LSD.test(anovarakl(),"Perlakuan", p.adj="none")
      (plot(lsdrakl1))
    }
    else if(input$sel.lanjutrakl == "Kelompok"){
      lsdrakl2 <- LSD.test(anovarakl(),"Kelompok", p.adj="none")
      plot(lsdrakl2)
    }
    
  })
  
  output$ujilanjut_rakllsd <- renderPrint({
    input$kliklanjutrakl
    isolate(lsdrakl())
  })
  output$ujilanjut_rakllsd_plot <- renderPlot({
    input$kliklanjutrakl
    isolate(lsdrakl4())
  })
  
  tukeyrakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    if(input$sel.lanjutrakl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrakl == "Perlakuan"){
      tukeyrakl1 <- TukeyHSD(anovarakl(),"Perlakuan")
      return(tukeyrakl1)
    }
    else if(input$sel.lanjutrakl == "Kelompok"){
      tukeyrakl2 <- TukeyHSD(anovarakl(),"Kelompok")
      return(tukeyrakl2)
    }
  })
  
  tukeyrakl4 <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    if(input$sel.lanjutrakl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrakl == "Perlakuan"){
      tukeyrakl1 <- TukeyHSD(anovarakl(),"Perlakuan")
      plot(tukeyrakl1)
    }
    else if(input$sel.lanjutrakl == "Kelompok"){
      tukeyrakl2 <- TukeyHSD(anovarakl(),"Kelompok")
      plot(tukeyrakl2)
    }
  })
  
  output$ujilanjut_rakltukey <- renderPrint({
    input$kliklanjutrakl
    isolate(tukeyrakl())
  })
  
  output$ujilanjut_rakltukey_plot <- renderPlot({
    input$kliklanjutrakl
    isolate(tukeyrakl4())
  })
  
  duncanrakl <-reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    if(input$sel.lanjutrakl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrakl == "Perlakuan"){
      duncanrakl1 <- duncan.test(anovarakl(),"Perlakuan")
      return(duncanrakl1)
    }
    else if(input$sel.lanjutrakl == "Kelompok"){
      duncanrakl2 <- duncan.test(anovarakl(),"Kelompok")
      return(duncanrakl2)
    }
  })
  
  duncanrakl4 <-reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    if(input$sel.lanjutrakl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrakl == "Perlakuan"){
      duncanrakl1 <- duncan.test(anovarakl(),"Perlakuan")
      plot(duncanrakl1)
    }
    else if(input$sel.lanjutrakl == "Kelompok"){
      duncanrakl2 <- duncan.test(anovarakl(),"Kelompok")
      plot(duncanrakl2)
    }
  })
  
  output$ujilanjut_raklduncan <- renderPrint({
    input$kliklanjutrakl
    isolate(duncanrakl())
  })
  
  output$ujilanjut_raklduncan_plot <- renderPlot({
    input$kliklanjutrakl
    isolate(duncanrakl4())
  })
  
  heterorakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    model.regrakl <- lm(Respon ~ Perlakuan + Kelompok)
    if (input$ujihomogenragamsisaan == "Breusch-Pagan Test"){
      heterorakl1 <- bptest(model.regrakl)
      return(heterorakl1)}
    else if (input$ujihomogenragamsisaan == "Non-constant Variance Score Test"){
      heterorakl2 <- ncvTest(model.regrakl)
      return(heterorakl2)
    }
    else if (input$ujihomogenragamsisaan == "Glejser"){
      heterorakl3 <- glejser(model.regrakl)
      return(heterorakl3)}
  })
  
  output$ujihomogenrakl <- renderPrint({
    input$klikhomogensisaanrakl
    isolate(heterorakl())
  })
  
  result_ujihomogenrakl <- reactive({
    req(heterorakl())
    if(input$ujihomogenragamsisaan == "Breusch-Pagan Test"){
      if(heterorakl()$p.value > 0.05){
        return("Ragam sisaan homogen")
      }
      else{
        return("Ragam sisaan tidak homogen")
      }}
    else if (input$ujihomogenragamsisaan == "Non-constant Variance Score Test"){
      if(heterorakl()$p > 0.05){
        return("Ragam sisaan homogen")
      }
      else{
        return("Ragam sisaan tidak homogen")
      }}
    else if (input$ujihomogenragamsisaan == "Glejser"){
        if(heterorakl()$p.value > 0.05){
          return("Ragam sisaan homogen")
        }
        else{
          return("Ragam sisaan tidak homogen")
        }}
  })
  
  output$result.ujihomogenrakl <- renderPrint(({
    input$klikhomogensisaanrakl
    isolate(result_ujihomogenrakl())
  }))
  
  autokorrakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    model.regrakl <- lm(Respon ~ Perlakuan + Kelompok)
    if (input$ujikebebasansisaanrakl == "Durbin-Watson Test"){
      bebasrakl1 <- dwt(model.regrakl)
      return(bebasrakl1)}
    else if (input$ujikebebasansisaanrakl == "Runs Test"){
      bebasrakl2 <- runs.test(model.regrakl$residuals)
      return(bebasrakl2)}
    else if (input$ujikebebasansisaanrakl == "Breusch-Godfrey"){
      bebasrakl3 <- bgtest(model.regrakl)
      return(bebasrakl3)}
  })
  result_ujikebebasanrakl <- reactive({
    req(autokorrakl())
    if(input$ujikebebasansisaanrakl == "Durbin-Watson Test"){
      if(autokorrakl()$p > 0.05){
        return("Antar sisaan saling bebas")
      }
      else{
        return("Antar sisaan tidak saling bebas")
      }}
    else if (input$ujikebebasansisaanrakl == "Runs Test"){
      if(autokorrakl()$p.value > 0.05){
        return("Antar sisaan saling bebas")
      }
      else{
        return("Antar sisaan tidak saling bebas")
      }} else if (input$ujikebebasansisaanrakl == "Breusch-Godfrey"){
        if(autokorrakl()$p.value > 0.05){
          return("Antar sisaan saling bebas")
        }
        else{
          return("Antar sisaan tidak saling bebas")
        }}
  }) 
  output$ujikebebasanrakl <- renderPrint({
    input$klikbebassisaanrakl
    isolate(autokorrakl())
  })
  output$result.ujikebebasanrakl <- renderPrint({
    input$klikbebassisaanrakl
    isolate(result_ujikebebasanrakl())
  })
  normalrakl <- reactive({
    Respon <- as.numeric(inData()[[input$varr1]])
    Perlakuan <- as.factor(inData()[[input$varr2]])
    Kelompok <- as.factor(inData()[[input$varr3]])
    model.regrakl <- lm(Respon ~ Perlakuan + Kelompok)
    if (input$ujikenormalansisaanrakl == "Kolmogorov-Smirnov Test"){
      normalrakl1 <- ks.test(model.regrakl$residuals, "pnorm", mean=mean(model.regrakl$residuals), sd=sd(model.regrakl$residuals))
      return(normalrakl1)}
    else if (input$ujikenormalansisaanrakl == "Shapiro-Wilk Test"){
      normalrakl2 <- shapiro.test(model.regrakl$residuals)
      return(normalrakl2)}
    else if (input$ujikenormalansisaanrakl == "Anderson-Darling"){
      normalrakl3 <- ad.test(model.regrakl$residuals)
      return(normalrakl3)}
  })
  result_ujikenormalanrakl <- reactive({
    req(normalrakl())
    if(normalrakl()$p.value > 0.05){
      return("Sisaan menyebar normal")
    }
    else{
      return("Sisaan tidak menyebar normal")
    }
  }) 
  output$ujikenormalanrakl <- renderPrint({
    input$kliknormalsisaanrakl
    isolate(normalrakl())})
  output$result.ujikenormalanrakl <- renderPrint({
    input$kliknormalsisaanrakl
    isolate(result_ujikenormalanrakl())
  }) 
  
  
  inDatarbsl <- reactive({filerbsl <- input$filerbsl
  ext <- tools::file_ext(filerbsl$datapath)
  req(filerbsl)
  
  if(ext == "txt" | ext == "csv"){
    
    dataIn <- read.table(filerbsl$datapath, sep = input$pemisah, header = input$header)
    
    if(input$header == T) {
      main <- colnames(dataIn)
      
    } else {
      main <- NULL
    }
    
    return(dataIn)
  }
  
  else{
    dataIn <- readxl::read_excel(filerbsl$datapath, col_names = input$header)
  }
  
  })
  
  
  output$tabel_rbsl <- renderDataTable(inDatarbsl(), options = list(pageLength = 10))
  
  
  observe(
    updateSelectInput(session = session, inputId = "variabel1", 
                      label = "Pilih Variabel Respon", choices = colnames(inDatarbsl()))
  )
  
  observeEvent(input$variabel1,{
    updateSelectInput(session = session, inputId = "variabel2",label = "Pilih Variabel Perlakuan",
                      choices = colnames(inDatarbsl())[!(colnames(inDatarbsl()) %in% input$variabel1)])})
  
  observeEvent(input$variabel2,{
    updateSelectInput(session = session, inputId = "variabel3",label = "Pilih Variabel Baris",
                      choices = colnames(inDatarbsl())[!(colnames(inDatarbsl()) %in% input$variabel2 )])})
  
  observeEvent(input$variabel3,{
    updateSelectInput(session = session, inputId = "variabel4",label = "Pilih Variabel Kolom",
                      choices = colnames(inDatarbsl())[!(colnames(inDatarbsl()) %in% input$variabel3)])})
  
  summaryresponrbsl <- reactive({
    Respon <- as.numeric(inDatarbsl()[[input$variabel1]])
    summary(Respon)
  })
  
  output$summary_responrbsl <- renderPrint(summaryresponrbsl())
  
  summaryperlakuanrbsl <- reactive({
    Perlakuan <- as.factor(inDatarbsl()[[input$variabel2]])
    summary(Perlakuan)
  })
  
  output$summary_perlakuanrbsl <- renderPrint(summaryperlakuanrbsl())
  
  summarybarisrbsl <- reactive({
    Baris <- as.factor(inDatarbsl()[[input$variabel3]])
    summary(Baris)
  })
  
  output$summary_barisrbsl <- renderPrint(summarybarisrbsl())
  
  summarykolomrbsl <- reactive({
    Kolom <- as.factor(inDatarbsl()[[input$variabel3]])
    summary(Kolom)
  })
  
  output$summary_kolomrbsl <- renderPrint(summarykolomrbsl())
  
  datarbsl <- reactive({
    Respon <- as.numeric(inDatarbsl()[[input$variabel1]])
    Perlakuan <- as.factor(inDatarbsl()[[input$variabel2]])
    Baris <- as.factor(inDatarbsl()[[input$variabel3]])
    Kolom <- as.factor(inDatarbsl()[[input$variabel4]])
    data.rbsl <- data.frame(Respon, Perlakuan, Baris, Kolom)
    return(data.rbsl)
  })
  
  modelrbsl <- reactive({
    modelrbsl1 <- lm(Respon~Perlakuan+Baris+Kolom, data = datarbsl())
    return(modelrbsl1)
  })
  
  anovarbsl <- reactive({
    if(is.null(input$variabel2)){
      return(NULL)
    }
    else{
      return(aov(Respon ~ Perlakuan + Baris + Kolom,datarbsl()))
    }
  })
  
  anovarbslres1 <- reactive({
    anovarbsl3 <- anova(modelrbsl())
    resultrbsl <- data.frame(anovarbsl3$`Pr(>F)`)
    anovafixrbsl <- resultrbsl[1,]
    return(anovafixrbsl)
  })
  
  resultanovarbsl1 <- reactive({
    req(anovarbslres1())
    if (anovarbslres1() > 0.05){
      return("Perlakuan Tidak Berpengaruh Signifikan Terhadap Respon")
    }
    else{
      return("Perlakuan Berpengaruh Signifikan Terhadap Respon")
    }
  })
  
  anovarbslres2 <- reactive({
    anovarbsl4 <- anova(modelrbsl())
    resultrbsl1 <- data.frame(anovarbsl4$`Pr(>F)`)
    anovafixrbsl1 <- resultrbsl1[2,]
    return(anovafixrbsl1)
  })
  
  resultanovarbsl2 <- reactive({
    req(anovarbslres2())
    if (anovarbslres2() > 0.05){
      return("Baris Tidak Berpengaruh Signifikan Terhadap Respon")
    }
    else{
      return("Baris Berpengaruh Signifikan Terhadap Respon")
    }
  })
  
  anovarbslres3 <- reactive({
    anovarbsl5 <- anova(modelrbsl())
    resultrbsl2 <- data.frame(anovarbsl5$`Pr(>F)`)
    anovafixrbsl2 <- resultrbsl2[3,]
    return(anovafixrbsl2)
  })
  
  resultanovarbsl3 <- reactive({
    req(anovarbslres3())
    if (anovarbslres3() > 0.05){
      return("Kolom Tidak Berpengaruh Signifikan Terhadap Respon")
    }
    else{
      return("Kolom Berpengaruh Signifikan Terhadap Respon")
    }
  })
  
  output$anova_rbsl <- renderPrint(summary(anovarbsl()))
  
  output$anova_rbsl.result1 <- renderPrint(resultanovarbsl1())
  
  output$anova_rbsl.result2 <- renderPrint(resultanovarbsl2())
  
  output$anova_rbsl.result3 <- renderPrint(resultanovarbsl3())
  
  lsdrbsl <- reactive({
    req(anovarbsl())
    if(input$sel.lanjutrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrbsl == "Perlakuan"){
      lsdrbsl1 <- LSD.test(anovarbsl(),"Perlakuan", p.adj="none")
      return(lsdrbsl1)
    }
    else if(input$sel.lanjutrbsl == "Baris"){
      lsdrbsl2 <- LSD.test(anovarbsl(),"Baris", p.adj="none")
      return(lsdrbsl2)
    }
    else if(input$sel.lanjutrbsl == "Kolom"){
      lsdrbsl3 <- LSD.test(anovarbsl(),"Kolom", p.adj="none")
      return(lsdrbsl3)
    }
  })
  
  output$ujilanjut_rbsllsd <- renderPrint({
    input$kliklanjutrbsl
    isolate(lsdrbsl())
  })
  
  lsdrbslplot <- reactive({
    req(anovarbsl())
    if(input$sel.lanjutrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrbsl == "Perlakuan"){
      lsdrbsl1 <- LSD.test(anovarbsl(),"Perlakuan", p.adj="none")
      return(plot(lsdrbsl1))
    }
    else if(input$sel.lanjutrbsl == "Baris"){
      lsdrbsl2 <- LSD.test(anovarbsl(),"Baris", p.adj="none")
      return(plot(lsdrbsl2))
    }
    else if(input$sel.lanjutrbsl == "Kolom"){
      lsdrbsl3 <- LSD.test(anovarbsl(),"Kolom", p.adj="none")
      return(plot(lsdrbsl3))
    }
  })
  
  output$plotlsdrbsl <- renderPlot({
    input$kliklanjutrbsl
    isolate(lsdrbslplot())
  })
  
  tukeyrbsl <- reactive({
    req(anovarbsl())
    if(input$sel.lanjutrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrbsl == "Perlakuan"){
      tukeyrbsl1 <- TukeyHSD(anovarbsl(),"Perlakuan")
      return(tukeyrbsl1)
    }
    else if(input$sel.lanjutrbsl == "Baris"){
      tukeyrbsl2 <- TukeyHSD(anovarbsl(),"Baris")
      return(tukeyrbsl2)
    }
    else if(input$sel.lanjutrbsl == "Kolom"){
      tukeyrbsl3 <- TukeyHSD(anovarbsl(),"Kolom")
      return(tukeyrbsl3)
    }
  })
  
  output$ujilanjut_rbsltukey <- renderPrint({
    input$kliklanjutrbsl
    isolate(tukeyrbsl())
  })
  
  tukeyrbslplot <- reactive({
    req(anovarbsl())
    if(input$sel.lanjutrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrbsl == "Perlakuan"){
      tukeyrbsl1 <- TukeyHSD(anovarbsl(),"Perlakuan")
      return(plot(tukeyrbsl1))
    }
    else if(input$sel.lanjutrbsl == "Baris"){
      tukeyrbsl2 <- TukeyHSD(anovarbsl(),"Baris")
      return(plot(tukeyrbsl2))
    }
    else if(input$sel.lanjutrbsl == "Kolom"){
      tukeyrbsl3 <- TukeyHSD(anovarbsl(),"Kolom")
      return(plot(tukeyrbsl3))
    }
  })
  
  output$plottukeyrbsl <- renderPlot({
    input$kliklanjutrbsl
    isolate(tukeyrbslplot())
  })
  
  duncanrbsl <- reactive({
    req(anovarbsl())
    if(input$sel.lanjutrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrbsl == "Perlakuan"){
      duncanrbsl1 <- duncan.test(anovarbsl(),"Perlakuan",group = T, console = T)
      return(duncanrbsl1)
    }
    else if(input$sel.lanjutrbsl == "Baris"){
      duncanrbsl2 <- duncan.test(anovarbsl(),"Baris",group = T, console = T)
      return(duncanrbsl2)
    }
    else if(input$sel.lanjutrbsl == "Kolom"){
      duncanrbsl3 <- duncan.test(anovarbsl(),"Kolom",group = T, console = T)
      return(duncanrbsl3)
    }
  })
  
  output$ujilanjut_rbslduncan <- renderPrint({
    input$kliklanjutrbsl
    isolate(duncanrbsl())
  })
  
  duncanrbslplot <- reactive({
    req(anovarbsl())
    if(input$sel.lanjutrbsl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan")
    }
    else if(input$sel.lanjutrbsl == "Perlakuan"){
      duncanrbsl1 <- duncan.test(anovarbsl(),"Perlakuan",group = T, console = T)
      return(plot(duncanrbsl1))
    }
    else if(input$sel.lanjutrbsl == "Baris"){
      duncanrbsl2 <- duncan.test(anovarbsl(),"Baris",group = T, console = T)
      return(plot(duncanrbsl2))
    }
    else if(input$sel.lanjutrbsl == "Kolom"){
      duncanrbsl3 <- duncan.test(anovarbsl(),"Kolom",group = T, console = T)
      return(plot(duncanrbsl3))
    }
  })
  
  output$plotduncanrbsl <- renderPlot({
    input$kliklanjutrbsl
    isolate(duncanrbslplot())
  })
  
  heterorbsl <- reactive({
    if(input$sel.heterorbsl == "bptestrbsl"){
      heterorbsl1 <- bptest(modelrbsl())
      return(heterorbsl1)
    }
    else if(input$sel.heterorbsl == "nonconstantrbsl"){
      heterorbsl2 <- ncvTest(modelrbsl())
      return(heterorbsl2)
    }
    else if(input$sel.heterorbsl == "glejser_rbsl"){
      heterorbsl3 <- glejser(modelrbsl())
      return(heterorbsl3)
    }
  })
  
  output$homoskedastis_rbsl <- renderPrint({
    input$klikheterorbsl
    isolate(heterorbsl())
  })
  
  heterorbsl.result <- reactive({
    req(heterorbsl())
    if(input$sel.heterorbsl == "bptestrbsl"){
      if(heterorbsl()$p.value > 0.05){
        return("Ragam sisaan homogen")
      }
      else{
        return("Ragam sisaan tidak homogen")
      }}
    else if(input$sel.heterorbsl == "nonconstantrbsl"){
      if(heterorbsl()$p > 0.05){
        return("Ragam sisaan homogen")
      }
      else{
        return("Ragam sisaan tidak homogen")
      }}
    else if(input$sel.heterorbsl == "glejser_rbsl"){
      if(heterorbsl()$p.value > 0.05){
        return("Ragam sisaan homogen")}
      else{
        return("Ragam sisaan homogen")}
      }
    }
)
  
  output$homoskedastis_rbsl.result <- renderPrint(({
    input$klikheterorbsl
    isolate(heterorbsl.result())
  }))
  
  autokorrbsl <- reactive({
    if(input$sel.autorbsl == "dwtestrbsl"){
      autocorrbsl1 <- dwt(modelrbsl())
      return(autocorrbsl1)}
    else if(input$sel.autorbsl == "runtestrbsl"){
      autocorrbsl2 <- runs.test(modelrbsl()$residuals)
      return(autocorrbsl2)}
    else if(input$sel.autorbsl == "breusch_rbsl"){
      autocorrbsl3 <- bgtest(modelrbsl())
      return(autocorrbsl3)
    }
  })
  
  output$autokor_rbsl <- renderPrint({
    input$klikautokorrbsl
    isolate(autokorrbsl())
  })
  
  autokorrbsl.result <- reactive({
    req(autokorrbsl())
    if(input$sel.autorbsl == "dwtestrbsl"){
      if(autokorrbsl()$p > 0.05){
        return("Antar sisaan saling bebas")
      }
      else{
        return("Antar sisaan tidak saling bebas")
      }}
    else if(input$sel.autorbsl == "runtestrbsl"){
      if(autokorrbsl()$p.value > 0.05){
        return("Antar sisaan saling bebas")
      }
      else{
        return("Antar sisaan tidak saling bebas")
      }}
    else if(input$sel.autorbsl == "breusch_rbsl"){
      if(autokorrbsl()$p.value > 0.05){
        return("Antar sisaan saling bebas")}
      else{
        return("Antar siaan tidak saling bebas")
      }
      }
  })
  
  output$autokor_rbsl.result <- renderPrint({
    input$klikautokorrbsl
    isolate(autokorrbsl.result())
  })
  
  normalrbsl <- reactive({
    if(input$sel.normrbsl == "kstestrbsl"){
      normalrbsl1 <- ks.test(modelrbsl()$residuals, "pnorm", mean=mean(modelrbsl()$residuals),sd=sd(modelrbsl()$residuals))
      return(normalrbsl1)}
    else if(input$sel.normrbsl == "shapirorbsl"){
      normalrbsl2 <- shapiro.test(modelrbsl()$residuals)
      return(normalrbsl2)}
    else if(input$sel.normrbsl == "adarling_rbsl"){
      normalrbsl3 <- ad.test(modelrbsl()$residuals)
      return(normalrbsl3)
    }
  })
  
  output$normalitas_rbsl <- renderPrint({
    input$kliknormalrbsl
    isolate(normalrbsl())
  })
  
  normalrbsl.result <- reactive({
    req(normalrbsl())
    if(normalrbsl()$p.value > 0.05) {
      return("Sisaan menyebar normal")
    }
    else{
      return("Sisaan tidak menyebar normal")
    }
  })
  
  output$normalitas_rbsl.result <- renderPrint({
    input$kliknormalrbsl
    isolate(normalrbsl.result())
  })
  
}

shinyApp(ui, server)