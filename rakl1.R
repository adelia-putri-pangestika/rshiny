library(shiny)
library(shinydashboard)
library(stringr)
library(agricolae)
library(lmtest)
library(car)
library(randtests)
library(nortest)

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
                                                 selectInput(inputId = "ujihomogenragamsisaan",
                                                             label = "Pilih Jenis Uji Kehomogenan Ragam Sisaan",
                                                             choices = c("Breush-Pagan Test" = "Breusch-Pagan Test",
                                                                         "Non-constant Variance Score Test" = "Non-constant Variance Score Test",
                                                                         "Glejser" = "Glejser"),
                                                             selected = NULL),
                                                 actionButton("klikhomogensisaanrakl","Pilih"),
                                                 verbatimTextOutput(outputId = "ujihomogenrakl"),
                                                 verbatimTextOutput(outputId = "result.ujihomogenrakl")),
                                             box(title="Uji Kebebasan Sisaan",
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
    if(input$sel.lanjutrakl == "Tidak Ada"){
      return("Tidak Ada Variabel yang Signifikan, Tidak Dapat Melanjutkan Uji Lanjut")
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
      return("Tidak Ada Variabel yang Signifikan, Tidak Perlu Melanjutkan Uji Lanjut")
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
      return("Tidak Ada Variabel yang Signifikan, Tidak Perlu Melanjutkan Uji Lanjut")
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
      return("Tidak Ada Variabel yang Signifikan, Tidak Perlu Melanjutkan Uji Lanjut")
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
      return(heterorakl1)
      }
    else if (input$ujihomogenragamsisaan == "Non-constant Variance Score Test"){
      heterorakl2 <- ncvTest(model.regrakl)
      return(heterorakl2)}
    else if (input$ujihomogenragamsisaan == "Glejser"){
      heterorakl3 <- ncvTest(model.regrakl)
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
      }
    }
    else if (input$ujihomogenragamsisaan == "Glejser"){
      if(heterorakl()$p > 0.05){
        return("Ragam sisaan homogen")
      }
      else{
        return("Ragam sisaan tidak homogen")
      }
    }
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
      }}
    else if (input$ujikebebasansisaanrakl == "Breusch-Godfrey"){
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
}

shinyApp(ui, server)