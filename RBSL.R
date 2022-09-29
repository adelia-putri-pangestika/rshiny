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
                                             verbatimTextOutput(outputId = "anova_rbsl.result")),
                                    
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
                                                                         "Non-constant Variance Score Test" = "nonconstantrbsl"),
                                                             selected = "NULL"),
                                                 actionButton("klikheterorbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "homoskedastis_rbsl"),
                                                 verbatimTextOutput(outputId = "homoskedastis_rbsl.result")),
                                             box(title = "Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "sel.autorbsl",
                                                             label = "Pilih Jenis Uji Kebebasan Sisaan",
                                                             choices = c("Durbin-Watson Test" = "dwtestrbsl",
                                                                         "Runs Test" = "runtestrbsl"),
                                                             selected = "NULL"),
                                                 actionButton("klikautokorrbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "autokor_rbsl"),
                                                 verbatimTextOutput(outputId = "autokor_rbsl.result")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "sel.normrbsl",
                                                             label = "Pilih Jenis Uji Kenormalan Sisaan",
                                                             choices = c("Kolmogorov-Smirnov Test"= "kstestrbsl",
                                                                         "Shapiro-Wilk Test"="shapirorbsl"
                                                             ),
                                                             selected = "Shapiro-Wilk"),
                                                 actionButton("kliknormalrbsl","Pilih"),
                                                 verbatimTextOutput(outputId = "normalitas_rbsl"),
                                                 verbatimTextOutput(outputId = "normalitas_rbsl.result")))
                                  )
                                )
                                
                        )
                      )
                      
                    )
)
server <- function(input, output, session){
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
  
  output$anova_rbsl <- renderPrint(summary(anovarbsl()))
  
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
  })
  
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
      normalrbsl1 <- shapiro.test(modelrbsl()$residuals)
      return(normalrbsl1)}
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

