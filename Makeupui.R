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
                                                                         "Non-constant Variance Score Test" = "nonconstantral"),
                                                             selected = NULL),
                                                 actionButton("klikheteroral","Pilih"),
                                                 verbatimTextOutput(outputId = "ujiheteroral"),
                                                 verbatimTextOutput(outputId = "result.ujiheteroral")),
                                             box(title = "Uji Kebebasan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "tabautokorral",
                                                             label = "Pilih Jenis Uji Kebebasan Sisaan",
                                                             choices = c("Durbin-Watson Test" = "dwtestral",
                                                                         "Runs Test" = "runtestral"),
                                                             selected = NULL),
                                                 actionButton("klikautokorral","Pilih"),
                                                 verbatimTextOutput(outputId = "ujiautokorral"),
                                                 verbatimTextOutput(outputId = "result.ujiautokorral")),
                                             box(title = "Uji Kenormalan Sisaan",
                                                 collapsible = TRUE, width=12,
                                                 selectInput(inputId = "normalitasral",
                                                             label = "Pilih Jenis Uji Kenormalan Sisaan",
                                                             choices = c("Kolmogorov-Smirnov Test" = "kstestral",
                                                                         "Shapiro-Wilk Test" = "shapiroral"),
                                                             selected = NULL),
                                                 actionButton("kliknormalral","Pilih"),
                                                 verbatimTextOutput(outputId = "ujinormalitasral"),
                                                 verbatimTextOutput(outputId = "result.ujinormalitasral")))
                                             ),
                                  )
                                )
                                
                        )
                      )
                      
                    )


server <- function(input, output, session){
  inDataral <- reactive({file <- input$file
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
  
  output$tabel_ral <- renderDataTable(inDataral(), options = list(pageLength = 10))
  
  output$summary_ral <- renderPrint(summary(inDataral()))
  
  observe(
    updateSelectInput(session = session, inputId = "var1", 
                      label = "Pilih Variabel Respon", choices = colnames(inDataral())[sapply(inDataral(), is.numeric)])
  )
  
  observeEvent(input$var1,{
    updateSelectInput(session = session, inputId = "var2",label = "Pilih Variabel Perlakuan",
                      choices = colnames(inDataral())[!(colnames(inDataral()) %in% input$var1)])})
  
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
  
  output$anova_ral <- renderPrint(summary(anovaral()))
  
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
  
}

shinyApp(ui, server)