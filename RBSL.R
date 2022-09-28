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
  inDatarbsl <- reactive({file <- input$file
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
  
  
  output$tabel_rbsl <- renderDataTable(inDatarbsl(), options = list(pageLength = 10))
  
  
  observe(
    updateSelectInput(session = session, inputId = "var1", 
                      label = "Pilih Variabel Respon", choices = colnames(inDatarbsl()))
  )
  
  observeEvent(input$var1,{
    updateSelectInput(session = session, inputId = "var2",label = "Pilih Variabel Perlakuan",
                      choices = colnames(inDatarbsl())[!(colnames(inDatarbsl()) %in% input$var1)])})
  
  observeEvent(input$var2,{
    updateSelectInput(session = session, inputId = "var3",label = "Pilih Variabel Baris",
                      choices = colnames(inDatarbsl())[!(colnames(inDatarbsl()) %in% input$var2 )])})
  
  observeEvent(input$var3,{
    updateSelectInput(session = session, inputId = "var4",label = "Pilih Variabel Kolom",
                      choices = colnames(inDatarbsl())[!(colnames(inDatarbsl()) %in% input$var3)])})
  
  summaryresponrbsl <- reactive({
    Respon <- as.numeric(inDatarbsl()[[input$var1]])
    summary(Respon)
  })
  
  output$summary_responrbsl <- renderPrint(summaryresponrbsl())
  
  summaryperlakuanrbsl <- reactive({
    Perlakuan <- as.factor(inDatarbsl()[[input$var2]])
    summary(Perlakuan)
  })
  
  output$summary_perlakuanrbsl <- renderPrint(summaryperlakuanrbsl())
  
  summarybarisrbsl <- reactive({
    Baris <- as.factor(inDatarbsl()[[input$var3]])
    summary(Baris)
  })
  
  output$summary_barisrbsl <- renderPrint(summarybarisrbsl())
  
  summarykolomrbsl <- reactive({
    Kolom <- as.factor(inDatarbsl()[[input$var3]])
    summary(Kolom)
  })
  
  output$summary_kolomrbsl <- renderPrint(summarykolomrbsl())
  
  datarbsl <- reactive({
    Respon <- as.numeric(inDatarbsl()[[input$var1]])
    Perlakuan <- as.factor(inDatarbsl()[[input$var2]])
    Baris <- as.factor(inDatarbsl()[[input$var3]])
    Kolom <- as.factor(inDatarbsl()[[input$var4]])
    data.rbsl <- data.frame(Respon, Perlakuan, Baris, Kolom)
    return(data.rbsl)
  })
  
  modelrbsl <- reactive({
    modelrbsl1 <- lm(Respon~Perlakuan+Baris+Kolom, data = datarbsl())
    return(modelrbsl1)
  })
  
  anovarbsl <- reactive({
    if(is.null(input$var2)){
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
