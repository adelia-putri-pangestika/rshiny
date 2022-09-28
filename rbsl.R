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
                                                        label = "Pilih Baris",
                                                        choices = NULL),
                                            selectInput(inputId = "var2",
                                                        label = "Pilih Kolom",
                                                        choices = NULL),
                                            selectInput(inputId = "var3",
                                                        label = "Pilih Perlakuan",
                                                        choices = NULL),
                                            selectInput(inputId = "var4",
                                                        label = "Pilih Respon",
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
                                                     verbatimTextOutput(outputId = "summary_rbsl")),
                                            tabPanel("Anova",
                                                     verbatimTextOutput(outputId = "anova_rbsl"),
                                                     verbatimTextOutput(outputId = "anova.result")),
                                            tabPanel("Uji Lanjut",
                                                     box(title = "Uji LSD",
                                                         collapsible = TRUE, width=12,
                                                         verbatimTextOutput(outputId = "ujilanjut_rbsllsd")),
                                                     box(title = "Uji Tukey",
                                                         collapsible = TRUE,width=12,
                                                         verbatimTextOutput(outputId = "ujilanjut_rbsltukey")),
                                                     box(title = "Uji Duncan",
                                                         collapsible = TRUE,width=12,
                                                         verbatimTextOutput(outputId = "ujilanjut_rbslduncan"))),
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
                                            )
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
    
    output$summary_rbsl <- renderPrint(summary(inData()))
    
    observe(
        updateSelectInput(session = session, inputId = "var1", 
                          label = "Pilih Variabel Baris", choices = colnames(inData()))
    )
    
    observeEvent(input$var1,{
        updateSelectInput(session = session, inputId = "var2",label = "Pilih Variabel Kolom",
                          choices = colnames(inData())[!(colnames(inData()) %in% input$var1)])})
    
    observeEvent(input$var2,{
        updateSelectInput(session = session, inputId = "var3",label = "Pilih Variabel Perlakuan",
                          choices = colnames(inData())[!(colnames(inData()) %in% input$var2 )])})
    
    observeEvent(input$var3,{
        updateSelectInput(session = session, inputId = "var4",label = "Pilih Variabel Respon",
                          choices = colnames(inData())[!(colnames(inData()) %in% input$var3)])})
    datanew <- reactive({
        baris <- as.factor(inData()[[input$var1]])
        kolom <- as.factor(inData()[[input$var2]])
        perlakuan <- as.factor(inData()[[input$var3]])
        respon <- as.numeric(inData()[[input$var4]])
        datafix <- data.frame(baris,kolom,perlakuan,respon)
        return(datafix)
    })
    
    modelrbsl <- reactive({
        modelrbsl1 <- lm(respon ~ baris + kolom + perlakuan,datanew())
        return(modelrbsl1)
    })
    
    anovarbsl <- reactive({
        if(is.null(input$var1)){
            return(NULL)
        }
        else{
            return(aov(respon ~ baris + kolom + perlakuan,datanew()))
        }
    })
    
    output$anova_rbsl <- renderPrint(summary(anovarbsl()))
    
    lsdrbsl <- reactive({
        lsdrbsl1 <- LSD.test(anovarbsl(),"perlakuan", p.adj="none")
        return(lsdrbsl1)
    })
    
    output$ujilanjut_rbsllsd <- renderPrint(lsdrbsl())
    
    tukeyrbsl <- reactive({
        tukeyrbsl1 <- TukeyHSD(anovarbsl(),"perlakuan")
        return(tukeyrbsl1)
    })
    
    output$ujilanjut_rbsltukey <- renderPrint(tukeyrbsl())
    
    duncanrbsl <- reactive({
        duncanrbsl1 <- duncan.test(anovarbsl(),"perlakuan",group = T, console = T)
        return(duncanrbsl1)
    })
    
    output$ujilanjut_rbslduncan <- renderPrint(duncanrbsl())
    
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