
# Load R packages
library(shiny)
library(shinythemes)
library(lubridate)






# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Kelompok 6",
                  tabPanel("Dashboard",
                           sidebarPanel(width = 6,
                             tabsetPanel(
                               tabPanel("Personal Data Input", 
                                        tags$h3("Input:"),
                                        
                                        textInput("str1", "Enter Your Name", ""),
                                        numericInput("str2", "Enter Your Role", ""),
                                        dateRangeInput("str3", "Semester Start & End Date", "",start  = "2022-07-30",
                                                       end    = "2022-07-30",),
                                        dateInput("str4", "Exam Date", ""),
                                        actionButton("proses","Submit",class = "btn-primary")       
                               ),
                               tabPanel("Model Data Input", 
                                        checkboxInput("EF", "Collect Data"),
                                        actionButton("Go", "Submit",class = "btn-primary")
                               ),
                             ),
                           ), # sidebarPanel
                           
                           mainPanel(width = 6,
                             tabsetPanel(
                               tabPanel("Personal Info", 
                                        verbatimTextOutput("name"),
                                        verbatimTextOutput("role"),
                                        verbatimTextOutput("semester"),
                                        verbatimTextOutput("date"), 
                                        
                               ),
                               tabPanel("Data",
                                        tags$style(
                                          ".FixedHeightContainer
                                            {
                                              
                                              height: 450px;
                                              width:550px; 
                                              padding:3px; 

                                            }
                                            .Content
                                            {
                                              height:224px;
                                               overflow:auto;
                                                
                                            }"
                                        ),
                                        div(
                                          id = 'myDiv', class = 'FixedHeightContainer',
                                          div(
                                            id = 'myDiv', class = 'Content',
                                            verbatimTextOutput("renderprint")
                                          ),
                                        ),
                                        
                               ),
                               tabPanel("Plot", 
                                        fluidRow(
                                          align = "center", 
                                          uiOutput("plot1"),
                                          plotOutput("GraphMC"),
                                          plotOutput("GraphDensity")
                                        )     
                               ),
                               tabPanel("MLE", 
                                        plotOutput("MLE1"),
                                        plotOutput("MLE2"),
                                        plotOutput("MLE"),
                                        
                               ),
                             ),
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("About Us",
                           sidebarPanel(width = 15,
                             tabsetPanel(
                               tabPanel("", 
                                        tags$style(
                                          " .Contents
                                            {
                                              
                                              background-color: rgb(255,255,255);
                                              padding : 20px;
                                              border-radius: 30px;
                                                
                                            }"
                                        ),
                                        div(
                                          id = 'myDiv', class = 'Contents',
                                          h3('Members of Group 6'),
                                          h5('1. Miranti Setyaningrum (19611044)'),
                                          h5('2. Rahmi Anandra (19611069)'),
                                          h5('3. Nindala Divaoni Ayudhia(19611075)'),
                                          h5('4. Safira Nugrahandini (19611096)'),
                                          h5('5. Nur Fajri Apriliani (19611116)'),
                                          h5('6. Deden Nurhasanah (19611143)'),
                                          
                                        ),
                               ),
                               
                             ),
                           ), # sidebarPanel
                           
                           
                           
                  ), # Navbar 1, tabPanel
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output,session) {
  observeEvent( input$proses,{
    output$name <- renderText({
      paste("I, ",input$str1, sep = "" )
    })
    output$role <- renderText({
      paste( "With Role Number",input$str2,"Attending This", sep = " " )
    })
    output$semester <- renderText({
      paste( "Semester From",input$str3[1],"to",input$str3[2], sep = " " )
    })
    output$date <- renderText({
      paste( "Giving Semester Exam on",input$str4,"at",now() ,sep = " " )
    })
  },
  
  )
  GEF <- eventReactive(input$Go, {
    if (input$EF) {
      data=read.csv('data1.csv')
      
      plot(x = data$x,
           xlab = "x-axis",
           main = "Plot"
      )
      
    } else {
      NULL
    }
    data <- read.csv("data1.csv")
    
    
    output$renderprint <- renderPrint({
      for (i in 1: nrow(data))
      {
        print(data[i,])
      }
    })  
  })
  
  Histograms <- eventReactive(input$Go, {
    if (input$EF) {
      data=read.csv('data1.csv')
      
      # Add a Normal Curve (Thanks to Peter Dalgaard)
      x <- data$x
      h<-hist(x, breaks=10, col="green", xlab="Miles Per Gallon",
              main="Histogram with Normal Curve")
      xfit<-seq(min(x),max(x),length=40)
      yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
      yfit <- yfit*diff(h$mids[1:2])*length(x)
      lines(xfit, yfit, col="red", lwd=2)
    } else {
      NULL
    }
  })
  
  Density <- eventReactive(input$Go, {
    if (input$EF) {
      data=read.csv('data1.csv')
      
      # Add a Normal Curve (Thanks to Peter Dalgaard)
      d <- density(data$x)
      plot(d, main="Kernel Density of Miles Per Gallon")
      polygon(d, col="red", border="blue")
    } else {
      NULL
    }
  })
  
  Normal <- eventReactive(input$Go, {
    if (input$EF) {
      data=read.csv('data1.csv')
      
      dat=data.frame(x=data$x, 
                     y=dnorm(1000, mean=0.1, sd=0.1))
      min.RSS <- function(data, par) {
        with(data, sum((par[1] + par[2] * x - y)^2))
      }
      result <- optim(par = c(0, 1), min.RSS, data = dat)
      plot(y ~ x, data = dat,main = "Normal optim Function")
      abline(a = result$par[1], b = result$par[2], col = "red")
    } else {
      NULL
    }
  })
  
  Gaussian <- eventReactive(input$Go, {
    if (input$EF) {
      data=read.csv('data1.csv')
      
      dat=data.frame(x=data$x, 
                     y=rnorm(1000, mean=0.1, sd=0.1))
      min.RSS <- function(data, par) {
        with(data, sum((par[1] + par[2] * x - y)^2))
      }
      result <- optim(par = c(0, 1), min.RSS, data = dat)
      plot(y ~ x, data = dat,main = "Gausian optim Function")
      abline(a = result$par[1], b = result$par[2], col = "red")
    } else {
      NULL
    }
  })
  
  Poison <- eventReactive(input$Go, {
    if (input$EF) {
      data=read.csv('data1.csv')
      
      dat=data.frame(x=data$x, 
                     y=dpois(0:999, lambda = 0.1))
      min.RSS <- function(data, par) {
        with(data, sum((par[1] + par[2] * x - y)^2))
      }
      result <- optim(par = c(0, 1), min.RSS, data = dat)
      plot(y ~ x, data = dat,main = "Poisson optim Function")
      abline(a = result$par[1], b = result$par[2], col = "red")
    } else {
      NULL
    }
  })
  
  showme <- eventReactive(input$Go, {
    if (input$EF) TRUE else FALSE
  })
  
  
  output$GraphEF <- renderPlot({ # Efficient Frontier
    GEF()
  })
  
  output$GraphMC <- renderPlot({
    Histograms()
  })
  
  output$GraphDensity <- renderPlot({
    Density()
  })
  
  output$MLE <- renderPlot({
    Normal()
  })
  
  output$MLE1 <- renderPlot({
    Gaussian()
  })
  
  output$MLE2 <- renderPlot({
    Poison()
  })
  
  output$plot1 <- renderUI({
    if (showme()) {
      plotOutput("GraphEF")
    } else NULL
  })
  
} # server




# Create Shiny object
shinyApp(ui = ui, server = server)