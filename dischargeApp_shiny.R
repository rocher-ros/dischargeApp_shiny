##################################
# Shiny app to calculate Discharge using a slug injection of salt and a conductivity logger
#
# Code by G.Rocher-Ros, developed for research purposes and internal use, so no responsability 
# over third party use is taken, for further details and questions contact: gerardrocher@gmail.com
# 
#Updated version, sample file and other information can be found at https://github.com/gmrocher/dischargeApp_shiny
# 
#Calculations done as "Moore, R. D. "Slug injection using salt in solution." 
#                       Streamline Watershed Management Bulletin 8.2 (2005): 1-6.
#
#To launch use the command runApp(Discharge_App.R) or click "run App" in RStudio
#


library(shiny)
library(ggplot2)


#the server function is where all calculations are done
server <- function(input, output) {

  #read the file and select the peak period  
  myData <- reactive({
     inFile <- input$file1
     
    if (is.null(inFile))
       return(NULL)
     
    #reading the file, in case your file has more or less columns, or in different order, it should be changed here
    data <- read.csv(inFile$datapath, header=F, sep=",", col.names = c("x","date", "EC", "Temp"), skip=2)
    data <- as.data.frame(data)
    #select the period according to the input on the bg_min and end of slug
    data= data[data$x  >input$bg_min & data$x < input$slug_end,]

  })
    #here the calculations of the discharge are done
    calcs <- reactive({  
      #obtain the mean conductivity as the average of the selected period
      EC_mean <- mean(myData()$EC[myData()$x > input$bg_min & myData()$x < input$bg_max])
      #substract the mean conductivity to the whole period
      EC_corr <- myData()$EC - EC_mean
      # remove < 0 
      EC_corr <- ifelse(EC_corr <0.9, 0, EC_corr)
      #convert conductivity to slt using the calibration curve
      NaCl_s= EC_corr*input$slope
      
      #create a vector of the required length and with value of the time interval for the integration
      t=rep(input$interval,1, length(NaCl_s))
      #"Integral" of the area of the peak, as the amount of salt * the time interval
      A=sum(NaCl_s * t)
      
      #Q in m3/s, mass of salt injected dividead by the area of salt measured
      Q=round(input$nacl/A,digits = 5)
      Q
    })
      
     peak_time <- reactive({
    #   #obtain the mean conductivity as the average of the selected period
       peak <- myData()$date[max(myData()$EC)]
       peak
     })
  
  #function to make the plot, using ggplot 
  plotInput <- function(){
    
    #create a vector of the background period selected
    condbg <- cut(myData()$x, c(input$bg_min, input$bg_max))
    #obtain the mean conductivity as the average of the selected period
    EC_mean <- mean(myData()$EC[myData()$x>input$bg_min & myData()$x<input$bg_max])
    
    p<- ggplot(myData(), aes(x = x, y= EC)) + 
      theme_minimal() +
      geom_point(aes(colour = condbg), size = 2) + 
      geom_ribbon(data=myData(), aes(x = x, ymin = EC_mean, ymax = EC), fill = 'yellow1')+
      ggtitle(input$siteDate) +
      theme(legend.position = "none") +
      ylab(expression(paste("Conductivity ", "(µS/cm)"))) +
      annotate("text", x = input$bg_max, y = max(myData()$EC), label = paste("Discharge is ", calcs(), "m3/s"), size = 6)

    print(p)
  }
  
  #structure to show the plot interactively
  output$plot <- renderPlot({
    plotInput()
    })
  
  #structure to download the plot, only works if openeed in browser
  output$downloadPlot <- downloadHandler(
    filename =  function() { paste(input$siteDate, '.png', sep='') },
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    })    
  
  #structure to show the discharge value
  output$Q <- renderText({
    paste("The discharge is", calcs(), "m3/s")
  })
  
  #structure to show the discharge value
   output$peak <- renderText({
     paste("The peak was at", peak_time())
   })
  
}
  
#the "ui" is the "user interface"
ui <- fluidPage(
  
  titlePanel("Discharge measurement using a salt slug"),
  sidebarLayout(
    sidebarPanel(
      h5("Open a csv file obtained from a HoBo conductivity logger, with the columns: x, time,
         conductivity and temperature."),
      h5("Then select the peak period and the background period (coloured in orange), and introduce the amount of salt, logging interval, and the calibration curve.", align = "left"),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      numericInput("nacl", "Amount of salt (g):", 1000,
                   min = 1, max = 50000),
      numericInput("interval", "Frequency of the logger (s):", 1,
                   min = 0, max = 100),
      numericInput("slope", "Calibration constant:", 0.4441,
                   min = 0, max = 10000),
      numericInput("slug_end", "Select the end of the slug (x)", 10000,
                   min = 1, max = 100000),
      numericInput("bg_min", "Select the start of the slug (x)", 10,
                   min = 1, max = 100000),
      numericInput("bg_max", "Select the before the peak to obtain BG conductivity (x)", 2000,
                   min = 1, max = 100000),
      helpText("Observation: This app is intended to measure discharge using a slug injection of salt, it requires 
               data recorded with a HoBo conductivity logger, a correction
               coefficient of the response of conductivity against NaCl and the exact amount of salt injected.")
     
    ),
    mainPanel(
     plotOutput('plot'),

    h2(strong(textOutput("Q"))),
    h2(strong(textOutput("peak"))),
    textInput('siteDate', "Enter site and date", 'SS YY/MM/DD') ,
    downloadButton('downloadPlot', 'Download Plot (only works if opened in the browser)'),    
   
    helpText("Shiny app developed by G. Rocher-Ros (2016), developed for internal use at Umeå University. 
             Any third party use is under the users' responsability. For any questions contact gerardrocher@gmail.com ")
    
    )
  )
)

shinyApp(ui = ui, server = server)
