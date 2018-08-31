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
#To launch, run the script step by step and click "run App" in RStudio
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
      
      StreamTemp <- mean(myData()$Temp[myData()$x > input$bg_min & myData()$x < input$bg_max])
      
      if(input$convSC==T){
        EC_corr <- EC_corr/(1+0.0191*(StreamTemp-25))   #convert conductivity to specific conductance
      }
      #create a vector of the required length and with value of the time interval for the integration
      t=rep(input$interval,1, length(EC_corr))
      #"Integral" of the area of the peak, as the conductivity departure from background * the time interval
      A=sum(EC_corr * t)
      
      #Q in L/s, input of cond*volume, divided by the integral A
      Q=round(input$init_cond*input$init_vol/A,digits = 2)
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
    
    peak <- strftime(peak_time(), format="%H:%M:%S")
    
    p<- ggplot(myData(), aes(x = x, y= EC)) + 
          geom_point(aes(colour = condbg), size = 2) + 
          geom_ribbon(data=myData(), aes(x = x, ymin = EC_mean, ymax = EC), fill = 'yellow1')+
          ggtitle(input$siteDate) +
          labs( y= "Conductivity (µS/cm)", title=input$siteDate) +
          annotate("text", -Inf, Inf, label =paste("Q= ", calcs(), "L/s, peak:", peak), size = 8, hjust = 0, vjust = 1)+    
          theme_minimal() +
          theme(legend.position = "none") 
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
    paste("The discharge is", calcs(), "L/s")
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
      h5("Open a csv file obtained from a conductivity logger, with the columns: x, time,
         conductivity and temperature."),
      h5("Then select the peak period (yellow) and the background period (coloured in orange), and introduce the amount of salt and logging interval", align = "left"),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      numericInput("init_cond", "Conductivity of the initial solution (µS cm-2):", 10000,
                   min = 1, max = 50000000),
      numericInput("init_vol", "Volume of the solution injected, in L", 1,
                   min = 0, max = 10000),
      numericInput("slug_end", "Select the end of the slug (x)", 10000,
                   min = 0.001, max = 1000000),
      numericInput("bg_min", "Select the start of the slug (x)", 10,
                   min = 1, max = 1000000),
      numericInput("bg_max", "Select a time before the peak to obtain BG conductivity (x)", 2000,
                   min = 1, max = 1000000),
      numericInput("interval", "Frequency of the logger (s):", 1,
                   min = 0, max = 100),
      checkboxInput("convSC", "Convert conductivity to specific conductance. Uncheck the box if your logger measures SC", TRUE),
      helpText("Observation: This app is intended to measure discharge using a slug injection of salt, it requires 
               a datafile with four columns in this order : index, date, EC, Temp. we use a HoBo conductivity logger.")
     
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

