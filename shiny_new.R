##################################
# Shiny app to calculate Discharge using a slug injection of salt and a conductivity logger
#
# Code by G.Rocher-Ros, developed for research purposes and internal use, so no responsability
# over third party use is taken, for further details and questions contact: gerardrocher@gmail.com
#
# Updated version, sample file and other information can be found at https://github.com/rocher-ros/dischargeApp_shiny
# Updated July 2017
# Calculations done as "Moore, R. D. "Slug injection using salt in solution."
#                       Streamline Watershed Management Bulletin 8.2 (2005): 1-6.

library(shiny)
library(tidyverse)
library(Cairo)
library(patchwork)

options(shiny.usecairo = T)



ui <- fluidPage(tabsetPanel(
  tabPanel(
    "Screen the data",
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        h2('Upload the file'),
        fileInput(
          'file',
          'Choose file to upload',
          accept = c(
            'text/csv',
            'text/comma-separated-values',
            'text/tab-separated-values',
            'text/plain',
            '.csv',
            '.tsv'
          )
        ),
        # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
        tags$hr(),
        actionButton("choice", "Refresh"),
        radioButtons('sep', 'Separator',
                     c(
                       Comma = ',',
                       Semicolon = ';',
                       Tab = '\t'
                     ),
                     ','),
        selectInput("date_lab", "Select column with date time", choices = NULL),
        selectInput("cond_lab", "Select column with conductivity (uS/cm)", choices = NULL),
        # no choices before uploading
        selectInput("temp_lab", "Select column with temperature (Celsius)", choices = NULL)
        
        
      ),
      mainPanel(
        tableOutput("table"),
        helpText(
          "Shiny app developed by G. Rocher-Ros (2016), developed for internal use at Umeå University. Any third party use is under the users' responsability. For any questions contact gerardrocher@gmail.com "
        )
        
      )
    )
  ),
  
  tabPanel(
    "Calculations and visualization",
    fluid = TRUE,
    sidebarPanel(
      numericInput(
        "nacl",
        "Amount of salt  in the solution (g):",
        1000,
        min = 1,
        max = 50000
      ),
      numericInput(
        "slug_end",
        "Select the end of the slug, with some slack (x)",
        10000,
        min = 1,
        max = 100000
      ),
      numericInput(
        "bg_min",
        "Select the start of the slug (x)",
        10,
        min = 1,
        max = 100000
      ),
      numericInput(
        "bg_max",
        "Select an x before the peak to obtain background conductivity (x)",
        2000,
        min = 1,
        max = 100000
      ),
      helpText(
        "This is the average conductivity between the start of the slug and here"
      ),
      numericInput(
        "interval",
        "Frequency of the logger (s):",
        1,
        min = 0,
        max = 100
      ),
      numericInput(
        "slope",
        "Calibration constant (slope):",
        NA,
        min = 0,
        max = 10000
      ),
      helpText(
        "Enter a number if you did a field calibration, otherwise we convert to specific conductance. Field calibrations can be important if salinity is high"
      ),
      textInput('siteDate', "Enter site and date", 'Site Date') ,
      
      
    ),
    mainPanel(
      plotOutput('plot'),
      h2(strong(textOutput("Q"))),
      downloadButton('downloadPlot', 'Download Plot'),
      
      helpText(
        "Shiny app developed by G. Rocher-Ros (2016), developed for internal use at Umeå University. Any third party use is under the users' responsability. For any questions contact gerardrocher@gmail.com "
      )
      
    )
  )
))








server <-
  function(input, output, session) {
    # added session for updateSelectInput
    
    info <- eventReactive(input$choice, {
      inFile <- input$file
      # Instead # if (is.null(inFile)) ... use "req"
      req(inFile)
      
      # Changes in read.table
      f <- read.table(inFile$datapath, sep = input$sep, header = T)
      vars <- names(f)
      # Update select input immediately after clicking on the action button.
      updateSelectInput(session,
                        "date_lab",
                        "Select column with date time",
                        choices = vars)
      updateSelectInput(session,
                        "cond_lab",
                        "Select column with conductivity (µS/cm)",
                        choices = vars)
      updateSelectInput(session,
                        "temp_lab",
                        "Select column with temperature (Celsius)",
                        choices = vars)
      
      f
    })
    
    
    output$table <- renderTable({
      # f <- info()
      #f <- subset(f, select = input$temp_lab) #subsetting takes place here
      f <- info()
      
      data <- data.frame(
        x = 1:nrow(f),
        date.time = subset(f, select = input$date_lab),
        cond = subset(f, select = input$cond_lab),
        temp = subset(f, select = input$temp_lab)
      )
      colnames(data) <- c("x", "date.time", "cond", "temp")
      
      head(data, n = 100)
    })
    
    
    
    
    plotInput <- function() {
      f <- info()
      
      data <- data.frame(
        x = 1:nrow(f),
        date.time = subset(f, select = input$date_lab),
        cond = subset(f, select = input$cond_lab),
        temp = subset(f, select = input$temp_lab)
      )
      colnames(data) <- c("x", "date.time",  "cond", "temp")
      
      data_selected <- data %>%
        filter(x > input$bg_min & x < input$slug_end)
      
      #create a vector of the background period selected
      data_prepeak <- data %>%
        filter(x > input$bg_min & x < input$bg_max)
      
      #obtain the mean conductivity as the average of the selected period
      EC_mean <- mean(data_prepeak$cond)
      
      #subtract the mean conductivity to the whole period
      EC_corr <- data_selected$cond - EC_mean
      
      # remove < 0
      EC_corr <- ifelse(EC_corr < 0.9, 0, EC_corr)
      
      #Mean temperature during the peak
      stream.temp <- mean(data_selected$temp)
      
      
      #And calculate the theoretical slope of Cond vs NaCl, see github.com/gmrocher/Sal_Cond_Temp
      slopeT <-
        0.908 - 0.0304 * stream.temp + 6.63e-04 * stream.temp ^ 2 - 7.27e-06 *
        stream.temp ^ 3
      
      #convert conductivity to salt using the calibration curve
      if (is.na(input$slope) == FALSE)
        NaCl_s = EC_corr * input$slope
      else
        NaCl_s = EC_corr * slopeT
      
      #create a vector of the required length and with value of the time interval for the integration
      t = rep(input$interval, 1, length(NaCl_s))
      #"Integral" of the area of the peak, as the amount of salt * the time interval
      A = sum(NaCl_s * t)
      
      #Q in m3/s, mass of salt injected divided by the area of salt measured
      Q = round(input$nacl / A, digits = 5)
      
      peak_time <-
        data_selected$date.time[max(data_selected$cond)]
      
      p1 <- ggplot() +
        geom_point(data = data_selected,
                   aes(x, cond),
                   size = 2,
                   alpha = .5) +
        geom_ribbon(data = data_selected,
                    aes(x = x, ymin = EC_mean, ymax = cond),
                    fill = 'cornflowerblue') +
        theme(legend.position = "none") +
        labs(y = "Conductivity (microS/cm)") +
        theme_classic()
      
      text_out <-
        paste("Discharge at",
              input$siteDate ,
              "\n was",
              Q * 1000,
              " L \n the peak was at ",
              peak_time)
      
      p1 / grid::textGrob(text_out) +
        plot_layout(heights  = c(2, 1))
    }
    
    
    #structure to show the plot interactively
    output$plot <- renderPlot({
      plotInput()
    })
    
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(input$siteDate, '_Q_slug.png', sep = '')
      },
      content = function(file) {
        ggsave(
          file,
          plot = plotInput(),
          device = "png",
          width = 8,
          height = 6
        )
      }
    )
    
    
  }




shinyApp(ui, server)
