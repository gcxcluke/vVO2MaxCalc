
library(shiny)

equiv=data.frame(Distance = c("Aerobic Threshold", "", "", "","Lactate Threshold",
                              "Lactate Threshold","","10K","8k", "5k", "vVO2Max/3k", "",
                              "1500m", "","800m",""), 
                 Percentage=c("65%", "70%", "75%","80%", "85%", "88%", "90%","92%","95%",
                              "97%","100%","102%","110%","112%","120%", "136%"),
                 Pace = rep(NA,16),
                 Equivalent = rep(NA,16)
)

training=data.frame(Distance = c("Aerobic Threshold", "", "", "","Lactate Threshold",
                                     "Lactate Threshold","","10K","8k", "5k", "vVO2Max/3k", "",
                                     "1500m", "","800m",""), 
                    Percentage=c("65%", "70%", "75%","80%", "85%", "88%", "90%","92%","95%",
                                 "97%","100%","102%","110%","112%","120%", "136%"),
                    a200m = rep(NA,16),
                    a300m = rep(NA,16),
                    a400m = rep(NA,16),
                    a600m = rep(NA,16),
                    a800m = rep(NA,16),
                    a1000m = rep(NA,16),
                    a1200m = rep(NA,16),
                    a1600m = rep(NA,16)
        
                 
)


min2sec=function(min,sec=0){
    return((min*60)+sec)
}

sec2min=function(sec){
    min=floor(sec/60)
    sec=sec %% 60
    return(sprintf("%02d:%02d", as.integer(min), as.integer(sec)))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("vVO2Max Calculator"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "dist",
                        label = "Race or Trial Distance:",
                        choices = c("3k", "2 mile", "5k","8k", "10k","1500m")),
            
            
            numericInput(inputId = "min",
                         label = "Minutes:",
                         min = 0,
                         max = 100,
                         value = 0),
            
            numericInput(inputId = "sec",
                         label = "Seconds:",
                         min=0,
                         max=59,
                         value = 0),
            
            width=3
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          
            span(textOutput("vvo2max"), style="color:red"),
   
            
            tabsetPanel(type = "tabs",
                        tabPanel("Equivalent", tableOutput("equivalent")),
                        tabPanel("Training", tableOutput("training")),
                        tabPanel("Team File Upload",
                                 textOutput("select"),
                                 fileInput(inputId = "team", "Choose File", multiple = FALSE, accept = NULL,
                                                     width = NULL, buttonLabel = "Browse...",
                                                     placeholder = "No file selected"),
                                 downloadButton("report", "Generate report"),
                                 tableOutput("teamtimes"))
                        
                      
                        
                        
            ),
            span(textOutput("credit"),style="color:orange")
        )
        
    )
)


server <- function(input, output) {
    
    output$equivalent <- renderTable({
        
        factor2=switch(input$dist,
                      "1500m" = 0.932057,
                      "3k" = 1.86411,
                      "2 mile" = 2,
                      "5k" = 3.10686,
                      "8k" = 4.97097,
                      "10k" = 6.21371)
        
        factor1=switch(input$dist,
                       "1500m" = 1.1,
                       "3k" = 1,
                       "2 mile" = 1,
                       "5k" = .97,
                       "8k" = .95,
                       "10k" = .92)
        
        seconds=min2sec(input$min, input$sec)
        vvo2=(seconds/factor2)*factor1
        
        temp1=vvo2 / c(.65, .70, .75, .80, .85, .88, .90, .92, .95, .97, 1, 1.02, 1.1, 1.12, 1.2, 1.36)
        temp2=c(0,0,0,0,0,0,0,6.21371,4.97097,3.10686,1.86411,1.86411,0.932057,0.932057,0.497097,0.497097)*temp1
        
        paces=sec2min(temp1)
        equivs=sec2min(temp2)
        
        equiv[,3]=paces
        equiv[8:16,4]=equivs[8:16]
        
        
        head(equiv,n=16)
        
        
    })
    
    output$training <- renderTable({
        
        factor2=switch(input$dist,
                       "1500m" = 0.932057,
                       "3k" = 1.86411,
                       "2 mile" = 2,
                       "5k" = 3.10686,
                       "8k" = 4.97097,
                       "10k" = 6.21371)
        
        factor1=switch(input$dist,
                       "1500m" = 1.1,
                       "3k" = 1,
                       "2 mile" = 1,
                       "5k" = .97,
                       "8k" = .95,
                       "10k" = .92)
        
        seconds=min2sec(input$min, input$sec)
        vvo2=(seconds/factor2)*factor1
        
        temp1=vvo2 / c(.65, .70, .75, .80, .85, .88, .90, .92, .95, .97, 1, 1.02, 1.1, 1.12, 1.2, 1.36)
        
        training$a200m=sec2min(temp1*0.124274)
        training$a300m=sec2min(temp1*0.186411)
        training$a400m=sec2min(temp1*0.248548)
        training$a600m=sec2min(temp1*0.372823)
        training$a800m=sec2min(temp1*0.497097)
        training$a1000m=sec2min(temp1*0.621371)
        training$a1200m=sec2min(temp1*0.7456454)
        training$a1600m=sec2min(temp1*0.9941939)
        
        names(training)=c("Pace","Percentages", "200m", "300m", "400m", "600m", "800m", "1000m", "1200m", "1600m")
        
        head(training,n=16)
        
    })
    
    output$vvo2max <- renderText({ 
      
      factor2=switch(input$dist,
                     "1500m" = 0.932057,
                     "3k" = 1.86411,
                     "2 mile" = 2,
                     "5k" = 3.10686,
                     "8k" = 4.97097,
                     "10k" = 6.21371)
      
      factor1=switch(input$dist,
                     "1500m" = 1.1,
                     "3k" = 1,
                     "2 mile" = 1,
                     "5k" = .97,
                     "8k" = .95,
                     "10k" = .92)
      
      seconds=min2sec(input$min, input$sec)
      vvo2=(seconds/factor2)*factor1
      
      paste("Estimated vVO2Max: ", sec2min(vvo2), ".", sep="")
    })
    
    output$teamtimes <- renderTable({
      
      inFile <- input$team
      
      if (is.null(inFile))
        return(NULL)
      
      temp=read.csv(inFile$datapath, header = TRUE)

      
      head(temp)
      
    })
    
    
    output$credit <- renderText({ 
    
      paste("Created by: Luke Garnett, Georgetown College. Lucas_Garnett@GeorgetownCollege.edu")
    })
    
    output$select <- renderText({ 
      
      paste("Please include four columns, Name, Minutes, Seconds, Distance in a .csv file.")
    })
        
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "paces.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "paces.Rmd")
        file.copy("paces.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        inFile <- input$team
        
        if (is.null(inFile))
          return(NULL)
        
        
        temp=read.csv(inFile$datapath, header = TRUE)
        params <- list(file1=temp)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)