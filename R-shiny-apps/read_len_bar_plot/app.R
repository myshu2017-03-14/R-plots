#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# set max load file size 30M
options(shiny.maxRequestSize=100*1024^2)
library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Bar Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose a csv File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".out")
      ),
      # Horizontal line ----
      tags$hr(),
      
      # sliderInput("top_taxa_number",
      #             "Show top n taxa",
      #             min = 10,
      #             max = 100,
      #             value = 20,
      #             step = 1),
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "max_length",
                   label = "Max reads length:",
                   value = 2000),

      numericInput(inputId = "min_length",
                   label = "Min reads length:",
                   value = 100),
      numericInput(inputId = "step_size",
                   label = "Step size:",
                   value = 100),      
      
      # Horizontal line ----
      tags$hr(),
      #------------------
      #  x text format
      #------------------
      sliderInput("x_text_size",
                  "X axis text size:",
                  min = 1,
                  max = 10,
                  value = 5,
                  round=FALSE,
                  step = 0.1
      ),  #ticks=FALSE
      radioButtons("x_text_angle", "X text angle:",
                   choices = c("0" = 0,
                               "30" = 30,
                               "90" = 90),
                   selected = "0"),
      # Horizontal line ----
      tags$hr(),
      #------------------
      #  legend format
      #------------------     
      # Button
      downloadButton("downloadData", "Download plot")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  #---------------------------------------for plot dataset----------------------------------------------------
  datasetInput <- reactive({
    library(ggplot2)
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    min_raw <- input$min_length
    max_raw <- input$max_length
    step <- input$step_size
    
    m<-seq(0,min_raw,by=min_raw)   
    min <-data.frame(table(cut(data$V2,m,dig.lab = 4)) )
    min$Var1 <- paste("<=",min_raw,sep = "")
    
    m<-seq(min_raw,max_raw,by=step) 
    med<-data.frame(table(cut(data$V2,m,dig.lab = 4)) )  
    
    max <- max(data$V2)
    len <- max-max_raw
    m<-seq(max_raw,max,by=len)   
    max <-data.frame(table(cut(data$V2,m,dig.lab = 4)) )
    max$Var1<- paste(">",max_raw,sep = "")
    
    t <- rbind(min,med,max)
    
    p <- ggplot(t,aes(x = Var1,y = Freq )) + geom_bar(stat = "identity",fill="skyblue") +
      scale_x_discrete(limits=t$Var1)+
      theme(axis.text.x = element_text(angle = as.integer(input$x_text_angle),hjust = .5, vjust = .5,size = as.integer(input$x_text_size))) +
      xlab("Read Length")
    p
  })
  #---------------------------------------for plot----------------------------------------------------
  
  output$distPlot <- renderPlot({
    p<- datasetInput()
    print(p)
    # ggsave(paste(file,"cat_taxa_abundance.ITS.level",tmp,".pdf",sep=""),width=8,height=8)
  })
  
  #---------------------------------------for table----------------------------------------------------
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    library(ggplot2)
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    min_raw <- input$min_length
    max_raw <- input$max_length
    step <- input$step_size
    
    m<-seq(0,min_raw,by=min_raw)   
    min <-data.frame(table(cut(data$V2,m,dig.lab = 4)) )
    min$Var1 <- paste("<=",min_raw,sep = "")
    
    m<-seq(min_raw,max_raw,by=step) 
    med<-data.frame(table(cut(data$V2,m,dig.lab = 4)) )  
    
    max <- max(data$V2)
    len <- max-max_raw
    m<-seq(max_raw,max,by=len)   
    max <-data.frame(table(cut(data$V2,m,dig.lab = 4)) )
    max$Var1<- paste(">",max_raw,sep = "")
    
    t <- rbind(min,med,max)
    # save the table
    t
  })
  #---------------------------------------for download plot----------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('plot', '.pdf', sep='')
    },
    content=function(file){
      pdf(file)
      print(datasetInput())
      dev.off()
    },
    contentType='image/pdf')
}

# Run the application 
shinyApp(ui = ui, server = server)

