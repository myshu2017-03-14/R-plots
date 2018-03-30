#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
      #------------------
      #  sample text format
      #------------------
      sliderInput("sample_text_size",
                  "Sample text size:",
                  min = 1,
                  max = 10,
                  value = 2,
                  round=FALSE,
                  step = 0.1
      ),  #ticks=FALSE
      #------------------
      #  taxa number size
      #------------------     

      sliderInput("taxa_number_size",
                  "Taxa number size:",
                  min = 1,
                  max = 10,
                  value = 1.6,
                  round=FALSE,
                  step = 0.1
      ),  #ticks=FALSE
      sliderInput("core_taxa_number_size",
                  "Core taxa number size:",
                  min = 1,
                  max = 10,
                  value = 1.5,
                  round=FALSE,
                  step = 0.1
      ),  #ticks=FALSE
      #------------------
      #  flower format (<start_angle> <minor_axis_length> <major_axis_length>)
      #------------------     
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "start_angle",
                   label = "Flower start Angle:",
                   value = 90),
      numericInput(inputId = "minor_axis_length",
                   label = "Petal minor axis length:",
                   value = 0.5),
      numericInput(inputId = "major_axis_length",
                   label = "Petal major axis length:",
                   value = 2),
      numericInput(inputId = "core_circle_size",
                   label = "Core circle size:",
                   value = 0.8),
      
      # Horizontal line ----
      tags$hr(),
      
      
      # Horizontal line ----
      tags$hr(),
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
    #--------------plot function-----
    library(plotrix)
    # plot function
    flower_plot <- function(sample, value, core, start, a, b, 
                            #ellipse_col = rainbow(14,alpha=0.5), #,start = .7,end = .1
                            circle_col = rgb(0, 162, 214, 200, max = 255),
                            circle_text_cex = input$sample_text_size#1.5
    ) {
      par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
      plot(c(0,10),c(0,10),type="n")
      n   <- length(sample)
      deg <- 360 / n
      res <- lapply(1:n, function(t){
        draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                     y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                     col = rainbow(n,alpha = 0.5)[t],#ellipse_col[t-1],
                     border = rainbow(n,alpha = 0.5)[t],#ellipse_col[t-1],#rgb(0, 0, 0, max = 255),
                     a = a, b = b, angle = deg * (t - 1))
        text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
             y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
             value[t],cex = input$taxa_number_size
        )
        
        if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
          text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
               y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
               sample[t],
               srt = deg * (t - 1) - start,
               adj = 1,
               cex = circle_text_cex
          )
          
        } else {
          text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
               y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
               sample[t],
               srt = deg * (t - 1) + start,
               adj = 0,
               cex = circle_text_cex
          )
        }			
      })
      #draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = circle_col)
      draw.circle(x = 5, y = 5, r = as.numeric(input$core_circle_size), col = circle_col, border = circle_col)
      text(x=5,y=5,paste("Core\n",core),cex = input$core_taxa_number_size)
    }
    
    # ---------plot -----
    # input data
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',header=T,check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    
    venn_list<-list()
    for(i in 1:dim(data)[2]){ # 1:84 sample ID
      # point the colname (sample IDs)
      colname <- colnames(data)[i]
      # first col is the taxa
      if(i==1){
        taxa <- data[,1]
      }else{
        for(j in 1:dim(data)[1]){ # 1:580 OTUS
          if(data[j,i]>0){
            # add taxa to every groups 
            venn_list[[as.character(colname)]]<-append(venn_list[[as.character(colname)]],as.character(taxa[j]))
            # venn_list[[as.character()]]<-append(venn_list[[]],as.character(otuid[j]))
          }
        }
      }
    }
    core_taxa <-Reduce(intersect,  venn_list)
    core<-length(core_taxa)
    # intersect(venn_list$S85,venn_list$S94,venn_list$S95,venn_list$S96,venn_list$S103,venn_list$S109,venn_list$S112,venn_list$S113,venn_list$S15,venn_list$S21,venn_list$S31,venn_list$S57,venn_list$S77,venn_list$S78)
    uniq_taxa<-c()
    for(s in 2:dim(data)[2]){
      uniq_taxa<-c(uniq_taxa,length(setdiff(venn_list[[as.character(colnames(data)[s])]],core_taxa)))
    }
    # <sample_ID> <uniq_value><core value> <start_angle> <minor_axis_length> <major_axis_length>
    flower_plot(colnames(data[,-1]),
                #c("S85","S94","S95","S96","S103","S109","S112","S113","S15","S21","S31","S57","S77","S78"),
                uniq_taxa, core,as.numeric(input$start_angle), as.numeric(input$minor_axis_length), as.numeric(input$major_axis_length))
    
    

  })
  #---------------------------------------for plot----------------------------------------------------
  
  output$distPlot <- renderPlot({
    p<- datasetInput()
    p
    # ggsave(paste(file,"cat_taxa_abundance.ITS.level",tmp,".pdf",sep=""),width=8,height=8)
  },width = 500,height = 500)
  
  #---------------------------------------for table----------------------------------------------------
  # for the table is from the code before, so I repeat the code. Maybe it can solve by another way
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    
    library(plotrix)
    # plot function
    flower_plot <- function(sample, value, core, start, a, b, 
                            #ellipse_col = rainbow(14,alpha=0.5), #,start = .7,end = .1
                            circle_col = rgb(0, 162, 214, 200, max = 255),
                            circle_text_cex = 1.5#1.5
    ) {
      par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
      plot(c(0,10),c(0,10),type="n")
      n   <- length(sample)
      deg <- 360 / n
      res <- lapply(1:n, function(t){
        draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                     y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                     col = rainbow(n,alpha = 0.5)[t],#ellipse_col[t-1],
                     border = rainbow(n,alpha = 0.5)[t],#ellipse_col[t-1],#rgb(0, 0, 0, max = 255),
                     a = a, b = b, angle = deg * (t - 1))
        text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
             y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
             value[t]
        )
        
        if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
          text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
               y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
               sample[t],
               srt = deg * (t - 1) - start,
               adj = 1,
               cex = circle_text_cex
          )
          
        } else {
          text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
               y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
               sample[t],
               srt = deg * (t - 1) + start,
               adj = 0,
               cex = circle_text_cex
          )
        }			
      })
      #draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = circle_col)
      draw.circle(x = 5, y = 5, r = 0.8, col = circle_col, border = circle_col)
      text(x=5,y=5,paste("Core",core))
    }
    
    # ---------plot -----
    # input data
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',header=T,check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    
    venn_list<-list()
    for(i in 1:dim(data)[2]){ # 1:84 sample ID
      # point the colname (sample IDs)
      colname <- colnames(data)[i]
      # first col is the taxa
      if(i==1){
        taxa <- data[,1]
      }else{
        for(j in 1:dim(data)[1]){ # 1:580 OTUS
          if(data[j,i]>0){
            # add taxa to every groups 
            venn_list[[as.character(colname)]]<-append(venn_list[[as.character(colname)]],as.character(taxa[j]))
            # venn_list[[as.character()]]<-append(venn_list[[]],as.character(otuid[j]))
          }
        }
      }
    }
    core_taxa <-Reduce(intersect,  venn_list)
    core<-length(core_taxa)
    # intersect(venn_list$S85,venn_list$S94,venn_list$S95,venn_list$S96,venn_list$S103,venn_list$S109,venn_list$S112,venn_list$S113,venn_list$S15,venn_list$S21,venn_list$S31,venn_list$S57,venn_list$S77,venn_list$S78)
    uniq_taxa<-c()
    for(s in 2:dim(data)[2]){
      uniq_taxa<-c(uniq_taxa,length(setdiff(venn_list[[as.character(colnames(data)[s])]],core_taxa)))
    }
    
    table<-data.frame(colnames(data[,-1]),uniq_taxa)
    colnames(table) <-c("SampleID","Uniq taxa number")
    table
  })
  #---------------------------------------for download plot----------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('plot', '.png', sep='')
    },
    content=function(file){
      png(file)
      # datasetInput()
      # for the plot include the function, so I repeat the code. Maybe it can solve by another way
      ##########################################
      #--------------plot function-----
      library(plotrix)
      # plot function
      flower_plot <- function(sample, value, core, start, a, b, 
                              #ellipse_col = rainbow(14,alpha=0.5), #,start = .7,end = .1
                              circle_col = rgb(0, 162, 214, 200, max = 255),
                              circle_text_cex = 1.5#1.5
      ) {
        par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
        plot(c(0,10),c(0,10),type="n")
        n   <- length(sample)
        deg <- 360 / n
        res <- lapply(1:n, function(t){
          draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                       y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                       col = rainbow(n,alpha = 0.5)[t],#ellipse_col[t-1],
                       border = rainbow(n,alpha = 0.5)[t],#ellipse_col[t-1],#rgb(0, 0, 0, max = 255),
                       a = a, b = b, angle = deg * (t - 1))
          text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
               y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
               value[t]
          )
          
          if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
            text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
                 y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
                 sample[t],
                 srt = deg * (t - 1) - start,
                 adj = 1,
                 cex = circle_text_cex
            )
            
          } else {
            text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
                 y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
                 sample[t],
                 srt = deg * (t - 1) + start,
                 adj = 0,
                 cex = circle_text_cex
            )
          }			
        })
        #draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = circle_col)
        draw.circle(x = 5, y = 5, r = 0.8, col = circle_col, border = circle_col)
        text(x=5,y=5,paste("Core",core))
      }
      
      # ---------plot -----
      # input data
      req(input$file1)
      data <- read.table(input$file1$datapath,sep='\t',header=T,check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
      print(input$file1$datapath)
      
      
      venn_list<-list()
      for(i in 1:dim(data)[2]){ # 1:84 sample ID
        # point the colname (sample IDs)
        colname <- colnames(data)[i]
        # first col is the taxa
        if(i==1){
          taxa <- data[,1]
        }else{
          for(j in 1:dim(data)[1]){ # 1:580 OTUS
            if(data[j,i]>0){
              # add taxa to every groups 
              venn_list[[as.character(colname)]]<-append(venn_list[[as.character(colname)]],as.character(taxa[j]))
              # venn_list[[as.character()]]<-append(venn_list[[]],as.character(otuid[j]))
            }
          }
        }
      }
      core_taxa <-Reduce(intersect,  venn_list)
      core<-length(core_taxa)
      # intersect(venn_list$S85,venn_list$S94,venn_list$S95,venn_list$S96,venn_list$S103,venn_list$S109,venn_list$S112,venn_list$S113,venn_list$S15,venn_list$S21,venn_list$S31,venn_list$S57,venn_list$S77,venn_list$S78)
      uniq_taxa<-c()
      for(s in 2:dim(data)[2]){
        uniq_taxa<-c(uniq_taxa,length(setdiff(venn_list[[as.character(colnames(data)[s])]],core_taxa)))
      }
      # <sample_ID> <uniq_value><core value> <start_angle> <minor_axis_length> <major_axis_length>
      flower_plot(colnames(data[,-1]),
                  #c("S85","S94","S95","S96","S103","S109","S112","S113","S15","S21","S31","S57","S77","S78"),
                  uniq_taxa, core,90, 0.5, 2)
      
      ##########################################
      dev.off()
    },
    contentType='image/png')
}

# Run the application 
shinyApp(ui = ui, server = server)

