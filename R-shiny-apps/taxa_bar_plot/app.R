<<<<<<< HEAD
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
  titlePanel("Gene cluster"),
  
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
      numericInput(inputId = "top_taxa_number",
                   label = "Show top n taxa:",
                   value = 20),

      
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

      # Input: Checkbox if file has header ----
      checkboxInput("legend", "Legend", TRUE),
      sliderInput("legend_text_size",
                  "Legend text size:",
                  min = 1,
                  max = 10,
                  value = 5,
                  round=FALSE,
                  step = 0.1
      ),  #ticks=FALSE
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
    library(tidyverse)
    library(reshape)
    library(ggplot2)
    # input data
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',header=T,check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    # # filter the minimum aboundance cut off
    # cut_off=input$cut_off
    
    # for the top max taxa
    max=input$top_taxa_number
    
    nrow=nrow(data)
    norm = t(t(data[,-1])/colSums(data[,-1],na=T)) * 100 # normalization to total 100
    data<-data.frame(taxonomy=data[,1],norm)
    
    if(nrow<max){
      row=nrow 
    }else{
      row=max+1
      sum=data.frame(data,rowSums(data[,-1]))
      col=ncol(sum)
      sum=sum[order(sum[,col],decreasing = T),]
      sum<-sum[,-col]
      # low to 21 is defined to "others"
      others=colSums(sum[row:nrow,-1])
      # others=c("Others",others)
      data<-rbind(sum[1:max,-1],others)
      data<-data.frame(taxonomy=c(as.character(sum[1:max,1]),"Others"),data)
    }
    
    # save the table
    # table<-rbind(as.character(colnames(data)[-1]),data[,-1])
    # table<-data.frame(taxonomy=c("taxonomy",as.character(data$taxonomy)),table)
    # write.table(table, file = paste(file,"cat_taxa_abundance.ITS.level",tmp,"_first_",max,".txt",sep=""),quote = FALSE ,row.names = FALSE, col.names = F)
    
    # group <- c(rep("ALL_16S",5),rep("FL_16S",5),rep("V3V4_16S",5))
    # S85	S94	S95	S96	S103	S109	S112	S113	S15	S21	S31	S57	S77	S78
    # group <-c(rep("BC01",row),rep("BC02",row),rep("BC03",row),rep("BC04",row),rep("BC05",row))
    group <-c()
    for(sampleid in colnames(data[,-1])){
      # print(sampleid)
      group <- c(group,rep(sampleid,row))
    }
    rank <-data$taxonomy
    
    data <-melt.data.frame(data,id="taxonomy")
    data <- cbind(data,group)
    # set defalut rank
    
    data$taxonomy = factor(data$taxonomy, levels=rank) 
    
    cols<-c("#FF0000", "#0000FF", "#F27304", "#008000", "#91278D", "#FFFF00", "#7CECF4", "#F49AC2", "#5DA09E", "#6B440B", 
            "#808080", "#02F40E", "#F79679", "#7DA9D8", "#FCC688", "#80C99B", "#A287BF", "#FFF899", "#C0C0C0", "#ED008A", 
            "#00B6FF", "#C49C6B", "#808000", "#8C3FFF", "#BC828D", "#008080", "#800000", "#2B4200", "#A54700","#CD5C5C", "#8B8989")
    p <- ggplot(data, aes(x = variable,y=value,fill=taxonomy) )+ geom_bar(stat = "identity") +#geom_histogram(binwidth=10)
      # theme_bw()+
      theme(panel.grid.major = element_blank())+
      theme(axis.text.x = element_text(angle = as.integer(input$x_text_angle),hjust = .5, vjust = .5,size = as.integer(input$x_text_size))) +
      # scale_fill_discrete()+
      
      #facet_grid(. ~ group,scales = "free_x",space = "free_x")+
      # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
      # strip.text = element_text(face = "bold",size = rel(1)),strip.background = element_rect(fill = "lightblue",colour = "black",size = 0.5))+
      #  ggtitle(paste(opt$plot_name))+theme(plot.title = element_text(hjust = 0.5))+
      xlab("Samples")+ylab("Relative Abundance(%)")+
      scale_fill_manual(limits=as.character(data$taxonomy[1:row]),values = cols)
    #scale_fill_hue()+
    #scale_fill_brewer()+
    
    if(!input$legend){
      p<-p+theme(legend.position = "none")
    }else{
      p <- p +      # change the legend font
        theme(legend.text = element_text( size =as.integer(input$legend_text_size)),legend.key.size=unit(0.5,'cm'))+
        theme(legend.position="bottom")
    }
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
    library(tidyverse)
    library(reshape)
    library(ggplot2)
    # input data
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',header=T,check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    # # filter the minimum aboundance cut off
    # cut_off=input$cut_off
    
    # for the top max taxa
    max=input$top_taxa_number
    
    nrow=nrow(data)
    norm = t(t(data[,-1])/colSums(data[,-1],na=T)) * 100 # normalization to total 100
    data<-data.frame(taxonomy=data[,1],norm)
    
    if(nrow<max){
      row=nrow 
    }else{
      row=max+1
      sum=data.frame(data,rowSums(data[,-1]))
      col=ncol(sum)
      sum=sum[order(sum[,col],decreasing = T),]
      sum<-sum[,-col]
      # low to 21 is defined to "others"
      others=colSums(sum[row:nrow,-1])
      # others=c("Others",others)
      data<-rbind(sum[1:max,-1],others)
      data<-data.frame(taxonomy=c(as.character(sum[1:max,1]),"Others"),data)
    }
    
    # save the table
    table<-rbind(as.character(colnames(data)[-1]),data[,-1])
    table<-data.frame(taxonomy=c("taxonomy",as.character(data$taxonomy)),table)
    table[-1,]
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

=======
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
  titlePanel("Gene cluster"),
  
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
      numericInput(inputId = "top_taxa_number",
                   label = "Show top n taxa:",
                   value = 20),

      
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

      # Input: Checkbox if file has header ----
      checkboxInput("legend", "Legend", TRUE),
      sliderInput("legend_text_size",
                  "Legend text size:",
                  min = 1,
                  max = 10,
                  value = 5,
                  round=FALSE,
                  step = 0.1
      ),  #ticks=FALSE
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
    library(tidyverse)
    library(reshape)
    library(ggplot2)
    # input data
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',header=T,check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    # # filter the minimum aboundance cut off
    # cut_off=input$cut_off
    
    # for the top max taxa
    max=input$top_taxa_number
    
    nrow=nrow(data)
    norm = t(t(data[,-1])/colSums(data[,-1],na=T)) * 100 # normalization to total 100
    data<-data.frame(taxonomy=data[,1],norm)
    
    if(nrow<max){
      row=nrow 
    }else{
      row=max+1
      sum=data.frame(data,rowSums(data[,-1]))
      col=ncol(sum)
      sum=sum[order(sum[,col],decreasing = T),]
      sum<-sum[,-col]
      # low to 21 is defined to "others"
      others=colSums(sum[row:nrow,-1])
      # others=c("Others",others)
      data<-rbind(sum[1:max,-1],others)
      data<-data.frame(taxonomy=c(as.character(sum[1:max,1]),"Others"),data)
    }
    
    # save the table
    # table<-rbind(as.character(colnames(data)[-1]),data[,-1])
    # table<-data.frame(taxonomy=c("taxonomy",as.character(data$taxonomy)),table)
    # write.table(table, file = paste(file,"cat_taxa_abundance.ITS.level",tmp,"_first_",max,".txt",sep=""),quote = FALSE ,row.names = FALSE, col.names = F)
    
    # group <- c(rep("ALL_16S",5),rep("FL_16S",5),rep("V3V4_16S",5))
    # S85	S94	S95	S96	S103	S109	S112	S113	S15	S21	S31	S57	S77	S78
    # group <-c(rep("BC01",row),rep("BC02",row),rep("BC03",row),rep("BC04",row),rep("BC05",row))
    group <-c()
    for(sampleid in colnames(data[,-1])){
      # print(sampleid)
      group <- c(group,rep(sampleid,row))
    }
    rank <-data$taxonomy
    
    data <-melt.data.frame(data,id="taxonomy")
    data <- cbind(data,group)
    # set defalut rank
    
    data$taxonomy = factor(data$taxonomy, levels=rank) 
    
    cols<-c("#FF0000", "#0000FF", "#F27304", "#008000", "#91278D", "#FFFF00", "#7CECF4", "#F49AC2", "#5DA09E", "#6B440B", 
            "#808080", "#02F40E", "#F79679", "#7DA9D8", "#FCC688", "#80C99B", "#A287BF", "#FFF899", "#C0C0C0", "#ED008A", 
            "#00B6FF", "#C49C6B", "#808000", "#8C3FFF", "#BC828D", "#008080", "#800000", "#2B4200", "#A54700","#CD5C5C", "#8B8989")
    p <- ggplot(data, aes(x = variable,y=value,fill=taxonomy) )+ geom_bar(stat = "identity") +#geom_histogram(binwidth=10)
      # theme_bw()+
      theme(panel.grid.major = element_blank())+
      theme(axis.text.x = element_text(angle = as.integer(input$x_text_angle),hjust = .5, vjust = .5,size = as.integer(input$x_text_size))) +
      # scale_fill_discrete()+
      
      #facet_grid(. ~ group,scales = "free_x",space = "free_x")+
      # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
      # strip.text = element_text(face = "bold",size = rel(1)),strip.background = element_rect(fill = "lightblue",colour = "black",size = 0.5))+
      #  ggtitle(paste(opt$plot_name))+theme(plot.title = element_text(hjust = 0.5))+
      xlab("Samples")+ylab("Relative Abundance(%)")+
      scale_fill_manual(limits=as.character(data$taxonomy[1:row]),values = cols)
    #scale_fill_hue()+
    #scale_fill_brewer()+
    
    if(!input$legend){
      p<-p+theme(legend.position = "none")
    }else{
      p <- p +      # change the legend font
        theme(legend.text = element_text( size =as.integer(input$legend_text_size)),legend.key.size=unit(0.5,'cm'))+
        theme(legend.position="bottom")
    }
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
    library(tidyverse)
    library(reshape)
    library(ggplot2)
    # input data
    req(input$file1)
    data <- read.table(input$file1$datapath,sep='\t',header=T,check.names=F) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    # # filter the minimum aboundance cut off
    # cut_off=input$cut_off
    
    # for the top max taxa
    max=input$top_taxa_number
    
    nrow=nrow(data)
    norm = t(t(data[,-1])/colSums(data[,-1],na=T)) * 100 # normalization to total 100
    data<-data.frame(taxonomy=data[,1],norm)
    
    if(nrow<max){
      row=nrow 
    }else{
      row=max+1
      sum=data.frame(data,rowSums(data[,-1]))
      col=ncol(sum)
      sum=sum[order(sum[,col],decreasing = T),]
      sum<-sum[,-col]
      # low to 21 is defined to "others"
      others=colSums(sum[row:nrow,-1])
      # others=c("Others",others)
      data<-rbind(sum[1:max,-1],others)
      data<-data.frame(taxonomy=c(as.character(sum[1:max,1]),"Others"),data)
    }
    
    # save the table
    table<-rbind(as.character(colnames(data)[-1]),data[,-1])
    table<-data.frame(taxonomy=c("taxonomy",as.character(data$taxonomy)),table)
    table[-1,]
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

>>>>>>> 62eb657c8615a117d259c9866a98897e77d60bd7
