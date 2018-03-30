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
      sliderInput("high",
                  "Arrow height range:",
                  min = 0,
                  max = 5,
                  value = 1,
                  round=FALSE,
                  step = 0.1
                   ),  #ticks=FALSE
      sliderInput("size",
                  "Gene tag size",
                  min = 1,
                  max = 5,
                  value = 3,
                  step = 0.1),
      # Horizontal line ----
      tags$hr(),
      # Input: Select a file ----
      fileInput("file1", "Choose a GFF File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".gff3")
                ),
      
      # Horizontal line ----
      tags$hr(),
      # Input: Checkbox if file has header ----
      checkboxInput("legend", "Legend", TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    library(ggplot2)
    # input data
    req(input$file1)
    data <- read.table(input$file1$datapath) #"F:/run-tmp/R-shiny-apps/test-myshu/test.gff3"
    print(input$file1$datapath)
    
    ratio <- data[dim(data)[1],5]-data[1,4]
    
    
    x_pos=c()
    y_pos=c()
    ids=c()
    anno=c()
    anno_size= input$size
    #閹鏆辨惔锕侇啎娑??10
    total_len = 10
    #缁狀厼銇旈惃鍕彯鎼达箒顔曟稉?1
    arrow_high = input$high  # 闂勬劕鍩?0-5
    
    for(i in  1:dim(data)[1]){
      #print(data[i,])
      if(i == 1){
        start_tag=as.numeric(data[i,4])
      }
      start<-(as.numeric(data[i,4])-start_tag)*total_len/ratio  #閹鏆辨惔锕侇啎娑??1
      end<-(as.numeric(data[i,5])-start_tag)*total_len/ratio  #閹鏆辨惔锕侇啎娑??10
      strand<-as.character(data[i,7])
      # text annotate
      # anno : x=start+(end-start)/3, y=1.5 label =as.character(data[i,9])
      anno=rbind(anno,c(start+(end-start)/3,1.5,as.character(data[i,9])))
      
      if(strand == "+"){
        #print(strand)
        x_pos = c(x_pos,c(end,end-arrow_high/2,end-arrow_high/2,start,start,end-arrow_high/2,end-arrow_high/2))
        y_pos = c(y_pos,c(0, arrow_high, arrow_high/2, arrow_high/2, -arrow_high/2, -arrow_high/2, -arrow_high))   #缁狀厼銇旈惃鍕彯鎼达箒顔曟稉?1
        ids=c(ids,as.character(data[i,9]))
        
      }
      else if(strand == "-"){
        x_pos = c(x_pos,c(start,start+arrow_high/2,start+arrow_high/2,end,end,start+arrow_high/2,start+arrow_high/2))
        y_pos = c(y_pos,c(0, arrow_high, arrow_high/2, arrow_high/2, -arrow_high/2, -arrow_high/2, -arrow_high))   #缁狀厼銇旈惃鍕彯鎼达箒顔曟稉?1
        ids=c(ids,as.character(data[i,9]))    
      }
    }
    
    positions <- data.frame(
      id = rep(ids, each = 7), #濮??7娑擃亝鏆熼幑顔剧帛閸掓湹绔存稉顏勭殱闂傤厾娈戦崶?,閸楀厖绔存稉顏勭唨閸ョ姷娈戠粻顓炪仈
      x=x_pos,
      y=y_pos
      
    )
    
    values <- data.frame(
      id = ids,
      value = sample(colors(),dim(data)[1])
    )
    
    datapoly <- merge(values, positions, by = c("id"))
    
    p <- ggplot(datapoly, aes(x = x, y = y) )+
      ylim(-10,10)+
      geom_hline(yintercept = 0)+
      geom_polygon(aes(group = id,fill=value))+ #colour=value,
      scale_fill_discrete(labels=c(ids),name="Genes")+
      theme_void()
    if(!input$legend){
      p<-p+theme(legend.position = "none")
    }
    for(j in 1:dim(anno)[1]){
      p <-p + annotate("text",x=as.numeric(anno[j,1]),y=as.numeric(anno[j,2]),label=anno[j,3],size=anno_size)
    }
    p
    # ggsave("F:/run-tmp/R-shiny-apps/test-myshu/test.pdf")
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

