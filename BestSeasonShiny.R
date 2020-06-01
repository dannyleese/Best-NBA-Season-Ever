

data<-read.csv("https://raw.githubusercontent.com/dannyleese/Best-NBA-Season-Ever/master/sortablerankings.csv")
percentile<-read.csv("https://raw.githubusercontent.com/dannyleese/Best-NBA-Season-Ever/master/percentiles.csv")

ui <- fluidPage(
  titlePanel("NBA Best Season Ever"),
  
  sidebarLayout(
    sidebarPanel(h1("Overview"),
                 
                 p("To determine the Best NBA Season Ever, I created a stat named Score. Score equals the Z-Score summation of PER, WIN Shares/48, BPM, and VORP. As seen from the histogram, scores range from about -10 to 20, and the average score is -1."),
                 p("For a complete explanation on how I determined the best NBA season ever, check out ",
                   a("my article", 
                     href = "https://medium.com/the-sports-scientist/best-nba-seasons-ever-4d2a32ca3d27?source=friends_link&sk=6a39efb0ee1d260f5138cba4431ceae2")),
                 p("Here is a histogram of  all player's Score"),
                 plotOutput(outputId = "distPlot"),
                 # Input: Slider for the number of bins ----
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 25)
                 
    ),
    mainPanel(
  
      tabsetPanel(type = "tabs",
                  tabPanel("Table",
                           
                           DT::dataTableOutput("mytable1")),
                  tabPanel("Player Stats",
                           
                           fluidRow(
                             
                             column(3,
                                    selectInput("player", "Choose A Player:",
                                                         list(`NBA Players` = (sort( percentile$Player, decreasing = FALSE))),selected = 1)),
                             column(3,
                                    uiOutput("secondSelection")),
                             
                             uiOutput("img1")),
                           
                           fluidRow(
                            
                             column(10,
                                    h3(textOutput("selected_player")))),
                           
                           fluidRow(
                             plotOutput("plot2"))
                  )
      )
    )
  )
)
server <- function(input, output) {
  
  
  diamonds2 = data
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds2[, c(2:12), drop = FALSE])
  })
  
  output$distPlot <- renderPlot({
    
    x    <- diamonds2$score

    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Players Total Z-Score",
         main = "Histogram of Players Score")
  })
  
  output$secondSelection <- renderUI({
    selectInput("year", "year:",
                      choices=as.numeric(percentile[percentile$Player==input$player,"year"]))
  })
  
  
  output$selected_player <- renderText({ 
    paste(input$player, "percentile ranks compared to other",percentile[percentile$Player==input$player & percentile$year == input$year,"Posgroup"], "in",input$year )
  })

  
  output$img1 <- renderUI({
    img(src =percentile[percentile$Player==input$player & percentile$year == input$year,"url"])
  })
  
  
  
  output$plot2<-renderPlot({
    returnplayerseasons  <- function(playername,yearspecial) {
      return(season <- percentile[percentile$Player==playername & percentile$year == yearspecial,])
    }
    
    oneplayer<-returnplayerseasons(input$player, input$year)
    oneplayer$nameyear<-paste(oneplayer$Player, oneplayer$year)
    
    playerpercentile<- percentile[which(percentile$nameyear==oneplayer$nameyear) ,c(39:48)]
    statsnames<- c("TS%","3PAr","FTr","ORB%","DRB%","TRB%","AST%","STL%","BLK%","TO%")
    barchartstats<-data.frame()
    barchartstats<- rbind(statsnames,playerpercentile)
    barchartstats = as.data.frame(t(barchartstats))
    names(barchartstats)<-c("headers","numbers")
    barchartstats$numbers<-as.numeric(barchartstats$numbers)
    barchartstats$numbers<- barchartstats$numbers*100
    barchartstats$numbers <-round(barchartstats$numbers, digits = 0)
    
    ggplot(barchartstats, aes(x=headers, y=numbers))+
      geom_bar(stat = "identity",aes(fill = numbers))+
      scale_fill_gradient(
        low = "red",
        high = "green3",
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill"
      ) +
      coord_flip()+
      geom_text(aes(label=numbers, hjust = 1.5)) +
      ggtitle(paste(oneplayer$Player, oneplayer$year, sep = " ") ) +
      xlab("Advanced Statistics") +
      ylab("Percentile")+
      theme(line = element_blank(),
            axis.title =element_text(size=17) ,
            axis.text =element_text(size=17),
            legend.text = element_blank() ,
            legend.position = "blank" ,
            legend.title = element_blank() ,
            plot.title = element_text(size = 18, lineheight = 0.9, face = "bold"))
    
    })
}

shinyApp(ui, server)
