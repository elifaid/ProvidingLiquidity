library(shiny) #dashboard
library(tidyverse) #data manipulation
library(plotly) #interactive charts
library(scales) #percentage scales for charts
library(geckor)
library(DT)


ui <- fluidPage(
  
  # Application title
  titlePanel("Simulation of Providing Liquidity"),
  # Sidebar with a inputs 
  sidebarLayout(
    sidebarPanel(
      
      dateInput("start",   
                "Start Date", 
                value = "2021-06-25"),     # Default values for sim, works faster when you start from scatch versus just updating the info
      dateInput("end",   
                "End Date",
                value = "2022-12-31"),
      textInput("ticker2","First Asset",value="ethereum"),
      textInput("ticker1","Second Asset",value="dopex-rebate-token"),
      numericInput("yield_1","First Asset daily Yield (%)",value=0),
      numericInput("yield_2","Second Asset Daily Yield (%)",value=0),
      numericInput("yield_3","Liquidity Daily Yield (%)",value=0),
      checkboxInput("LOG","Logarithmic Y axis?",value=TRUE),
      width = 2
    ),
    mainPanel(
      plotlyOutput("Mainchart",height="720px",width="1400"),
      DTOutput("IL")
      
    )
  ))

server <- function(input, output) {
  
  data_load<-reactive({
    coin_history_range(
      coin_id = c(input$ticker1,input$ticker2),
      vs_currency = "usd",
      from = as.POSIXct(as.Date(input$start)-2),
      to = as.POSIXct(input$end)
    )
  })
  FINAL<-reactive({
    name1=str_to_title(gsub("-"," ",input$ticker1))
    name2=str_to_title(gsub("-"," ",input$ticker2))
    
    y<-data_load()%>%select(timestamp,coin_id,price)%>%
      pivot_wider(names_from = coin_id, values_from = price)%>%
      rename(A=3,B=2)%>%
      filter(!is.na(A))
      
    workLP<-y
    workLP$AmountA[1]<-workLP$B[1]/workLP$A[1]
    workLP$AmountB[1]<-1
    workLP$LP[1]<-workLP$AmountA[1]*workLP$A[1]+workLP$AmountB[1]*workLP$B[1]
    
    for (i in 2:nrow(workLP)) {
      workLP$AmountA[i]<-
        workLP$AmountA[i-1]*workLP$AmountB[i-1]/
        (sqrt(workLP$AmountA[i-1]*workLP$AmountB[i-1]*workLP$B[i]/workLP$A[i]))+
        (input$yield_3*workLP$AmountA[i-1]/200)
      
      workLP$AmountB[i]<-
        (sqrt(workLP$AmountA[i-1]*workLP$AmountB[i-1]*workLP$B[i]/workLP$A[i]))+
        (input$yield_3*workLP$AmountB[i-1]/200)
      
      workLP$LP[i]<-workLP$AmountA[i]*workLP$B[i]+workLP$AmountB[i]*workLP$B[i]
      
    }
    
    
    workLP<-workLP%>%mutate(ONLY_A=(LP[1]/A[1])*A*((1+input$yield_1/100)^(row_number()-1)),
                            ONLY_B=(LP[1]/B[1])*B*((1+input$yield_2/100)^(row_number()-1)))
    workLP<-workLP%>%slice(3:n()) %>%
      mutate(ONLY_A=ONLY_A/ONLY_A[1],
                            ONLY_B=ONLY_B/ONLY_B[1],
                            LP=LP/LP[1])
    })

    output$Mainchart<- renderPlotly({
      name1=str_to_title(gsub("-"," ",input$ticker1))
      name2=str_to_title(gsub("-"," ",input$ticker2))
    g<-ggplot(data=FINAL()%>%
                      select(timestamp,LP,ONLY_A,ONLY_B)%>%
                      rename("Providing Liquidity"=LP,!!name2:=ONLY_A,!!name1:=ONLY_B)%>% #:= to assign fixed values
                      pivot_longer(!timestamp,names_to = "token", values_to = "value"),
                    aes(y=value,x=timestamp,color=token))+
               geom_line(size=1.1)+
               scale_color_manual(name="Asset",values = c('steelblue',"grey", "red")) +
               xlab('Date')+
               ylab("Total Return")+
               ggtitle(paste("Staking",name1,"vs Providing Liquidity vs",name2))+
               theme_bw()
    
        g<-g+if(input$LOG==TRUE){scale_y_log10(breaks =10^(-10:10),
                                                      labels=scales::label_comma(),
                                                      minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)))}

    
  })
  output$IL<- renderDT({
    name1=str_to_title(gsub("-"," ",input$ticker1))
    name2=str_to_title(gsub("-"," ",input$ticker2))
    
    FINAL()%>%
      select(timestamp,LP,ONLY_A,ONLY_B)%>%
      rename("Date"=timestamp,"Providing Liquidity"=LP,!!name2:=ONLY_A,!!name1:=ONLY_B)%>%
      slice_tail(n = 1)
      
    })
  
}

shinyApp(ui = ui, server = server)
