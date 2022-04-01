library(shiny) #dashboard
library(tidyverse) #data manipulation
library(plotly) #interactive charts
library(scales) #percentage scales for charts
library(geckor)
library(DT)
library(RcppRoll)
library(zoo)
library(tidyquant)



ui <- fluidPage(
  
  # Application title
  titlePanel("Simulation of Providing Liquidity"),
  # Sidebar with a inputs 
  sidebarLayout(
    sidebarPanel(
      
      dateInput("start",   
                "Start Date", 
                value = "2021-06-27"),     
      dateInput("end",   
                "End Date",
                value = "2022-12-31"),
      textInput("ticker2","First Asset",value="ethereum"),
      textInput("ticker1","Second Asset",value="dopex-rebate-token"),
      numericInput("yield_1","First Asset APR (%)",value=0),
      numericInput("yield_2","Second Asset APR (%)",value=0),
      numericInput("yield_3","Liquidity APR (%)",value=0),
      sliderInput("rollingVol","Volatility Number of Days Rolling",min=1,max=100,value=30),
      sliderInput("rollingCorr","Correlation Number of Days Rolling",min=1,max=100,value=30),
      checkboxInput("LOG","Logarithmic Y axis?",value=TRUE),
      width = 2
    ),
    mainPanel(
      plotlyOutput("Mainchart",height="720px",width="1400"),
      plotlyOutput("RealizedVol",height="720px",width="1400"),
      plotlyOutput("Drawdown",height="720px",width="1400"),
      plotlyOutput("Correlation",height="720px",width="1400")
      
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
        ((input$yield_3/365)*workLP$AmountA[i-1]/200)
      
      workLP$AmountB[i]<-
        (sqrt(workLP$AmountA[i-1]*workLP$AmountB[i-1]*workLP$B[i]/workLP$A[i]))+
        ((input$yield_3/365)*workLP$AmountB[i-1]/200)
      
      workLP$LP[i]<-workLP$AmountA[i]*workLP$B[i]+workLP$AmountB[i]*workLP$B[i]
      
    }
    
    
    workLP<-workLP%>%mutate(ONLY_A=(LP[1]/A[1])*A*((1+input$yield_1/36500)^(row_number()-1)),
                            ONLY_B=(LP[1]/B[1])*B*((1+input$yield_2/36500)^(row_number()-1)))
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
                pivot_longer(!timestamp,names_to = "Token", values_to = "value"),
              aes(y=value,x=as.Date(timestamp),color=Token))+
      geom_line(size=1.1)+
      scale_color_manual(name="Asset",values = c('steelblue',"grey", "red")) +
      scale_x_date(NULL, date_labels = "%b %y", breaks = "month")+
      xlab('Date')+
      ylab("Total Return")+
      ggtitle(paste(name1," vs ",name2,"vs Providing Liquidity"))+
      theme_bw()
    
    g<-g+if(input$LOG==TRUE){scale_y_log10(breaks =10^(-10:10),
                                           labels=scales::label_comma(),
                                           minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)))}
    
    
  })
  output$Drawdown<- renderPlotly({
    name1=str_to_title(gsub("-"," ",input$ticker1))
    name2=str_to_title(gsub("-"," ",input$ticker2))
    g<-ggplot(data=FINAL()%>%
                select(timestamp,LP,ONLY_A,ONLY_B)%>%
                mutate(drawdownLP=-(1-LP/cummax(LP)),
                       drawdownA=-(1-ONLY_A/cummax(ONLY_A)),
                       drawdownB=-(1-ONLY_B/cummax(ONLY_B)))%>%
                select(timestamp,drawdownLP,drawdownA,drawdownB) %>% 
                rename("Providing Liquidity"=drawdownLP,!!name2:=drawdownA,!!name1:=drawdownB)%>% #:= to assign fixed values
                pivot_longer(!timestamp,names_to = "Token", values_to = "value"),
              aes(y=value,x=as.Date(timestamp),color=Token))+
      scale_color_manual(name="Asset",values = c('steelblue',"grey", "red")) +
      geom_line(size=1.1)+
      scale_x_date(date_labels = "%b %y", breaks = "month")+
      xlab('Date')+
      ylab("%")+
      ggtitle(paste("Drawdown"))+
      theme_bw()
  })
  output$RealizedVol<- renderPlotly({
    name1=str_to_title(gsub("-"," ",input$ticker1))
    name2=str_to_title(gsub("-"," ",input$ticker2))
    volData<-FINAL() %>% 
      select(timestamp,LP,ONLY_A,ONLY_B)%>%
      mutate(VolA= log(ONLY_A)-log(lag(ONLY_A))) %>% 
      replace_na(list(Vol = 0)) %>%
      mutate(Vol30A = roll_sd(VolA, input$rollingVol, fill = NA, align = "right"),
             Vol30A = (round((sqrt(365) * Vol30A * 100), 2)))%>% 
      mutate(VolB= log(ONLY_B)-log(lag(ONLY_B))) %>% 
      replace_na(list(Vol = 0)) %>%
      mutate(Vol30B = roll_sd(VolB, input$rollingVol, fill = NA, align = "right"),
             Vol30B = (round((sqrt(365) * Vol30B * 100), 2)))%>% 
      mutate(VolLP= log(LP)-log(lag(LP))) %>% 
      replace_na(list(VolLP = 0)) %>%
      mutate(Vol30LP = roll_sd(VolLP, input$rollingVol, fill = NA, align = "right"),
             Vol30LP = (round((sqrt(365) * Vol30LP * 100), 2))) %>%
      select(timestamp,Vol30LP,Vol30A,Vol30B) %>% 
      rename("Providing Liquidity"=Vol30LP,!!name2:=Vol30A,!!name1:=Vol30B)%>%
      pivot_longer(!timestamp,names_to = "Token", values_to = "Volatility")
      
    g<-ggplot(data=volData,
              aes(y=Volatility,x=as.Date(timestamp),color=Token))+
      geom_line(size=1.1)+
      scale_x_date(date_labels = "%b %y")+
      scale_color_manual(name="Asset",values = c('steelblue',"grey", "red")) +
      xlab('Date')+
      ylab("Vol (%)")+
      ggtitle(paste(input$rollingVol,"Day Realized Volatility"))+
      theme_bw()
  })
  FINAL2<-reactive({FINAL() %>% 
      select(timestamp,A,B) %>% 
      tq_transmute_xy(x          = A, 
                      y          = B,
                      mutate_fun = runCor,
                      n          = input$rollingCorr,
                      col_rename = "rolling.corr")
    })
  output$Correlation<- renderPlotly({
    ggplot(data=FINAL2(),
           aes(y=rolling.corr,x=as.Date(timestamp)))+
      geom_line(size=1.1)+
      scale_x_date(date_labels = "%b %y")+
      xlab('Date')+
      ylab("Correlation")+
      ggtitle(paste(input$rollingCorr,"Day Rolling Correlation"))+
      theme_bw()
  
  })
  
}

shinyApp(ui = ui, server = server)