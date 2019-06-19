library(shiny)
library(shinydashboard)
library(ggplot2) # Data visualization
library(plotrix)
library(slickR)
library(RColorBrewer)
library(dplyr)
library(base)
library(stats)

shinyServer(function(input,output)
{
  
  output$winAdv <- renderPlot({
    matches1$toss_match<-ifelse(as.character(matches1$toss_winner)==as.character(matches1$winner),"Won","Lost")
    ggplot(matches1[which(!is.na(matches1$toss_match)),],aes(toss_match, fill = matches1$toss_match))+ 
      geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("How much of a advantage is winning the toss")
    
  })
  
  
  output$slickr <- renderSlickR({
    imgs <- list.files("E:/R/ipl", pattern=".jpg", full.names = TRUE)
    slickR(imgs)
  })
  
  output$matchesWon <- renderPlot({
    ggplot(matches1,aes(winner)) +geom_bar(fill="#0072B2") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+ylab("Matches won")+
     ggtitle("Number Of matches win by each Team (All Seasons Together)")
  })
  

  output$textop1 <- renderText({
    paste("Is winning the toss really an advantage?") 
  })
  
  output$textop2 <- renderText({
    paste("Number of matches played by each team") 
  })
  
  output$textop3 <- renderText({
    paste("zxcvb is called.") 
  })
  
  output$city <- renderPlot({
    ggplot(matches1[which(!is.na(matches1$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
      ylab("Number of Matches Played") + guides(fill=FALSE)+ 
      ggtitle("Number of Matches Played in Each City:")
  })
  
  output$tossWin <- renderPlot({
    df <- subset(matches1, season == input$year)
    ggplot(data = df, aes(toss_winner,fill = toss_winner))+geom_bar()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  output$winChoose <- renderPlot({
    df <- subset(matches1, season >= input$range[1] & season <= input$range[2])
    ggplot(data = df, aes(toss_winner, fill = toss_decision))+ geom_bar()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Teams")+ylab("No.of Matches")
    
  })
  
  output$runsPie <- renderPlot({
    v <- paste("Runs conceded by", as.character(input$radio))
    df<-subset(deliveries,bowling_team== input$radio)
    slices <- c(sum(df$wide_runs),sum(df$bye_runs),sum(df$legbye_runs),sum(df$noball_runs),sum(df$penalty_runs))
    lbls <- c("Wide Runs", "Bye Runs", "LegBye Runs", "Noball Runs", "Penalty Runs")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    lbls <- paste(lbls,"(" )
    lbls <- paste(lbls,slices)
    lbls <- paste(lbls,")")
    pie(slices,labels=lbls,col=rainbow(length(lbls)),
        main=v)
  })
  
  output$runsByPlayer<- renderPlot({
    ipl %>% 
      filter(batsman==input$playerName1 | batsman==input$playerName2 |batsman==input$playerName3 |batsman==input$playerName4 ) %>% 
      
      group_by(batsman,season) %>% summarise(totscore = sum(batsman_runs)) %>%  
      ggplot(aes(season,totscore, col=batsman)) + geom_line(size=2) + ylab("Runs") + 
      ggtitle("Runs Scored By Player in Different Season:") + scale_x_continuous(breaks = 2008:2017)
  }) 

  
  output$wktsByBowler <- renderPlot({
    ipl5 %>%
      filter(bowler==input$bowlerName1 | bowler==input$bowlerName2 | bowler==input$bowlerName3 | bowler==input$bowlerName4) %>%
      group_by(bowler,season) %>% filter(player_dismissed!="") %>% summarise(totwickets = length(player_dismissed)) %>%
      ggplot(aes(season,totwickets, col=bowler)) + geom_line(size=2) + ylab("Wickets") + 
      ggtitle("Wickets By Player in Different Season:") + scale_x_continuous(breaks = 2008:2017)
  })
  

  output$noOfSix <- renderPlot({
    ipl2 %>% group_by(over,batsman_runs) %>% filter(season==input$check & batsman_runs!=5 & batsman_runs!=0) %>% summarise(no = length(batsman_runs)) %>% 
      ggplot(aes(over,no, fill = as.factor(batsman_runs))) + geom_bar(stat = "identity",position= "dodge") + ylab("") + scale_x_continuous(breaks = 1:20)
    
  })
  
  output$boundariesByPlayer <- renderPlot({
    merge(deliveries,matches1,by.x = "match_id", by.y = "id") %>% filter(batsman==input$playerName01 | batsman==input$playerName02 |batsman==input$playerName03 |batsman==input$playerName04) %>% 
      filter(batsman_runs==4|batsman_runs==6) %>% group_by(batsman,season) %>% summarise(boundaries= length(batsman_runs)) %>%
      ggplot(aes(season,boundaries, col=batsman)) +geom_line(size= 2) + ggtitle("Season wise comparision(Boundaries)") +
      scale_x_continuous(breaks = 2008:2016)
  })
  
  output$topBatsmen <- renderPlot({
    df<- deliveries %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>%
      filter(runs > 3000) 
    df %>% ggplot(aes(reorder(batsman,-runs),runs,fill=batsman)) +geom_bar(stat = "identity") +xlab("Batsman")+ ylab("Runs")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Batsmen")+ guides(fill=F)
    
  })
  
  output$topBowler <- renderPlot({
    df1<-deliveries %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets= length(player_dismissed)) %>% top_n(n=10,wt=wickets) 
    df1 %>% ggplot(aes(reorder(bowler,-wickets),wickets,fill=bowler))+geom_bar(stat = "identity") + ylab("Wickets")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Bowlers")+ guides(fill=F)
    
  })

  output$textques1 <- renderText({
    paste("<b>Q.1 - How many .csv files are used to Analyze the IPL Data?<br>", "<b>Ans:- ", 
          "Two datasets are used.<br>1. matches.csv<br>2. deliveries.csv")
  })
  
  n <- nrow(matches1)-1
  output$textques2 <- renderText({
    paste("<br><br><b>Q.2 - How many matches are played from 2008-2017 season of IPL?<br>", "<b>Ans:- ", 
          n)
  })
  
  m <- nrow(deliveries)-1
  output$textques3 <- renderText({
    paste("<br><br><b>Q.3 - How many deliveries are bowled from 2008-2017 season of IPL?<br>", "<b>Ans:- ", 
          m)
  })
  
  z <- list(choiceSeason)
  output$textques4 <- renderText({
    paste("<br><br><b>Q.4 - How many Seasons are Present in Dataset?<br>", "<b>Ans:- ", 
          z)
  })
}
)
