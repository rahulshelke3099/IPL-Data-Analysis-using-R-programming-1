library(shiny)
library(shinydashboard)
library(ggplot2) # Data visualization
library(plotrix)
library(slickR)
library(RColorBrewer)
library(dplyr)
library(base)
library(stats)

shinyUI(
  dashboardPage(
  dashboardHeader(
    titleWidth = 1425,
    title = "INDIAN PREMERE LEAGUE"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName = "home"),
      menuItem("Analyzing Toss Related Decisions", tabName = "toss"),
      menuItem("Analyzing IPL Statistical Data", tabName = "data"),
      menuItem("Generating General IPL Statistics", tabName = "stats")
    )
  ),
  
  dashboardBody(
    titlePanel(title = h1("IPL DATA ANALYSIS", align="center")),
    
    tabItems(
      tabItem(tabName = "home",
              slickROutput("slickr")
      ),      
      
      tabItem(
        tabName = "toss",
        selectInput(
          "y","Choose Query:",
          c("Is winning the toss really an advantage?","Toss Winning Teams of Particular Year?","What do Toss Winning Teams Choose?"),
          selected = NULL
        ),
        conditionalPanel(
          condition = "input.y == 'Is winning the toss really an advantage?'",
          textOutput(outputId = "textop1"),
          
          plotOutput(outputId = "winAdv")
        ),
        
        conditionalPanel(
          condition = "input.y == 'What do Toss Winning Teams Choose?'",
          # Input: Specification of range within an interval ----
          sliderInput("range", "Range:",
                      min = 2008, max = 2017,step = 1,
                      value = c(2009,2014)),
          
          plotOutput(outputId = "winChoose")
        ),
        
        conditionalPanel(
          condition = "input.y == 'Toss Winning Teams of Particular Year?'",
          sliderInput("year", "Select Particular Year:",
                      min = 2008, max = 2017,step = 1,value = 2008),
          #selectInput("year","Choose Year:",
          #           c(matches1$season)),
          plotOutput(outputId = "tossWin")
        )
      ),
      
      tabItem(
        tabName = "data",
        selectInput(
          "x","Choose Query:",
          c("Runs Conceded By Particular Bowling Team?","Comparison of Runs Scored By Different Players per Season:",
            "Comparision of Wickets Taken By Different Bowlers per Season:",
            "Number of 6s,4s,3s,2s,1s,0s scored per Season:","Season wise comparison(Boundaries)",
            "Number Of matches win by each Team (All Seasons Together)",
            "Number of Matches Played in Each City:","Top Batsmen:","Top Bowler:")
        ),
        
        conditionalPanel(
          condition = ("input.x == 'Runs Conceded By Particular Bowling Team?'"),
          radioButtons( "radio","Select Bowling Team:",choices = list("Royal Challengers Bangalore","Rising Pune Supergiants","Gujarat Lions",
                                                                      "Sunrisers Hyderabad","Mumbai Indians","Kolkata Knight Riders",
                                                                      "Kings XI Punjab","Delhi Daredevils","Deccan Chargers","Chennai Super Kings",
                                                                      "Rajasthan Royals","Kochi Tuskers Kerala")
          ),
          plotOutput(outputId = "runsPie")
        ),
        
        conditionalPanel(
          condition = ("input.x == 'Comparison of Runs Scored By Different Players per Season:'"),
          selectInput("playerName1","Choose Batsman 1:",choiceBatsman),
          selectInput("playerName2","Choose Batsman 2:",choiceBatsman),
          selectInput("playerName3","Choose Batsman 3:",choiceBatsman),
          selectInput("playerName4","Choose Batsman 4:",choiceBatsman),
          
          plotOutput(outputId = "runsByPlayer")
        ),
        
        conditionalPanel(
          condition = ("input.x == 'Comparision of Wickets Taken By Different Bowlers per Season:'"),
          selectInput("bowlerName1","Choose Batsman 1:",choiceBowler),
          selectInput("bowlerName2","Choose Batsman 2:",choiceBowler),
          selectInput("bowlerName3","Choose Batsman 3:",choiceBowler),
          selectInput("bowlerName4","Choose Batsman 4:",choiceBowler),
         
          plotOutput(outputId = "wktsByBowler")
          ),
        
        conditionalPanel(
          condition = ("input.x=='Number of 6s,4s,3s,2s,1s,0s scored per Season:'"),
          selectInput("check","Select Season:",choiceSeason),
          plotOutput(outputId = "noOfSix")
        ),
        
        conditionalPanel(
          condition = ("input.x == 'Season wise comparison(Boundaries)'"),
          selectInput("playerName01","Choose Batsman 1:",choiceBatsman),
          selectInput("playerName02","Choose Batsman 2:",choiceBatsman),
          selectInput("playerName03","Choose Batsman 3:",choiceBatsman),
          selectInput("playerName04","Choose Batsman 4:",choiceBatsman),
          
          plotOutput(outputId = "boundariesByPlayer")
        ),
        conditionalPanel(
          condition = ("input.x== 'Number Of matches win by each Team (All Seasons Together)'"),
          
          plotOutput(outputId = "matchesWon")
        ),
        conditionalPanel(
          condition = ("input.x == 'Number of Matches Played in Each City:'"),
          
          plotOutput(outputId = "city")
        ),
        conditionalPanel(
          condition = ("input.x== 'Top Batsmen:'"),
          
          plotOutput(outputId = "topBatsmen")
        ),
        conditionalPanel(
          condition = ("input.x == 'Top Bowler:'"),
          
          plotOutput(outputId = "topBowler")
        )
        

        
      
        ),
          tabItem(tabName = "stats",
                 htmlOutput(outputId = "textques1") ,
                 htmlOutput(outputId = "textques2"),
                 htmlOutput(outputId = "textques3"),
                 htmlOutput(outputId = "textques4")
            
       )
        
      )
    )
    
  )
)