library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("NCAA Men's Basketball - Tales of Two Conferences"),
  
  sidebarPanel(
    HTML("<br>Characteristic 1 will be the variable used for studying distributions of only one variable. 
         Characteristic 2 will be used for making scatter plots with Characteristic 1 on the other axis. Please note that these are player-level (in contrast to team-level) data.
         Therefore, numbers like Turnovers.Per.Game are to be interpreted as Turnovers.Per.Game for a player.<br><br> "),
    selectInput("Characteristic1", "Characteristic 1:",
                choices =c(
                  "Games.Played", "Minutes","Points.Per.Game","Rebounds.Per.Game","Assists.Per.Game","Steals.Per.Game","Blocks.Per.Game",
                  "Turnovers.Per.Game","Field.Goal.Percent","Free.Throw.Percent","Three.Point.FieldGoal.Percent")),
    
    selectInput("Characteristic2", "Characteristic 2:",
                choices =c(
                  "Minutes","Games.Played", "Points.Per.Game","Rebounds.Per.Game","Assists.Per.Game","Steals.Per.Game","Blocks.Per.Game",
                  "Turnovers.Per.Game","Field.Goal.Percent","Free.Throw.Percent","Three.Point.FieldGoal.Percent")),
    
    selectInput("Team1", "Team 1:",
                choices =c(
                  "Gonzaga Bulldogs", "Kansas Jayhawks","West Virginia Mountaineers", "Texas Tech Red Raiders","Texas Longhorns","TCU Horned Frogs","Oklahoma State Cowboys","Oklahoma Sooners",          
                  "Kansas State Wildcats","Iowa State Cyclones","Baylor Bears","Santa Clara Broncos","San Francisco Dons","San Diego Toreros",
                  "Saint Mary's Gaels","Portland Pilots","Pepperdine Waves","Pacific Tigers","Loyola Marymount Lions","Brigham Young Cougars")),
                 
    selectInput("Team2", "Team 2:",
                choices =c(
                  "Kansas Jayhawks","Gonzaga Bulldogs", "West Virginia Mountaineers", "Texas Tech Red Raiders","Texas Longhorns","TCU Horned Frogs","Oklahoma State Cowboys","Oklahoma Sooners",          
                  "Kansas State Wildcats","Iowa State Cyclones","Baylor Bears","Santa Clara Broncos","San Francisco Dons","San Diego Toreros",
                  "Saint Mary's Gaels","Portland Pilots","Pepperdine Waves","Pacific Tigers","Loyola Marymount Lions","Brigham Young Cougars")),
  
  HTML("<br><br>Data were scraped from 240 pages from espn.com. <a href = 'http://analyticsandvisualization.blogspot.com/2013/08/short-tales-of-two-ncaa-basketball.html' target='_blank'> 
       An accompanying blog post details this procedure.</a> <br>  Application code <a href = 'https://github.com/patilv/ESPNBball' target='_blank'>
       is available here.</a>")),

  
  mainPanel(
    tabsetPanel(
      tabPanel("Distribution of Characteristic 1", plotOutput("char1dplot1"),plotOutput("char1dplot3"),plotOutput("char1dplot2")),
      tabPanel("Means of Characteristic 1", plotOutput("char1mplot1"),plotOutput("char1mplot2"),plotOutput("char1mplot3")),
      tabPanel("Relationship between Both Characteristics",plotOutput("char1splot1"),plotOutput("char1splot3"),plotOutput("char1splot2") )
     
    )
    
  )
))

