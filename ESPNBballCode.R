library(XML)
library(ggmap)
library(ggplot2)
library(plyr)

# Data retrieved: 10 August, 2013, 5:11 pm Pacific

Gamestatistics=as.data.frame(matrix(ncol=16))
names(Gamestatistics) = c("Player", "GP", "MIN", "PPG", "RPG", "APG", "SPG", "BPG", "TPG", "FG%", "FT%", "3P%","Year","Team","City","Conference")
Seasonstatistics=as.data.frame(matrix(ncol=20))
names(Seasonstatistics) = c("Player", "MIN", "FGM", "FGA", "FTM", "FTA", "3PM", "3PA", "PTS", "OFFR", "DEFR", "REB", "AST",  "TO", "STL", "BLK","Year","Team","City","Conference")

getData=function(URLpart1,URLpart3,Team,City,Conference){
  for (i in 2002:2013){
  
  URL=paste(paste(URLpart1,as.character(i),sep=""),URLpart3,sep="")
  tablesfromURL = readHTMLTable(URL)
  
  gamestat=tablesfromURL[[1]]
  names(gamestat) = c("Player", "GP", "MIN", "PPG", "RPG", "APG", "SPG", "BPG", "TPG", "FG%", "FT%", "3P%")
  gamestat$Year=i
  gamestat$Team=Team
  gamestat$City=City
  gamestat$Conference=Conference
  Gamestatistics=rbind(gamestat,Gamestatistics) 

  seasonstat=tablesfromURL[[2]]
  names(seasonstat) = c("Player", "MIN", "FGM", "FGA", "FTM", "FTA", "3PM", "3PA", "PTS", "OFFR", "DEFR", "REB", "AST",  "TO", "STL", "BLK")
  seasonstat$Year=i
  seasonstat$Team=Team
  seasonstat$City=City
  seasonstat$Conference=Conference
  Seasonstatistics=rbind(seasonstat,Seasonstatistics)
  }
return(list(Gamestatistics,Seasonstatistics))
}
###########################################################################################
##Gonzaga Bulldogs: http://espn.go.com/mens-college-basketball/team/stats/_/id/2250/year/2011/gonzaga-bulldogs
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2250/year/" 
URLpart3 ="/gonzaga-bulldogs" 
Team="Gonzaga Bulldogs"
City="Spokane, WA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Brigham Young Cougars: http://espn.go.com/mens-college-basketball/team/stats/_/id/252/year/2011/brigham-young-cougars
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/252/year/" 
URLpart3 ="/brigham-young-cougars" 
Team="Brigham Young Cougars"
City="Provo, UT"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Loyola Marymount Lions:http://espn.go.com/mens-college-basketball/team/stats/_/id/2351/year/2010/loyola-marymount-lions
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2351/year/" 
URLpart3 ="/loyola-marymount-lions" 
Team="Loyola Marymount Lions"
City="Los Angeles, CA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Pacific Tigers: http://espn.go.com/mens-college-basketball/team/stats/_/id/279/year/2012/pacific-tigers
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/279/year/" 
URLpart3 ="/pacific-tigers" 
Team="Pacific Tigers"
City="Stockton, CA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Pepperdine: http://espn.go.com/mens-college-basketball/team/stats/_/id/2492/year/2011/pepperdine-waves
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2492/year/" 
URLpart3 ="/pepperdine-waves" 
Team="Pepperdine Waves"
City="Malibu, CA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Portland Pilots: http://espn.go.com/mens-college-basketball/team/stats/_/id/2501/year/2011/portland-pilots
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2501/year/" 
URLpart3 ="/portland-pilots" 
Team="Portland Pilots"
City="Portland, OR"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Saint Mary's Gaels: http://espn.go.com/mens-college-basketball/team/stats/_/id/2608/year/2012/saint-mary-gaels
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2608/year/" 
URLpart3 ="/saint-mary-gaels" 
Team="Saint Mary's Gaels"
City="Moraga, CA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##San Diego Toreros: http://espn.go.com/mens-college-basketball/team/stats/_/id/301/year/2011/san-diego-toreros
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/301/year/" 
URLpart3 ="/san-diego-toreros" 
Team="San Diego Toreros"
City="San Diego, CA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##San Francisco Dons:http://espn.go.com/mens-college-basketball/team/stats/_/id/2539/year/2012/san-francisco-dons
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2539/year/" 
URLpart3 ="/san-francisco-dons" 
Team="San Francisco Dons"
City="San Francisco, CA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Santa Clara Broncos: http://espn.go.com/mens-college-basketball/team/stats/_/id/2541/year/2012/santa-clara-broncos
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2541/year/" 
URLpart3 ="/santa-clara-broncos" 
Team="Santa Clara Broncos"
City="Santa Clara, CA"
Conference="West Coast"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################################################
##Rock Chalk Jayhawk: http://espn.go.com/mens-college-basketball/team/stats/_/id/2305/year/2012/kansas-jayhawks
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2305/year/" 
URLpart3 ="/kansas-jayhawks" 
Team="Kansas Jayhawks"
City="Lawrence, KS"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##Baylor Bears: http://espn.go.com/mens-college-basketball/team/stats/_/id/239/year/2012/baylor-bears
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/239/year/" 
URLpart3 ="/baylor-bears" 
Team="Baylor Bears"
City="Waco, TX"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##Iowa State: http://espn.go.com/mens-college-basketball/team/stats/_/id/66/year/2012/iowa-state-cyclones
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/66/year/" 
URLpart3 ="/iowa-state-cyclones" 
Team="Iowa State Cyclones"
City="Ames, IA"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##Kansas State: http://espn.go.com/mens-college-basketball/team/stats/_/id/2306/year/2012/kansas-state-wildcats
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2306/year/" 
URLpart3 ="/kansas-state-wildcats" 
Team="Kansas State Wildcats"
City="Manhattan, KS"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##Oklahoma Sooners: http://espn.go.com/mens-college-basketball/team/stats/_/id/201/year/2012/oklahoma-sooners
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/201/year/" 
URLpart3 ="/oklahoma-sooners" 
Team="Oklahoma Sooners"
City="Norman, OK"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##Oklahoma State Cowboys: http://espn.go.com/mens-college-basketball/team/stats/_/id/197/year/2012/oklahoma-state-cowboys
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/197/year/" 
URLpart3 ="/oklahoma-state-cowboys" 
Team="Oklahoma State Cowboys"
City="Stillwater, OK"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##TCU Horned Frogs: http://espn.go.com/mens-college-basketball/team/stats/_/id/2628/year/2012/tcu-horned-frogs
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2628/year/" 
URLpart3 ="/tcu-horned-frogs" 
Team="TCU Horned Frogs"
City="Fort Worth, TX"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##Texas Longhorns: http://espn.go.com/mens-college-basketball/team/stats/_/id/251/year/2012/texas-longhorns
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/251/year/" 
URLpart3 ="/texas-longhorns" 
Team="Texas Longhorns"
City="Austin, TX"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##Texas Tech Red Raiders: http://espn.go.com/mens-college-basketball/team/stats/_/id/2641/year/2012/texas-tech-red-raiders
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/2641/year/" 
URLpart3 ="/texas-tech-red-raiders" 
Team="Texas Tech Red Raiders"
City="Lubbock, TX"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
##West Virginia Mountaineers: http://espn.go.com/mens-college-basketball/team/stats/_/id/277/year/2012/west-virginia-mountaineers
URLpart1="http://espn.go.com/mens-college-basketball/team/stats/_/id/277/year/" 
URLpart3 ="/west-virginia-mountaineers" 
Team="West Virginia Mountaineers"
City="Morgantown, WV"
Conference="Big 12"
gameandseasonstats=getData(URLpart1,URLpart3,Team,City,Conference)
Gamestatistics=gameandseasonstats[[1]]
Seasonstatistics=gameandseasonstats[[2]]
##########################################################################################
###########################################################
# ON TO THE CLEANING
##############################################################

Gamestatistics=Gamestatistics[which(Gamestatistics$Player!="NA"),]
Gamestatistics=Gamestatistics[which(Gamestatistics$Player!="Player"),]
Gamestatistics=Gamestatistics[which(Gamestatistics$Player!="Totals"),]
for (i in 2:12){Gamestatistics[, i] = as.numeric(as.character(Gamestatistics[,i]))}
for(i in 14:16){Gamestatistics[,i]=as.factor(Gamestatistics[,i])}
names(Gamestatistics) = c("Player", "Games.Played", "Minutes", "Points.Per.Game", 
                          "Rebounds.Per.Game", "Assists.Per.Game", "Steals.Per.Game", 
                          "Blocks.Per.Game", "Turnovers.Per.Game", "Field.Goal.Percent", 
                          "Free.Throw.Percent", "Three.Point.FieldGoal.Percent",
                          "Year","Team","City","Conference")

Gamestatistics$Year<-gsub("2002", "2001-2002", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2003", "2002-2003", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2004", "2003-2004", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2005", "2004-2005", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2006", "2005-2006", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2007", "2006-2007", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2008", "2007-2008", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2009", "2008-2009", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2010", "2009-2010", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2011", "2010-2011", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2012", "2011-2012", Gamestatistics$Year)
Gamestatistics$Year<-gsub("2013", "2012-2013", Gamestatistics$Year)


Seasonstatistics=Seasonstatistics[which(Seasonstatistics$Player!="NA"),]
Seasonstatistics=Seasonstatistics[which(Seasonstatistics$Player!="Player"),]
Seasonstatistics=Seasonstatistics[which(Seasonstatistics$Player!="Totals"),]
for (i in 2:16){Seasonstatistics[, i] = as.numeric(as.character(Seasonstatistics[,i]))}
for(i in 18:20){Seasonstatistics[,i]=as.factor(Seasonstatistics[,i])}

names(Seasonstatistics) = c("Player", "Minutes", "Field.Goals.Made", "Field.Goals.Attempted", 
                            "Free.Throws.Made", "Free.Throws.Attempted", "Three.Point.FieldGoals.Made", 
                            "Three.Point.FieldGoals.Attempted", "Points", "Offensive.Rebounds", 
                            "Defensive.Rebounds", "Rebounds", "Assists",  "TurnOvers", 
                            "Steals", "Blocks","Year","Team","City","Conference")

Seasonstatistics$Year<-gsub("2002", "2001-2002", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2003", "2002-2003", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2004", "2003-2004", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2005", "2004-2005", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2006", "2005-2006", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2007", "2006-2007", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2008", "2007-2008", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2009", "2008-2009", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2010", "2009-2010", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2011", "2010-2011", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2012", "2011-2012", Seasonstatistics$Year)
Seasonstatistics$Year<-gsub("2013", "2012-2013", Seasonstatistics$Year)


save(Gamestatistics,file="Gamestatisticscleaned.rda")
save(Seasonstatistics,file="Seasonstatisticscleaned.rda")
#########################################################################
#Fun begins
# load("Gamestatisticscleaned.rda")
# Table of schools, Cities, and Conference
ggplot(Gamestatistics,aes(x=Conference,y=City,color=Conference))+geom_text(data=Gamestatistics,aes(label=Team))+
  theme(axis.text.x = element_text(color="black",size=12))+ 
  theme(axis.text.y = element_text(color="black",size=12))+theme(legend.position="none")+labs(y="",x="")


# Where exactly are these cities on the US map? Let's plot the cities of these teams

location=c(-125,24.207,-70,50) # It took a bit to figure these coordinates out - zoom to the appropriate location and level using openstreetmap.org 
# and find coordinates from the export link

map=get_map(location=location,maptype="roadmap",source="osm")
usmap=ggmap(map)

locs=geocode(as.character(unique(Gamestatistics$City))) # find the 20 cities from the data and identify their latitude and longitude; combine City information
locs$City=unique(Gamestatistics$City) 
Gamestatistics$lat=locs$lat[ match(Gamestatistics$City,locs$City)]# bring latitude and longitude information to main data frame
Gamestatistics$lon=locs$lon[ match(Gamestatistics$City,locs$City)]

# The plot
usmap+geom_point(data=Gamestatistics,aes(x=lon,y=lat,color=Conference),size=7)+ ggtitle("Location of WCC and Big 12 Schools")
  
# Let's plot histograms of some variable, say Points.Per.Game, for all teams 
ggplot(Gamestatistics,aes(x=Points.Per.Game, fill=Team))+
  geom_histogram()+ggtitle("Histogram of Points.Per.Game for All Teams - Data Collapsed Across All Years")+ 
  facet_wrap(~Team,ncol=4) + theme(legend.position="none")

# Two schools compared on One Variable --- say, Points.Per.Game - Kernel Density Plot

ggplot(subset(Gamestatistics,Team %in% c("Gonzaga Bulldogs","Kansas Jayhawks")),aes(x=Points.Per.Game, fill=Team))+
  geom_density(alpha=.3)+ggtitle("Kernel Density Plots of Points.Per.Game for Gonzaga Bulldogs and Kansas Jayhawks for all Years")+ facet_wrap(~Year,ncol=4) 


# Let's now find means for one variable across different seasons for all teams and plot them
# Mean calculation of Points.Per.Game of Team players for a season
ppgmean=ddply(Gamestatistics,.(Team,Year),summarize,Mean.Points.Per.Game=mean(Points.Per.Game))

#Plot
ggplot(ppgmean,aes(x=Year,y=Mean.Points.Per.Game,color=Team,group=Team))+
  geom_point()+geom_line()+facet_wrap(~Team,ncol=4)+theme(legend.position="none")+
  theme(axis.text.x = element_text(angle=-90))+ggtitle("Mean Points Per Game of Players of Different Teams in Different Seasons") 

# Mean points per game comparison for two teams, say, Gonzaga and Kansas, over years

ggplot(subset(ppgmean,Team %in% c("Gonzaga Bulldogs","Kansas Jayhawks")),aes(x=Year,y=Mean.Points.Per.Game,color=Team,group=Team))+
  geom_point()+geom_line()+ggtitle("Mean Points Per Game of Players of Gonzaga Bulldogs and Kansas Jayhawks in Different Seasons")


# We could also look at relationships between two variables (Points per game and Assists.Per.Game) in teams across different years and add in a smoothing curve
ggplot(Gamestatistics,aes(x=Points.Per.Game, y=Assists.Per.Game, color=Team))+
  geom_jitter()+ geom_smooth(method='loess',level=0,size=1,aes(color=Team))+
  ggtitle("Scatter Plots with LOESS smoothing of Points.Per.Game and Assists for All Teams -- Data Collapsed Across All Years")+ facet_wrap(~Team,ncol=4) + 
  theme(legend.position="none")  


# Comparing two schools on two variables ----Let's go back to (Points per game and Assists.Per.Game) 

ggplot(subset(Gamestatistics,Team %in% c("Gonzaga Bulldogs","Kansas Jayhawks")),aes(x=Points.Per.Game, y=Assists.Per.Game, color=Team))+
  geom_jitter()+ geom_smooth(method='loess',level=0,size=1,aes(color=Team))+ facet_wrap(~Year,ncol=4)+
  ggtitle("Scatter Plots with LOESS smoothing of Points.Per.Game and Assists for Gonzaga Bulldogs and Kansas Jayhawks -- Data Collapsed Across All Years")

# That's not all :-).... You can also do the same stuff for comparing the two conferences as well. 
# To do this and to explore other combinations of variables from this data, please visit this interactive application using RStudio's shiny server
# Two additional data files for plotting means of different variables to compare teams and conferences.
meansteams=ddply(Gamestatistics,.(Team,Year),summarize,
                 Points.Per.Game=mean(Points.Per.Game),
                 Games.Played=mean(Games.Played), 
                 Minutes = mean(Minutes), 
                 Rebounds.Per.Game=mean(Rebounds.Per.Game), 
                 Assists.Per.Game=mean(Assists.Per.Game), 
                 Steals.Per.Game=mean(Steals.Per.Game), 
                 Blocks.Per.Game=mean(Blocks.Per.Game), 
                 Turnovers.Per.Game=mean(Turnovers.Per.Game), 
                 Field.Goal.Percent=mean(Field.Goal.Percent), 
                 Free.Throw.Percent=mean(Free.Throw.Percent), 
                 Three.Point.FieldGoal.Percent=mean(Three.Point.FieldGoal.Percent)
                 )
meansconferences=ddply(Gamestatistics,.(Conference,Year),summarize,
                       Points.Per.Game=mean(Points.Per.Game),
                       Games.Played=mean(Games.Played), 
                       Minutes = mean(Minutes), 
                       Rebounds.Per.Game=mean(Rebounds.Per.Game), 
                       Assists.Per.Game=mean(Assists.Per.Game), 
                       Steals.Per.Game=mean(Steals.Per.Game), 
                       Blocks.Per.Game=mean(Blocks.Per.Game), 
                       Turnovers.Per.Game=mean(Turnovers.Per.Game), 
                       Field.Goal.Percent=mean(Field.Goal.Percent), 
                       Free.Throw.Percent=mean(Free.Throw.Percent), 
                       Three.Point.FieldGoal.Percent=mean(Three.Point.FieldGoal.Percent)
                )
save(meansteams,file="meansteams.rda")
save(meansconferences,file="meansconferences.rda")
