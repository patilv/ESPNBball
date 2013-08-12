library(ggplot2)
shinyServer(function(input, output) {
  
  #loading stored data file
  load("Gamestatisticscleaned.rda")
  load("meansconferences.rda")
  load("meansteams.rda")
  
  #Tab 1 Output
  
  output$char1dplot1=renderPlot({char1dplot1<-ggplot(Gamestatistics,aes_string(x=input$Characteristic1))+
                                    geom_histogram(aes(fill=Team))+ggtitle(paste(paste("Histogram of ", input$Characteristic1,""),"for All Teams - Data Collapsed Across All Seasons"))+ 
                                   facet_wrap(~Team,ncol=4) + theme(legend.position="none")                                    
                                 print(char1dplot1)})
  
  output$char1dplot3=renderPlot({
  
                                  char1dplot3<-ggplot(subset(Gamestatistics,Team %in% c(input$Team1,input$Team2)),aes_string(x=input$Characteristic1))+
                                   geom_density(alpha=.3,aes(fill=Team))+
                                   ggtitle(paste(paste(paste(paste(paste(paste("Kernel Density Plots of ", input$Characteristic1,""),"for",""),input$Team1,""),"and",""),input$Team2,""),"for all Seasons",""))+
                                   facet_wrap(~Year,ncol=4)                                     
                                  print(char1dplot3)})
               output$char1dplot2=renderPlot({                   
                                  char1dplot2<-ggplot(Gamestatistics,aes_string(x=input$Characteristic1))+
                                   geom_density(alpha=.3,aes(fill=Conference))+
                                   ggtitle(paste(paste("Kernel Density Plots of ",input$Characteristic1,""), "for West Coast and Big 12 Conferences for all Seasons","" ))+ facet_wrap(~Year,ncol=4)
                                  print(char1dplot2)
                                  
                                  })
    
  # Tab 2 Output
  
  output$char1mplot1=renderPlot({char1mplot1<-ggplot(meansteams,aes_string(y=input$Characteristic1))+
                                   geom_point(data=meansteams,aes(x=Year,color=Team))+
                                   geom_line(data=meansteams,aes(x=Year, color=Team,group=Team))+
                                   ggtitle(paste(paste("Mean of ", input$Characteristic1,""),"for All Teams Across Different Seasons"))+
                                   facet_wrap(~Team,ncol=4) + theme(legend.position="none") +
                                   theme(axis.text.x = element_text(angle=-90))
                                 print(char1mplot1)})
  
  
  
  output$char1mplot2=renderPlot({
  char1mplot2<-ggplot(subset(meansteams,Team %in% c(input$Team1,input$Team2)),aes_string(y=input$Characteristic1))+
                                   geom_point(data=subset(meansteams,Team %in% c(input$Team1,input$Team2)),aes(x=Year,color=Team))+
                                   geom_line(data=subset(meansteams,Team %in% c(input$Team1,input$Team2)),aes(x=Year, color=Team,group=Team))+
                                   ggtitle(paste(paste(paste(paste(paste(paste("Mean of ", input$Characteristic1,""),"for",""),input$Team1,""),"and",""),input$Team2,""),"Across Different Seasons",""))
  print(char1mplot2)})
  
  output$char1mplot3=renderPlot({char1mplot3<-ggplot(meansconferences,aes_string(y=input$Characteristic1))+
                                   geom_point(data=meansconferences,aes(x=Year,color=Conference))+
                                   geom_line(data=meansconferences,aes(x=Year, color=Conference,group=Conference))+
                                   ggtitle(paste(paste("Mean of ", input$Characteristic1,""),"for West Coast and Big 12 Conferences Across Different Seasons",""))
  
  
  print(char1mplot3)
  })
  
  # Tab 3 Output
  
  output$char1splot1=renderPlot({char1splot1<-ggplot(Gamestatistics,aes_string(x=input$Characteristic1, y=input$Characteristic2))+
                                   geom_jitter(aes(color=Team))+ geom_smooth(method='loess',level=0,size=1,aes(color=Team))+
                                   ggtitle(paste(paste(paste(paste("Scatter Plots with LOESS smoothing of ",input$Characteristic1,""), "and","" ),input$Characteristic2,""), "for All Teams -- Data Collapsed Across All Seasons",""))+ facet_wrap(~Team,ncol=4) + 
                                   theme(legend.position="none")                                    
                                 print(char1splot1)})
  
  
    
    output$char1splot3=renderPlot({char1splot3<-ggplot(subset(Gamestatistics,Team %in% c(input$Team1,input$Team2)),aes_string(x=input$Characteristic1, y=input$Characteristic2))+
                                   geom_jitter(aes(color=Team))+ geom_smooth(level=0,size=1,aes(color=Team))+
                                   ggtitle(paste(paste(paste(paste(paste(paste(paste(paste("Scatter Plots with LOESS smoothing of ", input$Characteristic1,""),"and",""),input$Characteristic2,""),"for",""),
                                                                   input$Team1,""),"and",""),input$Team2,""),"for all Seasons",""))+ facet_wrap(~Year,ncol=4)
      
    print (char1splot3)})
    
    
    output$char1splot2=renderPlot({char1splot2<-ggplot(Gamestatistics,aes_string(x=input$Characteristic1, y=input$Characteristic2))+
                                             geom_jitter(aes(color=Conference))+ geom_smooth(method='loess',level=0,size=1,aes(color=Conference))+
                                   ggtitle(paste(paste(paste(paste("Scatter Plots with LOESS smoothing of ", input$Characteristic1,""),"and",""),input$Characteristic2,""),"for West Coast and Big 12 Conferences for all Seasons",""))+
                                    facet_wrap(~Year,ncol=4)                                     
               
    print(char1splot2)
    })
    })
