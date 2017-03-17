#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dygraphs)
library(reshape2)
library(leaflet)
library(xts)
library(broom)
load('data/ShinyFullDataset.RData')

matt_theme <- theme_set(theme_bw())
matt_theme<- theme_update(axis.line = element_line(colour = "black"),
                          panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          text=element_text(family='sans','plain','black',16,0.5,0.5,0,0),
                          plot.margin=unit(c(6,20,6,2),'pt')
)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addPolygons(data=csub,color=csub$color,popup=paste((csub$HU_10_NAME)),group='Catchments') %>%
      addMarkers(data=station.sp,popup=paste('Station ID = ',station.sp$Station.ID)) %>%
      addProviderTiles('Esri.WorldTopoMap',group='Topo') %>%
      addProviderTiles('Esri.WorldImagery',group='Imagery') %>%
      addLayersControl(baseGroups=c('Topo','Imagery'),
                       overlayGroups='Catchments') 
      
    })
  
  output$q <- renderDygraph({
    dygraph(q.xts,group='dy') %>%
      dyOptions(colors=c('red','blue'),strokeWidth=1.5) %>%
      dyAxis('y',label='Q (cms)')
  })
  
  output$q2 <- renderDygraph({
    dygraph(q.xts,group='dy') %>%
      dyOptions(colors=c('red','blue'),strokeWidth=1.5) %>%
      dyAxis('y',label='Q (cms)')
  })
  
  output$time.chem <- renderDygraph({
    d1 <- q.chem %>% 
      filter(variable == input$analyte) %>%
      dcast(.,dateTime~Site,value.var='value',mean)
   xts(d1[,-1],order.by=d1$dateTime) %>%
     dygraph(.,group='dy')   %>%
     dyOptions(colors=c('red','blue'),strokeWidth=0, drawPoints=T,pointSize=4) %>%
     dyAxis('y',label='[Analyte Conc] (mg/l)')
    
  })
  
  output$chemostasis <- renderPlot({
    dts <- numeric()
    if(is.null(input$q2_date_window)){
      dts <- c(min(q.chem$dateTime),max(q.chem$dateTime))
    }else{
    dts[1] <- (as.Date(input$q2_date_window[[1]]))
    dts[2] <- (as.Date(input$q2_date_window[[2]]))
    }
    q.sub <- q.chem %>% 
      filter(dateTime > dts[1] & dateTime < dts[2]) %>%
      filter(!is.na(value)) %>%
      filter(variable == input$analyte1) %>%
      filter(value > 0) %>%
      mutate(month = month(dateTime))
    
    if(input$season=='all'){
      q.sub <- q.sub
    }
    if(input$season=='summer'){
      q.sub <- q.sub %>% filter(month %in% c(6,7,8,9))
    }
    
    if(input$season=='winter'){
      q.sub <- q.sub %>% filter(!month %in% c(6,7,8,9))
    }
    
      gplot <- ggplot(q.sub, aes(x=m3s,y=value,color=Site)) + 
      geom_point(shape=1,size=2) +
      geom_point(shape=1,size=2.1) + 
      scale_color_manual(name='',values=c('red','blue')) + 
      theme(legend.position=c(.8,.6)) + 
      xlab('Q (cms)') + 
      ylab('Analyte Concentration [mg/l]')
    if(input$model=='none'){
      print(gplot)
    }
    if(input$model=='yx'){
      mod <- lm(value ~ m3s+Site,data=q.sub) %>% glance(.)
      
      print(gplot + stat_smooth(method='lm') + 
              annotate("text",  x=Inf, y = Inf,
                           label =paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                           vjust=2, hjust=2.5,size=6))
    }
    if(input$model=='logx'){
      mod <- lm(value ~ log10(m3s)+Site,data=q.sub) %>% glance(.)
      
        print(gplot + scale_x_log10() + 
                annotate("text",  x=Inf, y = Inf,
                         label =paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                         vjust=2, hjust=2.5,size=6) + 
        stat_smooth(method='lm'))
    }
    if(input$model=='logy'){
      mod <- lm(log10(value) ~ m3s+Site,data=q.sub) %>% glance(.)
      
      print(gplot +  scale_y_log10() + 
              annotate("text",  x=Inf, y = Inf,
                       label =paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                       vjust=2, hjust=2.5,size=6) + 
        stat_smooth(method='lm'))
    }
    if(input$model=='logyx'){
      mod <- lm(log10(value) ~ log10(m3s)+Site,data=q.sub) %>% glance(.) 
      print(gplot + scale_y_log10() + 
        scale_x_log10() +
        annotate("text",  x=Inf, y = Inf,
                   label = paste('p = ',round(mod$p.value,3),'R2 =',round(mod$adj.r.squared,2)), 
                   vjust=2, hjust=2.5,size=6) + 
        stat_smooth(method='lm'))
    }
  })
})



