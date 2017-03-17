#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dygraphs)
vars <-
  c(
    "Cadmium",
    "Calcium",
    "Iron",
    "Manganese",
    "Magnesium",
    "pH",
    "Selenium",
    "Zinc",
    "Specific conductance",
    "Sulfur, sulfate (SO4) as SO4",
    "Copper",
    "Lead",
    "Sodium",
    "Phosphate-phosphorus as P",
    "Inorganic nitrogen (nitrate and nitrite) as N",
    "Aluminum"
  )               


# Define UI for application that draws a histogram
shinyUI(fluidPage(sidebarLayout(
  sidebarPanel(
    p('In this teaching module, you will explore changing water quality in the 
      Snake River Watershed (near Keystone Ski Area, in red) and Upper Blue River Watershed (near Breckenridge, in blue). 
      We will look at a 50-year record of concentration of key nutrients, pollutants, and discharge,
      to gain a better understanding of the factors (land use, discharge, pH)
      that influence water quality in high mountain watersheds. Markers on map show location of WQ monitoring sites'
    ),
    leafletOutput('map',height=350)
  ),
    mainPanel(
      tabsetPanel(
        tabPanel('WQ change over time',
      selectInput('analyte',label='Select a water quality analyte',
                  choices=vars),
      br(),
      h3('Daily Discharge'),
      dygraphOutput('q',height='225px'),
      h3('Analyte Concentration'),
      dygraphOutput('time.chem',height='225px')
                  ),
    tabPanel('WQ change with flow and season',
             fluidRow(column(
               4,
             selectInput('analyte1',label='Select an analyte',
                         choices=vars)),
             column(4,
                    selectInput('model',label='Select a model to fit',
                                choices=c('none','yx','logx','logy','logyx'))),
             column(4,
                    selectInput('season',label='Select a season',
                                choices=c('all','summer','winter')))),
             br(),
             h3('Daily Discharge'),
             dygraphOutput('q2', height='225px'),
             h3('QC plots'),
             plotOutput('chemostasis',height='350px')
             )
    )
))))
