require(highcharter)
require(dplyr)
require(tidyr)
require(shiny)
require(shinydashboard)
require(data.table)

#UI of the application

#reading the dataset
SurveyDf<-fread("multipleChoiceResponses.csv") #for faster data reading

attach(SurveyDf)




dashboardPage(
    dashboardHeader(title="Kaggle Survey data analysis app") ,
    
    #dashboard sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("Main Menu", tabName = "tab1",icon=icon("dashboard")) ,
        menuItem("Country-Wise analysis", tabName = "tab2"),
        menuItem("Preferred Tools", tabName = "tab3"),
        menuItem("Preferred ML methods", tabName = "tab4"),
        menuItem("Work", tabName = "tab5")
        
      )
    ) ,
    
    #dashboard body
    dashboardBody(
      #adding custom-css
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      
      tabItems(  
        
        #tab1 main menu
        tabItem(tabName="tab1",
                
                h2("Kaggle Data science data analysis App",align="center",style="margin-top:-5px;"),
                br()
                
                ),
        
        #tab 2-country-wise analysis
        tabItem(tabName ="tab2",
                h3("Country wise analysis",align="center") ,
                br(),
                
                h4("Histogram of country",align="center"),
                
                #chart for country histogram
                box(
                  
                  highchartOutput("country"),
                  width=12
                  
                ) ,
                
                fluidRow(
                  
                  
                  
                  
                  column(12, 
                         
                         box(
                           
                           selectInput("country",label="Most participants from countries",
                                       choices=c("India","United States")), 
                           width=6
                         )  #end box1
                         
                      )#end column 1
                    
                    )# end fluid Row
                
                  ),#end tab2
          
        #tab3    
        tabItem(tabName ="tab3",
                h3("Analyzing the preferred Tools used",align="center")
                
        ),
        
        #tab4
        tabItem(tabName ="tab4",
                h3("Analyzing the preferred ML techniques used",align="center")
                
        ),
        
        #tab5
        tabItem(tabName ="tab5",
                h3("What is used at work?",align="center")
                
        )
        
        
                
        )#end tabitems 
               
      
    )#end dashboard body
    
)#end dashboard page