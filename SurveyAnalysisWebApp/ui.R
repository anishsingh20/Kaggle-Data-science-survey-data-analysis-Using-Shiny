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


#country count data frame in descending order-top 20
countryCountApp<-as.data.frame(table(SurveyDf$Country)) %>%  top_n(20) %>% 
  arrange(desc(Freq))

colnames(countryCountApp)<-c("Country","Frequency")


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
      skin="white",
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
                
                br() ,
                
                
                fluidRow(
                  
                  
                  
                  
                  column(12, 
                         
                         box(
                           
                           selectInput("country",label="Select Country",
                                       choices=countryCountApp[,1]), 
                           width=12
                         ),  #end box1
                         
                         #box for plots
                         box(
                           
                          highchartOutput("jobTitle"), 
                          width=12 
                         ) ,
                         
                         
                         #chart for type of employer of participants country-wise
                         box(
                           
                           highchartOutput("employer"),
                           width=12
                           
                         ) 
                         
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