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


#top work industries where participants work
TopIndustry<-as.data.frame(table(SurveyDf$EmployerIndustry)) %>%   
  arrange(desc(Freq))
TopIndustry[1,1]<-NA
TopIndustry<-na.omit(TopIndustry)



#job titles
jobs<-as.data.frame(table(CurrentJobTitleSelect)) %>% arrange(desc(Freq))
jobs[1,1]<-NA
jobs<-na.omit(jobs)




dashboardPage(
  skin="black",
    dashboardHeader(title="Kaggle Survey Data analysis") ,
    
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
                
                h2("Kaggle Data science survey 2017 data analysis App",align="center",style="margin-top:-5px;"),
                br()
                
                ),
        
        #tab 2-country-wise analysis
        tabItem(tabName ="tab2",
                h3("Country wise analysis",align="center") ,
                
                #chart for country histogram
                box(
                  
                  highchartOutput("country"),
                  width=12
                  
                ) ,
                
                
                
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
                           
                         ) ,
                         
                         #chart for historgram of age of participants country-wise
                         box(
                           
                           highchartOutput("age"),
                           width=12
                           
                         )
                         
                         
                         
                      )#end column 1
                    
                    )# end fluid Row
                
                  ),#end tab2
          
        #tab3    
        tabItem(tabName ="tab3",
                h3("Which tools are the participants most excited about learning in the next year?"
,align="center"),
                
                #most used tools by participants
                box(
                  
                  highchartOutput("tools"),
                  width=12
                  
                ) ,
                
                fluidRow(
                  
                  column(12, 
                         
                        #type of industry and its preferred tools
                        h3("Tools most excited about learning in next year in different Industries of a country",align="center") ,
                        br(),
                        
                         box(
                           
                           selectInput("industry",label="Select Industry",
                                       choices=TopIndustry[,1]), 
                           
                           width=6
                         ),
                        #country select
                        box(
                          
                          selectInput("country1",label="Select Country",
                                      choices=countryCountApp[,1]), 
                          
                          width=6
                        ),
                         
                         #box for plots
                         box(
                           
                           highchartOutput("industryTools"), 
                           width=12 
                         )
                        ,
                        br(),
                        
                        h3("Tools most excited about learning in next year by different Job positions in Industry",align="center") ,
                        
                        #job title select
                        box(
                          
                          selectInput("job",label="Select Job title",
                                      choices=jobs[,1]), 
                          
                          width=6),
                        
                        #industry
                        box(
                          
                          selectInput("industry1",label="Select Industry",
                                      choices=TopIndustry[,1]), 
                          
                          width=6) ,
                        
                        #plot for tools and job positions
                        box(
                          
                          highchartOutput("JobTools"), 
                          width=12 
                        ) ,
                        
                        br(),
                        
                        h3("Tools most used at Work Place",align="center"),
                        
                        br(),
                        
                        #job title select
                        box(
                          
                          selectInput("job2",label="Select Job title",
                                      choices=jobs[,1]), 
                          
                          width=6),
                        
                        #industry
                        box(
                          
                          selectInput("industry2",label="Select Industry",
                                      choices=TopIndustry[,1]), 
                          
                          width=6) ,
                        
                        #plot for tools used at work 
                        box(
                          
                          highchartOutput("ToolsWork"), 
                          width=12 
                        ) 
                         
                         #chart for type of employer of participants country-wise
                         #
                         
                         
                         
                  )#end column 1
                  
                )# end fluid Row
                
                
        ),
        
        #tab4
        tabItem(tabName ="tab4",
                h3("Which ML techniques are the participants most excited about learning?",align="center") ,
                br(),
                box(
                  
                  highchartOutput("ML"),
                  width=12
                  
                ) ,
                
                fluidRow(
                  
                  column(12, 
                         
                         #type of industry and its preferred tools
                         h3("ML Techniques most excited about learning in next year in different Industries",align="center") ,
                         br(),
                         
                         box(
                           
                           selectInput("industry3",label="Select Industry",
                                       choices=TopIndustry[,1]), 
                           
                           width=6
                         ),
                         #country select
                         box(
                           
                           selectInput("country3",label="Select Country",
                                       choices=countryCountApp[,1]), 
                           
                           width=6
                         ),
                         
                         #box for plots
                         box(
                           
                           highchartOutput("industryML"), 
                           width=12 
                         ),
                         br(),
                         
                         h3("ML Techniques most excited about learning in next year by different job titles",align="center") ,
                         br(),
                    
                         #select job title
                         box(
                           
                           selectInput("jobs3",label="Select Job title",
                                       choices=jobs[,1]), 
                           
                           width=6
                         ),
                         
                         #country select
                         box(
                           
                           selectInput("industry4",label="Select Industry",
                                       choices=TopIndustry[,1]), 
                           
                           width=6
                         ),
                         
                         #box for plots
                         box(
                           
                           highchartOutput("JobML"), 
                           width=12 
                         )
                  ) #end column
                  
                )#end fluidRow
                
              ), #end tabItem

        
        #tab5
        tabItem(tabName ="tab5",
                h3("What is used at work?",align="center")
                
        )
        
        
                
        )#end tabitems 
               
      
    )#end dashboard body
    
)#end dashboard page


