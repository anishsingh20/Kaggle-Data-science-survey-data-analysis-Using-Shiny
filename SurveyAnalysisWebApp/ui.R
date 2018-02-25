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



#tidying the data frame

SkillImportance<-SurveyDf %>% select(JobSkillImportanceR,JobSkillImportanceKaggleRanking,
  JobSkillImportanceMOOC,JobSkillImportancePython,
  JobSkillImportanceEnterpriseTools,JobSkillImportanceSQL,JobSkillImportanceStats,
  JobSkillImportanceVisualizations, 
  JobSkillImportanceBigData,JobSkillImportanceDegree) %>% 
  gather(key="Skill",value="importance")

SkillImportance<-SkillImportance %>% group_by(Skill,importance) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

SkillImportance[1:10,]<-NA

SkillImportance<-na.omit(SkillImportance)






WorkTool<-SurveyDf %>% select(WorkToolsFrequencyAmazonML,WorkToolsFrequencyAWS,WorkToolsFrequencyAzure,
                              WorkToolsFrequencyMicrosoftSQL,WorkToolsFrequencyOracle,
                              WorkToolsFrequencyMicrosoftRServer,
                              WorkToolsFrequencyExcel,WorkToolsFrequencyCloudera,WorkToolsFrequencySpark,
                              WorkToolsFrequencyHadoop,WorkToolsFrequencyIBMCognos,WorkToolsFrequencyIBMWatson,
                              WorkToolsFrequencyIBMSPSSStatistics,
                              WorkToolsFrequencyTensorFlow,WorkToolsFrequencySQL, WorkToolsFrequencyR,WorkToolsFrequencyNoSQL,
                              WorkToolsFrequencyPython,
                              WorkToolsFrequencyTableau,
                              WorkToolsFrequencySASEnterprise,WorkToolsFrequencySASEnterprise,
                              WorkToolsFrequencyC
                              
) %>% 
  
  gather(key="WorkTools",value="Used")


#grouping by WorkTool and How often it is used and calculating count of each and summarising data

WorkTool<-WorkTool %>% group_by(WorkTools,Used) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))


#removing NA values

WorkTool[1:21,]<-NA

WorkTool<-na.omit(WorkTool)





#data frame for ML method which is often used at work

WorkMethod<-SurveyDf %>% select(
  'WorkMethodsFrequencyA/B' ,
  WorkMethodsFrequencyAssociationRules ,
  WorkMethodsFrequencyBayesian ,
  WorkMethodsFrequencyCNNs ,
  WorkMethodsFrequencyCollaborativeFiltering ,
  'WorkMethodsFrequencyCross-Validation'  ,
  WorkMethodsFrequencyDataVisualization ,
  WorkMethodsFrequencyDecisionTrees ,
  WorkMethodsFrequencyEnsembleMethods,
  WorkMethodsFrequencyEvolutionaryApproaches ,
  WorkMethodsFrequencyGANs,
  WorkMethodsFrequencyGBM,
  WorkMethodsFrequencyHMMs,
  WorkMethodsFrequencyKNN,
  WorkMethodsFrequencyLiftAnalysis,
  WorkMethodsFrequencyLogisticRegression,
  WorkMethodsFrequencyMLN,
  WorkMethodsFrequencyNaiveBayes,
  WorkMethodsFrequencyNLP,
  WorkMethodsFrequencyNeuralNetworks,
  WorkMethodsFrequencyPCA,
  WorkMethodsFrequencyPrescriptiveModeling,
  WorkMethodsFrequencyRandomForests,
  WorkMethodsFrequencyRecommenderSystems,
  WorkMethodsFrequencyRNNs,
  WorkMethodsFrequencySegmentation,
  WorkMethodsFrequencySimulation,
  WorkMethodsFrequencySVMs,
  WorkMethodsFrequencyTextAnalysis,
  WorkMethodsFrequencyTimeSeriesAnalysis) %>% 
  
  gather(key="WorkMethod",value="Used") 


WorkMethod<-WorkMethod %>% group_by(WorkMethod,Used) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))


#removing NA values

WorkMethod[1:30,]<-NA

WorkMethod<-na.omit(WorkMethod)









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
        menuItem("Skill importance", tabName = "tab5"),
        menuItem("Work", tabName = "tab6")
        
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
                         ) ,
                         
                         br(),
                         
                         h3("Techniques in which participants consider themselves competent",align="center"),
                         #box for ML competent plot
                         box(
                           
                           selectInput("jobs4",label="Select Job title",
                                       choices=jobs[,1]), 
                           
                           width=6
                         ),
                         
                         #country select
                         box(
                           
                           selectInput("industry5",label="Select Industry",
                                       choices=TopIndustry[,1]), 
                           
                           width=6
                         ),
                         box(
                           
                           highchartOutput("EasyML"), 
                           width=12 
                         )
                  ) #end column
                  
                )#end fluidRow
                
              ), #end tabItem

  
          #tab 5 -skills importance
          tabItem(
            tabName ="tab5",
                  h3("Which Skills are importanct for getting a data science job",align="center"),
                  br(),
            
              fluidRow(
                
                column(12,
                       
                  box(
                    selectInput("skill",label="Select type of skill",
                                choices=unique(SkillImportance[,1])) ,
                    width=12
                    
                  ),#end box
                  
                  #pie chart of skill importance
                  box(
                    
                    highchartOutput("Skillimportance"), 
                    width=12 
                  )#end box
                  
                )#end column
                
              )#end fluid row
                  
          ) ,#end tab Item 5


          #tab 6
          tabItem(
                  tabName ="tab6",
                  h3("What type of data is used at work?",align="center"),
                  fluidRow(
                    
                    column(12,
                           
                           box(
                             selectInput("industry6",label="Select industry",
                                         choices=TopIndustry[,1]) ,
                             width=6
                             
                           ),#end box
                           box(
                             selectInput("job6",label="Select Job position",
                                         choices=jobs[,1]) ,
                             width=6
                             
                           ),
                           
                           #pie chart of most used data at work
                           box(
                             
                             highchartOutput("Datatype"), 
                             width=12 
                           ) ,#end box 
                           br(),
                           
                           h3("What is the size of dataset used at work",align="center"),
                           br(),
                           
                           
                           #work data set size
                           box(
                             selectInput("industry7",label="Select industry",
                                         choices=TopIndustry[,1]) ,
                             width=6
                             
                           ),#end box
                           box(
                             selectInput("job7",label="Select Job position",
                                         choices=jobs[,1]) ,
                             width=6
                             
                           ),
                          
                           #chart of work dataset size
                           box(
                             
                             highchartOutput("DatasetSize"), 
                             width=12 
                           ) ,#end box 
                           
                           br(),
                           
                           h3("What tool is used at work often",align="center"),
                           
                           br(),
                           
                           
                           #work tool frequency 
                           box(
                             selectInput("workTool",label="Select Tools used at work",
                                         choices=unique(WorkTool[,1])) ,
                             width=12
                             
                           ),
                           
                           #pie chart of work tool 
                           box(
                             
                             highchartOutput("WorkToolUsed"), 
                             width=12 
                           ),#end box
                           
                           
                           #work ML method frequency 
                           box(
                             selectInput("MLoften",label="Select Ml techniques often used at work?",
                                         choices=unique(WorkMethod[,1])) ,
                             width=12
                             
                           ),
                           
                           #pie chart of work tool 
                           box(
                             
                             highchartOutput("WorkMethodUsed"), 
                             width=12 
                           )#end box
                           
                           
                    )#end column
                    
                  )
                  
                  
                  
          )


        
        
        
                
        )#end tabitems 
               
      
    )#end dashboard body
    
)#end dashboard page


