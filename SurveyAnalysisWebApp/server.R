require(highcharter)
require(dplyr)
require(tidyr)
require(shiny)
require(shinydashboard)
require(data.table)
require(stringr)

#reading the dataset
SurveyDf<-fread("multipleChoiceResponses.csv") #for faster data reading

attach(SurveyDf)


#country count data frame in descending order-top 20
countryCountApp<-as.data.frame(table(SurveyDf$Country)) %>%  top_n(20) %>% 
  arrange(desc(Freq))

colnames(countryCountApp)<-c("Country","Frequency")





#tools df
toolApp<-as.data.frame(table(MLToolNextYearSelect)) %>% arrange(desc(Freq))
#let's remove missing value
toolApp[1,]<-NA
toolApp<-na.omit(tooldf)
names(toolApp)<-c("Tool","Count")


datadf<-SurveyDf %>% select(EmployerIndustry,WorkDataTypeSelect,CurrentJobTitleSelect) %>% 
  filter(EmployerIndustry=="Technology",CurrentJobTitleSelect=="Data Scientist") %>% 
  group_by(WorkDataTypeSelect) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = round((Count/sum(Count))*100,digits = 2)) %>% 
  arrange(desc(Percentage))


#Skill importance data frame 
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



#data frame for tool which is often used at work

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








server<-function(input,output)
{
  output$country<-renderHighchart({
    hchart(countryCountApp,hcaes(x=Country,y=Frequency),type="column",name="Count",color="purple") %>% 
      hc_exporting(enabled = TRUE) %>%
      hc_title(text="Barplot of Top 20 Countries and Count of participants",align="center") %>%
      hc_add_theme(hc_theme_elementary()) 
    
  })
  
  
  #job titles of survey participants from countries
  output$jobTitle<-renderHighchart({
    
    dfJob <-SurveyDf %>% filter(Country==input$country) %>% 
      group_by(CurrentJobTitleSelect) %>% 
      select(CurrentJobTitleSelect) %>% 
      summarise(Count=n()) %>% 
      arrange(desc(Count)) 
    
    dfJob[1,1]<-NA
      
      hchart(na.omit(dfJob),hcaes(x=CurrentJobTitleSelect,y=Count),name="Count",color="  #262626",type="column") %>%
      hc_title(text="Barplot of Current Job titles of the participants from country",align="center") %>%
      hc_exporting(enabled=TRUE) %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  
  #chart of contry grouped by the employer type in each country
  output$employer<-renderHighchart({
    
    dfEmployer<-SurveyDf %>% filter(Country==input$country) %>% 
      group_by(CurrentEmployerType) %>% 
      select(CurrentEmployerType,Country) %>% 
      summarise(Count=n()) %>% 
      top_n(15) %>% 
      arrange(desc(Count))
    
    dfEmployer[1,1]<-NA
    
    hchart(na.omit(dfEmployer),type="column",hcaes(x=CurrentEmployerType,y=Count),color="#0E2E93") %>%
      hc_title(text="Top 15 types of firms where participants were employed",align="center") %>%
      hc_exporting(enabled=TRUE) %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  #histogram of ages of participants from each country
  output$age<-renderHighchart({
    
    #filtering ages for country
    dfage<-SurveyDf %>% filter(Country==input$country) %>% 
      select(Age)
    
    
    hchart(dfage$Age,name="count",color="#FF3300") %>%
      hc_title(text="Histogram of Ages of the participants grouped by Country",align="center") %>%
      hc_exporting(enabled=TRUE) %>%
      hc_add_theme(hc_theme_elementary())
      
    
  })
  
  
  #tab-3
    output$tools<-renderHighchart({
      
      hchart(toolApp,hcaes(x=Tool,y=Count),type="column",name="Count",color="#80661A") %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Barplot of tools used by participants",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      
    })
    
   
    output$industryTools<-renderHighchart({
      
     
      
      industrydf <- SurveyDf %>% select(EmployerIndustry,MLToolNextYearSelect,Country) %>% 
        filter(EmployerIndustry==input$industry,Country==input$country1) %>% 
        group_by(MLToolNextYearSelect) %>% 
        summarise(Count = n()) %>% 
        arrange(desc(Count))
      
      industrydf[1,1]<-NA
      
      hchart(na.omit(industrydf),hcaes(x=MLToolNextYearSelect,y=Count),type="column",name="Count",color=" #539AAC") %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Tools most excited about learning next year in different industries and country ",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      
      
    })
    
    output$JobTools<-renderHighchart({
      
      JobTools <- SurveyDf %>% select(EmployerIndustry,MLToolNextYearSelect,CurrentJobTitleSelect) %>% 
        filter(CurrentJobTitleSelect==input$job,EmployerIndustry==input$industry1) %>% 
        group_by(MLToolNextYearSelect) %>% 
        summarise(Count = n()) %>% 
        arrange(desc(Count))
      
      
      
      hchart(na.omit(JobTools),hcaes(x=MLToolNextYearSelect,y=Count),type="column",name="Count",color=" #264CD9") %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Tools most excited about learning in next year by different Job positions in an Industry ",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      
    })
    
    
    
    #tools used at work place
    output$ToolsWork<-renderHighchart({
      
      WorkTools <- SurveyDf %>%  filter(CurrentJobTitleSelect==input$job2,EmployerIndustry==input$industry2) %>% 
        group_by(WorkToolsSelect) %>% 
        select(WorkToolsSelect) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>% 
        top_n(15)
      
        #removing NA values
        WorkTools[1,1]<-NA
      
      hchart(na.omit(WorkTools),hcaes(x=WorkToolsSelect,y=Count),type="column",name="Count",color="#ADD400") %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Tools used by different Job position at work ",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      
      
      
    })
    
    
    #tab-4-ML techniques
    output$ML<-renderHighchart({
      
      dfML<-SurveyDf %>% group_by(MLMethodNextYearSelect) %>% 
        summarise(Count = n()) %>% 
        arrange(desc(Count))
      
      dfML[1,1]<-NA
      names(dfML)<-c("Method","Count")
      
      hchart(na.omit(dfML),hcaes(x=Method,y=Count),type="column",name="Count",color="#82BBAA") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Barplot of ML Method used by participants",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      })
    
    
    #indstry and ML
    output$industryML<-renderHighchart({
      
      industryMLdf <- SurveyDf %>% select(EmployerIndustry,MLMethodNextYearSelect,Country) %>% 
        filter(EmployerIndustry==input$industry3,Country==input$country3) %>% 
        group_by(MLMethodNextYearSelect) %>% 
        summarise(Count = n()) %>% 
        arrange(desc(Count))
      
      #industryd[1,1]<-NA
      
      hchart(na.omit(industryMLdf),hcaes(x=MLMethodNextYearSelect,y=Count),type="column",name="Count",color=" #3123B5") %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="ML methods most excited about learning next year",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
        
      
    })
    
    
    output$JobML<-renderHighchart({
      
      JobMLdf <- SurveyDf %>% select(EmployerIndustry,MLMethodNextYearSelect,CurrentJobTitleSelect) %>% 
        filter(CurrentJobTitleSelect==input$jobs3, EmployerIndustry==input$industry4) %>% 
        group_by(MLMethodNextYearSelect) %>% 
        summarise(Count = n()) %>% 
        arrange(desc(Count))
      
      hchart(na.omit(JobMLdf),hcaes(x=MLMethodNextYearSelect,y=Count),type="column",name="Count",color="#BD1491") %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="ML methods most excited about learning next year by different Jobs titles",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      
    })
    
    #competent ML methods
    output$EasyML<-renderHighchart({
      
      MLdfeasy <- SurveyDf %>% select(EmployerIndustry,MLTechniquesSelect,CurrentJobTitleSelect) %>% 
        filter(CurrentJobTitleSelect==input$jobs4, EmployerIndustry==input$industry5) %>% 
        group_by(MLTechniquesSelect) %>% 
        summarise(Count = n()) %>% 
        arrange(desc(Count)) %>% 
        top_n(10)
      
      hchart(na.omit(MLdfeasy),hcaes(x=MLTechniquesSelect,y=Count),type="column",name="Count",color="#574C9B") %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="ML techniques in which participnts consider themselves most competent",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      
      
    })
    
    output$Skillimportance<-renderHighchart({
      
      skilldf<-SkillImportance %>% filter(Skill %in% input$skill)
      
      colors<-c("red", "blue", "green")
        
      hchart(skilldf,hcaes(x=importance,y=count),type="funnel",name="Count",color=colors) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Funnel Chart of importance of skill",align="center") %>%
        hc_add_theme(hc_theme_ffx())
      
    })
    
    
    #tab-6 work data
    output$Datatype<-renderHighchart({
      
      datadf<-SurveyDf %>% select(EmployerIndustry,WorkDataTypeSelect,CurrentJobTitleSelect) %>% 
        filter(EmployerIndustry==input$industry6,CurrentJobTitleSelect==input$job6) %>% 
        group_by(WorkDataTypeSelect) %>% 
        summarise(Count = n()) %>% 
        mutate(Percentage = round((Count/sum(Count))*100,digits = 2)) %>% 
        arrange(desc(Percentage))
      
      hchart(datadf,hcaes(x=WorkDataTypeSelect,y=Percentage),type="column",name="Percentage % ") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Bar plot of type of data used",align="center") %>%
        hc_add_theme(hc_theme_ffx())
      
      
    })
    
    #bar chart for size of data set used at work in different industries by different job positions
    output$DatasetSize<-renderHighchart({
      
      datasizedf<-SurveyDf %>% select(EmployerIndustry,WorkDatasetSize,CurrentJobTitleSelect) %>% 
        filter(EmployerIndustry==input$industry7,CurrentJobTitleSelect==input$job7) %>% 
        group_by(WorkDatasetSize) %>% 
        summarise(Count = n()) %>%
        mutate(Percentage = round((Count/sum(Count))*100,digits = 2)) %>% 
        arrange(desc(Percentage))
      
      hchart(datasizedf,hcaes(x=WorkDatasetSize,y=Percentage),type="column",name="Percentage % ",color="#3B22AE") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Bar plot of size of dataset used",align="center") %>%
        hc_add_theme(hc_theme_ffx())
      
      
      
      
      
    })
    
    
    #pie chart for how often a tool is used at work
    output$WorkToolUsed<-renderHighchart({
      
      WorkToolOftendf<-WorkTool %>% filter(WorkTools %in% input$workTool)
      
      colors<-c("#20C200", "#22BBE8", "#E83122","#C722E8")
      
      hchart(WorkToolOftendf,hcaes(x=Used,y=count),type="pie",name="Count",color=colors) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Pie Chart of how often a tool is used at work",align="center") %>% 
        hc_add_theme(hc_theme_ffx())
      
      
    })
    
    
    #pie chart of how often an ML technique is used at work
    output$WorkMethodUsed<-renderHighchart({
      
      WorkMethodOftendf<-WorkMethod %>% filter(WorkMethod %in% input$MLoften)
      
      colors<-c("#20C200", "#22BBE8", "#E83122","#C722E8")
      
      hchart(WorkMethodOftendf,hcaes(x=Used,y=count),type="pie",name="Count",color=colors) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Pie Chart of how often a tool is used at work",align="center") %>% 
        hc_add_theme(hc_theme_ffx())
      
      
    })
    
    
    #what is the type of Data storage at work
    output$DataStorageWork<-renderHighchart({
      
      DataStoragedf<-SurveyDf %>% select(EmployerIndustry,WorkDataStorage) %>% 
        filter(EmployerIndustry==input$industry8) %>% 
        group_by(WorkDataStorage) %>% 
        summarise(Count = n()) %>%
        mutate(Percentage = round((Count/sum(Count))*100,digits = 2)) %>% 
        top_n(15) %>% 
        arrange(desc(Percentage))
      
      DataStoragedf[1,]<-NA
      
      hchart(na.omit(DataStoragedf),hcaes(x=WorkDataStorage,y=Percentage),type="column",name="Percentage % ",color="#C70039") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Bar plot what type of data storge is used at work",align="center") %>%
        hc_add_theme(hc_theme_ffx())
      
      
    })
}