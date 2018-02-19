require(highcharter)
require(dplyr)
require(tidyr)
require(shiny)
require(shinydashboard)
require(data.table)

#reading the dataset
SurveyDf<-fread("multipleChoiceResponses.csv") #for faster data reading

attach(SurveyDf)


#country count data frame in descending order-top 20
countryCountApp<-as.data.frame(table(SurveyDf$Country)) %>%  top_n(20) %>% 
  arrange(desc(Freq))

colnames(countryCountApp)<-c("Country","Frequency")


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
      
      hchart(dfJob,hcaes(x=CurrentJobTitleSelect,y=Count),name="Count",color="  #262626",type="column") %>%
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
    
    hchart(na.omit(dfEmployer),type="column",hcaes(x=CurrentEmployerType,y=Count),color="#0E2E93") %>%
      hc_title(text="Top 15 types of firms where participants from were employed",align="center") %>%
      hc_exporting(enabled=TRUE) %>%
      hc_add_theme(hc_theme_elementary())
    
  })
}