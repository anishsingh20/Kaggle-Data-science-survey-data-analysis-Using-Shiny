require(highcharter)
require(dplyr)
require(tidyr)
require(shiny)
require(shinydashboard)
require(data.table)

#reading the dataset
SurveyDf<-fread("multipleChoiceResponses.csv") #for faster data reading

attach(SurveyDf)


#country count data frame in descending order
countryCountApp<-as.data.frame(table(SurveyDf$Country)) %>%  top_n(20) %>% 
  arrange(desc(Freq))

colnames(countryCountApp)<-c("Country","Frequency")




server<-function(input,output)
{
  output$country<-renderHighchart({
    hchart(countryCountApp,hcaes(x=Country,y=Frequency),type="column",name="Count",color="purple") %>% 
      hc_exporting(enabled = TRUE) %>%
      hc_title(text="Barplot of Country and Count of participants",align="center") %>%
      hc_add_theme(hc_theme_elementary()) 
    
  })
}