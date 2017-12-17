require(data.table)
require(highcharter)
require(ggplot2)
require(tidyverse)

SurveyDf<-fread("../Datasets/kagglesurvey2017/multipleChoiceResponses.csv") #for faster data reading

attach(SurveyDf) #attaching the dataset
