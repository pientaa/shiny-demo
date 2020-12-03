library(shiny)
library(readxl)
library(summarytools)
library(gtsummary)
library(tidyverse)
library(dplyr)
library(knitr)
library(DT)
library(ggplot2)
library(reshape2)
library(gganimate)
library(hrbrthemes)
library( rgl )
library(magick)
library(tibble)
library(caret)

df <- read_excel("wuhan_blood_sample_data_Jan_Feb_2020.xlsx")

patients_df <- df %>% group_by(`Admission time`, `Discharge time`, gender, age, outcome) %>%
  summarise(PATIENT_ID = sum(PATIENT_ID, na.rm = TRUE), `Total records` = n()) 

patients_df <- patients_df %>%
  mutate(`Days in hospital` = ceiling(difftime(`Discharge time`, `Admission time`, units = "days")))

df <- full_join(patients_df %>% ungroup() %>% select(`Admission time`, PATIENT_ID), df %>% select(-PATIENT_ID), by="Admission time")

df$gender<-ifelse(df$gender==1, 'Male', 'Female') 
df <- df %>% mutate(gender = as.factor(gender))

patients_df$gender<-ifelse(patients_df$gender==1, 'Male', 'Female') 
patients_df <- patients_df %>% mutate(gender = as.factor(gender))

df$outcome<-ifelse(df$outcome==1, 'Death', 'Survival')
df <- df %>% mutate(outcome = as.factor(outcome))

patients_df$outcome<-ifelse(patients_df$outcome==1, 'Death', 'Survival')
patients_df <- patients_df %>% mutate(outcome = as.factor(outcome))

patients_df <- df %>% group_by(PATIENT_ID, outcome, `Admission time`, `Discharge time`, gender, age) %>% summarise(missing_test=is.na(RE_DATE)) %>%
  group_by(PATIENT_ID, outcome, `Admission time`, `Discharge time`, gender, age) %>%
  summarise(test_provided = ifelse(missing_test, 0, 1)) %>%
  group_by(PATIENT_ID, outcome, `Admission time`, `Discharge time`, gender, age) %>%
  summarise(`Total blood tests`=sum(test_provided)) %>%
  mutate(`Days in hospital` = ceiling(difftime(`Discharge time`, `Admission time`, units = "days")))

last_test_df <- df %>% 
  group_by(PATIENT_ID, outcome, gender, age) %>% 
  fill_(names(df)) %>% 
  fill_(names(df), "up") %>% 
  summarise_at(vars(`Hypersensitive cardiac troponinI`:creatinine), function(x) last(x,order_by = is.na(x)))


cleaned_df <- last_test_df %>% ungroup() %>% select(outcome, `neutrophils(%)`, `Lactate dehydrogenase`, `High sensitivity C-reactive protein`) %>% filter_all(function(x) !is.na(x))  %>%
  rename(CRP=`High sensitivity C-reactive protein`, LDH=`Lactate dehydrogenase`, NEUTR= `neutrophils(%)`)

set.seed(23)
inTraining <- 
  createDataPartition(
    y = cleaned_df$outcome,
    p = .70,
    list = FALSE)


training <- cleaned_df[ inTraining,]
testing  <- cleaned_df[-inTraining,]

rfGrid <- expand.grid(mtry = 10:30)
ctrl <- trainControl(
  method = "repeatedcv",
  classProbs = TRUE,
  number = 2,
  repeats = 5)

set.seed(23)
fit <- train(outcome ~ .,
             data = training,
             method = "rf",
             metric = "ROC",
             preProc = c("center", "scale"),
             trControl = ctrl,
             tuneGrid = rfGrid,
             ntree = 30)
rfClasses <- predict(fit, newdata = testing)
confusionMatrix(data = rfClasses, testing$outcome)

knit_hooks$set(webgl = hook_webgl)
webgl=TRUE

shinyServer(
  function(input, output) {
      
      output$prediction <- renderText({
        CRP <- input$CRP
        LDH <-input$LDH
        NEUTR <- input$NEUTR
        new_data <- data.frame(NEUTR, LDH, CRP)
      
        paste("Percentage chances of patient's survival:  ", predict(fit, newdata=new_data,  type = "prob")$Survival * 100)
        
        }) 
      
      getPage<-function() {
        try(rgl.close())
        CRP <- input$CRP
        LDH <-input$LDH
        NEUTR <- input$NEUTR
        color <- 'yellow'
        new_data <- data.frame(NEUTR, LDH, CRP)
        outcome <- predict(fit, newdata=new_data)
        size <- 16
        new_data <- data.frame(outcome, NEUTR, LDH, CRP, color, size)
        
        corr_visualize_df  <- cleaned_df
        mycolors <- c('royalblue1', 'darkcyan')
        corr_visualize_df$color <- mycolors[ as.numeric(corr_visualize_df$outcome) ]
        corr_visualize_df$size <- 6
        corr_visualize_df <- rbind(corr_visualize_df, new_data)
        mycolors <- c('royalblue1', 'darkcyan', 'yellow')
        par(mar=c(0,0,0,0))
        plot3d( 
          x=corr_visualize_df$CRP, y=corr_visualize_df$LDH, z=corr_visualize_df$NEUTR, 
          col = corr_visualize_df$color, 
          type = 's', 
          radius = corr_visualize_df$size,
          legend=TRUE,
          xlab="CRP", ylab="LDH", zlab="Neutr")
        
        legend3d("topright", legend = c('Death', 'Survival', 'New patient'), pch = 10, col = mycolors, cex=0.8, inset=c(0.02))
        
        writeWebGL( filename="3d_correlation_mean.html" ,  width=600, height=600)
        
        rgl.close()
        return(includeHTML("./3d_correlation_mean.html"))
      }
      
      output$ui <- renderUI({getPage()})
  }
)