library(shiny)
library(tidyr)
library(randomForest)
library(readxl)
library(dplyr)
library(magick)
library(tibble)
library(caret)
library(e1071)
library(plotly)

df <- read_excel("wuhan_blood_sample_data_Jan_Feb_2020.xlsx")
df <- df %>% select(-c(34,37,68))

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
             preProc = c("center", "scale"),
             trControl = ctrl,
             ntree = 30)
rfClasses <- predict(fit, newdata = testing)
confusionMatrix(data = rfClasses, testing$outcome)

shinyServer(
  function(input, output) {
      
      output$prediction <- renderText({
        CRP <- input$CRP
        LDH <-input$LDH
        NEUTR <- input$NEUTR
        new_data <- data.frame(NEUTR, LDH, CRP)
      
        paste("Chance of Survival: ", round(predict(fit, newdata=new_data,  type = "prob")$Survival * 100, digits = 0), "%")
        }) 
      
      output$plot <- renderPlotly({
        CRP <- input$CRP
        LDH <-input$LDH
        NEUTR <- input$NEUTR
        color <- 'yellow'
        new_data <- data.frame(NEUTR, LDH, CRP)
        outcome <- predict(fit, newdata=new_data)
        size <- 40
        new_data <- data.frame(outcome, NEUTR, LDH, CRP, color, size)

        corr_visualize_df  <- cleaned_df
        mycolors <- c('royalblue1', 'darkcyan')
        corr_visualize_df$color <- mycolors[ as.numeric(corr_visualize_df$outcome) ]
        corr_visualize_df$size <- 20
        corr_visualize_df <- rbind(corr_visualize_df, new_data)
        mycolors <- c('royalblue1', 'darkcyan', 'yellow')
        
        par(mar=c(0,0,0,0))
        
        plot1 <- plot_ly(x = corr_visualize_df$CRP,
                         y = corr_visualize_df$LDH,
                         z = corr_visualize_df$NEUTR) %>%
          add_markers(color = ~ corr_visualize_df$outcome,
                      size = ~ corr_visualize_df$size,
          ) %>%
          layout(scene = list(
            xaxis = list(title = "CRP"),
            yaxis = list(title = "LDH"),
            zaxis = list(title = "NEUTR"),
            legend = list(orientation = 'h')
          ))
        
          })

      
  }
)