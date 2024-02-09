# Recidivism bar plot

# set up ----
{
  library(lubridate)
  library(tidyverse)
  library(caret)
  library(kableExtra)
  library(ModelMetrics)
  library(plotROC)
  library(knitr)
  library(grid)
  library(gridExtra)
  library(QuantPsyc)
  
  set.seed(712)
  
  root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
  source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
  
  palette_3_colors <- c("#f54281","#f57842","#f5da42")
}

{
  raw_data <- read.csv(file.path(root.dir,"Chapter7/compas-scores-two-years.csv"))
  
  df <- 
    raw_data %>%
    filter(days_b_screening_arrest <= 30) %>%
    filter(days_b_screening_arrest >= -30) %>%
    filter(is_recid != -1) %>%
    filter(c_charge_degree != "O") %>%
    filter(priors_count != "36") %>%
    filter(priors_count != "25") %>%
    mutate(length_of_stay = as.numeric(as.Date(c_jail_out) - as.Date(c_jail_in)),
           priors_count = as.factor(priors_count),
           Recidivated = as.factor(ifelse(two_year_recid == 1,"Recidivate","notRecidivate")),
           recidivatedNumeric = ifelse(Recidivated == "Recidivate", 1, 0),
           race2 = case_when(race == "Caucasian"        ~ "Caucasian",
                             race == "African-American" ~ "African-American", 
                             TRUE                       ~ "Other")) %>%
    dplyr::select(sex,age,age_cat,race,race2,priors_count,two_year_recid,r_charge_desc,
                  c_charge_desc,c_charge_degree,r_charge_degree,juv_other_count,
                  length_of_stay,priors_count,Recidivated,recidivatedNumeric) %>%
    filter(priors_count != 38)
}

{
  train <- df %>% dplyr::sample_frac(.75)
  train_index <- as.numeric(rownames(train))
  test <- df[-train_index, ]
}

{
  reg.noRace <- glm(Recidivated ~ ., data = 
                      train %>% dplyr::select(sex, age, age_cat,
                                              juv_other_count, length_of_stay, 
                                              priors_count, Recidivated),
                    family = "binomial"(link = "logit"))
  
}

{
  testProbs <- 
    data.frame(class = test$recidivatedNumeric,
               probs = predict(reg.noRace, test, type = "response"),
               Race = test$race2)
}

{
  iterateThresholds <- function(data, observedClass, predictedProbs, group) {
    observedClass <- enquo(observedClass)
    predictedProbs <- enquo(predictedProbs)
    group <- enquo(group)
    x = .01
    all_prediction <- data.frame()
    
    if (missing(group)) {
      
      while (x <= 1) {
        this_prediction <- data.frame()
        
        this_prediction <-
          data %>%
          mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
          count(predclass, !!observedClass) %>%
          summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                    Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                    Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                    Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                    Rate_TP = Count_TP / (Count_TP + Count_FN),
                    Rate_FP = Count_FP / (Count_FP + Count_TN),
                    Rate_FN = Count_FN / (Count_FN + Count_TP),
                    Rate_TN = Count_TN / (Count_TN + Count_FP),
                    Accuracy = (Count_TP + Count_TN) / 
                      (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
          mutate(Threshold = round(x,2))
        
        all_prediction <- rbind(all_prediction,this_prediction)
        x <- x + .01
      }
      return(all_prediction)
    }
    else if (!missing(group)) { 
      while (x <= 1) {
        this_prediction <- data.frame()
        
        this_prediction <-
          data %>%
          mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
          group_by(!!group) %>%
          count(predclass, !!observedClass) %>%
          summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                    Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                    Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                    Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                    Rate_TP = Count_TP / (Count_TP + Count_FN),
                    Rate_FP = Count_FP / (Count_FP + Count_TN),
                    Rate_FN = Count_FN / (Count_FN + Count_TP),
                    Rate_TN = Count_TN / (Count_TN + Count_FP),
                    Accuracy = (Count_TP + Count_TN) / 
                      (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
          mutate(Threshold = round(x, 2))
        
        all_prediction <- rbind(all_prediction, this_prediction)
        x <- x + .01
      }
      return(all_prediction)
    }
  }
}

{
  testProbs.thresholds <- 
    iterateThresholds(data=testProbs, observedClass = class, 
                      predictedProbs = probs, group = Race) %>% 
    rename(`LR, Rec.` = Rate_FN,
           `HR, did not Rec.` = Rate_FP,
           `LR, did not Rec.` = Rate_TN,
           `HR, Rec.` = Rate_TP)
  
  thresh5 <- 
    filter(testProbs.thresholds, Threshold == .5)  %>%
    dplyr::select(matches("Accuracy|Race|Rec.")) %>%
    gather(Variable, Value, -Race) %>%
    mutate(Variable = factor(Variable, levels = c("Accuracy","LR, Rec.","LR, did not Rec.","HR, Rec.","HR, did not Rec."))) %>% 
    ggplot(aes(Variable, Value, fill = Race)) +
    geom_bar(aes(fill = Race), position = "dodge", stat = "identity") +
    scale_fill_manual(values = palette_3_colors) +
    labs(title="Figure 1. Revidivation Rate by Race",
         subtitle = "50% Likelihood as Cutoff for High Risk", 
         x = "Outcome",y = "Rate",
         caption = "LR = Low Risk; HR = High Risk; Rec. = Recidivate(d)") +
    plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # print 50% threshold plot
  # pdf("~/Documents/MUSA5080/Assignments/HW06/recidivism_50pct_threshold_barplot.pdf",height=9,width=12)
  # thresh5
  # dev.off()
  
  optimum <- testProbs.thresholds %>% 
    mutate(cost = Count_TN*-3274 - Count_TP*43716 - Count_FN*3274 - Count_FP*167218) %>% 
    arrange(cost)
  
  thresh_opt <- 
    filter(testProbs.thresholds, Threshold == 0.55)  %>%
    dplyr::select(matches("Accuracy|Race|Rec.")) %>%
    gather(Variable, Value, -Race) %>%
    mutate(Variable = factor(Variable, levels = c("Accuracy","LR, Rec.","LR, did not Rec.","HR, Rec.","HR, did not Rec."))) %>% 
    ggplot(aes(Variable, Value, fill = Race)) +
    geom_bar(aes(fill = Race), position = "dodge", stat = "identity") +
    scale_fill_manual(values = palette_3_colors) +
    labs(title="Figure 2. Revidivation Rate by Race",
         subtitle = "55% Likelihood as Cutoff for High Risk", 
         x = "Outcome",y = "Rate",
         caption = "LR = Low Risk; HR = High Risk; Rec. = Recidivate(d)") +
    plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # print optimum threshold plot
  # pdf("~/Documents/MUSA5080/Assignments/HW06/recidivism_optimumpct_threshold_barplot.pdf",height=9,width=12)
  # thresh_opt
  # dev.off()
}



