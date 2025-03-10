library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(rsample)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(modelr)
library(scatterplot3d)

studenti<-read.csv("student.csv")

studenti_new<-studenti 

studenti_new <- studenti_new %>%
  mutate(studenti_new,average_score=(math_score+history_score+physics_score+chemistry_score+biology_score+english_score+geography_score)/7)
print(studenti_new)

studenti_clear<- studenti_new
studenti_clear<- select(studenti_clear,-(math_score:geography_score))

studenti_clear<- select (studenti_clear, -(first_name:email))
studenti_clear<-select(studenti_clear,-id)

studenti_clear %>%
select_if(is.numeric) 

studenti_clear$career_aspiration<-as.factor(studenti_clear$career_aspiration)

studenti_clear$gender<-as.factor(studenti_clear$gender)

studenti_clear|>
  select_if(is.numeric) |>
  gather (metric, value) |>
  ggplot(aes(value,fill=metric))+
  geom_density(show.legend = FALSE)+
  facet_wrap(~metric, scales="free")


studenti_clear %>%
  ggplot(aes(weekly_self_study_hours, average_score)) + geom_point() + geom_smooth()

studenti_clear %>%
  ggplot(aes(absence_days, average_score)) + 
  geom_point() + 
  geom_smooth() +
  scale_x_continuous(breaks = 1:10)

studenti_clear %>%
  ggplot(aes(gender, average_score)) + geom_point() + geom_smooth()

proportie_antrenament <- 0.7 

studenti_split <- initial_split(studenti_clear, prop = proportie_antrenament, strata = "average_score")

studenti_antrenament <- training(studenti_split)
studenti_test <- testing(studenti_split)

#REGRESIE weekly_self_study_hours

mod_score_study <- lm(data = studenti_antrenament, average_score ~ weekly_self_study_hours)
summary(mod_score_study)  

grid_score_study <- studenti_antrenament %>%  
  data_grid(weekly_self_study_hours = seq_range(weekly_self_study_hours, 100)) %>%
  add_predictions(mod_score_study, "average_score") 

ggplot(studenti_antrenament, aes(weekly_self_study_hours, average_score)) + 
  geom_point() +  
  geom_line(data = grid_score_study, color = "red", size = 1) +  
  labs(x = "Numărul de ore de studiu pe săptămână", y = "Nota medie obținută de fiecare student") +  
  ggtitle("Relația dintre numărul de ore de studiu și nota medie") 

confint(mod_score_study) 


# REGRESIE absence_days

mod_absence_score <- lm(data = studenti_antrenament, average_score ~ absence_days)
summary(mod_absence_score)  

grid_absence_score <- studenti_antrenament %>%  
  data_grid(absence_days = seq_range(absence_days, 100)) %>%
  add_predictions(mod_absence_score, "average_score")  

ggplot(studenti_antrenament, aes(absence_days, average_score)) + 
  geom_point() +  
  geom_line(data = grid_absence_score, color = "blue", size = 1) +  
  labs(x = "Zile absentate", y = "Nota medie obținută de fiecare student") +  
  ggtitle("Relația dintre numărul de zile absentate și nota medie") +
  scale_x_continuous(breaks = seq(0, 10, by = 1))  

confint(mod_absence_score) 

# REGRESIE gender

mod_gender_score <- lm(data = studenti_antrenament, average_score ~ gender)
summary(mod_gender_score)  

grid_gender_score <- studenti_antrenament %>%
  data_grid(gender = unique(studenti_antrenament$gender)) %>%  
  add_predictions(mod_gender_score, "average_score")  

ggplot(studenti_antrenament, aes(x = gender, y = average_score)) +
  geom_point() +  
  geom_boxplot(fill = "skyblue", alpha = 0.5) +
  labs(x = "Gen", y = "Nota medie") +  
  ggtitle("Relația dintre gen și nota medie") +  
  scale_x_discrete(labels = c("F" = "Feminin", "M" = "Masculin"))  

confint(mod_gender_score)  

# REGRESIE part_time_job

mod_part_time_job <- lm(average_score ~ part_time_job, data = studenti_antrenament)

summary(mod_part_time_job)
confint(mod_part_time_job) 

ggplot(studenti_antrenament, aes(x = part_time_job, y = average_score)) +
  geom_point() +  
  geom_boxplot(fill = "skyblue", alpha = 0.5) +
  labs(x = "Part-time-job", y = "Nota medie") +  
  ggtitle("Relația dintre existența unui Part-time-job și nota medie") +  
  scale_x_discrete(labels = c("FALSE" = "Fără job part-time", "TRUE" = "Cu job part-time"))  

# REGRESIE ACTIVIT EXTRA

studenti_antrenament$extracurricular_activities_dummy <- ifelse(studenti_antrenament$extracurricular_activities == "TRUE", 1, 0)

model <- lm(average_score ~ extracurricular_activities_dummy, data = studenti_antrenament)

summary(model)

grid_extra <- studenti_antrenament %>%  
  data_grid(extracurricular_activities_dummy = seq_range(extracurricular_activities_dummy, 100)) %>%
  add_predictions(model, "average_score")  

ggplot(studenti_antrenament, aes(x = extracurricular_activities, y = average_score)) +
  geom_point() +  
  geom_boxplot(fill = "skyblue", alpha = 0.5) +  
  labs(x = "Activități extracurriculare", y = "Nota medie") +  
  ggtitle("Relația dintre activitățile extracurriculare și nota medie") +  
  scale_x_discrete(labels = c("FALSE" = "Fără implicare în activități", "TRUE" = "Cu implicare în activități")) +
  scale_y_continuous(limits = c(62, 100))  

confint(mod_absence_score) 

# REGRESIE career_aspiration

model_regresie <- lm(average_score ~ career_aspiration, data = studenti_antrenament)

summary(model_regresie)

ggplot(studenti_antrenament, aes(x = factor(career_aspiration), y = average_score)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  labs(x = "Aspirația de carieră", y = "Nota medie") +
  ggtitle("Relația dintre aspirația de carieră și nota medie") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# REGRESIE ore + carieră + part_time_job + absence_days

mod_ore_cariera <- lm(data = studenti_antrenament, average_score ~ weekly_self_study_hours + career_aspiration + part_time_job + absence_days)
summary(mod_ore_cariera)

# REGRESIE MARE

mod_multi_regression <- lm(average_score ~ ., data = studenti_antrenament)

summary(mod_multi_regression)

predictii_antrenament <- predict(mod_ore_cariera, newdata = studenti_antrenament)
erori_antrenament <- studenti_antrenament$average_score - predictii_antrenament
mse_antrenament <- mean(erori_antrenament^2)
rmse_antrenament <- sqrt(mse_antrenament)

print(paste("Root Mean Squared Error (RMSE) pentru setul de antrenament:", rmse_antrenament))

predictii_test <- predict(mod_ore_cariera, newdata = studenti_test)
erori_test <- studenti_test$average_score - predictii_test
mse_test <- mean(erori_test^2)
rmse_test <- sqrt(mse_test)

print(paste("Root Mean Squared Error (RMSE) pentru setul de test:", rmse_test))

# PREDICȚII

predict(mod_ore_cariera, newdata = studenti_test, interval = "confidence")

predict(mod_ore_cariera, newdata = studenti_test, interval = "prediction")


#ARBORI DE DECIZIE
studenti_clear <- studenti_clear %>%
  rename(self_study_hw = weekly_self_study_hours)

set.seed(123)
studenti_split<-initial_split(studenti_clear, prop=0.7)
studenti_train<-training(studenti_split)
studenti_test<-testing(studenti_split)

m1 <- rpart(
  formula = average_score ~ self_study_hw+career_aspiration, 
  data = studenti_train,
  method = "anova"
)

m1
rpart.plot(m1)
plotcp(m1)
m1$cptable


m2 <- rpart(
  formula = average_score ~ self_study_hw+career_aspiration, 
  data = studenti_train,
  method = "anova",
  control = list(cp = 0, xval = 10)  
)  
rpart.plot(m2)
plotcp(m2)
m2$cptable
abline(v = 4, lty = "dashed") 


m3 <- rpart(
  formula = average_score ~ self_study_hw+career_aspiration, 
  data = studenti_train, 
  method = "anova",
  control = list(minsplit = 10, maxdepth = 5, xval = 10)
)
m3
rpart.plot(m3)
plotcp(m3)

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(5, 9, 1)
)
head(hyper_grid)
models <- list()
for (i in 1:nrow(hyper_grid)) {
  
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = average_score ~ self_study_hw+career_aspiration, 
    data = studenti_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )  
mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_tree <- rpart(
  formula = average_score ~ self_study_hw+career_aspiration,
  data = studenti_train,
  method = "anova",
  control = list(minsplit = 11, maxdepth = 6, cp = 0.0108982)
)

pred <- predict(m1, newdata = studenti_test)
RMSE(pred = pred, obs = studenti_test$average_score)
optimal_tree


#BAGGING
library(ipred)
set.seed(123)

bagged_m1 <- bagging(
  formula = average_score ~ self_study_hw+career_aspiration,
  data = studenti_train,
  coob = TRUE
)
bagged_m1

ntree <- 10:50
rmse <- vector(mode = "numeric", length = length(ntree))
for (i in seq_along(ntree)) {
  set.seed(123)
  model <- bagging(
    formula = average_score ~ self_study_hw+career_aspiration,
    data = studenti_train,
    coob = TRUE,
    nbagg = ntree[i]
  )
  rmse[i] = model$err
}
plot(ntree, rmse, type ="l", lwd=2)


#Bagging with CARET
fitControl <- trainControl(
  method = "cv",
  number = 10
)
bagged_cv <- train(
  average_score ~ self_study_hw+career_aspiration,
  data = studenti_train,
  method = "treebag",
  trControl = fitControl,
  importance = TRUE
)
bagged_cv
plot(varImp(bagged_cv), 20)

pred <- predict(bagged_cv, studenti_test)
RMSE(pred, studenti_test$average_score)

for_plotting <- tibble(
  i = 1:600,
  pred = pred[],
  actual = studenti_test$average_score
)

ggplot(for_plotting, aes(x=i)) +
  geom_point(aes(y = pred, color = "red")) +
  geom_point(aes(y = actual, color = "blue"))

ggplot(for_plotting, aes(x=i)) + 
  geom_point(aes(y = pred-actual))


#random forest
set.seed(123)
library(randomForest)
m1_rf <- randomForest(
  formula = average_score ~ self_study_hw+career_aspiration,
 data = studenti_train
)
m1_rf
plot(m1_rf)
m1_rf$mse
which.min(m1_rf$mse)
sqrt(m1_rf$mse[which.min(m1_rf$mse)])
