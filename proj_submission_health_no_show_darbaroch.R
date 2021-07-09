# Warning: depending on the dplyr version a friendly warning may appear when 
# 'summarise' is called. It is just ok.

# 1.2. Introduction
# load data and split into train and test
library(tidyverse)
library(lubridate)
library(caret)

dl <- tempfile()
download.file("https://github.com/dbaroch/med_app_no_show/raw/main/KaggleV2-May-2016.csv", dl)

df <- read_delim(file =  dl,
                 delim = ",")

rm(dl)

df

#data set structure
str(df)

# 2.1. Check for missing values and adapt variable classes
# check absence of missing values
df %>% complete.cases() %>% all()

# convert classes and change no_show name
df <- df %>%
  mutate(Gender = if_else(Gender == "F", TRUE, FALSE),
         `No-show` = if_else(`No-show` == "Yes", TRUE, FALSE),
         Neighbourhood = as.factor(Neighbourhood),
         Age = as.integer(Age),
         Handcap = as.integer(Handcap),
         Scholarship = as.logical(Scholarship),
         Hipertension = as.logical(Hipertension),
         Diabetes = as.logical(Diabetes),
         Alcoholism = as.logical(Alcoholism),
         SMS_received = as.logical(SMS_received))

names(df)[names(df) == "No-show"] <- "no_show"

# 2.2. Data split into training and final test sets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = df$AppointmentID, times = 1, p = 0.1, list = FALSE)
train <- df[-test_index[,1],]
final_test <- df[test_index[,1],]
rm(df,test_index)

# 2.3. Data exploration

# 2.3.1. check appointmentId
train %>% pull(AppointmentID) %>% unique() %>% length()

# appointment vs sched: plot
train %>%
  select(ScheduledDay, AppointmentID) %>%
  ggplot(aes(x=ScheduledDay, y = AppointmentID)) + geom_line()

# appointmentID vs sched: correlation
cor(as.numeric(train$ScheduledDay), train$AppointmentID)

# 2.3.2. Time span
# time span of the training set - min
train %>% pull(AppointmentDay) %>% unique() %>% min()

# time span of the training set - max
train %>% pull(AppointmentDay) %>% unique() %>% max()

# 2.3.3. No-show rate
# no show rate mean
no_show_rate <- mean(train$no_show == TRUE)
no_show_rate

# 2.3.4. Patients and number of appointments per patient
# number of patients
train %>% pull(PatientId) %>% unique() %>% length()

# number of appointments per patient
train %>%
  group_by(PatientId) %>%
  summarise(app_number_per_patient = n()) %>%
  group_by(app_number_per_patient) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(app_number = count * app_number_per_patient,
         app_number_per_patient = if_else(app_number_per_patient > 9, "ten_or_more", as.character(app_number_per_patient))) %>%
  group_by(app_number_per_patient) %>%
  summarise(app_number = sum(app_number)) %>%
  mutate(cumulated_percentage = 100*cumsum(app_number)/sum(app_number)) %>%
  ggplot(aes(x = as.factor(app_number_per_patient), y = cumulated_percentage)) +
  geom_bar(stat = "identity")

# no show rate per number of app per patient: bar plot
train %>%
  group_by(PatientId) %>%
  mutate(app_per_patient = n()) %>%
  mutate(app_per_patient = ifelse(app_per_patient > 5, "more than 5", as.character(app_per_patient))) %>%
  ungroup() %>%
  group_by(app_per_patient) %>%
  summarise(no_show_ratio = mean(no_show == TRUE)) %>%
  ggplot(aes(x=app_per_patient, y = no_show_ratio)) + geom_bar(stat = "identity")

# 2.3.5. Variables that characterize patients
# check patients variables (gender, age, hipertension, etc)  uniqueness
train %>%
  select(PatientId, Gender, Age, Scholarship, Hipertension, Diabetes, Alcoholism, Handcap) %>%
  unique() %>%
  nrow()

# check patients age changes
train %>%
  select(PatientId, Age) %>%
  unique() %>%
  group_by(PatientId) %>%
  mutate(count = n(),
         age_avg = mean(Age),
         age_diff = abs(Age - age_avg)) %>%
  filter(count > 1) %>%
  ungroup() %>%
  pull(age_diff) %>%
  unique()

#check patients variables (NOT AGE) uniqueness
train %>%
  select(PatientId, Gender, Neighbourhood, Scholarship, Hipertension, Diabetes, Alcoholism, Handcap) %>%
  unique() %>%
  nrow()

# 2.3.6. Appointments and gender
# gender, number of appointments
train %>%
  group_by(Gender) %>%
  summarise(count = n())

# no show rate by gender
train %>%
  group_by(Gender, no_show) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(ratio = count/sum(count)) %>%
  ggplot(aes(x = no_show, y = ratio)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ Gender)

# 2.3.7. Appointments and age
# age maximum and minimum
train %>% pull(Age) %>% max()
train %>% pull(Age) %>% min()

# removing observation with wrong age
train <- train %>% filter(Age >= 0)

# age
train %>%
  select(Age) %>%
  ggplot(aes(x=Age)) + geom_histogram(binwidth = 5)

# age and patients
train %>%
  select(PatientId, Age) %>%
  group_by(PatientId) %>%
  arrange(-Age) %>%
  slice(1) %>%
  ggplot(aes(x=Age)) + geom_histogram()

# no show ratio for 10-year stratification
train %>%
  mutate(rounded_age = 10*floor(Age/10),
         rounded_age = if_else(rounded_age >= 90, 90, rounded_age)) %>%
  filter(rounded_age >= 0) %>%
  group_by(rounded_age) %>%
  summarise(ratio = mean(no_show ==  TRUE)) %>%
  ggplot(aes(x=rounded_age, y = ratio)) + geom_bar(stat = "identity")

# 2.3.8. Appointments and neighbourhood
# neighbourhood quantity
train %>%
  pull(Neighbourhood) %>%
  unique() %>%
  length()

# appointments per neighbourhood top 5
train %>%
  group_by(Neighbourhood) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:5)

# appointments per neighbourhood bottom 5
train %>%
  group_by(Neighbourhood) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  slice(1:5)

# neighbourhood effect
train %>%
  group_by(Neighbourhood) %>%
  summarise(no_show_ratio = mean(no_show == TRUE),
            count = n()) %>%
  filter(count >= 20) %>%
  arrange(-no_show_ratio) %>%
  slice(-(11:(nrow(.)-10))) %>%
  ggplot(aes(x=Neighbourhood, y = no_show_ratio)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 2.3.9. No-show rate along time span
# no show along time
train %>%
  group_by(app_date = date(AppointmentDay)) %>%
  summarise(no_show_rate = mean(no_show == TRUE)) %>%
  ggplot(aes(x=app_date, y =  no_show_rate)) + geom_line()

# 2.3.10. Time between schedulling and the appointment
# time between schedulling and appointment
train %>%
  mutate(days_between = date(AppointmentDay) - date(ScheduledDay)) %>%
  group_by(days_between) %>%
  summarise(ratio = mean(no_show == TRUE))

# 2.3.11. Number of appointments that day
# number of appointments that day
train %>%
  group_by(PatientId, AppointmentDay) %>%
  mutate(nbr_app_that_day = n()) %>%
  ungroup() %>%
  group_by(nbr_app_that_day) %>%
  summarise(bias = mean(no_show == TRUE) - no_show_rate,
            count = n()) %>%
  mutate(count = count/nbr_app_that_day)

# 2.3.12. Weekday influence
# weekday influence
train %>%
  mutate(weekday = wday(AppointmentDay)) %>%
  group_by(weekday) %>%
  summarise(no_show_mean = mean(no_show == TRUE))

# 2.3.13. sms and days between
train %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay)),
         SMS_received = factor(if_else(days_between < 3, "does_not_apply",
                                       if_else(SMS_received == TRUE, "yes", "no")))) %>%
  select(days_between, SMS_received, no_show) %>%
  group_by(SMS_received) %>%
  summarise(count = n(),
            no_show_rate = mean(no_show ==  TRUE))

# 2.6. split into train and dev sets
# As final test set is to be used for the final evaluation only, the training set
# is split again
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = train$AppointmentID, times = 1, p = 0.1, list = FALSE)
dev_set <- train[test_index[,1],]
train <- train[-test_index[,1],]

rm(test_index)

# 2.7. Model
# 2.7.1. logistic regression
library(PRROC)
library(AUC)

# the AppointmentID and Neighbourhood are removed for the reasons explained in the report
train_for_model <- train %>% select(-AppointmentID, -Neighbourhood)
test_for_model <- dev_set %>% select(-AppointmentID, -Neighbourhood)

train_glm <- train(as.factor(no_show) ~ ., method = "glm", data = train_for_model)

test_preds <- predict(train_glm, newdata = test_for_model, type= "prob")

roc_test_glm <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test_glm)


# 2.7.2. first RF approach
library(randomForest)

train_for_model <- train %>% select(-Neighbourhood, -AppointmentID)
test_for_model <- dev_set %>% select(-Neighbourhood, -AppointmentID)

rf_model_1 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

roc <- roc(rf_model_1$votes[,2],factor(1 * (rf_model_1$y == TRUE)))
auc(roc)

test_preds <- predict(rf_model_1, newdata = test_for_model, type= "prob")

roc_test_1 <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test_1)

# RF1 ROC curve for dev set
plot(roc_test_1)

# RF1 imporante
rf_model_1$importance


# 2.7.3. Random forest without PatientId variable.
# RF2 approach

train_for_model <- train %>% select(-Neighbourhood, -AppointmentID, -PatientId)
test_for_model <- dev_set %>% select(-Neighbourhood, -AppointmentID, -PatientId)

rf_model_2 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF2 ROC for training set
roc <- roc(rf_model_2$votes[,2],factor(1 * (rf_model_2$y == TRUE)))
auc(roc)

# RF2 ROC for dev set
test_preds <- predict(rf_model_2, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# 2.7.4. Random forest with bias per patient

# bias per patient on the training set
no_show_rate <- mean(train$no_show == TRUE)

bias_per_patient <- train %>%
  group_by(PatientId) %>%
  mutate(count = n()) %>%
  filter(count > 4) %>%
  summarise(bias_per_patient = mean(no_show == TRUE) - no_show_rate)

# RF3 approach

train_for_model <- train %>%
  left_join(bias_per_patient) %>%
  mutate(bias_per_patient = if_else(is.na(bias_per_patient), 0, bias_per_patient)) %>%
  select(-Neighbourhood, -AppointmentID, -PatientId)

test_for_model <- dev_set %>%  
  left_join(bias_per_patient) %>%
  mutate(bias_per_patient = if_else(is.na(bias_per_patient), 0, bias_per_patient)) %>%
  select(-Neighbourhood, -AppointmentID, -PatientId)

rf_model_3 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF3 ROC for training set
roc <- roc(rf_model_3$votes[,2],factor(1 * (rf_model_3$y == TRUE)))
auc(roc)

# RF3 ROC for dev set
test_preds <- predict(rf_model_3, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# 2.7.5. Random forest with both bias per patient and PatientId

# RF4 approach

train_for_model <- train %>%
  left_join(bias_per_patient) %>%
  mutate(bias_per_patient = if_else(is.na(bias_per_patient), 0, bias_per_patient)) %>%
  select(-Neighbourhood, -AppointmentID)

test_for_model <- dev_set %>%  
  left_join(bias_per_patient) %>%
  mutate(bias_per_patient = if_else(is.na(bias_per_patient), 0, bias_per_patient)) %>%
  select(-Neighbourhood, -AppointmentID)

rf_model_4 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF4 ROC for training set
roc <- roc(rf_model_4$votes[,2],factor(1 * (rf_model_4$y == TRUE)))
auc(roc)

# RF4 ROC for dev set
test_preds <- predict(rf_model_4, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# 2.7.6. Random forest with bias per neighbourhood

# bias per neighbourhood on the training set
no_show_rate <- mean(train$no_show == TRUE)

bias_per_nbh <- train %>%
  group_by(Neighbourhood) %>%
  mutate(count = n()) %>%
  filter(count > 15) %>%
  summarise(bias_per_nbh = mean(no_show == TRUE) - no_show_rate)

# RF5 approach

train_for_model <- train %>%
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID)

test_for_model <- dev_set %>%  
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID)

rf_model_5 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF5 ROC for training set
roc <- roc(rf_model_5$votes[,2],factor(1 * (rf_model_5$y == TRUE)))
auc(roc)

# RF5 ROC for dev set
test_preds <- predict(rf_model_5, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# RF5 importance
rf_model_5$importance

# 2.7.7. Random forest with days between scheduling and appointment

# RF6 approach
train_for_model <- train %>%
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days"))

test_for_model <- dev_set %>%  
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days"))

rf_model_6 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF6 ROC for training set
roc <- roc(rf_model_6$votes[,2],factor(1 * (rf_model_6$y == TRUE)))
auc(roc)

# RF6 ROC for dev set
test_preds <- predict(rf_model_6, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# RF6 importance
rf_model_6$importance

# 2.7.8. Random forest with a modified SMS_received variable

# RF7 approach
train_for_model <- train %>%
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days"),
         SMS_received = factor(if_else(days_between < 3, "does_not_apply",
                                       if_else(SMS_received == TRUE, "yes", "no"))))

test_for_model <- dev_set %>%  
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days"),
         SMS_received = factor(if_else(days_between < 3, "does_not_apply",
                                       if_else(SMS_received == TRUE, "yes", "no"))))

rf_model_7 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF7 ROC for training set
roc <- roc(rf_model_7$votes[,2],factor(1 * (rf_model_7$y == TRUE)))
auc(roc)

# RF7 ROC for dev set
test_preds <- predict(rf_model_7, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# RF7 importance
rf_model_7$importance

# 2.7.9. Random forest with weekdays and hour

# RF8 approach

train_for_model <- train %>%
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days")) %>%
  mutate(wday_sched = wday(ScheduledDay),
         wday_app = wday(AppointmentDay),
         hour_sched = hour(ScheduledDay))

test_for_model <- dev_set %>%  
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days")) %>%
  mutate(wday_sched = wday(ScheduledDay),
         wday_app = wday(AppointmentDay),
         hour_sched = hour(ScheduledDay))

rf_model_8 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF8 ROC for training set
roc <- roc(rf_model_8$votes[,2],factor(1 * (rf_model_8$y == TRUE)))
auc(roc)

# RF8 ROC for dev set
test_preds <- predict(rf_model_8, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# 2.7.10. Random forest incorporating number of appointment that day and per patient
# nbr_app_that_day and nbr_app_per_patient
nbr_app_that_day <- train %>%
          group_by(PatientId, AppointmentDay) %>%
          summarise(nbr_app_that_day = n())

nbr_app_per_patient <- train %>%
          group_by(PatientId) %>%
          summarise(nbr_app_per_patient = n())

# RF9 approach
train_for_model <- train %>%
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days")) %>%
  mutate(wday_sched = wday(ScheduledDay),
         wday_app = wday(AppointmentDay),
         hour_sched = hour(ScheduledDay)) %>%
  left_join(nbr_app_per_patient) %>%
  left_join(nbr_app_that_day)

test_for_model <- dev_set %>%  
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days")) %>%
  mutate(wday_sched = wday(ScheduledDay),
         wday_app = wday(AppointmentDay),
         hour_sched = hour(ScheduledDay))  %>%
  left_join(nbr_app_per_patient) %>%
  left_join(nbr_app_that_day) %>%
  mutate(nbr_app_that_day = if_else(is.na(nbr_app_that_day), 0, as.double(nbr_app_that_day)),
         nbr_app_per_patient = if_else(is.na(nbr_app_per_patient), 0, as.double(nbr_app_per_patient)))

rf_model_9 <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500)

# RF9 ROC for training set
roc <- roc(rf_model_9$votes[,2],factor(1 * (rf_model_9$y == TRUE)))
auc(roc)

# RF9 ROC for dev set
test_preds <- predict(rf_model_9, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# RF9 importance
rf_model_9$importance

# 2.7.11. Downsampling approach for the FALSE class
# RF10 downsampling
train_for_model <- train %>%
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days")) %>%
  mutate(wday_sched = wday(ScheduledDay),
         wday_app = wday(AppointmentDay),
         hour_sched = hour(ScheduledDay)) %>%
  left_join(nbr_app_per_patient) %>%
  left_join(nbr_app_that_day)

test_for_model <- dev_set %>%  
  left_join(bias_per_nbh) %>%
  mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
  select(-Neighbourhood, -AppointmentID) %>%
  mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days")) %>%
  mutate(wday_sched = wday(ScheduledDay),
         wday_app = wday(AppointmentDay),
         hour_sched = hour(ScheduledDay))  %>%
  left_join(nbr_app_per_patient) %>%
  left_join(nbr_app_that_day) %>%
  mutate(nbr_app_that_day = if_else(is.na(nbr_app_that_day), 0, as.double(nbr_app_that_day)),
         nbr_app_per_patient = if_else(is.na(nbr_app_per_patient), 0, as.double(nbr_app_per_patient)))

# the downsampling is achieved with the sampsize argument.
# ntree is then increased to compensate for that
rf_model_10 <- randomForest(as.factor(no_show) ~ ., data = train_for_model,
                            strata = train_for_model$no_show,
                            sampsize = c(500,500),
                            ntree = 5000)

# RF10 ROC for training set
roc <- roc(rf_model_10$votes[,2],factor(1 * (rf_model_10$y == TRUE)))
auc(roc)

# RF10 ROC for dev set
test_preds <- predict(rf_model_10, newdata = test_for_model, type= "prob")

roc_test <- roc(test_preds[,2], factor(1 * (test_for_model$no_show==TRUE)))
auc(roc_test)

# 2.7.12. Random forest. Final parameter tuning.
# convertion function
convert_df <- function(df, bias_per_nbh, nbr_app_per_patient, nbr_app_that_day){
  df %>%
    left_join(bias_per_nbh) %>%
    mutate(bias_per_nbh = if_else(is.na(bias_per_nbh), 0, bias_per_nbh)) %>%
    select(-Neighbourhood, -AppointmentID) %>%
    mutate(days_between = as.integer(date(AppointmentDay) - date(ScheduledDay), units = "days")) %>%
    mutate(wday_sched = wday(ScheduledDay),
           wday_app = wday(AppointmentDay),
           hour_sched = hour(ScheduledDay))  %>%
    left_join(nbr_app_per_patient) %>%
    left_join(nbr_app_that_day) %>%
    mutate(nbr_app_that_day = if_else(is.na(nbr_app_that_day), 0, as.double(nbr_app_that_day)),
           nbr_app_per_patient = if_else(is.na(nbr_app_per_patient), 0, as.double(nbr_app_per_patient)))
}

# con K fold
entire_train <- train %>% bind_rows(dev_set)

set.seed(1, sample.kind="Rounding")
indexes <- sample(length(train$no_show))

folds_nbr <- 2 #change to 10 to run the 10-fold cross validation

params <- expand.grid(tune_mtry = c(2,4,6), # 2,3,4,5,6)
                      lambda_nbh = c(50, 100, 300)) #50, 100, 300, 500, 1000, 1500, 5000 ,10000))

rf_1_fold <- function(df, indexes, k, folds, params){
  valid_index <- indexes[(1+length(indexes)/folds*(k-1)):(length(indexes)/folds*k)]
  train_set <- df[-valid_index,]
  valid_set <- df[valid_index,]
  
  nbr_app_per_patient <- train_set %>%
    group_by(PatientId) %>%
    summarise(nbr_app_per_patient = n())
  
  nbr_app_that_day <- train_set %>%
    group_by(PatientId, AppointmentDay) %>%
    summarise(nbr_app_that_day = n())
  
  
  lapply(1:length(params$tune_mtry), function(i){
    
    params <- params[i,]
    
    no_show_rate <- mean(train_set$no_show == TRUE)
    
    bias_per_nbh <- train_set %>%
      group_by(Neighbourhood) %>%
      mutate(count = n()) %>%
      filter(count > params$lambda_nbh) %>%
      summarise(bias_per_nbh = mean(no_show == TRUE) - no_show_rate)
    
    
    train_para_rf <- convert_df(train_set, bias_per_nbh, nbr_app_per_patient, nbr_app_that_day)
    
    rf_model <- randomForest(as.factor(no_show) ~ ., data = train_para_rf, ntree = 500, mtry = params$tune_mtry)
    
    test_para_rf <- convert_df(valid_set, bias_per_nbh, nbr_app_per_patient, nbr_app_that_day)
    
    test_preds <- predict(rf_model, newdata = test_para_rf, type= "prob")  
    
    params %>%
      mutate(k = k,
             auc_roc = auc(roc(test_preds[,2], factor(1 * (test_para_rf$no_show==TRUE)))))
    
  }) %>%
    bind_rows()
  
}  

results <- lapply(1:folds_nbr, rf_1_fold, df = entire_train, indexes = indexes, folds = 10, params = params)

results %>%
  bind_rows() %>%
  group_by(tune_mtry, lambda_nbh) %>%
  summarise(auc_roc = mean(auc_roc))

# parameters achieving top performance
results %>%
  bind_rows() %>%
  group_by(tune_mtry, lambda_nbh) %>%
  summarise(auc_roc = mean(auc_roc)) %>%
  ungroup() %>%
  arrange(-auc_roc) %>%
  slice(1)


# 2.7.13. KNN model
# knn using same features as RF9
# for KNN no-show variable is transformed to Yes/No instead of TRUE/FALSE because
# caret issues an error if TRUE/FALSE are used.

train_for_knn <- train_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No"))

test_for_knn <- test_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No"))

fitControl <- trainControl(method = "cv",
                           number = 10, 
                           # repeats = 10, # uncomment for repeatedcv 
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

train_knn <- train(no_show ~ ., method = "knn", 
                   data = train_for_knn,
                   tuneGrid = data.frame(k = seq(35, 35, 10)), #5, 65, 10)),
                   trControl = fitControl,
                   metric = "ROC")

test_preds_knn <- predict(train_knn, newdata = test_for_knn, type= "prob")  
auc(roc(test_preds_knn[,2], factor(1 * (test_for_knn$no_show == "Yes"))))

# knn using same features as RF9 without PatientId
train_for_knn <- train_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-PatientId)

test_for_knn <- test_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-PatientId)

train_knn <- train(no_show ~ ., method = "knn", 
                   data = train_for_knn,
                   tuneGrid = data.frame(k = seq(35, 35, 10)), #5, 65, 10)),
                   trControl = fitControl,
                   metric = "ROC")

test_preds_knn <- predict(train_knn, newdata = test_for_knn, type= "prob")  
auc(roc(test_preds_knn[,2], factor(1 * (test_for_knn$no_show == "Yes"))))


# knn using same features as RF9 without everything added except bias per nbh

train_for_knn <- train_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-PatientId, -nbr_app_per_patient, -nbr_app_that_day, -days_between, -wday_sched, -wday_app, -hour_sched)

test_for_knn <- test_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-PatientId, -nbr_app_per_patient, -nbr_app_that_day, -days_between, -wday_sched, -wday_app, -hour_sched)

train_knn <- train(no_show ~ ., method = "knn", 
                   data = train_for_knn,
                   tuneGrid = data.frame(k = seq(35, 35, 10)), #5, 65, 10)),
                   trControl = fitControl,
                   metric = "ROC")

test_preds_knn <- predict(train_knn, newdata = test_for_knn, type= "prob")  
auc(roc(test_preds_knn[,2], factor(1 * (test_for_knn$no_show == "Yes"))))

# knn using same features as before and removing other features

train_for_knn <- train_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-PatientId, -nbr_app_per_patient, -nbr_app_that_day, -days_between, -wday_sched, -wday_app, -hour_sched, -Diabetes, -Alcoholism, -Hipertension, -Handcap)

test_for_knn <- test_for_model %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-PatientId, -nbr_app_per_patient, -nbr_app_that_day, -days_between, -wday_sched, -wday_app, -hour_sched, -Diabetes, -Alcoholism, -Hipertension, -Handcap)

train_knn <- train(no_show ~ ., method = "knn", 
                   data = train_for_knn,
                   tuneGrid = data.frame(k = seq(25, 45, 10)),
                   trControl = fitControl,
                   metric = "ROC")

test_preds_knn <- predict(train_knn, newdata = test_for_knn, type= "prob")  
auc(roc(test_preds_knn[,2], factor(1 * (test_for_knn$no_show == "Yes"))))


# knn using same features as data set after removing Neighbourhood and AppID
train_for_knn <- train %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-Neighbourhood, -AppointmentID, -PatientId)

test_for_knn <- dev_set %>%
  mutate(no_show = if_else(no_show == TRUE, "Yes", "No")) %>%
  select(-Neighbourhood, -AppointmentID, -PatientId)

train_knn <- train(no_show ~ ., method = "knn", 
                   data = train_for_knn,
                   tuneGrid = data.frame(k = seq(25, 45, 10)), #5, 65, 10)),
                   trControl = fitControl,
                   metric = "ROC")

test_preds_knn <- predict(train_knn, newdata = test_for_knn, type= "prob")  
auc(roc(test_preds_knn[,2], factor(1 * (test_for_knn$no_show == "Yes"))))


# 3.1. Training the ultimate model, predicting and testing on the test set

# train ultimate model
lambda_nbh <- 50
tune_mtry <- 4

train <- train %>% bind_rows(dev_set) #put together all the training set back again

no_show_rate <- mean(train$no_show == TRUE)

bias_per_nbh <- train %>%
  group_by(Neighbourhood) %>%
  mutate(count = n()) %>%
  filter(count > lambda_nbh) %>%
  summarise(bias_per_nbh = mean(no_show == TRUE) - no_show_rate)

nbr_app_that_day <- train %>%
  group_by(PatientId, AppointmentDay) %>%
  summarise(nbr_app_that_day = n())

nbr_app_per_patient <- train %>%
  group_by(PatientId) %>%
  summarise(nbr_app_per_patient = n())


train_for_model <- convert_df(train, bias_per_nbh, nbr_app_per_patient, nbr_app_that_day)

rf_ultimate_model <- randomForest(as.factor(no_show) ~ ., data = train_for_model, ntree = 500, mtry = tune_mtry)

# predictions on final test set
test_for_eval <- convert_df(final_test, bias_per_nbh, nbr_app_per_patient, nbr_app_that_day)

test_preds <- predict(rf_ultimate_model, newdata = test_for_eval, type= "prob")

# ROC AUC evaluation for final test set
roc_test <- roc(test_preds[,2], factor(1 * (test_for_eval$no_show==TRUE)))
auc(roc_test)

# plot final ROC}
plot(roc_test)

# cutoff 0.5 sensitivity
confusionMatrix(predict(rf_ultimate_model,
                        newdata = test_for_eval,
                        cutoff = c(0.68, 0.32)),
                as.factor(test_for_eval$no_show),
                positive = "TRUE")

# cutoff 0.75 TPR
confusionMatrix(predict(rf_ultimate_model,
                        newdata = test_for_eval, 
                        cutoff = c(0.45, 0.55)), 
                as.factor(test_for_eval$no_show), 
                positive = "TRUE")
