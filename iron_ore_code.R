#Load the needed packages
library(tidyverse)
library(lubridate)
install.packages("corrplot")
library(corrplot)

#Read in the dataset 

data <- read.csv("~/MiningProcess_Flotation_Plant_Database.csv", 
                 header = TRUE)

#Inspect the dataset and clean

head(data)
dim(data)

#Makes all variables numeric and datetime type

data <- data %>% mutate_at(vars(2:24), ~ str_replace(.,",","."))
data <- data %>% mutate_at(vars(2:24), ~ as.numeric(.))
data$date <- gsub('""'," ", data$date)
data <- data %>% mutate_at(vars(1), ~ as_datetime(.))

str(data)

#Rename variables 

data <- data %>% rename(silica_concentrate = X..Silica.Concentrate,
                        iron_concentrate = X..Iron.Concentrate,
                        pulp_density = Ore.Pulp.Density,
                        pulp_flow = Ore.Pulp.Flow,
                        pulp_ph = Ore.Pulp.pH,
                        amina_flow = Amina.Flow,
                        starch_flow = Starch.Flow,
                        silica_feed = X..Silica.Feed,
                        iron_feed = X..Iron.Feed)


names(data)[9:15] <- paste0("air_flow_",1:7)
names(data)[16:22] <- paste0("column_level_",1:7)
names(data)

#Combine hourly data

data <- aggregate(data[,2:24],list(data$date), mean)
data <- data %>% rename(date = Group.1)

#Check dataset structure again
str(data)

#Exploratory Data Analysis
#Distribution of response variable
hist(data$silica_concentrate)

#Summary statistics of all variables
summary(data)

#Check response variable changes with dates
data %>% ggplot(aes(date,silica_concentrate)) + geom_line()

#Create month and day variables
data <- data %>% mutate(month = month(date),
                        day = day(date))
head(data)

#Check response variable changes within one month
data %>% filter (month == 3) %>% 
  ggplot(aes(date,silica_concentrate)) +
  geom_line()

#Response variable variability within one day
data %>% filter(month == 3, day == 10) %>%
  ggplot(aes(date,silica_concentrate)) +
  geom_line()

data %>% filter (month == 3, day == 11) %>%
  ggplot(aes(date,silica_concentrate)) +
  geom_line()

#Remove non numeric variable and check correlation among variables
data %>% select(-date) %>% cor()%>% corrplot()


#Split dataset into training and test sets

library(caret)
test_index <- createDataPartition(data$silica_concentrate,times = 1,
                                p = 0.2, list = FALSE)
train_set <- data[-test_index,]
test_set <- data[test_index,]

#Train a linear regression model on dataset

lm_model <- train(silica_concentrate ~ ., data = train_set, 
                  method = "lm")

#Check how model fit the data

summary(lm_model)

#Make predictions on test set and check performance

lm_predictions <- predict(lm_model, test_set)
RMSE(lm_predictions, test_set$silica_concentrate)

#Train regression model with principal components and check fit

train_pca <- train_set %>% select(-date)
pca_1 <- prcomp(as.matrix(train_pca),center = TRUE, scale. = TRUE)
summary(pca_1)
components <- cbind(silica = train_pca[,"silica_concentrate"],
                   pca_1$x[,1:12]) %>% as.data.frame()
pca_model <- train(silica ~ ., method = "lm", data = components)
summary(pca_model)

#Test pca model performance on new data

test_pca <- predict(pca_1,test_set) %>% as.data.frame() %>%
  mutate(silica = test_set[,"silica_concentrate"]) %>%
  select(PC1:PC12,silica)
head(test_pca)
pca_predictions <- predict(pca_model,test_pca)
RMSE(pca_predictions,test_pca$silica)

