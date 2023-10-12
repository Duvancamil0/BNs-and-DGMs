## Load the necessary libraries
library(bnlearn)
library(readxl)
library(Metrics)
library(caret)
library(dplyr)
library(Rgraphviz)

## Load the data
initial_data <- read.csv(
              "/mineros/SMOTE.csv")

## Adjust the data
data <- initial_data[,c(2,12,3,4,5,6,7,8,9,10,11)]
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                     as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], 
                                                     as.factor)
summary(data)

data_training <- data
data_training <- as.data.frame(data_training)

## Test dataset
set.seed(1)

data_testing <- read.csv(
  "/mineros/testing.csv")
rownames(data_testing) <- data_testing$ID
data_testing <- as.data.frame(data_testing)

data_testing[sapply(data_testing, is.character)] <- lapply(data_testing[sapply(data_testing, is.character)], 
                                           as.factor)
data_testing[sapply(data_testing, is.numeric)] <- lapply(data_testing[sapply(data_testing, is.numeric)], 
                                         as.factor)

levels(data_testing$Category_Time) <- c("Large",levels(data_testing$Category_Time))

data_testing <- data_testing[,-c(1)]


## Crear la red

create_model = empty.graph(c("Category_Time", "Category_Overrun",
                             "Category_Cost", "International_Project",
                             "Fabrication._Risk", "Weather_Risk",
                             "Provider._Risk", "Engineering_Risk", "Decision_Risk",
                             "Analysis_Risk", "Labor_Risk"))

arc.set = matrix(c(
                   "International_Project", "Labor_Risk",
                   "Category_Cost", "Provider._Risk",
                   "Category_Cost", "Analysis_Risk",
                   "Category_Cost", "International_Project",
                   "Analysis_Risk", "Labor_Risk",
                   "Analysis_Risk", "Decision_Risk",
                   "Analysis_Risk", "Fabrication._Risk",
                   "Provider._Risk", "Fabrication._Risk",
                   "Labor_Risk", "Decision_Risk",
                   "Decision_Risk", "Engineering_Risk",
                   "Category_Time", "Category_Overrun",
                   "Engineering_Risk", "Category_Overrun",
                   "Fabrication._Risk", "Category_Overrun",
                   "Weather_Risk", "Category_Overrun",
                   "Decision_Risk", "Category_Overrun",
                   "Provider._Risk", "Category_Overrun",
                   "Analysis_Risk", "Category_Overrun"
),
ncol = 2, byrow = TRUE,
dimnames = list(NULL, c("from", "to")))

arcs(create_model) = arc.set
graphviz.plot(create_model, shape = "ellipse")

## Aprender las probabilidades

model <- bn.fit(create_model, as.data.frame(data_training), method = "bayes")


predicted = predict(model, node = "Category_Overrun", data = data_testing)
predicted


## Probabilidad posterior de los riesgos

evidence = data.frame(Engineering_Risk = factor("0", levels = levels(data$Analysis_Risk)),
                      Provider._Risk = factor("0", levels = levels(data$Provider._Risk)))
predict(model, "Fabrication._Risk", evidence,
        method = "bayes-lw", prob = TRUE)
predict(model_syn, "Fabrication._Risk", evidence,
                    method = "bayes-lw", prob = TRUE)
