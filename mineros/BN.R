## Load necessary libaries
library(bnlearn)
library(readxl)
library(Metrics)
library(caret)
library(dplyr)
library(Rgraphviz)

## Load the data
data <- as.data.frame(read_excel(path = 
              "/mineros/training.csv"))

## Adjust the data
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                     as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], 
                                                     as.factor)
summary(data)

## Create the network

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

## Fit the parameters with the real small dataset

model <- bn.fit(create_model, data, method = "bayes")

data_testing <- as.data.frame(read_excel(path = 
                                        "/mineros/testing.csv"))


predicted = predict(model, node = "Category_Overrun", data = data_testing)
predicted

## Load the synthetic dataset
syn_data <- as.data.frame(read.csv(
  "/mineros/mineros_syn.csv"))

## Adjust the data
syn_data[sapply(syn_data, is.character)] <- lapply(syn_data[sapply(syn_data, is.character)], 
                                                   as.factor)
syn_data[sapply(syn_data, is.numeric)] <- lapply(syn_data[sapply(syn_data, is.numeric)], 
                                                 as.factor)

## unite both datasets
syn_real <- rbind(syn_data, data_training)
summary(syn_real)

## Fit the parameters

model_syn <- bn.fit(create_model, as.data.frame(syn_real), method = "bayes")
predicted_syn = predict(model_syn, node = "Category_Overrun", data = data_testing)

## Results
results = as.data.frame(data_testing$Category_Overrun)
results$pred_real = predicted
results$pred_syn = predicted_syn
results

