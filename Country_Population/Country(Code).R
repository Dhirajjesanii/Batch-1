
#Q1 Load/Import dataset

country_populations = read.csv("country_populations.csv",header = F)
colnames(country_populations) = c(
    "name",	
    "pop_2018",
    "pop_2017",
    "growth_rate",	
    "area",
    "density_2018"
)

#Q2) To split data into test and train first we have to sort the data --------

sort(country_populations$area,decreasing = F)

#Split into test and train

train = subset(country_populations,country_populations$area <= 580367)
test = subset(country_populations,country_populations$area > 580367)


#Q3) TO bulid model first we have to create formula than model than we can predict.....

lm_formula_train = as.formula("growth_rate ~ pop_2017 + pop_2018")
lm_model_train = lm(lm_formula_train,data = train)
lm_predict_train = predict(lm_model_train,newdata = train)

#Q4) Apply model on test ...we already aplied model in train(Q3) as above.....tha we plot for both test and train 

lm_formula_test = as.formula("growth_rate ~ pop_2017 + pop_2018")
lm_model_test = lm(lm_formula_test,data = test)
lm_predict_test = predict(lm_model_test,newdata = test,type = "response")

#plot the train prediction outcome...............
library(ggplot2)
ggplot(train) + geom_histogram(aes(x = lm_predict_train),fill = "pink")

#plot the test prediction outcome...............
ggplot(test) + geom_histogram(aes(x = lm_predict_test),fill = "black")

#Q5) 

library('ROCR')
data("ROCR.simple")
eval <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])
