library(tidyverse)
# library(MASS)

data <- read.csv('dataset.csv', header = TRUE) %>% 
  # on enlève les variables de type texte
  select(-firstthought, -tpfeel, -benefit, -eitcthink, -recent, -biggest, 
         -paystub, -depend, -glad, -upset, -wastethink, -eitcexp)

data <- data %>% 
  mutate(statename = as.factor(statename), 
         gender = as.factor(gender), 
         educ = as.factor(educ), 
         partyid = as.factor(partyid), 
         taxpayer = as.factor(taxpayer), 
         wagesal = as.factor(wagesal), 
         labforce = as.factor(labforce), 
         polideo = as.factor(polideo), 
         polinffreq = as.factor(polinffreq), 
         regvote = as.factor(regvote),
         voted = as.factor(voted), 
         discusspol = as.factor(discusspol),
         poleffic = as.factor(poleffic),
         polvol = as.factor(polvol),
         polknow1 = as.factor(polknow1), 
         polknow2 = as.factor(polknow2), 
         polkno3 = as.factor(polkno3), 
         marital = as.factor(marital), 
         ownhome = as.factor(ownhome), 
         stateresid = as.factor(stateresid), 
         yearbirth = as.factor(yearbirth), 
         raceeth = as.factor(raceeth), 
         hhinc = as.factor(hhinc))

dataTaxes <- data %>% select(-statename)

fit1 <- lm(taxpayer_gap ~ gender, data = na.omit(dataTaxes)) # BIC : 6979.592

fit <- lm(taxpayer_gap ~ ., data = na.omit(dataTaxes)) # BIC : 5712.947
stepwise_model <- MASS::stepAIC(fit, direction = "forward")

# library(flexmix)
best_model <- stepwise_model[which.min(BIC(stepwise_model))]

myModel <- lm(taxpayer_gap ~ gender + percenttp + polinffreq, data = na.omit(dataTaxes)) # BIC : 5484.227

ibtiModel <- lm(taxpayer_gap ~ gender + hhinc + partyid + polinffreq, data = na.omit(dataTaxes)) # BIC : 7079.835

library(caret)
set.seed(123) # set the seed for reproducibility

dataTaxes <- na.omit(dataTaxes)
myModel <- lm(taxpayer_gap ~ gender + percenttp + polinffreq + poleffic + benefit_sentiment, data = dataTaxes)

split_index <- createDataPartition(dataTaxes$taxpayer_gap, p = 0.8, list = FALSE)
train_data <- dataTaxes[split_index, ]
test_data <- dataTaxes[-split_index, ]

test_predictions <- predict(myModel, newdata = test_data)
test_error <- mean((test_data$taxpayer_gap - test_predictions)^2)


