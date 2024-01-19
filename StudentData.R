library(earth)

setwd('/Users/kiranmayi/Downloads')
fullFile = read.csv('StudentData.csv', stringsAsFactors=TRUE)

# Convert 'card' to a numeric variable
fullFile$card = fullFile$card == TRUE

# Split the data into training and validation sets
isTraining = runif(nrow(fullFile)) < .8
fullFileTrain = subset(fullFile,isTraining)
fullFileValid = subset(fullFile,!isTraining)

getStudentDataKFoldRMSE = function(testFit){
  set.seed(12345)
  totalFold = 10
  foldNum = floor(runif(nrow(fullFile))*totalFold)+1
  
  thisModelRMSE = rep(NA,totalFold)
  for(thisFold in 1:totalFold){
    trainingData = subset(fullFile,foldNum!=thisFold)
    validationData = subset(fullFile,foldNum==thisFold)
    thisModel = update(testFit,data=trainingData)
    thisFit = mean((predict(thisModel,validationData) - validationData$card)^2)^.5
    thisModelRMSE[thisFold] = thisFit
  }
  return(mean(thisModelRMSE))
}


model1A = lm(card ~ .-reports , data = fullFile)
getStudentDataKFoldRMSE(model1A)

model1A1 = lm(card ~ log(share) * owner * active * poly(majorcards, 1) * poly(months, 2) * expenditure + poly(income, 2) - reports, data = fullFile)
getStudentDataKFoldRMSE(model1A1)

model1A2 = lm(card ~ + log(share) + income + owner + active + majorcards + expenditure - reports , data = fullFile)
getStudentDataKFoldRMSE(model1A2)

model1A3 = lm(card ~ log(share) * log(income) + majorcards * expenditure - reports, data = fullFile)
getStudentDataKFoldRMSE(model1A3)

model1A4 = earth(card ~ log(share) + owner + active + majorcards * expenditure * log(income) - reports, data = fullFile)
getStudentDataKFoldRMSE(model1A4)

model1A5 = earth(card ~ log(share) * owner * majorcards * sqrt(I((expenditure)^(1/500000))) * log(income) - reports, data = fullFile)
getStudentDataKFoldRMSE(model1A5)

model1A6 = earth(card ~ log(share) * owner * active * majorcards * poly(expenditure, 2) * income + dependents - reports, data = fullFile)
getStudentDataKFoldRMSE(model1A6)

model1A7 = lm(card ~ share * majorcards * expenditure * income + active  - reports, data = fullFile)
getStudentDataKFoldRMSE(model1A7)

model1A8 = earth(card ~ log(share) * owner * active * majorcards * expenditure * log(income) + months - reports, data = fullFile)
getStudentDataKFoldRMSE(model1A8)

model1A9 = lm(card ~ log(share) * owner * active * majorcards * I((expenditure + 1e-6)^(1/3)) * log(income) - reports,  data = fullFile)
getStudentDataKFoldRMSE(model1A9)


```
model1B = lm(card ~ . , data = fullFile)
getStudentDataKFoldRMSE(model1B)

model1B1 = lm(card ~ share * owner * active * majorcards * months * expenditure, data = fullFileTrain)
getStudentDataKFoldRMSE(model1B1)

model1B2 = earth(card ~ . * poly(income, 2) * poly(share, 2) + expenditure, data = fullFile)
getStudentDataKFoldRMSE(model1B2)

model1B3 = lm(card ~ expenditure * share * owner * majorcards * log(income) + dependents, data = fullFile)
getStudentDataKFoldRMSE(model1B3)

model1B4 = lm(card ~ share * owner * active * majorcards * expenditure * income + poly(months, 2) + active * dependents + income * owner, data = fullFile)
getStudentDataKFoldRMSE(model1B4)

model1B5 = lm(card ~ log(share) * owner * active * majorcards * I((expenditure + 1e-6)^(1/3)) * log(income) * months, data = fullFile)
getStudentDataKFoldRMSE(model1B5)

model1B6 = lm(card ~ log(share) * owner * majorcards * I((expenditure + 1e-6)^(1/3)) * log(income), data = fullFile)
getStudentDataKFoldRMSE(model1B6)

model1B6 = lm(card ~ log(share) * owner * majorcards * I((expenditure + 1e-6)^(1/3)) * log(income), data = fullFile)
getStudentDataKFoldRMSE(model1B6)

model1B7 = earth(card ~ log(share) * owner * majorcards * sqrt(I((expenditure)^(1/500000))) * log(income), data = fullFile)
getStudentDataKFoldRMSE(model1B7)

model1B8 = earth(card ~ log(share) * owner * majorcards * (1 / sqrt(I((expenditure)^(1/500000)))) * log(income) * expenditure, data = fullFile)
getStudentDataKFoldRMSE(model1B8)

model1B9 = lm(card ~ log(share) * owner * majorcards * (1 / sqrt(I((expenditure)^(1/500000)))) * log(income) * expenditure, data = fullFile)
getStudentDataKFoldRMSE(model1B9)

```
```{r}
save(model1A8, file = "model1A.RData")
save(model1B2, file = "model1B.RData")
```