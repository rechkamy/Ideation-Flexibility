setwd("/Users/Amy/Documents")
read_Data <- read.csv("Framing-Outcomes-Individual-Shifts.csv")

#Separate out by year
yr1Inc <- subset(read_Data, Year == 1 & FramingType == 'Incremental')
yr2Inc <- subset(read_Data, Year == 2 & FramingType == 'Incremental')
yr3Inc <- subset(read_Data, Year == 3 & FramingType == 'Incremental')
yr4Inc <- subset(read_Data, Year == 4 & FramingType == 'Incremental')
yr1Rad <- subset(read_Data, Year == 1 & FramingType == 'Radical')
yr2Rad <- subset(read_Data, Year == 2 & FramingType == 'Radical')
yr3Rad <- subset(read_Data, Year == 3 & FramingType == 'Radical')
yr4Rad <- subset(read_Data, Year == 4 & FramingType == 'Radical')

#Accounting for KAI score
KAIyr1Inc <- subset(yr1Inc, KAIValid. == 'Yes')
KAIyr2Inc <- subset(yr2Inc, KAIValid. == 'Yes')
KAIyr3Inc <- subset(yr3Inc, KAIValid. == 'Yes')
KAIyr4Inc <- subset(yr4Inc, KAIValid. == 'Yes')
KAIyr1Rad <- subset(yr1Rad, KAIValid. == 'Yes')
KAIyr2Rad <- subset(yr2Rad, KAIValid. == 'Yes')
KAIyr3Rad <- subset(yr3Rad, KAIValid. == 'Yes')
KAIyr4Rad <- subset(yr4Rad, KAIValid. == 'Yes')

df1Inc <- data.frame(AverageN = yr1Inc$NeutralAverage, ContextN = yr1Inc$NeutralContext, AverageF = yr1Inc$FramedAverage, ContextF = yr1Inc$FramedContext, Framing = yr1Inc$FramingType, Shift = yr1Inc$Shift.N.F.)
df2Inc <- data.frame(AverageN = yr2Inc$NeutralAverage, ContextN = yr2Inc$NeutralContext, AverageF = yr2Inc$FramedAverage, ContextF = yr2Inc$FramedContext, Framing = yr2Inc$FramingType, Shift = yr2Inc$Shift.N.F)
df3Inc <- data.frame(AverageN = yr3Inc$NeutralAverage, ContextN = yr3Inc$NeutralContext, AverageF = yr3Inc$FramedAverage, Framing = yr3Inc$FramingType, Shift = yr3Inc$Shift.N.F.)
df4Inc <- data.frame(AverageN = yr4Inc$NeutralAverage, ContextN = yr4Inc$NeutralContext, AverageF = yr4Inc$FramedAverage, Framing = yr4Inc$FramingType, Shift = yr4Inc$Shift.N.F.)

KAIdf1Inc <- data.frame(AverageN = KAIyr1Inc$NeutralAverage, ContextN = KAIyr1Inc$NeutralContext, AverageF = KAIyr1Inc$FramedAverage, ContextF = KAIyr1Inc$FramedContext, Framing = KAIyr1Inc$FramingType, Shift = KAIyr1Inc$Shift.N.F., KAITotal = KAIyr1Inc$KAITotal)
KAIdf2Inc <- data.frame(AverageN = KAIyr2Inc$NeutralAverage, ContextN = KAIyr2Inc$NeutralContext, AverageF = KAIyr2Inc$FramedAverage, ContextF = KAIyr2Inc$FramedContext, Framing = KAIyr2Inc$FramingType, Shift = KAIyr2Inc$Shift.N.F., KAITotal = KAIyr2Inc$KAITotal)
KAIdf3Inc <- data.frame(AverageN = KAIyr3Inc$NeutralAverage, ContextN = KAIyr3Inc$NeutralContext, AverageF = KAIyr3Inc$FramedAverage, Framing = KAIyr3Inc$FramingType, Shift = KAIyr3Inc$Shift.N.F., KAITotal = KAIyr3Inc$KAITotal)
KAIdf4Inc <- data.frame(AverageN = KAIyr4Inc$NeutralAverage, ContextN = KAIyr4Inc$NeutralContext, AverageF = KAIyr4Inc$FramedAverage, Framing = KAIyr4Inc$FramingType, Shift = KAIyr4Inc$Shift.N.F., KAITotal = KAIyr4Inc$KAITotal)

df1Rad <- data.frame(AverageN = yr1Rad$NeutralAverage, ContextN = yr1Rad$NeutralContext, AverageF = yr1Rad$FramedAverage, ContextF = yr1Rad$FramedContext, Framing = yr1Rad$FramingType, Shift = yr1Rad$Shift.N.F.)
df2Rad <- data.frame(AverageN = yr2Rad$NeutralAverage, ContextN = yr2Rad$NeutralContext, AverageF = yr2Rad$FramedAverage, ContextF = yr2Rad$FramedContext, Framing = yr2Rad$FramingType, Shift = yr2Rad$Shift.N.F)
df3Rad <- data.frame(AverageN = yr3Rad$NeutralAverage, ContextN = yr3Rad$NeutralContext, AverageF = yr3Rad$FramedAverage, Framing = yr3Rad$FramingType, Shift = yr3Rad$Shift.N.F.)
df4Rad <- data.frame(AverageN = yr4Rad$NeutralAverage, ContextN = yr4Rad$NeutralContext, AverageF = yr4Rad$FramedAverage, Framing = yr4Rad$FramingType, Shift = yr4Rad$Shift.N.F.)

KAIdf1Rad <- data.frame(AverageN = KAIyr1Rad$NeutralAverage, ContextN = KAIyr1Rad$NeutralContext, AverageF = KAIyr1Rad$FramedAverage, ContextF = KAIyr1Rad$FramedContext, Framing = KAIyr1Rad$FramingType, Shift = KAIyr1Rad$Shift.N.F., KAITotal = KAIyr1Rad$KAITotal)
KAIdf2Rad <- data.frame(AverageN = KAIyr2Rad$NeutralAverage, ContextN = KAIyr2Rad$NeutralContext, AverageF = KAIyr2Rad$FramedAverage, ContextF = KAIyr2Rad$FramedContext, Framing = KAIyr2Rad$FramingType, Shift = KAIyr2Rad$Shift.N.F., KAITotal = KAIyr2Rad$KAITotal)
KAIdf3Rad <- data.frame(AverageN = KAIyr3Rad$NeutralAverage, ContextN = KAIyr3Rad$NeutralContext, AverageF = KAIyr3Rad$FramedAverage, Framing = KAIyr3Rad$FramingType, Shift = KAIyr3Rad$Shift.N.F., KAITotal = KAIyr3Rad$KAITotal)
KAIdf4Rad <- data.frame(AverageN = KAIyr4Rad$NeutralAverage, ContextN = KAIyr4Rad$NeutralContext, AverageF = KAIyr4Rad$FramedAverage, Framing = KAIyr4Rad$FramingType, Shift = KAIyr4Rad$Shift.N.F., KAITotal = KAIyr4Rad$KAITotal)

model1Inc <- lm(Shift ~ AverageN + ContextN + ContextF, data=df1Inc)
summary(model1Inc)
model2Inc <- lm(Shift ~ AverageN + ContextN + ContextF, data=df2Inc)
summary(model2Inc)
model3Inc <- lm(Shift ~ AverageN + ContextN, data=df3Inc)
summary(model3Inc)
model4Inc <- lm(Shift ~ AverageN + ContextN, data=df4Inc)
summary(model4Inc)

model1Rad <- lm(Shift ~ AverageN + ContextN + ContextF, data=df1Rad)
summary(model1Rad)
model2Rad <- lm(Shift ~ AverageN + ContextN + ContextF, data=df2Rad)
summary(model2Rad)
model3Rad <- lm(Shift ~ AverageN + ContextN, data=df3Rad)
summary(model3Rad)
model4Rad <- lm(Shift ~ AverageN + ContextN, data=df4Rad)
summary(model4Rad)

KAImodel1Inc <- lm(Shift ~ AverageN + ContextN + ContextF + as.numeric(KAITotal), data=KAIdf1Inc)
summary(KAImodel1Inc)
KAImodel2Inc <- lm(Shift ~ AverageN + ContextN + ContextF + as.numeric(KAITotal), data=KAIdf2Inc)
summary(KAImodel2Inc)
KAImodel3Inc <- lm(Shift ~ AverageN + ContextN + as.numeric(KAITotal), data=KAIdf3Inc)
summary(KAImodel3Inc)
KAImodel4Inc <- lm(Shift ~ AverageN + ContextN + as.numeric(KAITotal), data=KAIdf4Inc)
summary(KAImodel4Inc)

KAImodel1Rad <- lm(Shift ~ AverageN + ContextN + ContextF + as.numeric(KAITotal), data=KAIdf1Rad)
summary(KAImodel1Rad)
KAImodel2Rad <- lm(Shift ~ AverageN + ContextN + ContextF + as.numeric(KAITotal), data=KAIdf2Rad)
summary(KAImodel2Rad)
KAImodel3Rad <- lm(Shift ~ AverageN + ContextN + as.numeric(KAITotal), data=KAIdf3Rad)
summary(KAImodel3Rad)
KAImodel4Rad <- lm(Shift ~ AverageN + ContextN + as.numeric(KAITotal), data=KAIdf4Rad)
summary(KAImodel4Rad)

#Year 1 Regression
new_year1 <- data.frame(matrix(ncol = 3))
colnames(new_year1) <- c("Context", "Average", "Framing")
for(i in 1:nrow(yr1)){
  neutral <- c(as.character(yr1[i,]$NeutralContext), yr1[i,]$NeutralAverage, "Neutral") 
  framed <- c(as.character(yr1[i,]$FramedContext), yr1[i,]$FramedAverage, as.character(yr1[i,]$Framing))
  
  new_year1 <- rbind(new_year1, neutral)
  new_year1 <- rbind(new_year1, framed)
}

new_year1 <- new_year1[2:nrow(new_year1),]
new_year1$Framing <- relevel(as.factor(new_year1$Framing), ref="Neutral")
new_year1$Context <- relevel(as.factor(new_year1$Context), ref="Snow")


#Year 2 Regression
new_year2 <- data.frame(matrix(ncol = 3))
colnames(new_year2) <- c("Context", "Average", "Framing")
for(i in 1:nrow(yr2)){
  neutral <- c(as.character(yr2[i,]$NeutralContext), yr2[i,]$NeutralAverage, "Neutral") 
  framed <- c(as.character(yr2[i,]$FramedContext), yr2[i,]$FramedAverage, as.character(yr2[i,]$Framing))
  
  new_year2 <- rbind(new_year2, neutral)
  new_year2 <- rbind(new_year2, framed)
}

new_year2 <- new_year2[2:nrow(new_year2),]
new_year2$Framing <- relevel(as.factor(new_year2$Framing), ref="Neutral")
new_year2$Context <- relevel(as.factor(new_year2$Context), ref="Snow")


#Year 3 Regression
new_year3 <- data.frame(matrix(ncol = 3))
colnames(new_year3) <- c("Context", "Average", "Framing")
for(i in 1:nrow(yr3)){
  neutral <- c(as.character(yr3[i,]$NeutralContext), yr3[i,]$NeutralAverage, "Neutral") 
  framed <- c(as.character(yr3[i,]$FramedContext), yr3[i,]$FramedAverage, as.character(yr3[i,]$Framing))
  
  new_year3 <- rbind(new_year3, neutral)
  new_year3 <- rbind(new_year3, framed)
}

new_year3 <- new_year3[2:nrow(new_year3),]
new_year3$Framing <- relevel(as.factor(new_year3$Framing), ref="Neutral")
new_year3$Context <- relevel(as.factor(new_year3$Context), ref="Snow")


#Year 4 Regression
new_year4 <- data.frame(matrix(ncol = 3))
colnames(new_year4) <- c("Context", "Average", "Framing")
for(i in 1:nrow(yr4)){
  neutral <- c(as.character(yr4[i,]$NeutralContext), yr4[i,]$NeutralAverage, "Neutral") 
  framed <- c(as.character(yr4[i,]$FramedContext), yr4[i,]$FramedAverage, as.character(yr4[i,]$Framing))
  
  new_year4 <- rbind(new_year4, neutral)
  new_year4 <- rbind(new_year4, framed)
}

new_year4 <- new_year4[2:nrow(new_year4),]
new_year4$Framing <- relevel(as.factor(new_year4$Framing), ref="Neutral")
new_year4$Context <- relevel(as.factor(new_year4$Context), ref="Snow")


modelyr1 <- lm(Average ~ Context + Framing, data=new_year1)
summary(modelyr1)

modelyr2 <- lm(Average ~ Context + Framing, data=new_year2)
summary(modelyr2)

modelyr3 <- lm(Average ~ Context + Framing, data=new_year3)
summary(modelyr3)

modelyr4 <- lm(Average ~ Context + Framing, data=new_year4)
summary(modelyr4)




#Year 1 Regression
newKAIyear1 <- data.frame(matrix(ncol = 4))
colnames(newKAIyear1) <- c("Context", "Average", "Framing", "TotalKAI")
for(i in 1:nrow(KAIyr1)){
  neutral <- c(as.character(KAIyr1[i,]$NeutralContext), KAIyr1[i,]$NeutralAverage, "Neutral", as.numeric(KAIyr1[i,]$KAITotal)) 
  framed <- c(as.character(KAIyr1[i,]$FramedContext), KAIyr1[i,]$FramedAverage, as.character(KAIyr1[i,]$Framing), as.numeric(KAIyr1[i,]$KAITotal))
  
  newKAIyear1 <- rbind(newKAIyear1, neutral)
  newKAIyear1 <- rbind(newKAIyear1, framed)
}

newKAIyear1 <- newKAIyear1[2:nrow(newKAIyear1),]
newKAIyear1$Framing <- relevel(as.factor(newKAIyear1$Framing), ref="Neutral")
newKAIyear1$Context <- relevel(as.factor(newKAIyear1$Context), ref="Snow")

KAIModelYr1 <- lm(Average ~ Context + Framing + as.numeric(TotalKAI), data=newKAIyear1)
summary(KAIModelYr1)

#Year 2 Regression
newKAIyear2 <- data.frame(matrix(ncol = 4))
colnames(newKAIyear2) <- c("Context", "Average", "Framing", "TotalKAI")
for(i in 1:nrow(KAIyr2)){
  neutral <- c(as.character(KAIyr2[i,]$NeutralContext), KAIyr2[i,]$NeutralAverage, "Neutral", as.numeric(KAIyr2[i,]$KAITotal)) 
  framed <- c(as.character(KAIyr2[i,]$FramedContext), KAIyr2[i,]$FramedAverage, as.character(KAIyr2[i,]$Framing), as.numeric(KAIyr2[i,]$KAITotal))
  
  newKAIyear2 <- rbind(newKAIyear2, neutral)
  newKAIyear2 <- rbind(newKAIyear2, framed)
}

newKAIyear2 <- newKAIyear2[2:nrow(newKAIyear2),]
newKAIyear2$Framing <- relevel(as.factor(newKAIyear2$Framing), ref="Neutral")
newKAIyear2$Context <- relevel(as.factor(newKAIyear2$Context), ref="Snow")

KAIModelYr2 <- lm(Average ~ Context + Framing + as.numeric(TotalKAI), data=newKAIyear2)
summary(KAIModelYr2)

#Year 3 Regression
newKAIyear3 <- data.frame(matrix(ncol = 4))
colnames(newKAIyear3) <- c("Context", "Average", "Framing", "TotalKAI")
for(i in 1:nrow(KAIyr3)){
  neutral <- c(as.character(KAIyr3[i,]$NeutralContext), KAIyr3[i,]$NeutralAverage, "Neutral", as.numeric(KAIyr3[i,]$KAITotal)) 
  framed <- c(as.character(KAIyr3[i,]$FramedContext), KAIyr3[i,]$FramedAverage, as.character(KAIyr3[i,]$Framing), as.numeric(KAIyr3[i,]$KAITotal))
  
  newKAIyear3 <- rbind(newKAIyear3, neutral)
  newKAIyear3 <- rbind(newKAIyear3, framed)
}

newKAIyear3 <- newKAIyear3[2:nrow(newKAIyear3),]
newKAIyear3$Framing <- relevel(as.factor(newKAIyear3$Framing), ref="Neutral")
newKAIyear3$Context <- relevel(as.factor(newKAIyear3$Context), ref="Snow")

KAIModelYr3 <- lm(Average ~ Context + Framing + as.numeric(TotalKAI), data=newKAIyear3)
summary(KAIModelYr3)

#Year 4 Regression
newKAIyear4 <- data.frame(matrix(ncol = 4))
colnames(newKAIyear4) <- c("Context", "Average", "Framing", "TotalKAI")
for(i in 1:nrow(KAIyr4)){
  neutral <- c(as.character(KAIyr4[i,]$NeutralContext), KAIyr4[i,]$NeutralAverage, "Neutral", as.numeric(KAIyr4[i,]$KAITotal)) 
  framed <- c(as.character(KAIyr4[i,]$FramedContext), KAIyr4[i,]$FramedAverage, as.character(KAIyr4[i,]$Framing), as.numeric(KAIyr4[i,]$KAITotal))
  
  newKAIyear4 <- rbind(newKAIyear4, neutral)
  newKAIyear4 <- rbind(newKAIyear4, framed)
}

newKAIyear4 <- newKAIyear4[2:nrow(newKAIyear4),]
newKAIyear4$Framing <- relevel(as.factor(newKAIyear4$Framing), ref="Neutral")
newKAIyear4$Context <- relevel(as.factor(newKAIyear4$Context), ref="Snow")

KAIModelYr4 <- lm(Average ~ Context + Framing + as.numeric(TotalKAI), data=newKAIyear4)
summary(KAIModelYr4)