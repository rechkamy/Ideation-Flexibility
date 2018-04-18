setwd("/Users/Amy/Documents")
read_Data <- read.csv("Framing-Outcomes-Individual-Shifts.csv")

#Separate out by year
yr1 <- subset(read_Data, Year == 1)
yr2 <- subset(read_Data, Year == 2)
yr3 <- subset(read_Data, Year == 3)
yr4 <- subset(read_Data, Year == 4)

#Accounting for KAI score
KAIyr1 <- subset(yr1, KAIValid. == 'Yes')
KAIyr2 <- subset(yr2, KAIValid. == 'Yes')
KAIyr3 <- subset(yr3, KAIValid. == 'Yes')
KAIyr4 <- subset(yr4, KAIValid. == 'Yes')

df1 <- data.frame(AverageN = yr1$NeutralAverage, ContextN = yr1$NeutralContext, AverageF = yr1$FramedAverage, ContextF = yr1$FramedContext, Framing = yr1$FramingType, Shift = yr1$Shift.N.F.)
df2 <- data.frame(AverageN = yr2$NeutralAverage, ContextN = yr2$NeutralContext, AverageF = yr2$FramedAverage, ContextF = yr2$FramedContext, Framing = yr2$FramingType, Shift = yr2$Shift.N.F)
df3 <- data.frame(AverageN = yr3$NeutralAverage, ContextN = yr3$NeutralContext, AverageF = yr3$FramedAverage, Framing = yr3$FramingType, Shift = yr3$Shift.N.F.)
df4 <- data.frame(AverageN = yr4$NeutralAverage, ContextN = yr4$NeutralContext, AverageF = yr4$FramedAverage, Framing = yr4$FramingType, Shift = yr4$Shift.N.F.)

KAIdf1 <- data.frame(AverageN = KAIyr1$NeutralAverage, ContextN = KAIyr1$NeutralContext, AverageF = KAIyr1$FramedAverage, ContextF = KAIyr1$FramedContext, Framing = KAIyr1$FramingType, Shift = KAIyr1$Shift.N.F., KAITotal = KAIyr1$KAITotal)
KAIdf2 <- data.frame(AverageN = KAIyr2$NeutralAverage, ContextN = KAIyr2$NeutralContext, AverageF = KAIyr2$FramedAverage, ContextF = KAIyr2$FramedContext, Framing = KAIyr2$FramingType, Shift = KAIyr2$Shift.N.F., KAITotal = KAIyr2$KAITotal)
KAIdf3 <- data.frame(AverageN = KAIyr3$NeutralAverage, ContextN = KAIyr3$NeutralContext, AverageF = KAIyr3$FramedAverage, Framing = KAIyr3$FramingType, Shift = KAIyr3$Shift.N.F., KAITotal = KAIyr3$KAITotal)
KAIdf4 <- data.frame(AverageN = KAIyr4$NeutralAverage, ContextN = KAIyr4$NeutralContext, AverageF = KAIyr4$FramedAverage, Framing = KAIyr4$FramingType, Shift = KAIyr4$Shift.N.F., KAITotal = KAIyr4$KAITotal)

model1 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing, data=df1)
summary(model1)
model2 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing, data=df2)
summary(model2)
model3 <- lm(Shift ~ AverageN + ContextN + Framing, data=df3)
summary(model3)
model4 <- lm(Shift ~ AverageN + ContextN + Framing, data=df4)
summary(model4)

KAImodel1 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing + as.numeric(KAITotal), data=KAIdf1)
summary(KAImodel1)
KAImodel2 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing + as.numeric(KAITotal), data=KAIdf2)
summary(KAImodel2)
KAImodel3 <- lm(Shift ~ AverageN + ContextN + Framing + as.numeric(KAITotal), data=KAIdf3)
summary(KAImodel3)
KAImodel4 <- lm(Shift ~ AverageN + ContextN + Framing + as.numeric(KAITotal), data=KAIdf4)
summary(KAImodel4)

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