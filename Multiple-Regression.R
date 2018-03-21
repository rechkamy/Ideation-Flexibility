setwd("/Users/Amy/Documents")
read_Data <- read.csv("Framing-Outcomes-Individual-Shifts.csv")

yr1 <- subset(read_Data, Year == 1)
yr2 <- subset(read_Data, Year == 2)
yr3 <- subset(read_Data, Year == 3)
yr4 <- subset(read_Data, Year == 4)



df1 <- data.frame(AverageN = yr1$NeutralAverage, ContextN = yr1$NeutralContext, AverageF = yr1$FramedAverage, ContextF = yr1$FramedContext, Framing = yr1$FramingType, Shift = yr1$Shift.N.F.)
df2 <- data.frame(AverageN = yr2$NeutralAverage, ContextN = yr2$NeutralContext, AverageF = yr2$FramedAverage, ContextF = yr2$FramedContext, Framing = yr2$FramingType, Shift = yr2$Shift.N.F)
df3 <- data.frame(AverageN = yr3$NeutralAverage, ContextN = yr3$NeutralContext, AverageF = yr3$FramedAverage, Framing = yr3$FramingType, Shift = yr3$Shift.N.F.)
df4 <- data.frame(AverageN = yr4$NeutralAverage, ContextN = yr4$NeutralContext, AverageF = yr4$FramedAverage, Framing = yr4$FramingType, Shift = yr4$Shift.N.F.)

model1 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing, data=df1)
summary(model1)
model2 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing, data=df2)
summary(model2)
model3 <- lm(Shift ~ AverageN + ContextN + Framing, data=df3)
summary(model3)
model4 <- lm(Shift ~ AverageN + ContextN + Framing, data=df4)
summary(model4)

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


modelyr1 <- lm(Average ~ Context + Framing, data=new_year1)
summary(modelyr1)

modelyr2 <- lm(Average ~ Context + Framing, data=new_year2)
summary(modelyr2)

modelyr3 <- lm(Average ~ Context + Framing, data=new_year3)
summary(modelyr3)

modelyr4 <- lm(Average ~ Context + Framing, data=new_year4)
summary(modelyr4)


