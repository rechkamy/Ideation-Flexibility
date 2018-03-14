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
