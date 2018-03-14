setwd("/Users/maya/Documents")
read_Data <- read.csv("Framing-Outcomes-Individual-Shifts.csv")

#Only include ideas that are neutral
Data <- subset(read_Data, Intervention == "Neutral")

Contexts <- c("Snow", "Lids","Rainwater","Belongings")

#plot raw data PR scores 
PR_Score <- function(Context){
  Sample <- sample(which(Data$Context == Context))
  hist(Data$Average[Sample], breaks = seq(1,4,0.25), main = c("Average PR Score Distribution", Context), xlab = c())
  abline(v = mean(Data$Average[Sample]), col = "red",lwd = 2)
  abline(v = mean(Data$Average[Sample])+sd(Data$Average[Sample]),col = "blue", lwd = 2)
  abline(v = mean(Data$Average[Sample])-sd(Data$Average[Sample]),col = "blue", lwd = 2)
  legend(x = "topright",c("Mean",round(mean(Data$Average[Sample]),3),"SD",round(sd(Data$Average[Sample]),3)), col = c("red","red","blue","blue"), lwd = c(2,2,2))
  #print(round(shapiro.test(Data$Average[Sample]$p.value),15))
  qqnorm(Data$Average[Sample], main = c("Normal Q-Q Plot", Context));qqline(Data$Average[Sample], col = 2)
  print(sd(Data$Average[Sample]))
  return(c(mean(Data$Average[Sample]), sd(Data$Average[Sample])))
}


meanPR_Score <-0
All_Means_PR <- c()
All_SD_PR <- c()
for(k in 1:4){
  Mean_PR_Score <- PR_Score(Contexts[k])
  All_Means_PR[k] <- Mean_PR_Score[1]
  All_SD_PR[k] <- Mean_PR_Score[2]
}

#Calculate z-scores
Data_Yr4 <- subset(read_Data, Year == 4)
z_score_1_neutral = (Data_Yr4$Average[1] - All_Means_PR[1])/All_SD_PR[1]
z_score_1_framed = (Data_Yr4$Average[2] - All_Means_PR[1])/All_SD_PR[1]
z_score_diff = z_score_1_neutral - z_score_1_framed
Z_score_PR <- data.frame(Participant_ID = c(Data_Yr4$ParticipantID[1]), Neutral_Context = c(as.character(Data_Yr4$Context[1])), Neutral_Avg = c(Data_Yr4$Average[1]),
                      Neutral_Score = c(z_score_1_neutral), Framex_Context = c(as.character(Data_Yr4$Context[2])), Framed_Avg = c(Data_Yr4$Average[2]), Framed_Score = c(z_score_1_framed), 
                      Z_Score_Difference = c(z_score_diff), stringsAsFactors = FALSE)
colnames(Z_score) <- c("Participant ID", "Neutral Context", "Neutral Average", "Neutral Score", "Framed Context", "Framed Average", "Framed Score","Z-Score Difference")
for(x in seq(from=3, to=nrow(Data_Yr4), by=2)){
  if(Data_Yr4$Context[x] == "Snow"){
    z_score_neutral = (Data_Yr4$Average[x] - All_Means_PR[1])/All_SD_PR[1]
    z_score_framed = (Data_Yr4$Average[x+1] - All_Means_PR[1])/All_SD_PR[1]
  }
  else if(Data_Yr4$Context[x] == "Lids" ){
    z_score_neutral = (Data_Yr4$Average[x] - All_Means_PR[2])/All_SD_PR[2]
    z_score_framed = (Data_Yr4$Average[x+1] - All_Means_PR[2])/All_SD_PR[2]
  }
  Participant <- c(as.numeric(Data_Yr4$ParticipantID[x]),as.character(Data_Yr4$Context[x]),Data_Yr4$Average[x],z_score_neutral,
                   as.character(Data_Yr4$Context[x+1]),Data_Yr4$Average[x+1],z_score_framed,z_score_neutral - z_score_framed)
  Z_score_PR <- rbind(Z_score_PR, Participant)
}


#Central Limit Thm function 
Mean_of_Means <- c()
allSDs <- c()
CLT <- function(Context,Context_num){
  Means <-c()
  Sample_size <- 10000
  for(j in 1:Sample_size){
    Sample <- sample(which(Data$Context == Context), 25, replace = TRUE)
    Means[j] <- mean(Data$Average[Sample])
  }
  hist(Means,
       main = c("Normalized Neutral Distribution", Context), breaks = seq(1,4,0.25))
  qqnorm(Means, main = c("Normal Q-Q Plot", Context));qqline(Means, col = 2)
  #if all values printed are > 0.05, the distribution is normal 
  print(round(shapiro.test(Means)$p.value,3))
  return(c(mean(Means), All_SD_PR[Context_num]/sqrt(Sample_size)))
  print(round(ad.test(Means)$p.value,3))
}

meanValue <- 0
for(k in 1:4){
  meanValue <- CLT(Contexts[k],k)
  Mean_of_Means[k] <- meanValue[1]
  allSDs[k] <- meanValue[2]
}


#z-scores
Data_Yr4 <- subset(read_Data, Year == 4)
z_score_1_neutral = (Data_Yr4$Average[1] - Mean_of_Means[1])/allSDs[1]
z_score_1_framed = (Data_Yr4$Average[2] - Mean_of_Means[1])/allSDs[1]
z_score_diff = z_score_1_neutral - z_score_1_framed
Z_score <- data.frame(Participant_ID = c(Data_Yr4$ParticipantID[1]), Neutral_Context = c(as.character(Data_Yr4$Context[1])), Neutral_Avg = c(Data_Yr4$Average[1]),
                      Neutral_Score = c(z_score_1_neutral), Framex_Context = c(as.character(Data_Yr4$Context[2])), Framed_Avg = c(Data_Yr4$Average[2]), Framed_Score = c(z_score_1_framed), 
                      Z_Score_Difference = c(z_score_diff), stringsAsFactors = FALSE)
colnames(Z_score) <- c("Participant ID", "Neutral Context", "Neutral Average", "Neutral Score", "Framed Context", "Framed Average", "Framed Score","Z-Score Difference")
for(x in seq(from=3, to=nrow(Data_Yr4), by=2)){
  if(Data_Yr4$Context[x] == "Snow"){
    z_score_neutral = (Data_Yr4$Average[x] - Mean_of_Means[1])/allSDs[1]
    z_score_framed = (Data_Yr4$Average[x+1] - Mean_of_Means[1])/allSDs[1]
  }
  else if(Data_Yr4$Context[x] == "Lids" ){
    z_score_neutral = (Data_Yr4$Average[x] - Mean_of_Means[2])/allSDs[2]
    z_score_framed = (Data_Yr4$Average[x+1] - Mean_of_Means[2])/allSDs[2]
  }
  Participant <- c(as.numeric(Data_Yr4$ParticipantID[x]),as.character(Data_Yr4$Context[x]),Data_Yr4$Average[x],z_score_neutral,
                   as.character(Data_Yr4$Context[x+1]),Data_Yr4$Average[x+1],z_score_framed,z_score_neutral - z_score_framed)
  Z_score <- rbind(Z_score, Participant)
}
