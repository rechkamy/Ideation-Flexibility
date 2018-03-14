setwd("/Users/Amy/Documents")
Data <- read.csv("IF-Data-6-Ideas-v09-with-PR-Codes-v2.csv")

#Only include ideas that have been coded, are from year 1, and are framing or neutral
Yr1_Data <- subset(Data, (GroupID == 1 | GroupID == 2 | GroupID == 3)
                        & (StudyDesign == "Framing" | StudyDesign == "AllInterventions")
                        & (ActivityType == "Framing" | ActivityType == "Neutral")
                        & PRConsensusCode != " ")

Idea_Val <- Yr1_Data$PRConsensusCode

#Creates a bar plot that shows the amount of ideas given each score
idea_distribution <- function(ideas, title){
  Count <- table(factor(Idea_Val[ideas], levels = 0:4))
  Bar<- barplot(prop.table(Count), ylim=c(0,1), ylab = "Proportion", xlab = "Idea Score", main=title)
  text(x=Bar, y=prop.table(Count), pos = 3, cex= 0.8,col = "red", labels=Count)
}

Contexts <- c("Snow", "Lids", "Belongings", "Rainwater")
Interventions <- c("Adaptive", "Innovative")
#Interventions <- c("Adaptive", "Neutral", "Innovative")

par(mfrow=c(4,2))


#Gather data for specific context and intervention and create barplot
for(i in 1:length(Contexts)){
  for(j in 1:length(Interventions)){
    ideas <- which(Yr1_Data$ContextShortName == Contexts[i] & Yr1_Data$FramingDesc == Interventions[j])
    if(Interventions[j] == "Neutral"){
      title <- paste("Distribution of ", Contexts[i], "Neutral Ideas")
    }
    else{
      title <- paste("Distribution of ", Contexts[i], Interventions[j], "Framed Ideas")
    }
    idea_distribution(ideas, title)
  }
}
