setwd("/Users/Amy/Documents")
Data <- read.csv("IF-Data-6-Ideas-v09-with-PR-Codes-v2.csv")

#Only include ideas that have been coded, are from year 4, and are framing or neutral
Yr4_Data <- subset(Data, GroupID == 6
                        & PRConsensusCode != " "
                        & PRConsensusCode != 0)

Idea_Val <- Yr4_Data$PRConsensusCode
Prop_Mat <- matrix(ncol = 4)

#Creates a bar plot that shows the amount of ideas given each score
idea_distribution <- function(ideas, title){
  Count <- table(factor(Idea_Val[ideas], levels = 1:4))
  Proportions <- prop.table(Count)
  Bar<- barplot(Proportions, ylim=c(0,1), ylab = "Proportion", xlab = "Idea Score", main=title)
  text(x=Bar, y=Proportions, pos = 3, cex= 0.8,col = "red", labels=Count)
  return(Proportions)
}

Contexts <- c("Snow", "Lids")
Interventions <- c("Neutral", "FramingToolIncremental", "FramingToolRadical")

#Gather data for specific context and intervention and create barplot
for(i in 1:length(Contexts)){
  for(j in 1:length(Interventions)){
    ideas <- which(Yr4_Data$ContextShortName == Contexts[i] & Yr4_Data$ActivityType == Interventions[j])
    if(Interventions[j] == "Neutral"){
      title <- paste("Distribution of ", Contexts[i], "Neutral Ideas")
    }
    else{
      title <- paste("Distribution of ", Contexts[i], Interventions[j])
    }
    Prop_Mat <- rbind(Prop_Mat, idea_distribution(ideas, title))
  }
}

par(mfrow=c(3,2))

con <- 1
for (k in seq(from=2, to=7, by=3)){
  AN_colors <- c("red", "orange")
  IN_colors <- c("red", "blue")
  barplot(Prop_Mat[c(k,k+1),], ylim=c(0,0.8), beside=TRUE, col=AN_colors, main=c(Contexts[con], "Year 4 Neutral and Adaptive"))
  legend("topright", c("Neutral", "Adaptive"), fill=AN_colors)
  
  barplot(Prop_Mat[c(k,k+2),], ylim=c(0,0.8), beside=TRUE, col=IN_colors, main=c(Contexts[con], "Year 4 Neutral and Innovative"))
  legend("topright", c("Neutral", "Innovatve"), fill=IN_colors)
  con <- con + 1
}

rownames(Prop_Mat) <- c("Blank", "Neutral Snow", "Adaptive Snow", "Innovative Snow", "Neutral Lids", "Adaptive Lids", "Innovative Lids")
colnames(Prop_Mat) <- c("1", "2", "3", "4")

colors <- c("darkslateblue", "darkorchid3", "hotpink3", "indianred2")
con <- 1
for (j in seq(from=2, to=7, by=3)){
  barplot(t(Prop_Mat[c(j,j+1,j+2),]), col=colors, main=c(Contexts[con], "Year 4"))
  legend("topright", c("1", "2", "3", "4"), fill=colors)
  con <- con + 1
}
