library(dplyr)
library(plot3D)
library(rgl)

inputFile <- "../constraint-data.csv"

df <- read.csv(file = inputFile)

df$ratio <- df$ratio / 100

maxSatisfaction <- max(df$satisfaction)
minSatisfacton <- min(df$satisfaction)

# function to produce a classification accuracy plot for each DL
gen3dPlot <- function(dl,theTitle,fname){
  df <- filter(df, DL == dl)

  graphics.off()
  png(filename = fname, width = 7, height = 7, units = "in", res = 100)

  scatter3D(df$ratio, df$delta, df$satisfaction, ticktype = "detailed", xlab="Constraint weight", ylab="Classification delta", zlab="Constraint satisfaction", clab = c("Constraint", "satisfaction"), main=theTitle, phi = 0, bty ="g", cex=2, pch=20)

  dev.off()
}

gen3dPlot("LossFunction-DL2","Constraint Satisfaction of DL2 DL","DL-DL2-constraints.png")
gen3dPlot("LossFunction-Godel","Constraint Satisfaction of Godel DL","DL-Godel-constraints.png")
gen3dPlot("LossFunction-Lukasiewicz","Constraint Satisfaction of Lukasiewicz DL","DL-Lukasiewicz-constraints.png")
gen3dPlot("LossFunction-Product","Constraint Satisfaction of Product DL","DL-Product-constraints.png")
