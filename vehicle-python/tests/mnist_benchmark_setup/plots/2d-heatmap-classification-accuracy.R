library(ggplot2)
library(dplyr)

inputFile <- "../classification-data.csv"

df <- read.csv(file = inputFile)

df$ratio <- df$ratio / 100

# accuracy falls off a cliff beyond 85% weight on property based loss term
df <- filter(df, ratio <= 0.8)

maxAccuracy <- max(df$accuracy)
minAccuracy <- min(df$accuracy)

# function to produce a classification accuracy plot for each DL
gen2dPlot <- function(dl,theTitle,fname){
  df <- filter(df, DL == dl)

  ggplot(df, aes(x = epochs, y = ratio, fill = accuracy)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(colours=c("red","yellow","green"),limits=c(minAccuracy,maxAccuracy),labels = scales::percent) +
    theme_bw() +
    theme(text = element_text(size=18)) +
    labs(y="Constraint weight in loss function", title=theTitle) +
    scale_y_continuous(labels = scales::percent)

  ggsave(fname)
}

gen2dPlot("LossFunction-DL2","Classification accuracy of DL2 DL","DL-DL2.png")
gen2dPlot("LossFunction-Godel","Classification accuracy of Godel DL","DL-Godel.png")
gen2dPlot("LossFunction-Lukasiewicz","Classification accuracy of Lukasiewicz DL","DL-Lukasiewicz.png")
gen2dPlot("LossFunction-Product","Classification accuracy of Product DL","DL-Product.png")
