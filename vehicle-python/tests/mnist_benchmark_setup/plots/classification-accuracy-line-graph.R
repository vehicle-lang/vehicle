library(ggplot2)
library(dplyr)

inputFile <- "../classification-data.csv"

df <- read.csv(file = inputFile)

df$ratio <- df$ratio / 100

# accuracy falls off a cliff beyond 85% weight on property based loss term
df <- filter(df, epochs==20)

df <- filter(df, ratio <= 0.95)

df$DL[df$DL == 'LossFunction-DL2'] <- 'DL2'
df$DL[df$DL == 'LossFunction-Godel'] <- 'Godel'
df$DL[df$DL == 'LossFunction-Lukasiewicz'] <- 'Lukasiewicz'
df$DL[df$DL == 'LossFunction-Product'] <- 'Product'

# function to produce a line graph of classification accuracy for all
# DLs using 20 epochs.
genLinePlot <- function(fname){

  ggplot(df, aes(x=ratio, y=accuracy, group=DL)) +
     geom_line(aes(color=DL)) +
     geom_point(aes(color=DL)) +
     theme_classic() +
     theme(text = element_text(size=18)) +
     labs(y="Classification accuracy", x="Constraint weight in loss function", title="Classification accuracy with 20 epochs") +
     scale_y_continuous(labels = scales::percent) +
     scale_x_continuous(labels = scales::percent) +
     theme(legend.position = c(0.2, 0.2))

  ggsave(fname)
}

genLinePlot("all-DLs-classification-accuracy.png")
