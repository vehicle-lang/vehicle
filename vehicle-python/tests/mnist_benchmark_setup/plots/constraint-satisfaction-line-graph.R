library(ggplot2)
library(dplyr)

inputFile <- "../constraint-data.csv"

df <- read.csv(file = inputFile)

df$ratio <- df$ratio / 100

# accuracy falls off a cliff beyond 85% weight on property based loss term
df <- filter(df, delta==0.02)

df$DL[df$DL == 'LossFunction-DL2'] <- 'DL2'
df$DL[df$DL == 'LossFunction-Godel'] <- 'Godel'
df$DL[df$DL == 'LossFunction-Lukasiewicz'] <- 'Lukasiewicz'
df$DL[df$DL == 'LossFunction-Product'] <- 'Product'

# function to produce a line graph of constraint satisfaction for all
# DLs using a delta of 0.02
genLinePlot <- function(fname){

  ggplot(df, aes(x=ratio, y=satisfaction, group=DL)) +
     geom_line(aes(color=DL)) +
     geom_point(aes(color=DL)) +
     theme_classic() +
     theme(text = element_text(size=18)) +
     labs(y="Constraint satisfaction", x="Constraint weight in loss function", title="Constraint satisfaction with delta=0.02") +
     scale_y_continuous(labels = scales::percent) +
     scale_x_continuous(labels = scales::percent) +
     theme(legend.position = c(0.2, 0.2))

  ggsave(fname)
}

genLinePlot("all-DLs-constraint-satisfaction.png")
