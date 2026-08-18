# load package
install.packages("sjplot")
install.packages("sjmisc")
install.packages("sjlabelled")

library(sjPlot)
library(sjmisc)
library(sjlabelled)

# sample data
data("efc")
efc <- as_factor(efc, c161sex, c172code)
