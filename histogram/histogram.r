#!/usr/bin/env Rscript

# Load libraries
library(jsonlite)

data_name <- "ivv"

# Load data
data <- read.csv(paste("~/Downloads/", data_name, ".csv", sep=""))
x <- na.omit(data$Diff.percent)
summary(x)

info <- c()

info$std_dev <- sd(x)
info$mean <- mean(x)
info$median <- median(x)
info$min <- min(x)
info$max <- max(x)
info$positive_percentile <- (1-ecdf(x)(0.0))

print(info)
write(toJSON(info, pretty=TRUE), file = paste(data_name, ".json", sep=""))

# Build histogram
pdf(paste(data_name, ".pdf", sep=""), bg = "white")
par(mar = c(4,4,0.5,1))
h <- hist(x, col = "lightblue", xlab = "% difference over 364 days", main= "", breaks = seq(min(x), max(x), l=30))

# With Normal line
xfit <- seq(min(x), max(x), length = 80)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "black", lwd = 2)

dev.off()

## build a QQ normality plot
pdf(paste(data_name, "_qq.pdf", sep=""), bg = "white")
qqnorm(x, col="red")
qqline(x)
dev.off()
