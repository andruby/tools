#!/usr/bin/env Rscript

# Load libraries
library(jsonlite)

data_name <- "spy"

# Load data
data <- read.csv(paste("input/", data_name, ".csv", sep=""))
x <- na.omit(data$Diff.percent*100)
summary(x)

info <- c()

info$std_dev <- sd(x)
info$mean <- mean(x)
info$median <- median(x)
info$min <- min(x)
info$max <- max(x)
info$positive_percentile <- (1-ecdf(x)(0.0))

print(info)
write(toJSON(info, pretty=TRUE), file = paste("output/", data_name, ".json", sep=""))

# Build histogram
pdf(paste("output/", data_name, ".pdf", sep=""), bg = "white")
par(mar = c(5,5,2,2))
group_width <- 5
low_group <- floor(min(x)*(1/group_width))/(1/group_width)
high_group <- ceiling(max(x)*(1/group_width))/(1/group_width)
negative_groups <- abs(min(low_group/group_width, 0))
positive_groups <- max(high_group/group_width, 0)
h <- hist(x, col = c(rep("#e34a33", negative_groups), rep("#2ca25f", positive_groups)), main = data_name, xlab = "% difference over 364 days", breaks = seq(low_group, high_group, by=group_width))

# With Normal line
xfit <- seq(min(x), max(x), length = 80)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "black", lwd = 1)

dev.off()

## build a QQ normality plot
pdf(paste("output/", data_name, "_qq.pdf", sep=""), bg = "white")
qqnorm(x, col="red")
qqline(x)
dev.off()
