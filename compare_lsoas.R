setwd("C:/Users/agozacan/OneDrive - Humberside Fire and Rescue Service/Arson Project")
arson <- read.csv("arson_lsoa_numbers.csv")
flytipping <- read.csv("lsoa_numbers.csv")

arson <- as.data.frame(arson)
flytipping <- as.data.frame(flytipping)

View(arson)
View(flytipping)

plot1 <- barplot(flytipping$num_incidents,
                 names.arg = seq(1, length(flytipping$num_incidents)),
                 xlab = "Rank",
                 ylab = "Number of Incidents",
                 col = "light blue",
                 main = "Flytipping Incidents per LSOA")
lines(plot1, flytipping$num_incidents, lwd = 3, col = "blue")
lines(plot1, 1000 * dchisq(0.02 * 1:length(flytipping$num_incidents), 1), lwd = 3, col = "red")

plot2 <- barplot(arson$num_incidents,
                 names.arg = seq(1, length(arson$num_incidents)),
                 xlab = "Rank",
                 ylab = "Number of Incidents",
                 col = "pink",
                 main = "Arson Incidents per LSOA"
                 )
lines(plot2, arson$num_incidents, lwd = 3, col = "red")
lines(plot2, 200 * dchisq(0.02 * 1:length(arson$lsoa_code), 1), lwd = 3, col = "blue")

## We hypothesise that each LSOA has the same proportion of flytipping incidents as it does arson incidents ##

flytipping$percentage <- 100 * (flytipping$num_incidents / sum(flytipping$num_incidents))

arson$percentage <- 100 * (arson$num_incidents / sum(arson$num_incidents))

shared <- data.frame("lsoa_code" = intersect(flytipping$lsoa_code, arson$lsoa_code))
for(i in 1:length(shared$lsoa_code)){
  shared$flytipping_percentage[i] = flytipping$percentage[flytipping$lsoa_code == shared$lsoa_code[i]]
  shared$arson_percentage[i] = arson$percentage[arson$lsoa_code == shared$lsoa_code[i]]
}

View(shared)

## Larger arson percentages tend to be with the larger flytipping percentages, and vice versa.
## We need to test this properly though ##

reg_model <- lm(log(arson_percentage) ~ I(log(flytipping_percentage)), data = shared)
summary(reg_model)

library(ggplot2)

ggplot(data = shared, mapping = aes(log(flytipping_percentage), log(arson_percentage))) +
    geom_point() +
    labs(x = "Logarithm of Percentage Take-up of Flytipping Incidents",
         y = "Logarithm of Percentage Take-up of Arson Incidents") +
    geom_abline(aes(intercept = reg_model$coefficients[1],
                    slope = reg_model$coefficients[2]),
                    color = "blue",
                    linetype = "longdash",
                    size = 1) +
    theme(text = element_text(size = 12,  family = "mono"), panel.background = element_rect(fill = "gray95"))

## There does indeed seem to be a log-linear relationship between the two variables, and since the p-value
## of the slope is really small, we can say that there is a statistically significant relationship between
## the two. ##