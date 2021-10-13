setwd("C:/Users/agozacan/OneDrive - Humberside Fire and Rescue Service/Arson Project/Clean Data")
arson <- read.csv("arson_attacks.csv")
flytipping <- read.csv("flytipping_incidents.csv")

arson <- as.data.frame(arson)
flytipping <- as.data.frame(flytipping)
names(arson)[1] <- "inc_incident_ref"

View(arson)
View(flytipping)

## We want to produce a model that predicts the number of arson attacks taking place in each LSOA given
## the historic data. For that, we need to separate the data into training and testing. ##

## Let's take flytipping and arson data from 01/04/2015 to 30/03/2020 to be used as training data and then
## let the model predict arson levels from 01/04/2020 to 30/03/2021 with no flytipping data. We will do the
## join on number of incidents in each LSOA in each month.

permonth <- data.frame("Month" = unique(substring(arson$inc_date_of_call, 4, 10)))

for(i in 1:length(permonth$Month)){
  permonth$Flytipping_Count[i] = sum(permonth$Month[i] == substring(flytipping$creation_date, 4, 10))
  permonth$Arson_Count[i] = sum(permonth$Month[i] == substring(arson$inc_date_of_call, 4, 10))
}

View(permonth)

library(ggplot2)
library(reshape2)

mdf <- melt(permonth, id.vars = "Month")
for(i in 1:length(mdf$Month)){
  mdf$Month[i] <- paste0("01/", mdf$Month[i])
}
mdf$Month <- as.Date(mdf$Month, format = "%d/%m/%Y")

View(mdf)

xlabels <- permonth$Month
xlabels[-seq(3, length(permonth$Month), 3)] <- ""
xlabels

ggplot(data = mdf, mapping = aes(x = as.factor(Month), y = value, colour = variable, group = variable)) +
  geom_point() +
  geom_line(size = 1) +
  labs(x = "Month",
       y = "Count",
       title = "Number of Incidents over Time") +
  scale_color_manual(values = c("Flytipping_Count" = "blue", "Arson_Count" = "red")) +
  theme(text = element_text(size = 12,
                            family = "mono"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  scale_x_discrete(labels = xlabels)

perlsoa <- data.frame("lsoa_code" = union(arson$lsoa_code, flytipping$lsoa_code))
perlsoa <- as.data.frame(perlsoa[!apply(perlsoa == "", 1, all),])
names(perlsoa) <- c("lsoa_code")

for(i in 1:70){
  perlsoa[, paste("Flytipping", permonth$Month[i])] = 1
}

for(i in 1:72){
  perlsoa[, paste("Arson", permonth$Month[i])] = 1
}

for(i in 1:length(perlsoa$lsoa_code)){
  for(j in 2:71){
    perlsoa[i, j] = sum((substring(names(perlsoa)[j], 12, 20) == substring(flytipping$creation_date, 4, 10))
                         & (perlsoa[i, 1] == flytipping$lsoa_code))
  }
  for(j in 72:143){
    perlsoa[i, j] = sum((substring(names(perlsoa)[j], 7, 15) == substring(arson$inc_date_of_call, 4, 10))
                        & (perlsoa[i, 1] == arson$lsoa_code))
  }
}

View(perlsoa)

empty_indices = c()
for(i in 1:215){
  if(all(perlsoa[i,2:143] == 0) == TRUE){
    empty_indices <- c(empty_indices, i)
  }
}
empty_indices

# write.csv(perlsoa, "inc_per_lsoa.csv")
