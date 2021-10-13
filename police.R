setwd("C:/Users/agozacan/OneDrive - Humberside Fire and Rescue Service/Arson Project/Clean Data")
arson <- read.csv("arson_attacks.csv")
flytipping <- read.csv("flytipping_incidents.csv")
arson <- as.data.frame(arson)
names(arson)[1] <- "inc_incident_ref"
flytipping <- as.data.frame(flytipping)
View(arson)
View(flytipping)

setwd("C:/Users/agozacan/OneDrive - Humberside Fire and Rescue Service/Arson Project/Police Data")
crime <- read.csv("combined_data.csv")
crime <- as.data.frame(crime)
View(crime)
crime_arson <- crime[crime$Crime.type == "Criminal damage and arson",]
crime_asb <- crime[crime$Crime.type == "Anti-social behaviour",]

permonth <- data.frame("Month" = unique(substring(arson$inc_date_of_call, 4, 10))[41:72])

for(i in 1:length(permonth$Month)){
  permonth$Flytipping_Count[i] = sum(permonth$Month[i] == substring(flytipping$creation_date, 4, 10))
  permonth$Arson_Count[i] = sum(permonth$Month[i] == substring(arson$inc_date_of_call, 4, 10))
  permonth$Crime_Count[i] = sum(permonth$Month[i] == substring(
                                                     strftime(
                                                     strptime(
                                                     paste0(
                                                     crime$Month, "-01"),
                                                     format = "%Y-%m-%d"),
                                                     format = "%d/%m/%Y"),
                                                     4, 10))
  permonth$Crime_Arson_Count[i] = sum(permonth$Month[i] == substring(
    strftime(
      strptime(
        paste0(
          crime_arson$Month, "-01"),
        format = "%Y-%m-%d"),
      format = "%d/%m/%Y"),
    4, 10))
  permonth$Crime_ASB_Count[i] = sum(permonth$Month[i] == substring(
    strftime(
      strptime(
        paste0(
          crime_asb$Month, "-01"),
        format = "%Y-%m-%d"),
      format = "%d/%m/%Y"),
    4, 10))
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
xlabels

ggplot(data = mdf, mapping = aes(x = as.factor(Month), y = sqrt(value), colour = variable, group = variable)) +
  geom_point() +
  geom_line(size = 1) +
  labs(x = "Month",
       y = "Square Root of Incident Count",
       title = "Number of Incidents over Time") +
  scale_color_manual(values = c("Flytipping_Count" = "blue",
                                "Arson_Count" = "red",
                                "Crime_Count" = "orange",
                                "Crime_Arson_Count" = "brown",
                                "Crime_ASB_Count" = "green")) +
  theme(text = element_text(size = 12,
                            family = "mono"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  scale_x_discrete(labels = xlabels)

