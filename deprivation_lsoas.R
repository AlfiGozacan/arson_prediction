imd <- read.csv("imd_2019_lsoa.csv")
imd <- data.frame(imd)
View(imd)

for(i in 1:length(shared$lsoa_code)){
  shared$imd_decile[i] = imd$imd_decile[imd$lsoa_code == shared$lsoa_code[i]]
}

library(ggplot2)

ggplot(data = shared, mapping = aes(x = log(flytipping_percentage),
                                    y = log(arson_percentage),
                                    z = imd_decile)) +
  geom_point(aes(color = ..z..)) +
  scale_color_gradient(name = "IMD", low = "red", high = "green2") +
  labs(x = "Logarithm of % Take-up of Flytipping Incidents",
       y = "Logarithm of % Take-up of Arson Incidents",
       title = "Relationship between flytipping and arson, with deprivation index scale") +
  geom_abline(aes(intercept = reg_model$coefficients[1],
                  slope = reg_model$coefficients[2]),
              color = "blue",
              linetype = "longdash",
              size = 1) +
  theme(text = element_text(size = 12,  family = "mono"), panel.background = element_rect(fill = "gray95"))

share_union  <- data.frame("lsoa_code" = union(flytipping$lsoa_code, arson$lsoa_code))

for(i in 1:length(share_union$lsoa_code)){
  if (sum(flytipping$lsoa_code == share_union$lsoa_code[i]) > 0){
    share_union$flytipping_count[i] = flytipping$num_incidents[flytipping$lsoa_code == share_union$lsoa_code[i]]
  } else{
    share_union$flytipping_count[i] = 0
  }
  if (sum(arson$lsoa_code == share_union$lsoa_code[i]) > 0){
    share_union$arson_count[i] = arson$num_incidents[arson$lsoa_code == share_union$lsoa_code[i]]
  } else{
    share_union$arson_count[i] = 0
  }
}

for(i in 1:length(share_union$lsoa_code)){
  share_union$imd_decile[i] = imd$imd_decile[imd$lsoa_code == share_union$lsoa_code[i]]
}

View(share_union)

dep_num <- data.frame("imd_decile" = 1:10)

for(i in 1:10){
  dep_num$fly_count[i] = sum(share_union$flytipping_count[share_union$imd_decile == i])
}

for(i in 1:10){
  dep_num$arson_count[i] = sum(share_union$arson_count[share_union$imd_decile == i])
}

View(dep_num)

total <- data.frame("imd_decile" = 1:10)

for(i in 1:10){
  total$freq[i] = sum(share_union$imd_decile == i)
}

View(total)

ggplot(data = dep_num, mapping = aes(x = imd_decile, y = fly_count)) +
  geom_col(aes(fill = ..x..)) +
  scale_fill_distiller(guide = "none", palette = "Reds") +
  labs(x = "IMD Decile",
       y = "Count",
       title = "Number of flytipping incidents by IMD decile",
       subtitle = "(1 = most deprived, 10 = least deprived)") +
  theme(text = element_text(size = 12,
                            family = "mono"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_label(aes(label = imd_decile), nudge_y = 1000)

ggplot(data = dep_num, mapping = aes(x = imd_decile, y = arson_count)) +
  geom_col(aes(fill = ..x..)) +
  scale_fill_distiller(guide = "none", palette = "Reds") +
  labs(x = "IMD Decile",
       y = "Count",
       title = "Number of arson incidents by IMD decile",
       subtitle = "(1 = most deprived, 10 = least deprived)") +
  theme(text = element_text(size = 12,
                            family = "mono"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_label(aes(label = imd_decile), nudge_y = 200)

ggplot(data = total, mapping = aes(x = imd_decile, y = freq)) +
  geom_col(aes(fill = ..x..)) +
  scale_fill_distiller(guide = "none", palette = "Reds") +
  labs(x = "IMD Decile",
       y = "Count",
       title = "Number of LSOAs in Hull by IMD decile",
       subtitle = "(1 = most deprived, 10 = least deprived)") +
  theme(text = element_text(size = 12,
                            family = "mono"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_label(aes(label = imd_decile), nudge_y = 4)
