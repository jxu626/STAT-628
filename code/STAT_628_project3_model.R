data<-read.csv("./Downloads/merged_final (1).csv")
colnames(data)
selected_data=data[c("HOUSEHOLD.INCOME","year","count","mean","population")]
selected_data$year=as.factor(selected_data$year)
library(lme4)
library(dplyr)

data2<-selected_data%>%
  mutate(income_c=scale(HOUSEHOLD.INCOME,scale = TRUE),Population_c=scale(population,scale = TRUE))

rand_eff_count <- lmer(count ~ population + (population|year)+HOUSEHOLD.INCOME+(HOUSEHOLD.INCOME|year),
                 data = selected_data)
summary(rand_eff_count)


selected_data2=data[c("HOUSEHOLD.INCOME","year","count","mean","population")]
selected_data2$year=as.factor(selected_data2$year)
data3<-selected_data2%>%
  mutate(income_c=scale(HOUSEHOLD.INCOME,scale = TRUE),Population_c=scale(population,scale = TRUE))


data4 <- data3 %>%
  group_by(year) %>%
  mutate(
    mean_median = median(mean, na.rm = TRUE),
    count_median = median(count, na.rm = TRUE),
    success = ifelse(mean > mean_median & count > count_median, 1, 0)
  ) %>%
  ungroup()

data4$RestaurantsTakeOut <- ifelse(data$RestaurantsTakeOut == "True", 1, 0)


data$NoiseLevel <- ifelse(data$NoiseLevel %in% c("'loud'", "u'loud'"), 1,
                          ifelse(data$NoiseLevel %in% c("'average'", "u'average'"), 2,
                
                                                 ifelse(data$NoiseLevel %in% c("'quiet'", "u'quiet'"), 3, 0)))

data4$RestaurantsPriceRange2<-data$RestaurantsPriceRange2
data4$RestaurantsPriceRange2[is.na(data4$RestaurantsPriceRange2)] <- 0

colnames(data4)[colnames(data4) == "NoiseLevel"] <- "QuietLevel"

data$RestaurantsTableService<-ifelse(data$RestaurantsTableService=="True", 1,0)
data4$RestaurantsTableService=data$RestaurantsTableService

model<- glm(success ~ income_c+RestaurantsPriceRange2+RestaurantsTableService+QuietLevel, data = data4, family = binomial)
summary(model)

vif_model <- vif(model)
print(vif_model)


data_final<-data4[c("year","count","mean","success","income_c","RestaurantsPriceRange2","RestaurantsTableService","QuietLevel")]


data_final$postal_code=data$postal_code

file_path <- "./Downloads/data_final.csv"

write.csv(data_final, file_path, row.names = FALSE)

