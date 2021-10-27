data <- read.csv('AB_NYC_2019.csv',header=TRUE)
str(data)
summary(data)

#######package###########
#install.packages('fastDummies')
library('fastDummies')
library(corrplot)
library(tidyverse)
library(caret)
library(MASS)
#install.packages("ggmap")
library(ggmap)

############Exploratory Data###################
property_df <-  data %>% 
  group_by(neighbourhood_group, room_type) %>% 
  summarize(Freq = n())

total_property <-  data %>% 
  filter(room_type %in% c("Private room","Entire home/apt","Entire home/apt")) %>% 
  group_by(neighbourhood_group) %>% 
  summarize(sum = n())

property_ratio <- merge (property_df, total_property, by="neighbourhood_group")

property_ratio <- property_ratio %>% 
  mutate(ratio = Freq/sum)

ggplot(property_ratio, aes(x=neighbourhood_group, y = ratio, fill = room_type)) +
  geom_bar(position = "dodge", stat="identity") + 
  xlab("Borough") + ylab ("Count") +
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Which types of Listings are there in NYC?",
          subtitle = "Map showing Count of Listing Type by Borough ") +
  theme(plot.title = element_text(face = "bold", size = 14) ) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35", hjust = 0.5)) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) +
  xlab("Neighborhood") + ylab("Percentage")

data %>% 
  filter(!(is.na(neighbourhood_group))) %>% 
  filter(!(neighbourhood_group == "Unknown")) %>% 
  group_by(neighbourhood_group) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(neighbourhood_group, mean_price), y = mean_price, fill = neighbourhood_group)) +
  geom_col(stat ="identity", color = "black", fill="#357b8a") +
  coord_flip() +
  theme_gray() +
  labs(x = "Neighbourhood Group", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparison for each Neighbourhood Group", subtitle = "Price vs Neighbourhood Group") + 
  xlab("Neighbourhood Group") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())

data %>% 
  filter(!(is.na(room_type))) %>% 
  filter(!(room_type == "Unknown")) %>% 
  group_by(room_type) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(room_type, mean_price), y = mean_price, fill = room_type)) +
  geom_col(stat ="identity", color = "black", fill="#357b8a") +
  coord_flip() +
  theme_gray() +
  labs(x = "Room Type", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparison with all Room Types", subtitle = "Price vs Room Type") + 
  xlab("Room Type") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())

height <- max(data$latitude) - min(data$latitude)
width <- max(data$longitude) - min(data$longitude)
LA_borders <- c(bottom  = min(data$latitude)  - 0.1 * height, 
                top     = max(data$latitude)  + 0.1 * height,
                left    = min(data$longitude) - 0.1 * width,
                right   = max(data$longitude) + 0.1 * width)

map <- get_stamenmap(LA_borders, zoom = 10, maptype = "toner-lite")
ggmap(map) +
  geom_point(data = data, mapping = aes(x = longitude, y = latitude, 
                                        col = log(price))) +
  scale_color_distiller(palette = "RdYlGn", direction = 1)

airbnb_cor <- data[, sapply(data, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor, method = "spearman")
corrplot(correlation_matrix, method = "color")



###########Data preparation####################
#Find missing values
table(is.na(data))
apply(data,2,function(x) sum(is.na(x))) 

#Impute missing values review_per_month
miss=is.na(data$reviews_per_month)
miss
data$reviews_per_month[miss]=0
sum(is.na(data$reviews_per_month))

#last review,id,name,host_id,host_name,neighborhood preparation
data=data[,-(1:4)]
data=data[,-(2)]
data=data[,-(8)]
summary(data)

#Outliers
#boxplot for check outliers
boxplot(data$price, xlab="Price")
boxplot(data$number_of_reviews, xlab="Number of reviews")
boxplot(data$availability_365, xlab="availability_365")
boxplot(data$reviews_per_month, xlab="reviews_per_month")
boxplot(data$minimum_nights, xlab="minimum_nights")

#remove outliers
data <-data[-which(data$price%in%boxplot.stats(data$price)$out),]
data <-data[-which(data$number_of_reviews%in%boxplot.stats(data$number_of_reviews)$out),]
data <-data[-which(data$reviews_per_month%in%boxplot.stats(data$reviews_per_month)$out),]
data <-data[-which(data$minimum_nights%in%boxplot.stats(data$minimum_nights)$out),]

#boxplot check again
boxplot(data$price, xlab="Price")
boxplot(data$number_of_reviews, xlab="Number of reviews")
boxplot(data$availability_365, xlab="availability_365")
boxplot(data$reviews_per_month, xlab="reviews_per_month")
boxplot(data$minimum_nights, xlab="minimum_nights")

#Dummy variables
data <- dummy_cols(data, select_columns = c('neighbourhood_group', 'room_type'),
                         remove_selected_columns = TRUE)


#Min-max normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$price<-normalize(data$price)
data$latitude<-normalize(data$latitude)
data$longitude<-normalize(data$longitude)
data$minimum_nights<-normalize(data$minimum_nights)
data$number_of_reviews<-normalize(data$number_of_reviews)
data$calculated_host_listings_count<-normalize(data$calculated_host_listings_count)
data$availability_365<-normalize(data$availability_365)
summary(data)

###########Model##################


#Selection Independent variable
# Fit the full model 
full.model <- lm(price ~., data = data)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

#split data Train & Test
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.8*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]
#Linear Regression Model
model=lm(price~.,data = train)
summary(model)

model2=lm(price ~ latitude+longitude+minimum_nights
          +number_of_reviews+reviews_per_month+calculated_host_listings_count
          +availability_365+neighbourhood_group_Bronx+neighbourhood_group_Brooklyn
          +neighbourhood_group_Manhattan+neighbourhood_group_Queens
          +`room_type_Entire home/apt`+`room_type_Private room`,data=train)
summary(model2)
pred = predict(model2,test)
pred
rmse <- sqrt(mean((pred - test$price)^2))
rmse
rsq <- function (x, y) cor(x, y) ^ 2
r2 = rsq(test$price,pred)
r2







