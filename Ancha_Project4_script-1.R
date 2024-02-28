#Praveen Ancha,10-23-2023,Project 4

at("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
#clears packages
options(scipen = 100) # disables scientific notation for entire R
session


#1
df<-read.csv("C:\\Users\\PRAVEEN\\Desktop\\R folder\\Ancha_Project4\\AB_NYC_2019.csv")
library(dplyr)
library(janitor)
summary(df)
clean_names(df)
dim(df)


#2
#a.renaming columns
colnames(df)[c(5,11,12,13,15,16)]<-c("Location of stay","No of nights per stay","Total reviews","latest review","Total Hosted Properities","availability of rooms")
#b.Managing NAs
df_cleaned<-na.omit(df)
df_cleaned

#c Correcting datatypes

df$last_review<-as.Date(df$last_review)
df$last_review
df$number_of_reviews<-as.character(df$number_of_reviews)
df$number_of_reviews

#d Removing columns or rows
df$neighbourhood <- NULL

#e Manipulating strings
gsub("and","&",df$name) #this is used to replace string values 
df$name<-str_split(df$name,pattern="") #this is used to split the string in pattern
paste(df$neighbourhood,df$neighbourhood_group,sep=",")#this is used to concatenate string of two diffrent columns
toupper(df$room_type) #this function is used to change the string values to uppercase

#f Reorganizing the data 
library(tidyr)
library(reshape2)
library(dplyr)
install.packages("reshape2")

melt(df)
summarized_data<-df%>%group_by(reviews_per_month) %>% summarize(Avg_value = mean(reviews_per_month))
split(df,df$neighbourhood_group)
`arrange(df,price)

#g Other steps that prepare your data 
is.na(df)
distinct(df)



#3.descriptive statistics for interesting variables
summary(df)

#4 bar chart 
 library(ggplot2)
# Calculate minimum and maximum prices for each room type
room_type_summary <- df %>%
  group_by(room_type) %>%
  summarise(min_price = min(price), max_price = max(price))

# box plot
# Create a color palette for the room_type variable
room_type_color_palette <- c("Entire home/apt" = "skyblue", "Private room" = "lightcoral", "Shared room" = "green")

# Create a boxplot of price by room_type, with the fill aesthetic mapped to the room_type variable and the color palette specified
box_plot <- ggplot(df, aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot() +
  labs(title = "Distribution of Minimum and Maximum Prices for Each Room Type", x = "Room Type", y = "Price") +
  scale_fill_manual(values = room_type_color_palette)

box_plot


#bar graph for neighborhood groups
ggplot(df, aes(x = neighbourhood_group)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Preferred Neighborhood Groups in New York City", x = "Neighbourhood_group", y = "Count")
#Brooklyn is the most preferred neighborhood group in New York City, with over twice as many people preferring it as any other neighborhood group.

# scatter plot for the price vs number of reviews
ggplot(df, aes(x = number_of_reviews, y = price, color=neighbourhood_group )) +
  geom_point() +
  labs(title = "Number of Reviews vs. Price of Airbnb Listings",
       x = "Number of Reviews",
       y = "Price") +
  geom_smooth(method = "lm")


#EXPANDING

#1
#creating a new column 
df <- df %>% mutate(axes = paste(latitude,longitude,sep=","))
View(df)

#2
#creating a new data from the existing data frame 
library(dplyr)
df1<- df %>% filter(neighbourhood_group=="Brooklyn") %>% select(id,name,host_id,neighbourhood_group,neighbourhood,latitude,longitude,room_type,price,minimum_nights,number_of_reviews,last_review,reviews_per_month,calculated_host_listings_count,availability_365)
clean_names(df1)
na.omit(df1)

library(ggplot2)
#scatter plot of property locations in Brooklyn by latitude and longitude
ggplot(df1, aes(x = longitude, y = latitude, color = room_type)) +
  geom_point() +
  labs(title = "Property Locations in Brooklyn by Latitude and Longitude", x = "Longitude", y = "Latitude")

#box plot of Distribution of Minimum and Maximum Prices for Each Room Type in Brooklyn
box_plot<-ggplot(df1, aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot() +
  labs(title = "Distribution of Minimum and Maximum Prices for Each Room Type in Brooklyn", x = "Room Type", y = "Price") +
  scale_fill_manual(values = c("Entire home/apt" = "skyblue", "Private room" = "lightcoral", "Shared room" = "green"))
box_plot
