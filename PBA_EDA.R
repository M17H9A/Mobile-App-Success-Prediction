final_table_eda <- final_table

#PLOT 1: User rating by Prime genre and Paid Status
ggplot(final_table_eda, aes(x = prime_genre, y = user_rating, fill = factor(paid_status))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "User Rating by Prime Genre and Paid Status",
       x = "Prime Genre",
       y = "Mean User Rating",
       fill = "Paid Status") +
  theme_minimal()

#PLOT 2:Distribution of apps by success and paid status
library(ggplot2)
ggplot(final_table_binary, aes(x = interaction(factor(successful_app), factor(paid_status)))) +
  geom_bar(fill = "red") +
  labs(title = "Distribution of Apps by Success and Paid Status", x = "Categories", y = "Count") + scale_fill_manual(values = c("red"))

#PLOT 3:No of Apps in each genre
ggplot(final_table_eda, aes(x = prime_genre)) +
  geom_bar() +
  ggtitle("Number of Apps in Each Prime Genre") +
  xlab("Prime Genre") +
  ylab("Name of apps")

#PLOT 4:Comparison Between Total Rating Count Vs User Rating
ggplot(final_table_eda, aes(x = rating_count_tot, y = user_rating)) +
  geom_point() +
  ggtitle("Scatter Plot of Rating Count vs. User Rating") +
  xlab("Total Rating Count") +
  ylab("User Rating")

#PLOT 5:Comparison between Content Rating(age) and User Rating count
ggplot(final_table_eda, aes(x = cont_rating, y = user_rating)) +
  geom_boxplot() +
  ggtitle("Boxplot of User Ratings by Content Rating") +
  xlab("Content Rating") +
  ylab("User Rating")

#PLOT 6:Distribution of Paid and free apps
ggplot(final_table_eda, aes(x = "", fill = paid_status)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  ggtitle("Distribution of Paid and Free Apps") +
  scale_fill_manual(values = c("Paid" = "red", "Non-Paid" = "brown"))

#PLOT 7:Mean app size by prime genre 
library(scales)
ggplot(final_table_eda, aes(x = prime_genre, y = size_mb)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", fill = "skyblue") +
  labs(title = "Mean App Size by Prime Genre",
       x = "Prime Genre",
       y = "Mean App Size (MB)") +
  scale_y_continuous(labels = unit_format(unit = "MB", scale = 0.1)) +
  theme_minimal()

# PLOT 8:Comparison between paid status and total rating count
library(dplyr)
final_table_1<- final_table_1 %>%
  mutate(isFree = ifelse(price == 0.00, "Free app", "Paid app"))
print(final_table_1)
isFree_counts <- table(final_table_1$isFree)
library(ggplot2)
options(repr.plot.width=3.7, repr.plot.height=6.27)
mean_rating <- mean(final_table_eda$rating_count_tot)
ggplot(final_table_1, aes(x=isFree, y=rating_count_tot)) +
  geom_bar(stat="identity", position="dodge", fill="blue", width=0.7) +
  geom_point(aes(x=isFree, y=mean_rating), color="red", size=3) +
  geom_segment(aes(x=isFree, xend=isFree, y=0, yend=mean_rating), linetype="dashed", color="red") +
  labs(x="isFree", y="Rating Count Total") +
  theme_minimal()

#PLOT 9:Count of User Rating
library(ggplot2)
ggplot(final_table_eda, aes(x = "", fill = factor(user_rating))) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of User Ratings",
       fill = "User Rating") +
  theme_minimal()

#PLOT 10:Comparison between Language Supported and total rating count
library(ggplot2)
options(repr.plot.width=19.7, repr.plot.height=8.27)
ggplot(final_table_eda, aes(x = lang.num, y = rating_count_tot, color = 'Red')) +
  geom_point() + labs(x = 'No of language support', y = 'Total rating count') +
  theme_minimal()

#PLOT 11:Comparison between Prime genre and Total rating count 
library(ggplot2)
options(repr.plot.width = 29.7, repr.plot.height = 8.27)
ggplot(final_table_eda, aes(x = factor(prime_genre), y = rating_count_tot, fill = factor(user_rating))) +
  geom_bar(stat = "identity") +
  labs(x = "Prime Genre", y = "Total Rating Count") +
  theme_minimal()

#PLOT 12:Comparison between Supported device and Total rating count 
library(ggplot2)
options(repr.plot.width = 19.7, repr.plot.height = 8.27)
ggplot(final_table_eda, aes(x = factor(sup_devices.num), y = final_table_1$sup_devices.num, size = 3)) +
  geom_point(fill = "blue", alpha = 0.7, shape = 21) +
  scale_size_continuous(range = c(3, 15)) +  # Adjust the size range as needed
  labs(x = "Supporting Devices Number", y = "Count") +
  theme_minimal()

#PLOT 13:Comparison between No of Screenshots and Total rating count
library(ggplot2)
options(repr.plot.width = 29.7, repr.plot.height = 8.27)
ggplot(final_table_eda, aes(x = factor(ipadSc_urls.num), y = rating_count_tot)) +
  geom_segment(aes(xend = factor(ipadSc_urls.num), yend = 0), color = "blue", size = 1.5) +
  geom_point(color = "blue", size = 3) +  # Add points for each data point
  labs(x = "Number of iPad Screenshots", y = "Total Rating Count") +
  theme_minimal()

