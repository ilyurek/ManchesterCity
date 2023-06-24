library(rvest)
library(dplyr)

# 2022-2023 Manchester City: Premier League Data from fbref.com

#Scrap the data by using Selector Gadget Tool
document<-read_html("https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats")

data <- document %>% 
  html_nodes("#stats_standard_9 .right , #stats_standard_9 .center , 
             #stats_standard_9 .left") %>% 
  html_text()

#Data manipulation part
col_name<-data[7:39] # column names of the data frame
df <- data.frame(matrix(ncol = length(col_name),nrow = 32)) #empty data frame
colnames(df) <- col_name # set column names as desired
View(df)

interval_indexes <- grep("Matches",data) # Returns indexes labeled with matches
rep_value <- interval_indexes[2]-interval_indexes[1] # repeating 

#Some hard code to create desired data frame
num_col<-length(data[40:1128]) %/% 33
data1<-data[40:1128]
num_col
for(i in 1:num_col){
  start_index <- (i-1)*34 +1
  end_index <- i*34
  df[i,]<-data1[start_index+1:end_index]
}

df<-df[c(1:18),] # First 18 players
#Edit the nation columns for the simplicity
df$Nation<-sapply(strsplit(df$Nation, "\\s+"), function(x) x[2]) 
#Min variables seems separated by "," lets convert it to "."
df$Min <- gsub(",",".",df$Min)
#Convert necessary data from character to numeric
df[,c(4:33)] <- lapply(df[,c(4:33)],as.numeric)

#Let's investigate the basics

# Total Goals

total_goals <- sum(df$Gls)
cat("Total Number of Goals:",total_goals)

# Penalty Goals
total_penalty_goals<-sum(df$PK)
cat("Total Number of Penalty Goals",total_penalty_goals)

# Penalty Attempts
total_penalty_attemps<-sum(df$PKatt)
cat("Total Number of Penalty Attempts",total_penalty_attemps)

# Missing Penalties and Scored Penalties
missing_penalties<-total_penalty_attemps-total_penalty_goals

library(ggplot2)
# Create a data frame
data <- data.frame(
  category = c("Penalty Goals", "Missing Penalties"),
  value = c(total_penalty_goals, missing_penalties)
)

# Calculate the percentages
data$percentage <- data$value / sum(data$value) * 100

# Plot the pie chart
ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Penalty Goals vs. Missing Penalties") +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) +
  theme_void()

# Unique positions
unique(df$Pos)

# Total FW players
total_fw_players<-nrow(df[df$Pos=="FW",])
cat("Total FW players",total_fw_players,
    "\n",
    df[df$Pos=="FW",][1,1],
    df[df$Pos=="FW",][2,1],"and",
    df[df$Pos=="FW",][3,1]
    )
# Players from different nations

unique_nations <- unique(df$Nation)
total_num_of_nations <- length(unique_nations)
cat("Total Number of Different Nations",
    total_num_of_nations)

# Average age of first 18's

average_age <- mean(df$Age)
cat("Average age is:",average_age)

# Write data frame to a CSV file
write.csv(df,"ManchesterCityPlayerData.csv",row.names = FALSE)







