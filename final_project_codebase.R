library(tidyverse)
library(lubridate)
library(regclass)
library(kableExtra)
library(webshot2)

# Reads csv file
truth_data <- read.csv("data/TruthSocial_2024_ElectionDataset.csv")
head(truth_data)

# Converts columns into factors
truth_data$is_reply <- factor(truth_data$is_reply)
truth_data$is_quote <- factor(truth_data$is_quote)

# Selects columns for variables of interest
filtered_truth_data <- truth_data %>% 
  select(timestamp, like_count, retruth_count, is_quote, is_retruth, is_reply)

# Separates timestamp into two colulms
filtered_truth_data <- filtered_truth_data %>% 
  select(everything()) %>% 
  separate(col = timestamp, into = c("date", "time"), sep = " ")

# Converts Date column to date data type 
filtered_truth_data$date <- ymd(filtered_truth_data$date) 

# Shows first five rows of filtered dataset
head(filtered_truth_data)

# Filters data for posts posted in 2023
filtered_truth_data_2023 <- filtered_truth_data %>% 
  filter(year(date) == 2023)

# Converts Time into a time variable
filtered_truth_data_2023$time <- hms(filtered_truth_data_2023$time)

# Creates categorical variable representing the time of day
filtered_truth_data_2023 <- filtered_truth_data_2023 %>% 
  mutate(time_of_day = case_when(
    time >= hms("23:00:00") | time < hms("05:00:00") ~ "Night",
    time >= hms("05:00:00") & time < hms("11:00:00") ~ "Morning",
    time >= hms("11:00:00") & time < hms("16:00:00") ~ "Mid-day",
    time >= hms("16:00:00") & time < hms("23:00:00") ~ "Evening"
  ))

# Shows first five results of new filtered data set
head(filtered_truth_data_2023)

# Summary statistics of likes and retruths 
summary(filtered_truth_data_2023$like_count)
summary(filtered_truth_data_2023$retruth_count)

# Manually inputted stats into vectors
like_count_sum <- c(Min=0, Q1=0, Median=1, Mean=5.336, Q3=2, Max=10200)
retruth_count_sum <- c(Min=0, Q1=0, Median=0, Mean=1.729, Q3=0, Max=2580)

# Converts to data frames
summary_df <- bind_rows(
  as.data.frame(t(like_count_sum )) %>% mutate(Variable = "like_count"),
  as.data.frame(t(retruth_count_sum)) %>% mutate(Variable = "retruth_count")
) %>%
  select(Variable, everything())


# Kable summary table 
summary_table <- kable(summary_df, caption = "Summary Statistics") %>%
  kable_styling(full_width = FALSE) %>%
  scroll_box(width = "100%")

# Exports to PNG
save_kable(summary_table, "summary_stats.png", zoom = 2)



### Creation of columns table


# Column names
cols <- c("url", "external_id", "timestamp", "author_username",
          "associated_tags", "tagged_accounts", "status_links", "media_urls",
          "like_count", "reply_count", "retruth_count", "is_quote",
          "is_retruth", "is_reply", "replying_to", "status",
          "keywords", "Scraping.Date")

# Explanations
explanations <- c(
  "Link to post",
  "Unique ID linked to post",
  "Time when post was posted",
  "Username of author of post",
  "Tags that were linked with the post",
  "Accounts tagged to a specific post",
  "URLs shared in the post",
  "Links to media associated with the post",
  "Number of likes a post received",
  "Number of replies",
  "Number of times post was reshared",
  "Whether the post is a quote",
  "Whether the post is a re-share",
  "Whether the post is a reply",
  "Username of user being replied to",
  "Hashtags linked to post",
  "Keywords related to the 2024 election",
  "Date post was scraped"
)

# Datatypes
types <- c(
  "character", "numeric", "POSIXct", "character",
  "character", "character", "character", "character",
  "integer", "integer", "integer", "boolean",
  "boolean", "boolean", "character", "character",
  "character", "Date"
)

# creates table using Kable
kable_obj <- kable(
  column_table,
  caption = "Dataset Variable Descriptions with Data Types  {#tbl-variables}"
) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# Export as PNG
save_kable(kable_obj, "column.png", zoom = 2)



### Scatter plot of average number of likes and retruths  


num_likes_retruths <- filtered_truth_data_2023 %>% 
  select(date, retruth_count, like_count) %>% 
  group_by(date) %>% 
  mutate(avg_likes = mean(like_count), 
         avg_retruths = mean(retruth_count)) %>% 
  ggplot(aes(x = date, y = avg_likes)) + 
  geom_line() +
  geom_line(aes(y = avg_retruths), color = "red") + 
  xlab("Date") + 
  ylab("Number of Likes and Retruths") + 
  ggtitle("Average Number of Likes and Retruths throughout 2023")

ggsave(
  filename = "Number_of_Likes_retruths.png",
  plot = num_likes_retruths,
  width = 6,
  height = 3,
  units = "in", 
  dpi = 800     
)



### Plots retruth_count and like_count to determine polynomial terms


scatter_retruth_likes <- ggplot(data = filtered_truth_data_2023, aes(x = retruth_count, y = like_count)) + 
  geom_point() + 
  xlab("Number of Retruths") + 
  ylab("Number of Likes") + 
  ggtitle("Scatter plot of Number of Likes Based on Number \n of Retruths")


ggsave(
  filename = "scatter_retruth_likes.png",
  plot = scatter_retruth_likes,
  width = 6,
  height = 3,
  units = "in", 
  dpi = 800     
)




### Predctive models

# Models where first model does not include is_quote and second model includes is_quote 
mod_pois1 <- glm(like_count ~  retruth_count + is_quote,filtered_truth_data_2023,family = poisson())
mod_pois2 <-  glm(like_count ~  retruth_count * is_quote,filtered_truth_data_2023, family = poisson())

# Graphs the predicted values as a line graph and scatter plot based on the two models above
pred_is_quote <- filtered_truth_data_2023 %>% 
  mutate(pred_likes = mod_pois1$fitted.values,
         pred_likes2 = mod_pois2$fitted.values) %>%
  ggplot(aes(x=retruth_count,y=like_count ,colour=is_quote)) +
  geom_point(size=0.5) +
  geom_line(aes(y=pred_likes),lty=2) +
  geom_line(aes(y=pred_likes2)) + 
  xlab("Number of Retruths") + 
  ylab("Number of Likes") + 
  ggtitle("Scatter Plot with Predictive Lines with is_quote as \n the Secondary Explanatory variable")


ggsave(
  filename = "pred_is_quote.png",
  plot = pred_is_quote,
  width = 6,
  height = 3,
  units = "in", 
  dpi = 800     
)

# Models where first model does not include time_of_day and second model includes time_of_day

mod_pois1 <- glm(like_count ~  retruth_count + time_of_day,filtered_truth_data_2023, family = poisson())
mod_pois2 <-  glm(like_count ~  retruth_count * time_of_day,filtered_truth_data_2023, family = poisson())

# Graphs the predicted values as a line graph and scatter plot based on the two models above
pred_time_of_day <- filtered_truth_data_2023 %>% 
  mutate(pred_likes = mod_pois1$fitted.values,
         pred_likes2 = mod_pois2$fitted.values) %>%
  ggplot(aes(x=retruth_count,y=like_count ,colour=time_of_day)) +
  geom_point(size=0.5) +
  geom_line(aes(y=pred_likes),lty=2) +
  geom_line(aes(y=pred_likes2)) + 
  xlab("Number of Retruths") + 
  ylab("Number of Likes") + 
  ggtitle("Scatter Plot with Predictive Lines with time_of_day as \n the Secondary Explanatory variable")

ggsave(
  filename = "pred_time_of_day.png",
  plot = pred_time_of_day,
  width = 6,
  height = 3,
  units = "in", 
  dpi = 800     
)

# Models where first model does not include is_reply and second model includes is_reply

mod_pois1 <- glm(like_count ~  retruth_count + is_reply,filtered_truth_data_2023,family = poisson())
mod_pois2 <-  glm(like_count ~  retruth_count * is_reply,filtered_truth_data_2023, family = poisson())

# Graphs the predicted values as a line graph and scatter plot based on the two models above
pred_is_reply <- filtered_truth_data_2023 %>% 
  mutate(pred_likes = mod_pois1$fitted.values,
         pred_likes2 = mod_pois2$fitted.values) %>%
  ggplot(aes(x=retruth_count,y=like_count ,colour=is_reply)) +
  geom_point(size=0.5) +
  geom_line(aes(y=pred_likes),lty=2) +
  geom_line(aes(y=pred_likes2)) + 
  xlab("Number of Retruths") + 
  ylab("Number of Likes") + 
  ggtitle("Scatter Plot with Predictive Lines with is_reply as \n the Secondary Explanatory variable")


ggsave(
  filename = "pred_is_reply.png",
  plot = pred_time_of_day,
  width = 6,
  height = 3,
  units = "in", 
  dpi = 800     
)

### Maximum model summary and residuals plot

# Maximum model
mod_max <- glm(like_count ~ I(retruth_count) + I(retruth_count^2) + time_of_day + retruth_count:time_of_day  + is_quote + is_reply, filtered_truth_data_2023, family = poisson())

# Summary of maximum model
summary(mod_max)


# Open a PNG device to save residuals plot as png
png("mod_maxresiduals_plot.png", width = 600, height = 600) 

# Make the plot
plot(fitted(mod_max), rstudent(mod_max),
     xlab = "Fitted Values",
     ylab = "Studentized Residuals",
     main = "Residuals vs Fitted For Maximum Model")

# Saves the png
dev.off()


### In this section, the algorithm is implemented 5 times to remove outliers

## Execution 1

# Compute studentized residuals 
rstud <- rstudent(mod_max)

# Critical value
n <- nrow(filtered_truth_data_2023)
k <- 12                         
alpha <- 0.01
crit <- qt(1 - alpha/2, df = n - 1 - k)

# Identify outliers
outliers <- which(abs(rstud) > crit)

# Removes outliers
filtered_no_influential_vars <- filtered_truth_data_2023[-outliers, ]

# Refit model
mod_max_clean <- glm(
  like_count ~ I(retruth_count) + I(retruth_count^2) + time_of_day + retruth_count:time_of_day + is_quote + is_reply, filtered_no_influential_vars, family = poisson())

# Residuals Plot
plot(fitted(mod_max_clean),rstudent(mod_max_clean))


### Execution 2

# Compute studentized residuals
rstud <- rstudent(mod_max_clean)

# Critical value
n <- nrow(filtered_no_influential_vars)
crit <- qt(1 - alpha/2, df = n - 1 - k)

# Identify outliers
outliers <- which(abs(rstud) > crit)

# Removes outliers
filtered_no_influential_vars_V2 <- filtered_no_influential_vars[-outliers, ]

# Refit model
mod_max_clean_V2 <- glm(
  like_count ~ I(retruth_count) + I(retruth_count^2) + time_of_day + retruth_count:time_of_day + is_quote + is_reply, filtered_no_influential_vars_V2, family = poisson())

# Residuals Plot
plot(fitted(mod_max_clean_V2),rstudent(mod_max_clean_V2))


## Execution 3

# Compute studentized residuals
rstud <- rstudent(mod_max_clean_V2)

# Critical value
n <- nrow(filtered_no_influential_vars_V2)
crit <- qt(1 - alpha/2, df = n - 1 - k)

# Identify outliers
outliers <- which(abs(rstud) > crit)

# Removes outliers
filtered_no_influential_vars_V3 <- filtered_no_influential_vars_V2[-outliers, ]

# Refit model
mod_max_clean_V3 <- glm(
  like_count ~ I(retruth_count) + I(retruth_count^2) + time_of_day + retruth_count:time_of_day + is_quote + is_reply, filtered_no_influential_vars_V3, family = poisson())

# Residuals Plot
plot(fitted(mod_max_clean_V3),rstudent(mod_max_clean_V3))


## Execution 4

# Compute studentized residuals
rstud <- rstudent(mod_max_clean_V3)

# Critical value
n <- nrow(filtered_no_influential_vars_V3)
crit <- qt(1 - alpha/2, df = n - 1 - k)
nrow(filtered_no_influential_vars_V3)
# Identify outliers
outliers <- which(abs(rstud) > crit)

# Removes outliers
filtered_no_influential_vars_V4 <- filtered_no_influential_vars_V3[-outliers, ]

# Refit model
mod_max_clean_V4 <- glm(
  like_count ~ I(retruth_count) + I(retruth_count^2) + time_of_day + retruth_count:time_of_day + is_quote + is_reply, filtered_no_influential_vars_V4, family = poisson())

# Residuals Plot
plot(fitted(mod_max_clean_V4),rstudent(mod_max_clean_V4))


## Execution 5 (Fails in this execution)

# Compute studentized residuals
rstud <- rstudent(mod_max_clean_V4)

# Critical value
n <- nrow(filtered_no_influential_vars_V4)
crit <- qt(1 - alpha/2, df = n - 1 - k)

# Identify outliers
outliers <- which(abs(rstud) > crit)

# Removes outliers
filtered_no_influential_vars_V5 <- filtered_no_influential_vars_V4[-outliers, ]

# Refit model
mod_max_clean_V5 <- glm(
  like_count ~ I(retruth_count) + I(retruth_count^2) + time_of_day + retruth_count:time_of_day + is_quote + is_reply, filtered_no_influential_vars_V5, family = poisson())

# Residuals Plot
plot(fitted(mod_max_clean_V4),rstudent(mod_max_clean_V4))



### Best Regression Model 

mod_best <- step(mod_max_clean_V4, direction="both", trace = 0)

summary(mod_best)
plot(fitted(mod_best),residuals.glm(mod_best,type="pearson"))


# Open a PNG device to save residuals plot as png
png("mod_bestresiduals_plot.png", width = 600, height = 600)  

# Make the plot
plot(fitted(mod_best), rstudent(mod_best),
     xlab = "Fitted Values",
     ylab = "Studentized Residuals",
     main = "Residuals vs Fitted For Best Model")

# Saves the file
dev.off()

# Open a PNG device
png("mod_best_pearson_residuals_plot.png", width = 600, height = 600)

# Residual plot
plot(fitted(mod_best), residuals.glm(mod_best,type="pearson"),
     xlab = "Fitted Values",
     ylab = "Studentized Residuals",
     main = "Pearson Residuals vs Fitted For Best Model")

# Saves the file as a png
dev.off()

# Create the data frame
vif_table <- data.frame(
  Variable = c("retruth_count^2", "time_of_day", "is_quote", "is_reply", "time_of_day*retruth_count"),
  GVIF = c(7.487117, 1.897737, 1.007152, 1.000137, 14.142666),
  Df = c(1, 3, 1, 1, 4),
  `GVIF^(1/(2*Df))` = c(2.736260, 1.112686, 1.003570, 1.000069, 1.392568)
)

# Creates table using kable 
kbl_table <- kable(vif_table, caption = "Variance Inflation Factors (VIF) for Best Model") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover"))

# Save as PNG
save_kable(kbl_table, "vif_table.png")


# Calculation for chi square statistic 
chi2 <- summary(mod_best)$null - summary(mod_best)$deviance
chi2


pchisq(chi2,3,lower.tail=FALSE) # Statistical significance of the model


### Creates table with interpetations of coefficients


# Create the data frame for coefficients
coef_table <- data.frame(
  Variable = c(
    "(Intercept)",
    "I(retruth_count^2)",
    "time_of_dayMid-day",
    "time_of_dayMorning",
    "time_of_dayNight",
    "is_quoteTrue",
    "is_replyTrue",
    "time_of_dayEvening:retruth_count",
    "time_of_dayMid-day:retruth_count",
    "time_of_dayMorning:retruth_count",
    "time_of_dayNight:retruth_count"
  ),
  Estimate = c(
    0.569149, -0.024967, -0.015629, -0.955991, 0.050535, -0.566306, -0.453125,
    0.392499, 0.401430, 0.572539, 0.385291
  ),
  `Std. Error` = c(
    0.006276, 0.000714, 0.010175, 0.015779, 0.009630, 0.012474, 0.301552,
    0.005413, 0.005992, 0.008246, 0.005713
  ),
  `z value` = c(
    90.679, -34.969, -1.536, -60.586, 5.248, -45.398, -1.503,
    72.509, 66.996, 69.430, 67.436
  ),
  `Pr(>|z|)` = c(
    "<2e-16", "<2e-16", "0.125", "<2e-16", "1.54e-07", "<2e-16", "0.133",
    "<2e-16", "<2e-16", "<2e-16", "<2e-16"
  ),
  Interpretation = c(
    "The expected number of likes when all explanatory variables are constant and all categrical variables are equal to its baseline is exp(0.569149) = 1.7667629",
    "For every additional retruth (reshare), the average number of likes on a post per day changes by a factor of exp(0.2*(-0.024967)**2) = 1.0001247, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(-0.015629) = 0.9844925 for people posting mid-day than the evening, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(-0.955991) = 0.3844310 for people posting in the morning than the evening, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(0.050535) = 1.0518337 for people posting at night than the evening, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(-0.566306) =  0.5676184 when a post is a quote compared to when it is not a quote, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(-0.45312)=  0.6356387 when a post is a reply compared to when it is not a reply, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(0.392499) =   1.4806764 when there is an additional retruth and the post is posted in the evening, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(0.401430) =   1.4939595 when there is an additional retruth and the post is posted mid-day rather than the evening, holding all other variables constant",
    "The average number of likes on a post per day changes by a factor of exp(0.572539) =   1.7727624 when there is an additional retruth and the post is posted in the morning rather than the evening, holding all other variables constant", 
    "The average number of likes on a post per day changes by a factor of exp(0.385291) =   1.4700420 when there is an additional retruth and the post is posted in the morning rather than the evening,holding all other variables constant"
  ),
  `5%` = c(0.55682464, -0.02637128, -0.03559121, -0.98702690, 0.03164815,
           -0.59083720, -1.10860511, 0.38189233, 0.38968016, 0.55629318, 0.37409524),
  `95%` = c(0.58142814, -0.02357251, 0.00429589, -0.92517107, 0.06939671,
            -0.54193757, 0.08491933, 0.40311145, 0.41316823, 0.58862186, 0.39649172),
  stringsAsFactors = FALSE
)

# Create table using kable
kbl_obj <- kable(
  coef_table,
  caption = "GLM Coefficients for the Model",
  digits = 3
) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"), position = "center")

# Save as PNG
save_kable(kbl_obj, "glm_coefficients.pdf", zoom = 4)