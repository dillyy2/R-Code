#Jibu Final Project - Dominick Kubica, Cole Novara, Tabitha Tomori, Dylan Gordon

#Libraries needed
library(writexl)
library(corrplot)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)
library(factoextra)
library(lubridate)

#COMBINING AND CLEANING DATA SETS
#BOJ survey
BOJ<-as.data.frame(BOJ_Survey_2023_2024_02_08)
clean_names(BOJ)
colnames(BOJ)
BOJ <- BOJ %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
BOJ <- BOJ %>% select(15,557,558,561,566,585)
BOJ <- BOJ %>% rename("Franchise Name" = "Franchise Code & Name")

#FranchiseHealthData
FHD<-as.data.frame(Franchise_Health_Data_Input_Form_2023_2024_02_08)
clean_names(FHD)
colnames(FHD)
FHD <- FHD %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
FHD <- FHD %>% select(19, 26, 36:39, 43:50, 89:90)

#Copy of BOJ raw data
BOJ1<-as.data.frame(Copy_of_BoJ_Data_2022_Raw_Data_Final)
clean_names(BOJ1)
colnames(BOJ1)
BOJ1 <- BOJ1 %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
BOJ1<- BOJ1 %>% select(1,2,4,9:11)

#Copy of Copy
GomaOnly<-as.data.frame(Copy_of_Copy_of_BoJ_Score_Final_Nov_Copy)
clean_names(GomaOnly)
colnames(GomaOnly)
GomaOnly <- GomaOnly %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
GomaOnly<-GomaOnly %>% select(2,6,7,12,20,22,23,26,38:43)
#Limited data set - didn't use

#Responses
Responses<-as.data.frame(responses_2024_02_08)
clean_names(Responses)
colnames(Responses)
Responses <- Responses %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
Responses<-Responses %>% select(12,24,33,34,41:48,50)

#Hygiene
Hygiene<-as.data.frame(Franchise_Hygiene_Survey_2023_2024_02_08)
clean_names
colnames(Hygiene)
Hygiene <- Hygiene %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
Hygiene<- Hygiene %>% select(2,12,23,29:49,54:58,66:92)
#Limited data set - didn't use

#Cleaning
CleanHygiene <- Hygiene %>%
    mutate(across(29:50, ~ ifelse(. == 2, 1, .)))
CleanHygiene <- CleanHygiene %>%
  mutate(across(3:28, ~ ifelse(. == "yes", 1, ifelse(. == "no", 0, .))))

#Merging for Franchise data
consolidatedResponses <- Responses %>%
  group_by(`Franchise Name`) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
consolidatedFHD <- FHD %>%
  group_by(`Franchise Name`) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
HygieneFran<- CleanHygiene %>%
  select(-c(1:2))

FranchiseDataDF <- left_join(consolidatedFHD, consolidatedResponses, by = "Franchise Name")
FranchiseDataDF2 <- left_join(FranchiseDataDF, BOJ1, by = "Franchise Name")
df3 <- left_join(FranchiseDataDF2, BOJ, by = "Franchise Name")
#Chose to use health data, responses, and both BOJ dataframes

#Adding country to DF3
FHD2<-as.data.frame(Franchise_Health_Data_Input_Form_2023_2024_02_08)
clean_names(FHD2)
colnames(FHD2)
FHD2 <- FHD2 %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
FHD2 <- FHD2 %>% select(12, 19)
colnames(FHD2)[1]<-"Country_2"
consolidatedFHD2 <- FHD2 %>%
  group_by(`Franchise Name`) %>%
  summarise(Unique_Countries = paste(unique(Country_2), collapse = ", "))
df3 <- left_join(df3, consolidatedFHD2, by = "Franchise Name")

#Rename columns
colnames(df3)
colnames(df3)[3]<-"Active Retailers"
colnames(df3)[4]<-"Active Households"
colnames(df3)[5]<-"Active Businesses"
colnames(df3)[7]<-"New Retailers"
colnames(df3)[8]<-"New Households"
colnames(df3)[9]<-"New Businesses"
colnames(df3)[15]<-"Sales_2"
colnames(df3)[16]<-"Expenses_2"

#Calculating loyal customers
df3$`Loyal Retailers`<-df3$`Active Retailers` - df3$`New Retailers`
df3$`Loyal Households`<-df3$`Active Households` - df3$`New Households`
df3$`Loyal Businesses`<-df3$`Active Businesses` - df3$`New Businesses`

#Currency conversion to USD
df3_cleaned <- df3 %>%
  mutate(
    sales_2_usd = case_when(
      Unique_Countries == "kenya" ~ Sales_2 * 0.007435,
      Unique_Countries == "rwanda" ~ Sales_2 * 0.00078,
      Unique_Countries == "drcgoma" ~ Sales_2 * 0.00036,
      Unique_Countries == "uganda" ~ Sales_2 * 0.000257,
      TRUE ~ Sales_2
    ),
    Expenses_2_USD = case_when(
      Unique_Countries == "kenya" ~ Expenses_2 * 0.007435,
      Unique_Countries == "rwanda" ~ Expenses_2 * 0.00078,
      Unique_Countries == "drcgoma" ~ Expenses_2 * 0.00036,
      Unique_Countries == "uganda" ~ Expenses_2 * 0.000257,
      TRUE ~ Expenses_2
    )
  )

#Clean column names
df3_cleaned <- clean_names(df3_cleaned)

#df3 is ready to work with

#EXPLORATORY DATA ANALYSIS
#Summarize the data
summary(df3_cleaned)
#Check for missing values
sum(is.na(df3_cleaned))
#Overview of the structure
str(df3_cleaned)
#Basic statistical summary of numerical attributes
df3_cleaned %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE)
  )))

#Visualizing numerical data distributions
df3_cleaned %>%
  select(where(is.numeric)) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal()

#Visualizing categorical data distributions
numeric_columns <- df3_cleaned %>% select(where(is.numeric)) %>% names()
df3_cleaned %>%
  pivot_longer(cols = all_of(numeric_columns), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal()

#Boxplots for numerical variable to check for outliers
#Create a new dataframe that includes a numeric index for each column
df3_long <- df3_cleaned %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>%
  mutate(index = match(variable, names(df3_cleaned)))  # Create an index column
#Create boxplots using the index as the x-axis
ggplot(df3_long, aes(x = factor(index), y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplots of Numerical Variables", x = "Column Index", y = "Values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x labels for better readability

#Visual distribution of franchises across countries
ggplot(df3_cleaned, aes(x = unique_countries)) +
  geom_bar(fill = "dodgerblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Franchises by Country", x = "Country", y = "Number of Franchises")

#Visual correlation between sales and expenses
ggplot(df3_cleaned, aes(x = sales_2_usd, y = expenses_2_usd, color = unique_countries)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Correlation between Sales and Expenses by Country",
       x = "Sales in USD",
       y = "Expenses in USD",
       color = "Country") +
  geom_smooth(method = "lm", se = FALSE)

#MODEL 1 - linear regression (loyal and new retailers, businesses, and households)
model1_loyalretailers <- lm(sales_2_usd ~ loyal_retailers, data = df3_cleaned)
summary(model1_loyalretailers)
ggplot(df3_cleaned, aes(x = loyal_retailers, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Loyal Retailers", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_loyalbusinesses <- lm(sales_2_usd ~ loyal_businesses, data = df3_cleaned)
summary(model1_loyalbusinesses)
ggplot(df3_cleaned, aes(x = loyal_businesses, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Loyal Businesses", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_loyalhouseholds <- lm(sales_2_usd ~ loyal_households, data = df3_cleaned)
summary(model1_loyalhouseholds)
ggplot(df3_cleaned, aes(x = loyal_households, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Loyal Households", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_newretailers <- lm(sales_2_usd ~ new_retailers, data = df3_cleaned)
summary(model1_newretailers)
ggplot(df3_cleaned, aes(x = new_retailers, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "New Retailers", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_newbusinesses <- lm(sales_2_usd ~ new_businesses, data = df3_cleaned)
summary(model1_newbusinesses)
ggplot(df3_cleaned, aes(x = new_businesses, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "New Businesses", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_newhouseholds <- lm(sales_2_usd ~ new_households, data = df3_cleaned)
summary(model1_newhouseholds)
ggplot(df3_cleaned, aes(x = new_households, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "New Households", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_activeretailers <- lm(sales_2_usd ~ active_retailers, data = df3_cleaned)
summary(model1_activeretailers)
ggplot(df3_cleaned, aes(x = active_retailers, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Active Retailers", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_activebusinesses <- lm(sales_2_usd ~ active_businesses, data = df3_cleaned)
summary(model1_activebusinesses)
ggplot(df3_cleaned, aes(x = active_businesses, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Active Businesses", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

model1_activehouseholds <- lm(sales_2_usd ~ active_households, data = df3_cleaned)
summary(model1_activehouseholds)
ggplot(df3_cleaned, aes(x = active_households, y = sales_2_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Active Households", y = "P.2 Sales", title = "Dot Plot with Line of Best Fit")

#MODEL 2 - step regression (loyal and new retailers, businesses, and households)
#Create, clean, and rename columns in new dataframe
df4 <- na.omit(df3_cleaned)
df4_cleaned <- df4 %>% select(1,2,6,19:27,29,33:35,39:41,43)
colnames(df4_cleaned)
df4_renamed <- df4_cleaned %>%
  rename(rate_jibu_easy_to_reach_jibu_on_phone_to_restock_or_report_issues = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_you_find_it_easy_to_reach_jibu_on_phone_whenever_you_need_to_restock_or_report_issues_rating_between_1_10",
         rate_jibu_on_time_deliveries_from_jibu_staff = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_jibu_staff_make_deliveries_on_time_rating_between_1_10",
         rate_jibu_good_customer_service = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_jibu_staff_have_good_customer_service_rating_between_1_10",
         rate_jibu_bottles_in_good_condition = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_jibu_bottles_are_always_in_good_condition_when_you_receive_them_for_sale_rating_between_1_10",
         rate_jibu_water_quality = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_jibu_water_has_the_best_quality_rating_between_1_10",
         rate_jibu_water_afforability = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_jibu_water_is_affordable_for_your_customers_rating_between_1_10",
         rate_jibu_stocking_products_is_good_business = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_stocking_jibu_products_is_good_business_for_you_rating_between_1_10",
         rate_jibu_company_cares_about_you = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_jibu_as_a_company_cares_about_you_rating_between_1_10",
         rate_jibu_recommend_to_other_retailers = "i_am_now_going_to_read_out_some_statements_please_rate_jibu_on_a_scale_of_1_10_where_1_is_poor_and_10_is_excellent_how_likely_are_you_to_recommend_jibu_to_another_retailer_use_a_scale_of_1_10_where_1_is_not_likely_and_10_is_extremely_likely_rating_between_1_10")

#Step regression focusing on Loyal Retailers
#Start with the simplest model (intercept only)
model2_loyalretailers_start <- lm(loyal_retailers ~ 1, data = df4_renamed)
#Define the most complex model, excluding specific predictors
model2_loyalretailers_full <- lm(loyal_retailers ~ . - franchise_name - loyal_retailers - loyal_businesses - loyal_households, data = df4_renamed)
#Perform forward step regression
model2_loyalretailers_step <- step(model2_loyalretailers_start, 
                                      scope = list(lower = model2_loyalretailers_start, 
                                                   upper = model2_loyalretailers_full),
                                      direction = "forward")
#Check the summary of the selected model
summary(model2_loyalretailers_step)

#Step regression focusing on Loyal Businesses
model2_loyalbusinesses_start <- lm(loyal_businesses ~ 1, data = df4_renamed)
model2_loyalbusinesses_full <- lm(loyal_businesses ~ . - franchise_name - loyal_retailers - loyal_businesses - loyal_households, data = df4_renamed)
model2_loyalbusinesses_step <- step(model2_loyalbusinesses_start, 
                                   scope = list(lower = model2_loyalbusinesses_start, 
                                                upper = model2_loyalbusinesses_full),
                                   direction = "forward")
summary(model2_loyalbusinesses_step)

#Step regression focusing on Loyal Households
model2_loyalhouseholds_start <- lm(loyal_households ~ 1, data = df4_renamed)
model2_loyalhouseholds_full <- lm(loyal_households ~ . - franchise_name - loyal_retailers - loyal_businesses - loyal_households, data = df4_renamed)
model2_loyalhouseholds_step <- step(model2_loyalhouseholds_start, 
                                    scope = list(lower = model2_loyalhouseholds_start, 
                                                 upper = model2_loyalhouseholds_full),
                                    direction = "forward")
summary(model2_loyalhouseholds_step)

#Cluster analysis for model 2
#1.Scale RFM values to normalize
df4_scaled <- scale(df4_renamed[, -which(names(df4_renamed) == "franchise_name")])
df4_scaled <- as.data.frame(df4_scaled)
#2.Find Optimal Number of CLusters
fviz_nbclust(df4_scaled, kmeans, method = "silhouette", k.max = 10)
#3.Perform k-means clustering on the scaled data
set.seed(123)
k_cluster <- kmeans(df4_scaled, centers = 7, nstart = 25)
k_cluster$betweenss / k_cluster$totss
k_cluster$centers
#4.Adding cluster assignment to original data for interpretation
df4_renamed$cluster <- k_cluster$cluster
#5.Visualize Clusters
df4_numeric_data <- df4_renamed[, sapply(df4_renamed, is.numeric)]
fviz_cluster(k_cluster, data = df4_numeric_data)
#6.Do Cluster Profiling Using Original RFM data
#Add cluster information to df4_numeric_data
df4_renamed$Cluster <- k_cluster$cluster
#Aggregate data by cluster
df4_renamed$Cluster <- k_cluster$cluster
cluster_profiles <- aggregate(df4_renamed[, -which(names(df4_renamed) == "franchise_name")], 
                              by=list(cluster=df4_numeric_data$cluster), 
                              mean)
print(cluster_profiles)

#CALCULATING FINAL CLV USING CHURNED
FHD<-as.data.frame(Franchise_Health_Data_Input_Form_2023_2024_02_08)
clean_names(FHD)
FHD <- FHD %>%
  mutate(across(where(is.character), function(x) {
    x <- tolower(x)
    x <- str_replace_all(x, "[[:punct:]]", "")
    x <- str_replace_all(x, "\\s+", "")
    return(x)
  }))
FHD <- FHD %>% select(6, 19, 26, 36:39, 43:50, 89:90)

colnames(FHD)
colnames(FHD)[4]<-"Active Retailers"
colnames(FHD)[5]<-"Active Households"
colnames(FHD)[6]<-"Active Businesses"
colnames(FHD)[8]<-"New Retailers"
colnames(FHD)[9]<-"New Households"
colnames(FHD)[10]<-"New Businesses"
colnames(FHD)

FHD$`Loyal Retailers`<-FHD$`Active Retailers` - FHD$`New Retailers`
FHD$`Loyal Households`<-FHD$`Active Households` - FHD$`New Households`
FHD$`Loyal Businesses`<-FHD$`Active Businesses` - FHD$`New Businesses`

#Extract year, month, and day
FHD$day <- as.numeric(format(FHD$'Submitted On', "%d"))
FHD$month <- as.numeric(format(FHD$'Submitted On', "%m"))
FHD$year <- as.numeric(format(FHD$'Submitted On', "%Y"))

FHD <- FHD %>%
  group_by(`Franchise Name`)

#Group by "Franchise Names" and filter out groups with less than 3 entries
FHDchurn <- FHD %>%
  group_by(`Franchise Name`) %>%
  filter(n() >= 3)

#Add a row between each group
FHDchurn <- FHDchurn %>%
  group_by(`Franchise Name`) %>%
  group_modify(~ add_row(.x, .before = 0))

#Shift the existing columns upwards by 1 row
FHDchurn$New_Retailers_Rolled <- c(FHDchurn$'New Retailers'[-1], NA)
FHDchurn$New_Businesses_Rolled <- c(FHDchurn$'New Businesses'[-1], NA)
FHDchurn$New_Households_Rolled <- c(FHDchurn$'New Households'[-1], NA)
FHDchurn$Active_Retailers_Rolled <- c(FHDchurn$'Active Retailers'[-1], NA)
FHDchurn$Active_Businesses_Rolled <- c(FHDchurn$'Active Businesses'[-1], NA)
FHDchurn$Active_Households_Rolled <- c(FHDchurn$'Active Households'[-1], NA)

#Churned 1 = loyal 1 + New 1 + New 2 - Active 2

#Calculate the "ChurnedRetail" column
FHDchurn$ChurnedRetail <- with(FHDchurn, ifelse(
  is.na(`Loyal Retailers`) | is.na(`New Retailers`) | is.na(`New_Retailers_Rolled`) | is.na(`Active_Retailers_Rolled`),
  NA,
  `Loyal Retailers` + `New Retailers` + `New_Retailers_Rolled` - `Active_Retailers_Rolled`
))

#View the updated data frame
head(FHDchurn)
colnames(FHDchurn)
DFhelp<-FHDchurn %>% select(1,2,4,5,6,8,9,10,16,18:30)
head(DFhelp)

#Create a date column
FHDchurn$date <- as.Date(paste(FHDchurn$year, FHDchurn$month, FHDchurn$day, sep="-"), "%Y-%m-%d")

#Sort by Franchise Name and Date
FHDchurn <- FHDchurn %>%
  arrange(`Franchise Name`, date)

#Calculate the time interval in months between observations
FHDchurn <- FHDchurn %>%
  group_by(`Franchise Name`) %>%
  mutate(interval = interval(lag(date, default = first(date)), date) / months(1)) %>%
  ungroup()

calculate_churn_metrics <- function(df, new_col, loyal_col, active_col, prefix) {
  df %>%
    mutate(
      !!sym(paste0(prefix, " Churned Customers")) := abs(
        ifelse(is.na(lag(!!sym(new_col))) | is.na(!!sym(active_col)),
               NA,
               !!sym(active_col) - (lag(!!sym(new_col), default=0) + 
                                      !!sym(new_col) + 
                                      lag(!!sym(loyal_col), default=0))
        )),
      !!sym(paste0(prefix, " Churn Rate")) := ifelse(is.na(!!sym(paste0(prefix, " Churned Customers"))) | interval == 0, 
                                                     NA, 
                                                     !!sym(paste0(prefix, " Churned Customers")) / (lag(!!sym(active_col), default=0) * interval)),
      !!sym(paste0(prefix, " Average Lifespan (months)")) := ifelse(is.na(!!sym(paste0(prefix, " Churn Rate"))) | !!sym(paste0(prefix, " Churn Rate")) == 0,
                                                                    NA,
                                                                    1 / !!sym(paste0(prefix, " Churn Rate")))
    )
}

#Apply the churn metrics calculation to each category
FHDchurn <- calculate_churn_metrics(FHDchurn, 'New Retailers', 'Loyal Retailers', 'Active Retailers', 'Retail')
FHDchurn <- calculate_churn_metrics(FHDchurn, 'New Businesses', 'Loyal Businesses', 'Active Businesses', 'Business')
FHDchurn <- calculate_churn_metrics(FHDchurn, 'New Households', 'Loyal Households', 'Active Households', 'Household')

#Assuming that you have fixed 'Average Revenue per Loyal Customer' for each category
average_revenue_per_loyalretail <- 2.358
average_revenue_per_loyalbusiness <- 30.695
average_revenue_per_loyalhousehold <- 2.575

#Calculate CLV for each category using the average lifespan in months
FHDchurn <- FHDchurn %>%
  mutate(
    `Retail CLV` = ifelse(is.na(`Retail Average Lifespan (months)`),
                          NA,
                          average_revenue_per_loyalretail * `Retail Average Lifespan (months)`),
    `Business CLV` = ifelse(is.na(`Business Average Lifespan (months)`),
                            NA,
                            average_revenue_per_loyalbusiness * `Business Average Lifespan (months)`),
    `Household CLV` = ifelse(is.na(`Household Average Lifespan (months)`),
                             NA,
                             average_revenue_per_loyalhousehold * `Household Average Lifespan (months)`)
  )

average_clvs <- FHDchurn %>%
  summarize(
    Average_Retail_CLV = mean(`Retail CLV`, na.rm = TRUE),
    Average_Business_CLV = mean(`Business CLV`, na.rm = TRUE),
    Average_Household_CLV = mean(`Household CLV`, na.rm = TRUE)
  )

#Print the averages
print(average_clvs)
