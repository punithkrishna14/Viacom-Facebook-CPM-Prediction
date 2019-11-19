cpm <- read.csv('cpm_estimates-15Apr19.csv')

# Cancel scientific numbering
options(scipen = 999)

# Agegroup
library(dplyr)
agegroup <- group_by(cpm, age_max, age_min)
adj <- summarize(agegroup, number_cpm = n())
agegroup <- data.frame(adj$age_min, adj$age_max)
colnames(agegroup) <- c("age_min", "age_max")
agegroup <- agegroup[order(agegroup$age_min), ]
row.names(agegroup) <- NULL
agegroup$age_min <- as.character(agegroup$age_min)
agegroup$age_max <- as.character(agegroup$age_max)
agegroup[14:15, ] <- list(age_min = c("55", "65"), age_max = c("64", "65+"))
agegroup$age <- paste(agegroup$age_min, agegroup$age_max, sep = "-")
agegroup[15, 3] <- "65+"
agegroup_list <- unlist(agegroup[ , 3])

# Gendergroup
gendergroup <- group_by(cpm, female, male)
adj <- summarize(gendergroup, number_cpm = n())
gendergroup <- data.frame(adj$female, adj$male)
colnames(gendergroup) <- c("female", "male")
gendergroup_list <- c("M", "F", "U")

# Change cpm table
cpm[ , 8:9] <- data.frame(matrix(NA, nrow(cpm), 2))

for (i in 1:nrow(cpm)){
  for (a in 1:nrow(agegroup)){
    if (cpm[i, 1] == agegroup[a, 1] && cpm[i, 2] == agegroup[a, 2]){
    cpm[i, 8] <- a
    }
  }
}

for (i in 1:nrow(cpm)){
  for (b in 1:nrow(gendergroup)){
    if (cpm[i, 3] == gendergroup[b, 1] && cpm[i, 4] == gendergroup[b, 2]){
      cpm[i, 9] <- b
    }
  }
}

cpm <- data.frame(cpm$X1, cpm$X2, cpm[ ,5:7])
colnames(cpm)[1:2] <- c("agegroup", "gendergroup")

cpm <- data.frame(cpm[order(cpm$date), ])
row.names(cpm) <- NULL

# Import Jan2019
jan2019 <- read.csv('page_level_data-1-2019.csv')

# Variable list
adj <- group_by(jan2019, name)
jan2019_name <- summarize(adj)
jan2019_name <- unlist(jan2019_name)
jan2019_name_date_hid <- jan2019_name[-c(1, 2, 4, 5, 6, 7, 18)]
jan2019_name_gender_age <- jan2019_name[c(4, 18)]
jan2019_name_location <- jan2019_name[c(5, 6, 7)]

# Total fans_online
jan2019_fans_online_total <- subset(jan2019, jan2019$name == "page_fans_online")
jan2019_fans_online_total <- group_by(jan2019_fans_online_total, date, hID)
jan2019_fans_online_total <- summarize(jan2019_fans_online_total, fans_online = sum(value))

# Total cta clicks according to hid and date
jan2019_page_cta_clicks_logged_in_total <- subset(jan2019, jan2019$name == "page_cta_clicks_logged_in_total")
jan2019_page_cta_clicks_logged_in_total <- group_by(jan2019_page_cta_clicks_logged_in_total, date, hID)
jan2019_page_cta_clicks_logged_in_total <- summarize(jan2019_page_cta_clicks_logged_in_total, 
                                                     cta_clicks_logged_in_total = sum(value))

# Subset variables (Date+hID) from Jan2019
for (i in jan2019_name_date_hid){
  assign(paste("jan2019_", i, sep = ""), data.frame(subset(jan2019, jan2019$name == i)[1],
                                                    subset(jan2019, jan2019$name == i)[4],
                                                    subset(jan2019, jan2019$name == i)[5]))
}

# Subset variables (Gender+Age, Location) from Jan2019
for (i in jan2019_name_gender_age){
  assign(paste("jan2019_", i, sep = ""), data.frame(subset(jan2019, jan2019$name == i)))
}

for (i in jan2019_name_location){
  assign(paste("jan2019_", i, sep = ""), data.frame(subset(jan2019, jan2019$name == i)))
}

# Change subsets' column names
colnames(jan2019_page_impressions)[2] <- "impressions"
colnames(jan2019_page_impressions_organic)[2] <- "impressions_organic"
colnames(jan2019_page_impressions_paid)[2] <- "impressions_paid"
colnames(jan2019_page_post_engagements)[2] <- "post_engagements"
colnames(jan2019_page_posts_impressions)[2] <- "posts_impressions"
colnames(jan2019_page_posts_impressions_organic)[2] <- "posts_impressions_organic"
colnames(jan2019_page_posts_impressions_paid)[2] <- "posts_impressions_paid"
colnames(jan2019_page_video_views)[2] <- "video_views"
colnames(jan2019_page_video_views_organic)[2] <- "video_views_organic"
colnames(jan2019_page_video_views_paid)[2] <- "video_views_paid"
colnames(jan2019_page_views)[2] <- "views"
colnames(jan2019_page_views_total)[2] <- "views_total"

# Seperate gendergroup and agegroup for impressions_by_age_gender
adj <- data.frame(do.call('rbind', 
                          strsplit(as.character(jan2019_page_impressions_by_age_gender_unique$metric), 
                                   '.', fixed = TRUE)))

jan2019_page_impressions_by_age_gender_unique <- data.frame(jan2019_page_impressions_by_age_gender_unique, 
                                                            adj)[-c(2, 3)]

colnames(jan2019_page_impressions_by_age_gender_unique)[c(2, 4, 5)] <- c("impressions_by_age_gender", 
                                                                         "gendergroup", "agegroup")

row.names(jan2019_page_views_by_age_gender_logged_in_unique) <- NULL

# Seperate gendergroup and agegroup for views_by_age_gender
adj <- data.frame(do.call('rbind', 
                          strsplit(as.character(jan2019_page_views_by_age_gender_logged_in_unique$metric), 
                                   '.', fixed = TRUE)))

jan2019_page_views_by_age_gender_logged_in_unique <- data.frame(jan2019_page_views_by_age_gender_logged_in_unique, adj)[-c(2, 3)]

colnames(jan2019_page_views_by_age_gender_logged_in_unique)[c(2, 4, 5)] <- c("views_by_age_gender_logged_in", 
                                                                             "agegroup", "gendergroup")

row.names(jan2019_page_views_by_age_gender_logged_in_unique) <- NULL

# Replace gendergroup and agegroup into categorical number for impressions_by_age_gender
jan2019_page_impressions_by_age_gender_unique$gendergroup <- as.character(jan2019_page_impressions_by_age_gender_unique$gendergroup)
jan2019_page_impressions_by_age_gender_unique$agegroup <- as.character(jan2019_page_impressions_by_age_gender_unique$agegroup)

for (i in 1:3){
  jan2019_page_impressions_by_age_gender_unique[jan2019_page_impressions_by_age_gender_unique$gendergroup == gendergroup_list[i], ][4] <- i
}

for (i in 1:15){
  if (nrow(jan2019_page_impressions_by_age_gender_unique[jan2019_page_impressions_by_age_gender_unique$agegroup == agegroup_list[i], ]) > 0){
    jan2019_page_impressions_by_age_gender_unique[jan2019_page_impressions_by_age_gender_unique$agegroup == agegroup_list[i], ][5] <- i
  }
}

# Replace gendergroup and agegroup into categorical number for views_by_age_gender
jan2019_page_views_by_age_gender_logged_in_unique$gendergroup <- as.character(jan2019_page_views_by_age_gender_logged_in_unique$gendergroup)
jan2019_page_views_by_age_gender_logged_in_unique$agegroup <- as.character(jan2019_page_views_by_age_gender_logged_in_unique$agegroup)

for (i in 1:3){
  jan2019_page_views_by_age_gender_logged_in_unique[jan2019_page_views_by_age_gender_logged_in_unique$gendergroup == gendergroup_list[i], ][5] <- i
}

for (i in 1:15){
  if (nrow(jan2019_page_views_by_age_gender_logged_in_unique[jan2019_page_views_by_age_gender_logged_in_unique$agegroup == agegroup_list[i], ]) > 0){
    jan2019_page_views_by_age_gender_logged_in_unique[jan2019_page_views_by_age_gender_logged_in_unique$agegroup == agegroup_list[i], ][4] <- i
  }
}

# Inner join subsets (date+hid) with cpm table
library(plyr)
cpm_jan <- join_all(list(cpm, jan2019_page_cta_clicks_logged_in_total, 
                         jan2019_page_impressions, 
                         jan2019_page_impressions_organic, 
                         jan2019_page_impressions_paid,
                         jan2019_page_post_engagements, 
                         jan2019_page_posts_impressions, 
                         jan2019_page_posts_impressions_organic, 
                         jan2019_page_posts_impressions_paid, 
                         jan2019_page_video_views, 
                         jan2019_page_video_views_organic, 
                         jan2019_page_video_views_paid, 
                         jan2019_page_views, 
                         jan2019_page_views_total, 
                         jan2019_fans_online_total), 
                    by = c("date", "hID"), type='inner')

# Inner join subsets (age+gender) with cpm table
cpm_jan <- join_all(list(cpm_jan, jan2019_page_views_by_age_gender_logged_in_unique, 
                         jan2019_page_impressions_by_age_gender_unique), 
                    by = c("date", "hID", "gendergroup", "agegroup"), type='left')
