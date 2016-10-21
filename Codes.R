library(rattle) #  normVarNames().
library(tidyr) # Tidy the dataset.
library(ggplot2) # Visualise data.
library(dplyr) # Data preparation and pipes %>%.
library(lubridate) # Handle dates.
library(FSelector) # Feature selection
library(stringr)
library(Matrix)
library(arules) #Market Basket Analysis
library(grid)
library(vcd)  #Mosaic plots
library(ggplot2)
library(data.table)
library(arulesViz) #Market Basket Analysis
library(ROCR)#ROC
library(caTools)


#############################
# loading data
Output_1003_FromPC <- read.csv("VDAO_0410_To_07102016_Final_10032016.csv", sep = ",", fill=TRUE)
# Sorting
Output_1003_FromPC <- Output_1003_FromPC %>%
  arrange(fullVisitorId,visitNumber, date, hits_hitNumber)

# Understanding the dataset
names(Output_1003_FromPC)
dim(Output_1003_FromPC)
tbl_df(Output_1003_FromPC)
class(Output_1003_FromPC)

# Reviewing the observations
head(Output_1003_FromPC)
tail(Output_1003_FromPC)
Output_1003_FromPC[sample(nrow(Output_1003_FromPC), 5),]

# Structure of the dataset
str(Output_1003_FromPC)
glimpse(Output_1003_FromPC)
summary(Output_1003_FromPC)

# Cleaning, Format Conversion
names(Output_1003_FromPC) <- normVarNames(names(Output_1003_FromPC))
names(Output_1003_FromPC)
sapply(Output_1003_FromPC, class)
 
library(lubridate) # ymd()
head(Output_1003_FromPC$date)
Output_1003_FromPC$date <- ymd(as.character(Output_1003_FromPC$date))
head(Output_1003_FromPC$date, 10)
sapply(Output_1003_FromPC, class)
str(Output_1003_FromPC)
# convert full visitorid to a factor variable
Output_1003_FromPC$full_visitor_id <- as.factor(Output_1003_FromPC$full_visitor_id)
glimpse(Output_1003_FromPC) #dplyr package

Output_1003_FromPC <- Output_1003_FromPC %>%
      group_by(full_visitor_id, date, visit_number) %>%
    mutate(hadresultpage = ifelse('/open-account#result' %in% hits_page_page_path, 1, 0))
#keep only those with result page
Output_1003_FromPC_clean <- Output_1003_FromPC %>%
             filter(hadresultpage == 1)
# Remove event_value
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-event_value, -hadresultpage)

# We remove undefined event category and action on SSO page
Output_1003_FromPC_clean<-Output_1003_FromPC_clean %>% filter(hits_page_page_path != "/voyasso/index.html", event_category !="undefined", event_action != "undefined")

# new hit_number
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, date, visit_number) %>%
  mutate(hit_number=row_number())
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-hits_hit_number)
# Sorting
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
  arrange(full_visitor_id,visit_number, date, hit_number)


### Create New Variables 
#brokerage
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
    mutate(brokerage_clicked2 = ifelse('Product-Brokerage-Start-Button' %in% eventlabel, 'Product-Brokerage', NA))
# brokerage roth
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
    mutate(brokerage_roth_clicked2 = ifelse('Product-Brokerage-Roth-Start-Button' %in% eventlabel, 'Product-Brokerage-Roth', NA))
# emf
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
    mutate(emf_clicked2 = ifelse('Product-EMF-Start-Button' %in% eventlabel, 'Product-EMF', NA))
# emf roth
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
    mutate(emf_roth_clicked2 = ifelse('Product-EMF-Roth-Start-Button' %in% eventlabel, 'Product-EMF-Roth', NA))
# nonqual
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
    mutate(nonqual_clicked2 = ifelse('Product-Nonqual-Start-Button' %in% eventlabel, 'Product-Nonqual', NA))

# Create New Variables - Method1 dplyr package - Numeric Product Variables

######  Create new columns for each click on a product
#brokerage_numeric
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    mutate(brokerage_num_clicked2 = ifelse('Product-Brokerage' %in% brokerage_clicked2, 1 , 0))

# brokerage roth_numeric
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    mutate(brokerage_roth_num_clicked2 = ifelse('Product-Brokerage-Roth' %in% brokerage_roth_clicked2, 1, 0))
# emf_numeric
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    mutate(emf_num_clicked2 = ifelse('Product-EMF' %in% emf_clicked2, 1, 0))
# emf roth_numeric
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    mutate(emf_roth_num_clicked2 = ifelse('Product-EMF-Roth' %in% emf_roth_clicked2, 1, 0))
# nonqual_numeric
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    mutate(nonqual_num_clicked2 = ifelse('Product-Nonqual' %in% nonqual_clicked2, 1, 0))



###Create New Variables - Method1 dplyr package - Number of Products Clicked}
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    mutate(product_number_clicked = brokerage_num_clicked2 + brokerage_roth_num_clicked2 + emf_num_clicked2 + emf_roth_num_clicked2 + nonqual_num_clicked2)

### Create New Variables - Target Variable
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
    mutate(product_clicked = ifelse(product_number_clicked != 0, TRUE, FALSE))
 
### Create New Variables -Target Variable 2
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(app_submitted = ifelse('/openaccount/Congrats' %in% hits_page_page_path | '/openaccount/Congrats-Validation-Pending' %in% hits_page_page_path, TRUE, FALSE))

#Create New Variables -Advisor 
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(had_advisor = ifelse('/openaccount/Create-Profile-Advisor' %in% hits_page_page_path, TRUE, FALSE))

# Create New Variables - sso_successfull}
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(sso_successfull = ifelse('/openaccount/Address' %in% hits_page_page_path | 'Advisor-Verification-Code-Success-Continue' %in% eventlabel | '/myVoya/index' %in% hits_page_page_path , TRUE, FALSE))

# Create New Variables - Article Pages
Output_1003_FromPC_clean$read_article <- grepl("^/articles", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(read_article_clean = ifelse('TRUE' %in% read_article, TRUE, FALSE))
# remove read_article column as we don't need that anymore
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-read_article)

#Create New Variables - Products
Output_1003_FromPC_clean$product_viewed <- grepl("^/products", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(product_viewed_clean = ifelse('TRUE' %in% product_viewed, TRUE, FALSE))
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-product_viewed)

#Create New Variables - Tools
Output_1003_FromPC_clean$tool_viewed <- grepl("^/tool", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(tool_viewed_clean = ifelse('TRUE' %in% tool_viewed, TRUE, FALSE))
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-tool_viewed)
#Create New Variables - Planning & Advise
Output_1003_FromPC_clean$planning_viewed <- grepl("^/planning|^/action", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(planning_viewed_clean = ifelse('TRUE' %in% planning_viewed, TRUE, FALSE))
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-planning_viewed)
#Create New Variables - Contact Us
Output_1003_FromPC_clean$contact_viewed <- grepl("^/contact", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(contact_viewed_clean = ifelse('TRUE' %in% contact_viewed, TRUE, FALSE))
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-contact_viewed)
#Create New Variables - Search
Output_1003_FromPC_clean$had_search <- grepl("^/search", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(had_search_clean = ifelse('TRUE' %in% had_search, TRUE, FALSE))
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-had_search)
#Create New Variables - myVoya}
Output_1003_FromPC_clean$had_myVoya <- grepl("myVoya", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id, date, visit_number, totals_time_on_site) %>%
                    mutate(had_myVoya_clean = ifelse('TRUE' %in% had_myVoya, TRUE, FALSE))
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-had_myVoya)

# which element of column a is missing
which(is.na(Output_1003_FromPC_clean$totals_time_on_site))
# Replace all NA values in total time with 0.1 as the hits are less than 1 sec
Output_1003_FromPC_clean$totals_time_on_site[is.na(Output_1003_FromPC_clean$totals_time_on_site)]<- 1/10

#Create New Variables - entrance
setDT(Output_1003_FromPC_clean)[, entrance := hits_page_page_title[which(hits_is_entrance == "true")], by = .(full_visitor_id, date, visit_number, totals_hits )]
# replace NA's in entrance with page title of the same NA row as there were some NAs after creating entrance column
Output_1003_FromPC_clean$entrance[is.na(Output_1003_FromPC_clean$entrance)] <- Output_1003_FromPC_clean$hits_page_page_title[is.na(Output_1003_FromPC_clean$entrance)]

#Create New Variables - exit}
setDT(Output_1003_FromPC_clean)[, exit := hits_page_page_title[which(hits_is_exit == "true")], by = .(full_visitor_id, date, visit_number, totals_hits )]
# replace NA's in exit with page title of the same NA row as there were some NAs after creating exit column
Output_1003_FromPC_clean$exit[is.na(Output_1003_FromPC_clean$exit)] <- Output_1003_FromPC_clean$hits_page_page_title[is.na(Output_1003_FromPC_clean$exit)]


#Create New Variables - device_is_mobile_2}
Output_1003_FromPC_clean$device_is_mobile_2 <- ifelse(Output_1003_FromPC_clean$device_device_category == "desktop", "no",
                                                      ifelse(Output_1003_FromPC_clean$device_device_category == "mobile", "yes",
                                                             ifelse(Output_1003_FromPC_clean$device_device_category == "tablet", "no",
                                                                    NA)))

Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-device_is_mobile)
# Create maxhit_number on the result page - find the last hit on the product 
Output_1003_FromPC_clean<-Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
  mutate(maxhit_number= max(hit_number[which(hits_page_page_path =="/open-account#result")]))

Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      filter(hit_number <= maxhit_number)

# remove the hit number
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
  select(-hit_number)
# remove duplicates (with the same timestamp)
Output_1003_FromPC_clean <- unique(Output_1003_FromPC_clean[1:142433,  ])
# Add hit_number back
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, date, visit_number) %>%
  mutate(hit_number=row_number())

## Aggregate - total hits
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(totals_hits_clean = n())

#We remove total hit column as it is not correct and we don't need it anymore.
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-totals_hits)
# Aggregate - total pageviews
# for each row by visitorid, visitnumber, sum the hits when they are pages
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_pageviews_clean = sum(hits_type == 'PAGE'))
# remove the old total_pageview column
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-totals_pageviews)
# Aggregate - total events}
# for each row by visitorid, visitnumber, sum the hits when they are events
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_events = sum(hits_type == 'EVENT'))
# Aggregate - How many views on the result page -(loops)
# for each row by visitorid, visitnumber, sum the hits when they are events
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_resultpageview = sum(hits_page_page_path =='/open-account#result' & hits_type == 'PAGE'))
# Clean-Ignore IDs, Outputs, Missing}

############ Removing extra columns
#We remove extra columns that we don't need
### Removing varianles that we dont have data for all of observations
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-client_id, -plan_id, -person_id, -session_id, -ssopurpose, -businessid)
# "hits_is_entrance" , "hits_is_exit"
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-hits_is_entrance, -hits_is_exit)
### Removing hits_referer, host, geo_network_country, as we don't need them
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
             select(-hits_referer, -host, -hits_page_hostname, -geo_network_country)

#We also fill empty slots with a dummy value 'NA'

# campaign
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>% 
  mutate(campaign = replace(campaign, campaign == '', NA))
summary(Output_1003_FromPC_clean$campaign)
# keyword
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>% 
  mutate(keyword = replace(keyword, keyword == '', NA))
summary(Output_1003_FromPC_clean$keyword)
# ad_content
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>% 
  mutate(ad_content = replace(ad_content, ad_content == '', NA))
summary(Output_1003_FromPC_clean$ad_content)
# referral_path
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>% 
  mutate(referral_path = replace(referral_path, referral_path == '', NA))
summary(Output_1003_FromPC_clean$referral_path)
# hits_page_page_title
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>% 
  mutate(hits_page_page_title = replace(hits_page_page_title, hits_page_page_title == '', NA))
summary(Output_1003_FromPC_clean$hits_page_page_title)

# Outliers
#There is a chance that we consider some visit numbers as our ouliers as they might be testers.

boxplot(Output_1003_FromPC_clean$visit_number, horizontal = TRUE)
hist(Output_1003_FromPC_clean$visit_number, breaks = 5)
boxplot(Output_1003_FromPC_clean) # for the full dataset

#Create New Variables - Sequence Variables}


# Create a variable if reading the article was before clicking on the product
# create a new column to work on page path strings
Output_1003_FromPC_clean$pages <- Output_1003_FromPC_clean$hits_page_page_path
#Subtract the first 5 charachters
Output_1003_FromPC_clean$pages <- substr(Output_1003_FromPC_clean$pages, 1, 5)
#We replace the values on the "pages" column with the product clicks as "hit" for all the 5 products

#4159 = Product-EMF-Start-Button in pages with that event
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-EMF-Start-Button"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-EMF-Start-Button" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "4159" with "EMF"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4159", "hit")

################# EMF-Roth
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-EMF-Roth-Start-Button"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-EMF-Roth-Start-Button" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "4158" with "hit"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4158", "hit")
################# Brokerage
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-Brokerage-Start-Button"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-Brokerage-Start-Button" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "4154" with "hit"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4154", "hit")
################# Brokerage - Roth
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-Brokerage-Roth-Start-Button"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-Brokerage-Roth-Start-Button" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "4153" with "hit"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4153", "hit")
################# Nonqual
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-Nonqual-Start-Button"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-Nonqual-Start-Button" ]
unique(Output_1003_FromPC_clean$pages)

### In the page column, replace "4163" with "hit"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4163", "hit")

############################ comparison
#Create New Variables - Article before/after/before clicking on product
#article before (hit after article)
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(article_before_product = ifelse('/arti' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/arti")]), 'yes', 'no'))
#plot them
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(article_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

#Create New Variables - Product before/after/before clicking on product}

############################ comparison - product pages

#product before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(product_before_product = ifelse('/prod' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/prod")]), 'yes', 'no'))
      
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(product_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

#Ceate New Variables - Planing/Tool/Search/Contact-us/Terms/Privacy before clicking on product}

############################ comparison - planning pages

#planning before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(planning_before_product = ifelse(('/plan' %in% pages|'/acti' %in% pages)& ('hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/plan")])|('hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/acti")]))), 'yes', 'no'))
#plot
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(planning_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

############################ comparison - tool pages
#tool before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(tool_before_product = ifelse('/tool' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/tool")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(tool_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
############################ comparison - contact-us pages
#contact us before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(contact_before_product = ifelse('/cont' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/cont")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(contact_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

############################ comparison - Search pages
#search us before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(search_before_product = ifelse('/sear' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/sear")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(search_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

############################ comparison - Term pages
#Terms before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(term_before_product = ifelse('/term' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/term")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(term_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

############################ comparison - Privacy pages
#Privacy before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(privacy_before_product = ifelse('/priv' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="/priv")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(privacy_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)


# Create New Variables - Clicked on Overview before}
#Let's create another dummy variable as \texttt{clicked_overview_before}. 
#We replace the values on the "pages" column with the event label  as "over"
#3687 = keyfeatures in pages with that event

Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "overview"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "overview" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "4085" with "key"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4085", "over")
############################ comparison - overview features 
#overview before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(clicked_overview_before = ifelse('over' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="over")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(clicked_overview_before))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

### Create New Variables - Clicked on Key Features before}
#We replace the values on the "pages" column with the event label  as "key"
<<>>=
#3687 = keyfeatures in pages with that event
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "keyfeatures"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "keyfeatures" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "3687" with "key"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "3687", "key")
############################ comparison - key features 
#keyfeatures before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(clicked_keyfeatures_before = ifelse('key' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="key")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(clicked_keyfeatures_before))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

# Let's see how event categories are
head(sort(xtabs(data=Output_1003_FromPC_clean, formula = ~eventlabel), decreasing = TRUE), 100)

#Create New Variables - Viewed Fees - Replace values on pages column}
We replace the values on the "pages" column with the view fees clicks as "fees" for all the 5 products
<<>>=
#4160 = Product-EMF-View-Fees in pages with that event for (both roth-emf and emf)
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-EMF-View-Fees"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-EMF-View-Fees" ]
unique(Output_1003_FromPC_clean$pages)

### In the page column, replace "4160" with "fees"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4160", "fees")

################# Brokerage-View-Fees for both brokerage and brokerage-roth
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-Brokerage-View-Fees"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-Brokerage-View-Fees" ]
unique(Output_1003_FromPC_clean$pages)

### In the page column, replace "4155" with "fees"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4155", "fees")

################# Nonqual-View-Fees
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "Product-Nonqual-View-Fees"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "Product-Nonqual-View-Fees" ]
unique(Output_1003_FromPC_clean$pages)

### In the page column, replace "4164" with "fees"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4164", "fees")

############################ comparison
#Create New Variables - View Fees before/after/before clicking on product

#fees before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(fees_before_product = ifelse('fees' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="fees")]), 'yes', 'no'))
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(fees_before_product))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)


#Aggregate - Total Article/Product/Tool/Planning/Contact-us/Search/Fees/Privacy/Terms Views  }
#If we want to know how many views on different sections per visit each visitor has

# for each row by visitorid, visitnumber, sum the hits when they are events
# Article Views before clicking on product
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_articles_before =  sum(hits_type == 'PAGE' & pages == '/arti' & article_before_product=='yes'))
# Product Views before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_productpageview_before = sum(hits_type == 'PAGE' & pages == '/prod' & product_before_product=='yes'))
# Tool Views before clicking
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_tools_before = sum(hits_type == 'PAGE' & pages == '/tool' & tool_before_product=='yes'))
# Planning Views before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_planning_before = sum(hits_type == 'PAGE' & (pages == '/plan'|pages == '/acti' & planning_before_product=='yes')))
# Contact-us Views before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_contactus_before = sum(hits_type == 'PAGE' & pages == '/cont' & contact_before_product=='yes'))
# Search Views before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_search_before = sum(hits_type == 'PAGE' & pages == '/sear' & search_before_product=='yes'))
# FEE Views before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_fee_clicked_before = sum(hits_type == 'EVENT' & pages == 'fees' & fees_before_product=='yes'))
# Privacy Views before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_privacy_before = sum(hits_type == 'PAGE' & pages == '/priv' & privacy_before_product=='yes'))
# Terms Views before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_terms_before = sum(hits_type == 'PAGE' & pages == '/term' & term_before_product=='yes'))
# Hits_on_product
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
  mutate(total_hits_onprod = sum(hits_type == 'EVENT' & pages == 'hit'))

##### Aggregate - New Total Time Spent on the side  }

#calculate the time difference between each row timestamp
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
  group_by(full_visitor_id, visit_number) %>%
mutate(diff = c(difftime(tail(hit_timestamp, -1), head(hit_timestamp, -1)),0))
# calculate the new total time
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>% 
  group_by(full_visitor_id, visit_number) %>%
mutate(total_time_clean= sum(diff))


##### Create New Variables - Clicked on iraproductoptions before
#We replace the values on the "pages" column with the event label  as "iraoptions"
<<>>=
#3687 = iraproductoptions in pages with that event
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "iraproductoptions"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "iraproductoptions" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "3647" with "iraoptions"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "3647", "iraoptions")
############################ comparison - key features 
#iraoptions before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(iraproductoptions_before = ifelse('iraoptions' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="iraoptions")]), 'yes', 'no'))

ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(iraproductoptions_before))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

## Create New Variables - Clicked on IRA before
#We replace the values on the "pages" column with the event label  as "iraoptions"
<<>>=
#3687 = IRA in pages with that event
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "IRA"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "IRA" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "3641" with "IRA"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "3641", "IRA")
############################ comparison - IRA 
#IRA before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(IRA_before = ifelse('IRA' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="IRA")]), 'yes', 'no'))

ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(IRA_before))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

## Create New Variables - Clicked on considerarolloverira before
#We replace the values on the "pages" column with the event label  as "iraoptions"
#479 = considerarolloverira in pages with that event
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "considerarolloverira"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "considerarolloverira" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "479" with "considerarolloverira"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "479", "considerarolloverira")
############################ comparison - considerarolloverira 
#IRA before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(considerarolloverira_before = ifelse('considerarolloverira' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="considerarolloverira")]), 'yes', 'no'))

ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(considerarolloverira_before))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)


####Create New Variables - Clicked on PRODUCTS+- before}
#We replace the values on the "pages" column with the event label  as "iraoptions"
<<>>=
#4169 = PRODUCTS+- in pages with that event
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$eventlabel == "PRODUCTS+-"] <- Output_1003_FromPC_clean$eventlabel[Output_1003_FromPC_clean$eventlabel == "PRODUCTS+-" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "4169" with "PRODUCTS+-"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "4169", "PRODUCTS+-")
############################ comparison - key features 
#PRODUCTS+- before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(PRODUCTS_plusminus_before = ifelse('PRODUCTS+-' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="PRODUCTS+-")]), 'yes', 'no'))

ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(PRODUCTS_plusminus_before))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
######## Create New Variables - section nav scroll before
We replace the values on the "pages" column with the event label  as "iraoptions"
<<>>=
#450 = section nav scroll in pages with that event
Output_1003_FromPC_clean$pages[Output_1003_FromPC_clean$event_action == "section-nav-scroll"] <- Output_1003_FromPC_clean$event_action[Output_1003_FromPC_clean$event_action == "section-nav-scroll" ]
unique(Output_1003_FromPC_clean$pages)
### In the page column, replace "450" with "scroll"... - Find values in the column and replace them
Output_1003_FromPC_clean$pages <- str_replace(Output_1003_FromPC_clean$pages, "450", "scroll")
############################ comparison - key features 
#PRODUCTS+- before
Output_1003_FromPC_clean<- Output_1003_FromPC_clean %>%
      group_by(full_visitor_id, date, visit_number) %>%
      mutate(nav_scroll_before = ifelse('scroll' %in% pages & 'hit' %in% pages & max(hit_number[which(pages =="hit")]) > min(hit_number[which(pages =="scroll")]), 'yes', 'no'))

ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(nav_scroll_before))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
#see how event categories are
head(sort(xtabs(data=Output_1003_FromPC_clean, formula = ~event_category), decreasing = TRUE), 50)



# Create New Variables - First Time Visitor - Frequency of Visit}
#We create a factor variable for the frequency of visit to compare the behaviours in the first visit vs other visits.
#How is the visit_number distributed?
boxplot(Output_1003_FromPC_clean$visit_number, col = "blue" , horizontal = TRUE)
qplot (data= Output_1003_FromPC_clean, x=visit_number, fill = "red", binwidth=.5)
ggplot(df, aes(x = visit_number, y = product_number_clicked, col = device_device_category)) +
  geom_point(alpha=0.5, shape=1, size=4 , position="jitter")
  geom_smooth(method=lm, se=FALSE, linetype=2)
#
Output_1003_FromPC_clean$visit_frequency = cut(Output_1003_FromPC_clean$visit_number, 
                         breaks=c(0,1,5,20,Inf))

Output_1003_FromPC_clean$visit_frequency = factor(Output_1003_FromPC_clean$visit_frequency,  
labels=c('firstTime', 'low', 'medium', 'High'))

# plot a bar 
ggplot(Output_1003_FromPC_clean, aes(x = product_clicked, fill = factor(visit_frequency))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent) 


####Preparing the final dataframe}
# Let's remove the visitor Id's that have not seen the guidance wizard- Group by visitor ID, we remove the users with no "/open-account" 

Output_1003_FromPC_clean$had_vdao <- grepl("/open-account", Output_1003_FromPC_clean$hits_page_page_path)
Output_1003_FromPC_clean <- Output_1003_FromPC_clean %>%
                    group_by(full_visitor_id) %>%
                    mutate(had_vdao_clean = ifelse('TRUE' %in% had_vdao, TRUE, FALSE))



#Let's check to see if need the entrance column
head(sort(xtabs(data=Output_1003_FromPC_clean, formula = ~entrance), decreasing = TRUE), 50)
names(Output_1003_FromPC_clean)

### Removing hits_referer, host, geo_network_country, as we don't need them
df <- Output_1003_FromPC_clean %>%
             select(-hit_timestamp, -hits_type, -hits_page_page_title, -hits_page_page_path,
                    -event_category, -event_action,-eventlabel, -totals_time_on_site, -geo_network_region, -referral_path,-campaign,-keyword,
                    -ad_content, -brokerage_clicked2,-brokerage_roth_clicked2,-emf_clicked2,
                    -emf_roth_clicked2,-nonqual_clicked2,-app_submitted, -sso_successfull,-read_article_clean,
                    -product_viewed_clean,-tool_viewed_clean, -planning_viewed_clean, -contact_viewed_clean, -had_search_clean, 
                    -had_myVoya_clean,-exit, -device_is_mobile_2,-maxhit_number,-pages, -diff, -had_vdao, -had_vdao_clean)

glimpse(df)

#We remove the duplicate rows:
df <- select(df, date, full_visitor_id, visit_number, total_time_clean, total_time_clean, total_pageviews_clean , total_events, hit_number, 4:52)
# remove duplicates based on the hit number which is the first row for each user
df <- df %>% group_by(full_visitor_id, date, visit_number, total_time_clean, totals_hits_clean) %>% filter(hit_number == 1) 
# remove hit_number after removing duplicate rows
df <- df %>%
    select(-hit_number)

# Let's explore the data with Trifacta

ggplot(df, aes(x = product_clicked, fill = factor(product_clicked))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
head(sort(xtabs(data=df, formula = ~product_clicked), decreasing = TRUE), 4)
library(vcd)
mosaic(formula = product_clicked ~ visit_frequency, data = df, shade=T, gp = shading_max)
ggplot(df, aes(x = product_clicked, fill = factor(visit_frequency))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

######  Remove Time outliers}

#1- Remove the observations that spent less than 1 sec on the website
df <- df %>% filter (total_time_clean > 1)
ggplot(df, aes(x = product_clicked, fill = factor(product_clicked))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
head(sort(xtabs(data=df, formula = ~product_clicked), decreasing = TRUE), 4)

### Create an Updated Frequency
df$visit_frequency = cut(df$visit_number, 
                         breaks=c(0,1,5,20,Inf))

df$visit_frequency = factor(df$visit_frequency,  
labels=c('firstTime', '[2-5]', '[6-20]', '[21,]'))
ggplot(df, aes(x = visit_frequency, fill = factor(visit_frequency))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
head(sort(xtabs(data=df, formula = ~visit_frequency), decreasing = TRUE), 4)
mosaic(formula = product_clicked ~ visit_frequency, data = df, shade=T, gp = shading_max)

## Create A Categorical variable for total time on the site

df$total_time_clean = cut(df$total_time_clean, 
                         breaks=c(0,120,300,600,Inf))

#We define the parts and label:

df$time_on_site = factor(df$total_time_clean,  
labels=c('lessthantwo', 'betweenttwofive', 'betweenfiveten', 'morethanten'))
ggplot(df, aes(x = time_on_site, fill = factor(time_on_site))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
ggplot(df, aes(x = product_clicked, fill = factor(time_on_site))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
head(sort(xtabs(data=df, formula = ~time_on_site), decreasing = TRUE), 4)
mosaic(formula = product_clicked ~ time_on_site, data = df, shade=T, gp = shading_max)
mosaic(formula = product_clicked ~ visit_frequency + time_on_site, data = df, shade=T)

#Create A Categorical variable for total hits}

df$totalhit_catg = cut(df$totals_hits_clean, 
                         breaks=c(-Inf,10,16,28,Inf))

#We define the parts and labels:

df$totalhit_catg = factor(df$totalhit_catg,  
labels=c('(,10]', '[11,16]','[17,28]','[29,]'))
ggplot(df, aes(x = totalhit_catg, fill = factor(totalhit_catg))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
ggplot(df, aes(x = totalhit_catg, fill = factor(product_clicked))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
mosaic(formula = product_clicked ~ totalhit_catg, data = df, shade=T, gp = shading_max)

# Create A Categorical variable for total pageviews}

df$totalpageview_catg = cut(df$total_pageviews_clean, 
                         breaks=c(-Inf,7,12,Inf))

#We define the parts and label:

df$totalpageview_catg = factor(df$totalpageview_catg,  
labels=c('[1-7]','[8-12]','[13,]'))
ggplot(df, aes(x = totalpageview_catg, fill = factor(product_clicked))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent) 
mosaic(formula = product_clicked ~ totalpageview_catg, data = df, shade=T, gp = shading_max)

# Create A Categorical variable for total events}

df$totalevents_catg = cut(df$total_events, 
                         breaks=c(-Inf,7,19,Inf))

#We define the parts and labels:

df$totalevents_catg = factor(df$totalevents_catg,  
labels=c('[1-7]','[8-19]','[20,]'))
ggplot(df, aes(x = totalevents_catg, fill = factor(product_clicked))) +
   geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

# Create A Categorical variable for Product_clicked}

df$uniqueproduct_clicked<- as.factor(df$product_number_clicked)

#Create A Categorical variable for total articles}

df$totalarticle_catg = cut(df$total_articles_before, 
                         breaks=c(-Inf,0,1,Inf))

#We define the parts and labels:

df$totalarticle_catg = factor(df$totalarticle_catg,  
labels=c('zero','one','[2,]'))
ggplot(df, aes(x = totalarticle_catg, fill = factor(product_clicked))) +
   geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

## Create A Categorical variable for total product pages}

df$totalproductpage_catg = cut(df$total_productpageview_before, 
                         breaks=c(-Inf,0,1,Inf))

# We define the parts and labels:

df$totalproductpage_catg = factor(df$totalproductpage_catg,  
labels=c('zero','one','[2,]'))
ggplot(df, aes(x = totalproductpage_catg, fill = factor(product_clicked))) +
   geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)
 
#Create A Categorical variable for total tool pages}

df$totaltoolspage_catg = cut(df$total_tools_before, 
                         breaks=c(-Inf,0,1,Inf))

# We define the parts and labels:

df$totaltoolspage_catg = factor(df$totaltoolspage_catg,  
labels=c('zero','one','[2,]'))
ggplot(df, aes(x = totaltoolspage_catg, fill = factor(totaltoolspage_catg))) +
   geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)

# Create A Categorical variable for total planning pages}

df$totalplanning_catg = cut(df$total_planning_before, 
                         breaks=c(-Inf,0,1,Inf))

#We define the parts and labels:

df$totalplanning_catg = factor(df$totalplanning_catg,  
labels=c('zero','one','[2,]'))
ggplot(df, aes(x = totalplanning_catg, fill = factor(totalplanning_catg))) +
   geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_y_continuous(labels = scales::percent)


################ Explore the date -Plots

ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = totals_hits_clean)) + geom_point()+ geom_smooth()
ggplot(df, aes(x = total_articles, y = total_productpageview, col = total_hits_onprod)) + geom_point() #no insight
ggplot(df, aes(x = total_articles, y = total_productpageview, col = factor(total_hits_onprod))) + geom_point() #no insight
ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = product_number_clicked)) + geom_point() #no insight
ggplot(df, aes(x = visit_number, y = total_events, col = product_number_clicked)) + geom_point() #no insight
ggplot(df, aes(x = total_articles, y = total_productpageview, col = product_number_clicked!=0)) + geom_point() 
ggplot(df, aes(x = total_articles, y = total_productpageview, col = factor(product_number_clicked))) + geom_point() + geom_jitter()
# avoid overplotting
ggplot(df, aes(x = total_articles, y = total_productpageview, col = product_number_clicked)) + geom_point() + geom_jitter()
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = product_number_clicked)) + geom_point()
#Insight - If you view more pages, utake more actions and you click on more products
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = product_clicked)) + geom_point()

ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = sso_successfull)) + geom_point()
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = had_search_clean)) + geom_point()
#beacuse more users did not have search
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = device_is_mobile_2)) + geom_point()
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = device_device_category)) + geom_point()
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = had_advisor)) + geom_point()
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = had_myVoya_clean)) + geom_point()
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = device_browser)) + geom_point()
# new var before a
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = article_before_product)) + geom_point()
ggplot(df, aes(x = total_pageviews_clean, y = total_events, col = product_before_product, size = product_number_clicked )) + geom_point()
ggplot(df, aes(x = na.omit(total_pageviews_clean), y = total_events, col = product_before_product, size = product_number_clicked )) + geom_point()
ggplot(df, aes(x = na.omit(total_pageviews_clean), y = total_events, col = product_before_product, size = app_submitted )) + geom_point()
ggplot(df, aes(x = na.omit(total_pageviews_clean), y = total_events, col = product_before_product, size = product_number_clicked, shape = app_submitted )) + geom_point()


# Let's try this with jitter - a great solution to the overplotting problem here -geom_jitter(alpha=0.2, shape=1)
ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = totals_hits_clean)) + geom_jitter(alpha=0.2, shape=1)

# add a category
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = product_clicked)) + geom_point() + geom_smooth()
plot
# add alpha - remove smooth
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = product_clicked)) + geom_point(alpha = 0.4)

# category = product_clicked because it is not in all rows it does not show up
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  product_clicked)) + geom_point(alpha = 0.4)
###### category = device_operating_system
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = device_operating_system)) + geom_point(alpha = 0.2)
plot
#Next: Put more categories or variables and plot them

# Another way of assigning to a object - se = FALSE -> You don't want any error shading
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = device_operating_system))
plot <- plot + geom_point() + geom_smooth()
plot <- plot + geom_smooth(se=FALSE)

# put linear model for the whole data set and for each category
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col = device_operating_system)) + geom_point(alpha = 0.2) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))
plot
str((df))
# INSIGHT: Too messy to see any patterns, let's try to compare each in a separate plot or do a different plot such as bar
######### NEXT: add facet
ggplot(df, aes(x = visit_number, y = totals_time_on_site)) + geom_point(alpha = 0.2) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1)) +
facet_grid(. ~device_operating_system) 
# Insight: We can remove not set, blackberry and xbox
# let's do the same plot with device category
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_device_category)) + geom_point(alpha = 0.2) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))
# let's do the same plot with device_is_mobile variable
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_is_mobile)) + geom_point(alpha = 0.2) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))

# Let's change the size and shape
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_is_mobile)) + geom_point(alpha = 0.2, shape=1, size=4) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))

plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_is_mobile_2)) + geom_point(alpha = 0.2) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))
plot
# to fix the overlapping issue with large dataset alpha=0.5
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_is_mobile_2)) + geom_point(alpha = 0.5) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))
plot
# add jittering to geom_point fix the overlapping
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_is_mobile_2)) + geom_point(alpha = 0.5, position="jitter") +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))
plot
########### Let's compare is mobile variable within device category
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_device_category)) + geom_point(alpha = 0.2) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))+
geom_jitter() +
facet_grid(. ~device_is_mobile_2) # 2 plots, one for yes , one for no
#Insight: visits more than 600 happens only in desktop users
# Let's compare device category within mobile variable
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_is_mobile_2)) + geom_point(alpha = 0.2 ) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))+
geom_jitter() +
facet_grid(. ~device_device_category)

# Let's change the size and shape
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, col =  device_is_mobile_2)) + geom_point(alpha = 0.2, shape=1, size=4 ) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))+
geom_jitter() +
facet_grid(. ~device_device_category)
plot

# Let's change the shape=16 and add fill
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, fill =  device_is_mobile_2)) + geom_point(alpha = 0.2, shape=16, size=4 ) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))+
geom_jitter() +
facet_grid(. ~device_device_category)
plot
# Let's change the shape=21 and add fill - shape 21 has an outline
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, fill =  device_is_mobile_2)) + geom_point(alpha = 0.2, shape=21, size=4 ) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))+
geom_jitter() +
facet_grid(. ~device_device_category)
plot

# Id you put shape to 1 this allows for hollow circles
plot <- ggplot(df, aes(x = visit_number, y = totals_time_on_site, fill =  device_is_mobile_2)) + geom_point(alpha = 0.2, shape=1, size=4 ) +
geom_smooth(method=lm, se=FALSE, linetype=2) +
geom_smooth(aes(group=1))+
geom_jitter() +
facet_grid(. ~device_device_category)
plot


# Scaling the plot
# plot a bar - Breakdown of device category by businessid

plot <- ggplot(df, aes(x = device_device_category, fill=businessid)) +
  geom_bar()
plot
# product category by device category - we should remove NAs - Everything should be in form of factor
plot <- ggplot(df, aes(x = factor(product_clicked), fill= factor(device_device_category))) +
  geom_bar()
plot

# Bar plots with color ramp Palette="set1" and position - both variables should be in order and ordinal

ggplot(na.omit(df), aes(x = factor(hits_hit_number), fill = factor(product_clicked))) +
  geom_bar(position= "fill") +
  scale_fill_brewer(palette = "Set1" )
# Another option is changing it as an ordinal color - remove the palette = "Set1"
ggplot(na.omit(df), aes(x = factor(hits_hit_number), fill = factor(product_clicked))) +
  geom_bar(position= "fill") +
  scale_fill_brewer()
#INSIGHT= nonqual group has less hits until they click and emf has the highest number of hits
# Next STEP= Let's clean the hit numbers and remove undefined and find another ordinal variable




###########  Correlations
#Let's see how numeric variables are correlated:
names(df)
glimpse(df)
cor(df$visit_number, df$total_pageviews_clean, df$total_events,
    df$totals_hits_clean, df[,c(36:45)])
cor(df[,c(3,5,6,36:45)])

########### Association Rules

#Let's prepare the data for association rules:
dfrules <- df
dfF <- dfF %>% mutate_if(is.character,as.factor)
dfF <- dfF %>% mutate_if(is.logical,as.factor)

dfrules <- dfrules %>% select(-date, -total_pageviews_clean,-total_events, -device_operating_system,-device_screen_resolution,
                              -brokerage_num_clicked2,-brokerage_roth_num_clicked2, -emf_num_clicked2, 
                              -emf_roth_num_clicked2, -nonqual_num_clicked2, -product_number_clicked ,-totals_hits_clean,
                              -total_resultpageview,-total_articles_before,-total_resultpageview, -total_planning_before, 
                              -total_contactus_before,-total_search_before,-total_productpageview_before,-total_tools_before,
                              -total_fee_clicked_before,-total_privacy_before,-total_terms_before, -total_hits_onprod,
                              -iraproductoptions_before, -IRA_before, -considerarolloverira_before, -PRODUCTS_plusminus_before,
                              -time_on_site,-totalhit_catg, -totalpageview_catg, -totalarticle_catg, -totalproductpage_catg,
                              -totaltoolspage_catg, -totalplanning_catg)
dfrules <- dfrules %>% mutate_if(is.character,as.factor)
dfrules <- dfrules %>% mutate_if(is.logical,as.factor)
dfrules <- dfrules %>% select(-uniqueproduct_clicked)# may be later we put it into rhs
dfrulesF <- dfrules[,5:27]
lhs.vars = c('total_time_clean', 'device_device_category','device_browser',
             'geo_network_metro','source','medium','had_advisor', 'entrance',
             'article_before_product','product_before_product',
             'planning_before_product','tool_before_product','search_before_product',
             'term_before_product','privacy_before_product','clicked_overview_before',
             'clicked_keyfeatures_before', 'fees_before_product', 'nav_scroll_before', 'visit_frequency',
             'totalevents_catg')


rhs.var='product_clicked'
apriori.appearance = list (rhs =c('product_clicked=TRUE','product_clicked=FALSE'),
                           default= 'lhs')
apriori.parameter = list (support= 0.01, confidence=0.01,
                          minlen=1, maxlen=4)
rules = apriori (dfrulesF,parameter= apriori.parameter,
                 appearance=apriori.appearance)

inspect(rules[1:20])

items(rules) #for getting for each association a set of items involved in the association (e.g.,the union of the items in the LHS and the RHS for each rule),

# association rules sorted by support

rules.sorted.by.support = sort(rules, by='support')
inspect (rules.sorted.by.support [1:30])
# After running association rules by support, we realized that privacy_before_product, term_before_product , had_advisor and contact_before_product are not important as they account for small number of actions. There are not that many people who did those actions.(Between 5 and 1 percent of total users)
 #We re-run the association rules with taking out these variables

dfrulesF2 <- dfrulesF %>% select(-privacy_before_product, -term_before_product , -had_advisor, -contact_before_product)# we exclude privacy_before_product, term_before_product , had_advisor and contact_before_product
lhs.vars = c('total_time_clean', 'device_device_category','device_browser',
             'geo_network_metro','source','medium', 'entrance',
             'article_before_product','product_before_product',
             'planning_before_product','tool_before_product','search_before_product',
             'clicked_overview_before',
             'clicked_keyfeatures_before', 'fees_before_product', 'nav_scroll_before', 'visit_frequency',
             'totalevents_catg')


rhs.var='product_clicked'
apriori.appearance = list (rhs =c('product_clicked=TRUE','product_clicked=FALSE'),
                           default= 'lhs')
apriori.parameter = list (support= 0.01, confidence=0.01,
                          minlen=1, maxlen=4)
rules2 = apriori (dfrulesF2,parameter= apriori.parameter,
                 appearance=apriori.appearance)

#association rules sorted by support

rules.sorted.by.support = sort(rules2, by='support')
inspect (rules.sorted.by.support [1:30])

#association rules sorted by confidence}

rules.sorted.by.confidence= sort(rules2, by='confidence')
inspect (rules.sorted.by.support [1:40])

#association rules sorted by lift

rules.sorted.by.lift = sort(rules2, by='lift')
inspect (rules.sorted.by.support [1:30])

  
#Pruning Redundant Rules- Rules only for product_clicked=TRUE}

apriori.appearance = list (rhs = 'product_clicked=TRUE',
                           default= 'lhs')
apriori.parameter = list (support= 0.01, confidence=0.01,
                          minlen=1, maxlen=3)
rules3 = apriori (dfrulesF2,parameter= apriori.parameter,
                 appearance=apriori.appearance)
## find only rules with users who clicked on the product
apriori.parameter = list (support= 0.1, confidence=0.8,
                          minlen=1, maxlen=3)
rules3 <- apriori(dfrulesF2, parameter = apriori.parameter,
appearance = list(rhs = "product_clicked=TRUE",
default="lhs"))
inspect(rules[1:20])

# find redundant rules
rules.sorted.by.support = sort(rules2, by='support')
rules.sorted.by.confidence= sort(rules2, by='confidence')
rules.sorted.by.lift = sort(rules2, by='lift')

#find redundant rules by lyft and inspect non-redundant
inspect(rules.sorted.by.lift[!is.redundant(rules.sorted.by.lift)][1:30])
#find redundant rules by confidence and inspect non-redundant
inspect(rules.sorted.by.confidence[!is.redundant(rules.sorted.by.confidence)][1:30])
#find redundant rules by support and inspect non-redundant
inspect(rules.sorted.by.support[!is.redundant(rules.sorted.by.support)][1:30])
# remove redundant rules
rules.pruned <- rules.sorted.by.lift[!is.redundant(rules.sorted.by.support)]
inspect(rules.pruned [1:30])

#Visualizing Association Rules - Shiny App

library(arulesViz)
# http://brooksandrew.github.io/simpleblog/articles/association-rules-explore-app/
library('devtools')
library('shiny')
library('arules')
library('arulesViz')


devtools::source_url('https://raw.githubusercontent.com/brooksandrew/Rsenal/master/R/rules2df.R')
devtools::source_url('https://raw.githubusercontent.com/brooksandrew/Rsenal/master/R/bin.R')
arulesApp <- function (dataset, bin=T, vars=5, supp=0.1, conf=0.5) {
  
  ## binning numeric data
  for(i in 1:ncol(dataset)) {
    if(class(dataset[,i]) %in% c('numeric', 'integer')) dataset[,i] <- Rsenal::depthbin(dataset[,i], nbins=10)
  }
  
  ## calling Shiny App
  shinyApp(ui = shinyUI(pageWithSidebar(
    
    headerPanel("Association Rules"),
    
    sidebarPanel(
      
      conditionalPanel(
        condition = "input.samp=='Sample'",
        numericInput("nrule", 'Number of Rules', 5), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab=='graph'",
        radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
      ),
      
      conditionalPanel(
        condition = "input.lhsv=='Subset'", 
        uiOutput("choose_lhs"), br()
      ),
      
      conditionalPanel(
        condition = "input.rhsv=='Subset'", 
        uiOutput("choose_rhs"), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab=='grouped'",
        sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')", 
        radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
        uiOutput("choose_columns"), br(),
        sliderInput("supp", "Support:", min = 0, max = 1, value = supp , step = 1/10000), br(),
        sliderInput("conf", "Confidence:", min = 0, max = 1, value = conf , step = 1/10000), br(),
        selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
        numericInput("minL", "Min. items per set:", 2), br(), 
        numericInput("maxL", "Max. items per set::", 3), br(),
        radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
        radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br(),
        downloadButton('downloadData', 'Download Rules as CSV')
      )
      
    ),
    
    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                  tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                  tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                  tabPanel('Parallel Coordinates', value='paracoord', plotOutput("paracoordPlot", width='100%', height='100%')),
                  tabPanel('Matrix', value='matrix', plotOutput("matrixPlot", width='100%', height='100%')),
                  tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                  tabPanel('Table', value='table', verbatimTextOutput("rulesTable")),
                  tabPanel('Data Table', value='datatable', dataTableOutput("rulesDataTable"))
      )
    )
    
  )),
  
  server = function(input, output) {
    
    output$choose_columns <- renderUI({
      checkboxGroupInput("cols", "Choose variables:", 
                         choices  = colnames(dataset),
                         selected = colnames(dataset)[1:vars])
    })
    
    
    output$choose_lhs <- renderUI({
      checkboxGroupInput("colsLHS", "Choose LHS variables:", 
                         choices  = input$cols,
                         selected = input$cols[1])
    })
    
    output$choose_rhs <- renderUI({
      checkboxGroupInput("colsRHS", "Choose RHS variables:", 
                         choices  = input$cols,
                         selected = input$cols[1])
    })
    
    ## Extracting and Defining arules
    rules <- reactive({
      tr <- as(dataset[,input$cols], 'transactions')
      arAll <- apriori(tr, parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
      
      if(input$rhsv=='Subset' & input$lhsv!='Subset'){
        varsR <- character()
        for(i in 1:length(input$colsRHS)){
          tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
          varsR <- c(varsR, tmp)
        }
        ar <- subset(arAll, subset=rhs %in% varsR)
        
      } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
        varsL <- character()
        for(i in 1:length(input$colsLHS)){
          tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
          varsL <- c(varsL, tmp)
        }
        ar <- subset(arAll, subset=lhs %in% varsL)
        
      } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
        varsL <- character()
        for(i in 1:length(input$colsLHS)){
          tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
          varsL <- c(varsL, tmp)
        }
        varsR <- character()
        for(i in 1:length(input$colsRHS)){
          tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
          varsR <- c(varsR, tmp)
        }
        ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)
        
      } else {
        ar <- arAll
      }
      quality(ar)$conviction <- interestMeasure(ar, method='conviction', transactions=tr)
      quality(ar)$hyperConfidence <- interestMeasure(ar, method='hyperConfidence', transactions=tr)
      quality(ar)$cosine <- interestMeasure(ar, method='cosine', transactions=tr)
      quality(ar)$chiSquare <- interestMeasure(ar, method='chiSquare', transactions=tr)
      quality(ar)$coverage <- interestMeasure(ar, method='coverage', transactions=tr)
      quality(ar)$doc <- interestMeasure(ar, method='doc', transactions=tr)
      quality(ar)$gini <- interestMeasure(ar, method='gini', transactions=tr)
      quality(ar)$hyperLift <- interestMeasure(ar, method='hyperLift', transactions=tr)
      ar
    })
    
    # Rule length
    nR <- reactive({
      nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
    })
    
    ## Grouped Plot #########################
    output$groupedPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
    }, height=800, width=800)
    
    ## Graph Plot ##########################
    output$graphPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='graph', control=list(type=input$graphType))
    }, height=800, width=800)
    
    ## Scatter Plot ##########################
    output$scatterPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
    }, height=800, width=800)
    
    ## Parallel Coordinates Plot ###################
    output$paracoordPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='paracoord')
    }, height=800, width=800)
    
    ## Matrix Plot ###################
    output$matrixPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='matrix', control=list(reorder=T))
    }, height=800, width=800)
    
    ## Item Frequency Plot ##########################
    output$itemFreqPlot <- renderPlot({
      trans <- as(dataset[,input$cols], 'transactions')
      itemFrequencyPlot(trans)
    }, height=800, width=800)
    
    ## Rules Data Table ##########################
    output$rulesDataTable <- renderDataTable({
      ar <- rules()
      rulesdt <- rules2df(ar)
      rulesdt
    })
    
    ## Rules Printed ########################
    output$rulesTable <- renderPrint({
      #hack to disply results... make sure this match line above!!
      #ar <- apriori(dataset[,input$cols], parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
      ar <- rules()
      inspect(sort(ar, by=input$sort))
    })
    
    ## Download data to csv ########################
    output$downloadData <- downloadHandler(
      filename = 'arules_data.csv',
      content = function(file) {
        write.csv(rules2df(rules()), file)
      }
    )
    
    
  }
  )
}

arulesApp(dfrulesF2)

###### Preparing data for Logistic Regression

# Create a training and test dataset

library(caTools)
#remove NA's
dfrulesF3 <-na.omit(dfrulesF2)
#you can use the sample() function to get a random permutation of the row indices in a dataset, to use when making train/test splits
# Shuffle row indices: rows
rows <- sample(nrow(dfrulesF3))
#And then use those row indices to randomly reorder the dataset
dfrulesF3 <- dfrulesF3[rows, ]
#Identify the proper row to split on for a 60/40 split. Store this row number as split.
split <- round(nrow(dfrulesF3)* .60)
#save the first 60% as a training
dfrulesF3.train <- dfrulesF3[1:split, ]
#save the last 40% as the test set
dfrulesF3.test <- dfrulesF3[(split + 1):nrow(dfrulesF3), ]

##### Logistic Regression

library(doBy)


# The \texttt click.formula variable indicates the target variable to the left of the tilde and the independent variables to the right of the tilde. Let's start fitting our model with 4 important variables from association rules.
dfrulesF2$nav_scroll_before, dfrulesF2$product_before_product, dfrulesF2$fees_before_product, dfrulesF2$clicked_overview_before

glimpse(dfrulesF3.train)
click.formula1= product_clicked~product_before_product + nav_scroll_before + fees_before_product + clicked_overview_before
mylogit = glm(click.formula1, data = dfrulesF3.train, family = "binomial")
summary(mylogit)

# The model did not show any significant variable and we can build another model

click.formula2= product_clicked~total_time_clean+device_device_category+device_browser+
geo_network_metro+medium+source+medium+entrance+article_before_product+
product_before_product+planning_before_product+tool_before_product+search_before_product+clicked_overview_before+clicked_keyfeatures_before+fees_before_product+nav_scroll_before+visit_frequency+totalevents_catg
mylogit2 = glm(click.formula2, data = dfrulesF3.train, family = "binomial")
summary(mylogit2)


# subsection {Logistic Regression Efficiency and Accuracy

head(mylogit2$fitted)

#The values give us the probability that a user clicks on the product.To convert this probability to the binary output, we use the \texttt{round} function. The \texttt{round} function gives a value of 0 if the input is less than 0.05, and gives a value of 1 if the input is equal or more than 0.05.

predictedclick<-round(mylogit2$fitted)
predictedclick

# store the results in the variable and name it as \texttt{predictedclick}. Now, we have the predicted click values and actual click values. To compare these two, we create a comparison matrix using the \texttt{ftable} function.
comparisonmatrix<-ftable(predictedclick, dfrulesF3.train$product_clicked)


# The final step is to find the \texttt{accuracy} of the model. \texttt{Accuracy} of the model is calculated as the percentage of how successful the model has been in predicting the true against the actual values. Through \texttt{accuracy} we can decide the \texttt{effectiveness} of the model.

accuracy<-sum(diag(comparisonmatrix)/2733*100)
accuracy

#This shows that the model predicts the correct values 79 percent of the times.
#We can see our top predictors of the model by using the codes below.

pvals = as.data.frame(summary(mylogit2)$coefficients)
colnames(pvals) = c("Estimate", "se", "zval", "pval")
head(orderBy(~-Estimate, pvals[which
            (pvals$pval < 0.05 & pvals$zval > 0), ]),10)

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(mylogit2))
########### ROCR
library(ROCR)
predictTrain = predict(mylogit2, type ="response")
summary(predictTrain)
#Lets see if we predict higher for the actual
tapply(predictTrain, dfrulesF3.train$product_clicked,  mean) #Avg prediction for each of the true outcomes
ROCRpred = prediction(predictTrain, dfrulesF3.train$product_clicked)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
# Another ROCR
library(ROCR)
library(caTools)
# Predict on test: p
p <- predict(mylogit2, dfrulesF3.test,type = "response")
# Make ROC curve
colAUC(p, dfrulesF3.test[["product_clicked"]], plotROC = TRUE)


# Evaluating the model

p <- predict(mylogit2, dfrulesF3.test,type = "response")
p_class <- ifelse(p>0.65, "True", "False")
table(p_class)
p_class
# make simple 2-way frequency table to compare predicted vs. actual classes
mat.test <-table(p_class, dfrulesF3.test$product_clicked)

#We got this error:all arguments must have the same length
#we get a sample data with the same number of levels to match levels
length(p_class) # 1733 rows
length(dfrulesF3.test$product_clicked) # 1733 rows
#Sensitivity 
779/(90+779) #0.89
#Specitivity
506/(506+358) #0.58
#accuracy for test
accuracy.test<-sum(diag(mat.test)/1733*100)
accuracy.test #74.14
#accuracy for train
accuracy #79.25

