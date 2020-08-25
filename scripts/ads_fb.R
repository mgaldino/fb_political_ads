library(httr)
library(tidyverse)
library(lubridate)

token_face_mkt <- "EAAvjGQVxB9YBAE4JvMHCVUZCXMza0OtEeALsSRuid07JApnaqsF7yLOAxzmOm7ZAKNFbk7XPrcR6HKGx81bgb6C5hTMQZACHZAIxv0wkgnHUNSc9nOwexx7SOZAkdxFxYCdA4b5vbLgXztYxEV58ZBxokyMlGsvea6dmDMEqBhavdn1FI1RvsRZADzuvBZCJsKZCkco99FwmBNdWhCflYZAnZBQ"


#link to fb api
my_link<- "https://graph.facebook.com"

#define fields you are interested in
search_fields=c("ad_creation_time", "ad_delivery_start_time", "ad_delivery_stop_time",
                "ad_creative_body", 
                "page_id",
                "page_name",
                "currency",
                "spend",
                "demographic_distribution",
                "funding_entity",
                "impressions",
                "region_distribution") %>% 
  stringr::str_c(., collapse=", ")

#get the data from the first 'page' of data the api provides
page_one_response <- GET(my_link,
                         path = "/ads_archive",
                         query = list(access_token = token_face_mkt,
                                      limit=5000,
                                      ad_active_status="ALL",
                                      search_terms="''",
                                      fields=search_fields,
                                      ad_reached_countries="BR"))
page_one_content <- content(page_one_response)

x <- tibble(data=page_one_content$data)
df_imp <- x %>% 
  unnest_wider(data) 

glimpse(df_imp)

#get the link refering to the next page
next_link <- page_one_content$paging$`next`

#iterate over all pages until there is no further page
while(length(next_link)>0) {
  
  next_response <- GET(next_link)
  next_content<- content(next_response)
  
  y <- tibble(data=next_content$data)
  df_next <- y %>% 
    unnest_wider(data) 
  
  df_imp <- bind_rows(df_imp, df_next)  
  
  next_link <- next_content$paging$`next`
  print(length(next_link))
}

glimpse(df_imp)
# código adaptado de https://rpubs.com/zoowalk/FB_EP2019/

df_imp <- df_imp %>%
  mutate(ad_creation_time1 = gsub("T", " ", ad_creation_time),
         ad_creation_time1 = gsub("+0000", "", ad_creation_time1), 
         ad_creation_time2 = ymd_hms(ad_creation_time1))

df_imp %>%
  mutate(bol_bolsonaro  = str_detect(tolower(ad_creative_body), "bolsonaro"),
         bol_Lula  = str_detect(tolower(ad_creative_body), "lula")) %>%
  group_by(bol_bolsonaro, bol_Lula) %>%
  summarise(n())

df_imp %>%
  summarise(max(ad_creation_time2),
            min(ad_creation_time2))

setwd("/home/mgaldino/Documentos/")
saveRDS(df_imp, "ads_03_ago.rds")
df_imp1 <- readRDS("ads_03_ago.rds")
