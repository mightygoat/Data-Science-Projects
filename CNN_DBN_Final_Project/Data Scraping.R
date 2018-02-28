userreview=data.frame(Star_Rating=character(),
                      User=character(),
                      Title=character(),
                      Body=character(),
                      Date=character(),
                      stringsAsFactors=FALSE)
#Date=as.Date(character())
library(rvest)
for(i in 1:501){
url=paste("https://www.amazon.com/All-New-Amazon-Echo-Dot-Add-Alexa-To-Any-Room/product-reviews/B01DFKC2SO/ref=cm_cr_getr_d_paging_btm_",i,"?ie=UTF8&reviewerType=avp_only_reviews&showViewpoints=1&sortBy=helpful&pageNumber=",i,sep="")
echo_dot <- read_html(url)

stars <- echo_dot %>% 
  html_nodes("#cm_cr-review_list .review .review-rating .a-icon-alt") %>%
  html_text() 
user <- echo_dot %>% 
  html_nodes("#cm_cr-review_list .review .author") %>%
  html_text()
title <- echo_dot %>% 
  html_nodes("#cm_cr-review_list .review .review-title") %>%
  html_text() 
body <- echo_dot %>% 
  html_nodes("#cm_cr-review_list .review .review-data .review-text") %>%
  html_text() 
date <- echo_dot %>% 
  html_nodes("#cm_cr-review_list .review .review-date ") %>%
  html_text() 

print(i)

for(j in 1:length(stars)){
  
  months.regex <- paste(month.name, collapse='|')
  d <- gsub(paste0(".*(", months.regex, ")"), "\\1", date[j][grep(months.regex, date[j], TRUE)], TRUE)
  #ndate <- as.Date(d, "%B %d, %Y")
  userreview[nrow(userreview)+1,] <- c(stars[j],user[j],title[j],body[j],d)
  
  }
}

write.csv(userreview,file = "/Users/nithinkartha/Desktop/userreview.csv",row.names = FALSE)



