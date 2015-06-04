#BC US scrape
''''''''''''
main site frontpage

  
  the move into one of the forums

https://community.breastcancer.org/forum/62

or 
https://community.breastcancer.org/forum/147

then to a specific thread

https://community.breastcancer.org/forum/147/topic/830047
https://community.breastcancer.org/forum/147/topic/830047?page=2

# code to scrape diabetes posts
rm(list = ls())
library(rvest)
library(pipeR)
################################
###########################################
#########List of Level one urls (the sub-forums)
main_url<-"https://community.breastcancer.org/"
#retrieve the first forum
mst<-html_session(main_url)
#now extract to the first
forum_url_suff<-mst%>% html_nodes('.donate span , h3 a') %>% html_attr("href") 
#trim off the forst one which is an NA
forum_url_suff<-forum_url_suff[-1]

for(frm in 1:length(forum_url_suff)){
frm_url<-paste0(main_url,forum_url_suff[frm])
mst<-html_session(frm_url)
last_pg<-mst%>% html_nodes('.topic-text+ .paging_controls li:nth-child(5) a') %>% html_text() 
last_pg<-as.numeric(last_pg)

topic_url_suff1<-character()
for(pg in 1:last_pg){
curr_page_url<-paste0(frm_url,"?page=",pg)
mst<-html_session(curr_page_url)
topic_url_suff<-mst%>% html_nodes('h3 a') %>% html_attr("href")
topic_url_suff1<-c(topic_url_suff1,topic_url_suff)
}
}

