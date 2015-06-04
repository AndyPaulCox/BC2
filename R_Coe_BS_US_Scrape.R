#BC US scrape
''''''''''''

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
#last_pg<-mst%>% html_nodes('.topic-text+ .paging_controls li:nth-child(5) a') %>% html_text() 
last_pg<-mst%>% html_nodes('.topic-text+ .paging_controls p') %>% html_text() 
last_pg<-as.numeric(unlist(strsplit(last_pg," "))[4])
if(length(last_pg)==0)last_pg<-1
topic_url_suff1<-character()
views1<-character()
post_title1<-character()
for(pg in 1:last_pg){
curr_page_url<-paste0(frm_url,"?page=",pg)
mst<-html_session(curr_page_url)
topic_url_suff<-mst%>% html_nodes('h3 a') %>% html_attr("href")
views<-mst%>% html_nodes('.count-views strong') %>% html_text()
post_title<-mst%>% html_nodes('h3 a') %>% html_text()
topic_url_suff1<-c(topic_url_suff1,topic_url_suff)
views1<-c(views1,views)
post_title1<-c(post_title1,post_title)
}
}
length(views1)==length(topic_url_suff1)
length(topic_url_suff1)==length(post_title1)
top_views<-cbind(topic_url_suff1,views1,post_title1)
write.table(top_views, file = "/Users/AndyC/Dropbox/rdata/bc_us/top_views.csv", sep = ",", col.names = NA, na="NA", qmethod = "double")


thred_pg<-which(topic_url_suff1=="/forum/83/topic/829967")#just to get to a thread wiht multiple pages
for(thred_pg in 1:length(topic_url_suff1)){
  
thred_pg_url<-paste0(main_url,topic_url_suff1[thred_pg])

mst<-html_session(thred_pg_url)
no_pages<-mst%>% html_nodes('.original-topic+ .paging_controls p') %>% html_text()
no_pages<-as.numeric(unlist(strsplit(no_pages," "))[4])
for(pg in 1:no_pages){
  thred_pg_url_pg <-paste0(thred_pg_url,"?page=",pg)
  
  mst<-html_session(thred_pg_url_pg)
  #####now collect the data
  
  
  
  
  date_posted<-mst%>% html_nodes('#Section_Content div p > :nth-child(1)') %>% html_text()
  joined<-mst%>% html_nodes('.joined_date') %>% html_text()
  location<-mst%>% html_nodes('.original-topic+ .paging_controls p') %>% html_text()
  no_posts<-mst%>% html_nodes('.original-topic+ .paging_controls p') %>% html_text()
  username<-mst%>% html_nodes('strong a , .post-time a , a strong') %>% html_text()
  info_block<-mst%>% html_nodes('.original-topic+ .paging_controls p') %>% html_text()
  post<-mst%>% html_nodes('.original-topic+ .paging_controls p') %>% html_text()
  
  forum<-rep(mst%>% html_nodes('h1') %>% html_text(),length(username))
  thread<-rep(mst%>% html_nodes('h1') %>% html_text(),length(username))
  url<-rep(thred_pg_url_pg,length(username))
 views_n<-rep(views[thred_pg],length(username))
}
