# BC US scrape
# code to scrape diabetes posts
rm(list = ls())
library(rvest)
library(pipeR)
library(XML)
main_url <- "https://community.breastcancer.org"
################################ List of Level one urls (the sub-forums)

# retrieve the first forum
mst <- html_session(main_url)
# now extract to the first
forum_url_suff <- mst %>% html_nodes(".donate span , h3 a") %>% html_attr("href")
# trim off the forst one which is an NA
forum_url_suff <- forum_url_suff[-1]

# for(frm in 1:length(forum_url_suff)){ THERE ARE 80 FORUMS
frm = 4

frm_url <- paste0(main_url, forum_url_suff[frm])
mst <- html_session(frm_url)
# last_pg<-mst%>% html_nodes('.topic-text+ .paging_controls li:nth-child(5) a') %>% html_text()
last_pg <- mst %>% html_nodes(".topic-text+ .paging_controls p") %>% html_text()
last_pg <- as.numeric(unlist(strsplit(last_pg, " "))[4])
if (length(last_pg) == 0) last_pg <- 1

# now we have the last page of the thread
topic_url_suff1 <- character()
views1 <- character()
post_title1 <- character()
for (pg in 1:last_pg) {
  # move through the pages
  curr_page_url <- paste0(frm_url, "?page=", pg)
  mst <- html_session(curr_page_url)
  topic_url_suff <- mst %>% html_nodes("h3 a") %>% html_attr("href")
  views <- mst %>% html_nodes(".count-views strong") %>% html_text()
  post_title <- mst %>% html_nodes("h3 a") %>% html_text()
  topic_url_suff1 <- c(topic_url_suff1, topic_url_suff)
  views1 <- c(views1, views)
  post_title1 <- c(post_title1, post_title)
  
  
  length(views1) == length(topic_url_suff1)
  length(topic_url_suff1) == length(post_title1)
  top_views <- cbind(topic_url_suff1, views1, post_title1)
}
write.table(top_views, file = "/Users/AndyC/Dropbox/rdata/bc_us/top_views.csv", sep = ",", col.names = NA, 
            na = "NA", qmethod = "double")
######################################################################################## Now visit the collected URLs one at a time and collect the posts

dat1<- read.delim(file = "/Users/AndyC/Dropbox/rdata/bc_us/top_views.csv", header = T, 
                                           sep = ",", stringsAsFactors = F)
topic_url_suff1<-as.character(dat1$topic_url_suff1)
views<-as.numeric(gsub(",","",dat1$views1))
######################################################################################## Now visit the collected URLs one at a time and collect the posts

#thred_pg <- which(topic_url_suff1 == "/forum/83/topic/829967")  #just EXAMPLE  to get to a thread wiht multiple pages
for (thred_pg in 1:length(topic_url_suff1)) {
  # move through the threads
  thred_views<-views[thred_pg]
  thred_pg_url <- paste0(main_url, topic_url_suff1[thred_pg])
  mst <- html_session(thred_pg_url)
  no_pages <- mst %>% html_nodes(".original-topic+ .paging_controls p") %>% html_text()
  if(length(no_pages)>0)no_pages <- as.numeric(unlist(strsplit(no_pages, " "))[4])  
  if(length(no_pages)==0)no_pages<-1
  # create the data frame for the results
  posts3 <- data.frame(forum = character(), thread=character(),thread_no = numeric(), post_no = character(), views_n = character(), 
                       username = character(), joined = character(), no_posts = character(), location = character(), 
                       date_posted = character(), info_block = character(), post = character(), url = character())
  from<-1
  for (pg in 1:no_pages) {
    # move through the threadpost sub-pages
    thred_pg_url_pg <- paste0(thred_pg_url, "?page=", pg)
    
    mst <- html_session(thred_pg_url_pg)
    ##### now collect the data
    username <- mst %>% html_nodes("strong a,.user-info a") %>% html_text()#####causing issues picking up other items
    date_posted <- mst %>% html_nodes(".post-time > strong , .left") %>% html_text()
    joined <- mst %>% html_nodes(".joined_date") %>% html_text()
    joined <- gsub("Joined: ", "", joined)
    
    location1 <- mst %>% html_nodes(".author_location") %>% html_text()  ##
    # test if location is present
    all_posts <- mst %>% html_nodes(".post , .original-topic")
    loc_pres <- character(length(username))
    for (i in 1:length(username)) {
      x <- grep("author_location", xmlToList(all_posts[[i]]))
      if (length(x) == 0)loc_pres[i] <- 0
      if (length(x) > 0)loc_pres[i] <- 1
    }
    location <- character(length(username))
    location[loc_pres == "1"] <- location1
    
    no_posts <- mst %>% html_nodes(".post_count") %>% html_text()
    
    info_block1 <- mst %>% html_nodes(".post_sig") %>% html_text()
    info_block1 <- gsub("\n", "", gsub("\r", "", gsub("\t", "", info_block1)))
    info_block1 <- gsub("\\s+", " ", info_block1)
    ##### test if info block is present
    all_posts <- mst %>% html_nodes(".original-topic , .post")
    loc_pres <- character(length(username))
    for (i in 1:length(username)) {
      x <- grep("post_sig", xmlToList(all_posts[[i]]))
      if (length(x) == 0)loc_pres[i] <- 0
      if (length(x) > 0)loc_pres[i] <- 1
    }
    info_block <- character(length(username))
    info_block[loc_pres == "1"] <- info_block1
    ##### 
    post <- mst %>% html_nodes(".user-post") %>% html_text()
    post <- gsub("\n", "", gsub("\r", "", gsub("\t", "", post)))
    post <- gsub("\\s+", " ", post)
    post <- gsub(" Log in to post a reply ", "", post)
    post <- gsub(" Log in to post a reply ", "", post)
    forum <- rep(mst %>% html_nodes("#crumbs a+ a") %>% html_text(), length(username))
    thread <- rep(mst %>% html_nodes("h1") %>% html_text(), length(username))
    url <- rep(thred_pg_url_pg, length(username))
    views_n <- rep(views[thred_pg], length(username))
    thread_no<-rep(thred_pg,length(username))
    post_no<-from:(length(username)+from-1)
    from<-from+length(username)
    # put togeher the data in a data frame
    posts2 <- data.frame(forum, thread,thread_no, post_no, views_n, username, joined, no_posts, location, 
                         date_posted, info_block, post, url)
    posts3 <- rbind(posts3, posts2)
    Sys.sleep(0.25)
    print(paste0(round((pg/no_pages) * 100, 0), " % complete"))
  }  #pg loop
}  #thred_pg loop moving through threads
save_path <- paste0("/Users/AndyC/Dropbox/rdata/bc_us/results/BC_US_posts_forum",thred_pg, ".csv")
write.table(posts3, file = save_path, sep = ",", col.names = NA, na = "NA", qmethod = "double")
