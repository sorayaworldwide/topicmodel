library(tidyverse)
library(stm)
library(tidytext)
library(rtweet)
library(writexl)
library(readxl)
install.packages("igraph")
install.packages("reactable")
library(igraph)
##store api keys
app_name<-"TMdukeglobaled"
api_key<-"oJQwW4AKhHhde5jTm6p8vygfw"
api_secret_key<-"DAqTdDxnWxNRctBKnt0ujoXn38jtrxxwz31zwQI3VpXcHS69wc"
access_token<-"293694037-Ex0EnQydER3ujJ5xxogNJcAlwaGItlBJGYgdBzlj"
access_token_secret<-"gz5fLkIRkZDbM8NS2pIu24lfEwp40854yg0ONyL609VJS"
##authenticate via web browser
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
get_token()
##user data
globaeddata<-lookup_users("dukeglobaled")
##get timeline
globaledtime<-get_timelines("dukeglobaled", n=500)
##lookup status
tw<-lookup_statuses("1126484800204890112")
##get followers
dukeglobedfollower<-get_followers("dukeglobaled")
##
tmls<-get_timeline(as.character(dukeglobedfollower[["user_id"]]), n = 500, retryonratelimit = T)
##
users<-lookup_users(dukeglobedfollower$user_id)
##
top_users<- users$screen_name[order(users$followers_count, decreasing=TRUE)][1:100]
##
tmls <-get_timelines(top_users, n=3200)
## try with less tweets
tmls2 <-get_timelines(top_users, n=500)
##check who top number of followers
top_followers<-users%>%
  select(screen_name, followers_count)%>%
  arrange(desc(followers_count))%>%
  top_n()
##top 100 users who favorite our stuff
favorite_user<-users%>%
  select(screen_name, user_id, favorite_count)%>%
arrange(desc(favorite_count))%>%
top_n(100)
##top 100 users to retweet our stuff
retweet_user<-users%>%
  select(screen_name, retweet_count)%>%
  arrange(desc(retweet_count))%>%
  top_n(100)
##
retweetfavorite_user<-users%>%
  select(screen_name, retweet_favorite_count)%>%
  arrange(desc(retweet_favorite_count))%>%
  top_n(100)
##network analysis of followers just for fun :)
fds<-get_friends(c("ncstate", "dukeu", "unc"))
tbl<-table(fds$user_id)
fds3 <- subset(fds, user_id %in% names(tbl[tbl > 2L]))
mat <-as.matrix(fds3)
mat <-igraph::graph_from_edgelist(mat)
plot(mat)
##network of dukeglobaled
fds2<-get_friends(c("dukeglobaled", "uncstudyabroad", "NCSUStudyAbroad"))
tbl2<-table(fds2$user_id)
fds4<-subset(fds2, user_id %in% names(tbl2[tbl2 >2L]))
mat2 <-as.matrix(fds4)
mat2 <-igraph::graph_from_edgelist(mat2)
plot(mat2)
view(top_users)
##bottom 500 
less_popular_favorite_user<-users%>%
  select(screen_name, favorite_count)%>%
  arrange(desc(favorite_count))%>%
  slice_min(favorite_count, n=100)
##
tweetsdata_users<-tweets_data(users)
##Decided to use 'favorite user' - followers whose tweets are favorited the most as influencers
favorite_user2<-lookup_users(favorite_user$user_id)
##
top_favorites<-favorite_user2$screen_name[order(favorite_user2$favorite_count, decreasing=TRUE)][1:109]
##  
tmlsfav <-get_timelines(top_favorites, n=500) 
##write xlsx
write_xlsx(tmlsfav, "tmlsfav.xlsx")
##read xlsx
tmlsfav<-read_xlsx("tmlsfav.xlsx")
##clean
tmlsclean1<-tmlsfav%>%
  select(screen_name, created_at, text)%>%
  filter(is.na(tmlsfav$reply_to_status_id))

##
 

##clean 2
library(NLP)
library(tm)
library(topicmodels)
library(SnowballC)
library(httr)
#
tmlsclean2<-iconv(tmlsclean1, to = "ASCII", sub = " ")
tmlsclean2<-tolower(tmlsclean2)
##
tmlstidy<-tmlsclean1%>%
  unnest_tokens(output=word, input=text)%>%
  anti_join(stop_words, by="word")
##word count
tmlstidy%>%
  count(word, sort=TRUE)
##clean
tmlstidy1<-tmlstidy%>%
  group_by(word)%>%
  filter(word !="amp")%>%
  filter(word !="t.co")%>%
  filter(word !="https")%>%
  filter(word !="http")
##tmlstidy2
tmlstidy2<-tmlstidy1[-grep("[0-9]+", tmlstidy1$word),]
##word count tidy1
tmlstidy1%>%
  count(word, sort=TRUE)

#tmlstidy3
tmlstidy3<-gsub("http.+ |http.+$", " ", tmlstidy)
#tmlstidy4
tmlstidy4<-tmlstidy3%>%
  group_by(word)%>%
  filter(word !="amp")%>%
  filter(word !="t.co")
  

##dtm
tweets_dtm <-tmlstidy1%>%
  count(created_at, word)%>%
  cast_dtm(created_at, word, n)
list(tweets_dtm)

##dtm2 without numbers
tweets_dtm2 <-tmlstidy2%>%
  count(created_at, word)%>%
  cast_dtm(created_at, word, n)
list(tweets_dtm)

###use this for stm
library(stringr)
tmlsclean3<-tmlsfav%>%
  select(screen_name, created_at, text, favorite_count, retweet_count)%>%
  filter(is.na(tmlsfav$reply_to_status_id))

tmlsclean4<-gsub("amp","", tmlsclean3$text)
view(tmlsclean4)

tmlsclean5<-cbind(tmlsclean3, tmlsclean4)
###
tmlsclean6<-select(tmlsclean5, -text)
##
tmlsclean7<-rename(tmlsclean6, text=tmlsclean4)

temp <- textProcessor(tmlsclean7$text,
                     metadata=tmlsclean7,
                     lowercase=TRUE, 
                     removestopwords=TRUE, 
                     removenumbers=TRUE,  
                     removepunctuation=TRUE, 
                     wordLengths=c(3,Inf),
                     stem=TRUE,
                     onlycharacter= FALSE, 
                     striphtml=TRUE, 
                     customstopwords=NULL)
                     
## meta, vocab, docs
meta<-temp$meta
vocab <-temp$vocab
docs <-temp$documents

#tweets stm - uncleaned from amp
tweets_stm <-stm(documents=docs,
                 data=meta,
                 vocab=vocab,
                 prevalence =~ favorite_count + retweet_count,
                 K=20,
                 max.em.its=25,
                 verbose = FALSE)
##tweets stm20 - cleaned amp
tweets_stm20 <-stm(documents=docs,
                 data=meta,
                 vocab=vocab,
                 prevalence =~ favorite_count + retweet_count,
                 K=20,
                 max.em.its=25,
                 verbose = FALSE)

tweets_stm                   
plot.STM(tweets_stm, n=5)                  
plot.STM(tweets_stm20, n=5)                    
##find k 
library(ldatuning)
k_metrics <- FindTopicsNumber(
  tweets_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics)
##try 30 stm
tweets_stm30 <-stm(documents=docs,
                   data=meta,
                   vocab=vocab,
                   prevalence =~ favorite_count + retweet_count,
                   K=30,
                   max.em.its=25,
                   verbose = FALSE)
plot.STM(tweets_stm30, n=5)
#try 30 lda
tweets_lda30<-LDA(tweets_dtm,
                k=30,
                control=list(seed=1234)) 
##
terms(tweets_lda, 5)
##stm 15
tweets_stm15 <-stm(documents=docs,
                   data=meta,
                   vocab=vocab,
                   prevalence =~ favorite_count + retweet_count,
                   K=15,
                   max.em.its=25,
                   verbose = FALSE)
##
plot.STM(tweets_stm15, n=5)
##
tweets_lda<-LDA(tweets_dtm,
                k=15,
                control=list(seed=12345)) 
terms(tweets_lda, 5)
##
tweets2_lda15<-LDA(tweets_dtm2,
                  k=15,
                  control=list(seed=4567))
terms(tweets2_lda15, 5)
terms(tweets_lda15, 5)
##
tweets2_lda30<-LDA(tweets_dtm2,
                  k=30,
                  control=list(seed=1234))
terms(tweets_lda30, 5)
##graphs
library(wordcloud)
wordcloud(tweets2_lda30, min.freq=2, max.words = 150)
##
toLDAvis(mod = tweets_stm15, docs = docs)
##
tidy_lda15 <- tidy(tweets_lda)
##
top_terms <- tidy_lda15 %>%
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
##tidy 30
tidy_lda30 <- tidy(tweets_lda30)
##
top_terms30 <- tidy_lda30 %>%
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms30 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
##find thoughts
tmlsclean7_reduced <-tmlsclean7$text[-temp$docs.removed]

findThoughts(tweets_stm15,
             texts = tmlsclean7_reduced,
             topics = 1, 
             n = 10,
             thresh = 0.5)