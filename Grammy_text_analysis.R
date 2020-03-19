R script : 
library(textreadr)
library(dplyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)

getwd()
data <- read_document(file = "GrammyAwards - Song of the year.txt")

a <- 63 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- data[i*b+z-b]
  }#closing z loop
}
names(my_df) <- c("award annual","year","songwriter", "artist","song","lyrics")
View(my_df)

year <- my_df$year
year <- as.POSIXct(year,format = "%Y")
year

#group of the song in the decade
cust_period1 <- data_frame(
  id=c("1958","1959","1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979"),
  period=rep("period1",each=22) )

cust_period2 <- data_frame(
  id=c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999"),
  period=rep("period2",each=20) )

cust_period3 <- data_frame(
  id=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
  period=rep("period2",each=20) )

data(stop_words)
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

#generate the custom stop words
cust_stop <- data_frame(
  word=c("ay","na","ooh","oo"),
  period=rep("period1",each=4) )

#count the word between 1958-1979
period_sixty <- my_df$lyrics %>% 
  substr(start=1 , stop = 30000) %>% 
  data_frame(year=my_df$year, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(year,word, sort=TRUE) %>% 
  filter(year %in% cust_period1$id)%>%
  mutate(period ="1960s-1970s",
         total_words = n()) %>% 
  #View(period_sixty)
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>% 
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red","blue","black","orange","darkgreen","purple","darkyellow","magenta","blue","grey"),
                   max.words=100,
                   scale = c(1,1),
                   fixed.asp=TRUE,
                   title.size=1) 

period_sixty %>% 
  count(word,sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(7) %>% 
  ungroup() %>% 
  mutate (word =reorder (word,n)) %>% 
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales = "free") +
  coord_flip()+
  labs(title = "Most frequency words during 1958 - 1979")

#count the word between 1980-1999
period_eighty <- my_df$lyrics %>% 
  substr(start=1 , stop = 10000) %>% 
  data_frame(year=my_df$year, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(year,word, sort=TRUE) %>% 
  filter(year %in% cust_period2$id)%>%
  mutate(period ="1980s-1990s",
         total_words = n()) %>% 
  #View(period_ninety)
  inner_join(get_sentiments("bing")) 
#count(word, sentiment, sort=TRUE)  
#acast(word ~sentiment, value.var="n", fill=0) %>%
#comparison.cloud(colors = c("red","blue","black","orange","darkgreen","purple","darkyellow","magenta","blue","grey"),
# max.words=200,
#scale = c(1,1),
# fixed.asp=TRUE,
#title.size=1)

period_eighty %>% 
  count(word,sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  mutate (word =reorder (word,n)) %>% 
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales = "free") +
  coord_flip()+
  labs(title = "Most frequency words during 1980 - 1999")

#count the word between 2000-2019
period_twenty <- my_df$lyrics %>% 
  substr(start=1 , stop = 10000) %>% 
  data_frame(year=my_df$year, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(year,word, sort=TRUE) %>% 
  filter(year %in% cust_period3$id)%>%
  mutate(period ="2000s-2019s",
         total_words = n()) %>% 
  #View(period_twenty)
  inner_join(get_sentiments("bing")) 
#count(word, sentiment, sort=TRUE) %>% 
#acast(word ~sentiment, value.var="n", fill=0) %>%
#comparison.cloud(colors = c("red","blue","black","orange","darkgreen","purple","darkyellow","magenta","blue","grey"),
# max.words=200,
# scale = c(1,1),
# fixed.asp=TRUE,
# title.size=1)

period_twenty %>% 
  count(word,sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate (word =reorder (word,n)) %>% 
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  ggtitle("sentiment word during 2000s - 2019s")+
  facet_wrap(~sentiment,scales = "free") +
  coord_flip()+
  labs(title = "Most frequency words during 2000 - 2019")


#sentiment over time by 6 decade  
sentiments_by_time <- bind_rows(mutate(period_sixty, period = "1960s-1979s"),
                                mutate(period_eighty,period = "1980s-1999s"),
                                mutate(period_twenty,period = "2000s-2019s")) %>% 
  
  group_by(period) %>% 
  mutate(proportion = n/sum(n)) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("nrc")) 

b <- sentiments_by_time %>%
  count(period,sentiment,proportion) %>% 
  group_by(sentiment,period ) %>% 
  ungroup ()

ggplot(b,aes(period, proportion,color = sentiment))+
  geom_line(size = 3,linetype = 1)+
  ggtitle("Proportion of negative and positive words in 6 decades")+
  geom_smooth(method = "lm",se = FALSE,lty = 2)+
  expand_limits( y= 0)



#trend line of the positive and negative word over 62 years  
all_lyrics <- my_df$lyrics %>% 
  substr(start=1 , stop = 30000) %>% 
  data_frame(year, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(year,word, sort=TRUE) %>%  
  group_by(year) %>% 
  mutate(total_words = n(),
         percent = n/total_words) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("bing"))


trendline <-  all_lyrics %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  count(year,sentiment,percent) %>% 
  group_by(sentiment,year ) %>% 
  ungroup () 

ggplot(trendline,aes(year, percent,color = sentiment))+
  geom_line(size = 0.5,linetype = 1)+
  ggtitle("Sentiment over time")+
  geom_smooth(method = "lm",se = TRUE,lty = 2)+
  expand_limits( y= 0 ) 

#song by positive ratio 
positive_song <- my_df$lyrics %>% 
  substr(start=1 , stop = 30000) %>% 
  data_frame(song=my_df$song, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  #anti_join(cust_stop) %>%
  count(song ,word, sort=TRUE) %>%  
  group_by(song) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(song, sentiment) %>%
  summarize(score = n()) %>%
  spread(sentiment, score) %>% 
  mutate_at(vars(negative:positive), funs(replace_na(., 1))) %>%
  ungroup()%>%
  mutate(positiveratio = positive/negative, 
         song = reorder(song,positiveratio))

ggplot(top_n(positive_song, 5),aes(x = song, y = positiveratio)) +
  geom_point(color="#66b2b2", size = 4) +
  geom_segment(aes(x=song, xend=song, 
                   y=min(positiveratio), yend=max(positiveratio)),
               linetype="dashed", size=0.1, color ="grey") +
  coord_flip()+
  ggtitle("The song with the hightest postive ratio ( positive word/negative word)")

View(my_df)

#song by negative ratio
negative_song <- my_df$lyrics %>% 
  substr(start=1 , stop = 30000) %>% 
  data_frame(song=my_df$song, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  #anti_join(cust_stop) %>%
  count(song ,word, sort=TRUE) %>%  
  group_by(song) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(song, sentiment) %>%
  summarize(score = n()) %>%
  spread(sentiment, score) %>% 
  mutate_at(vars(positive:negative), funs(replace_na(., 1))) %>%
  ungroup()%>%
  mutate(negativeratio = negative/positive, 
         song = reorder(song,negativeratio))

ggplot(top_n(negative_song, 5),aes(x = song, y = negativeratio)) +
  geom_point(color="#66b2b2", size = 5) +
  geom_segment(aes(x=song, xend=song, 
                   y=min(negativeratio), yend=max(negativeratio)),
               linetype="dashed", size=0.1, color ="red") +
  coord_flip() +
  ggtitle("The song with the hightest negative ratio ( negative word/positive word)")


shortest_lyrics <-  my_df$lyrics %>% 
  substr(start=1 , stop = 30000) %>% 
  data_frame(song=my_df$song, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  group_by(song) %>%
  count(song , sort=TRUE) 
View(shortest_lyrics)  

library(data.table)
shortest_lyrics <- data.table(shortest_lyrics)
dt2 <- shortest_lyrics[,list(sumamount = sum(n), freq = .N), by = c("song")] 
(dt2$sumamount)
View(dt2)


# shortest_lyrics <-  my_df$lyrics %>% 
# substr(start=1 , stop = 30000) %>% 
# data_frame(song=my_df$song, text=my_df$lyrics) %>%
# unnest_tokens(word,text) %>% 
# anti_join(stop_words) %>% 
# anti_join(cust_stop) %>%
# count(song ,word, sort=TRUE) %>% 
# group_by(song) %>%
# summarize(total_words_insong = sum(n)) 
# View(shortest_lyrics)



#bigram
my_bigrams <- my_df$lyrics %>%
  unnest_tokens(bigram, text=my_df$lyrics, token = "ngrams", n=2)

my_bigrams #We want to see the bigrams (words that appear together, "pairs")

my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#Most frequency word from 63 songs
total_words <-  my_df$lyrics %>% 
  substr(start=1 , stop = 30000) %>% 
  data_frame(song=my_df$song, text=my_df$lyrics) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>% 
  count(word, sort=TRUE) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  acast(word ~sentiment, value.var="n", fill=0) %>% 
  comparison.cloud(colors = c("red","blue","black","orange","darkgreen","purple","darkyellow","magenta","blue","grey"),
                   max.words=100,
                   scale = c(1.5,1),
                   fixed.asp=TRUE,   #fixed aspect 
                   title.size=1)

view(total_words)




combine_decade  <- bind_rows(mutate(period_sixty, period = "1960s-1979s"),
                             mutate(period_eighty,period = "1980s-1999s"),
                             mutate(period_twenty,period = "2000s-2019s")) 



modif2 <- combine_decade %>%
  group_by(period) %>%
  summarize(total=sum(n))

leftjoined <- left_join(combine_decade, modif2)

tidy_tfidf <- leftjoined %>%
  bind_tf_idf(word, period, n)

tidy_tfidf # we get all the zeors because we are looking at stop words ... too common

tidy_tfidf %>%
  arrange(desc(tf_idf))

tidy_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(period) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=period))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~period, ncol=2, scales="free")+
  coord_flip()

combine_decade  <- bind_rows(mutate(period_sixty, period = "1960s-1979s"),
                             mutate(period_eighty,period = "1980s-1999s"),
                             mutate(period_twenty,period = "2000s-2019s"))  %>% #closing bind_rows
  
  
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(period, word) %>%
  group_by(period) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(period, proportion) %>%
  gather(period, proportion, `1980s-1999s`, `2000s-2019s`) %>% 
  filter(proportion>0.001)


View(combine_decade)



