rm(list = ls())
gc()


library(ggplot2)
df = read.csv("tmdb_movies_data.csv") # enter data source (scaled, logtransform)
head(df)



#Number = c(262,615, 496, 79)
# use budget_adj and revenue_adj - considered inflation, replace budget and revenue
df$budget <- df$budget_adj
df$revenue <- df$revenue_adj
df$budget_adj <- NULL
df$revenue_adj <- NULL


# limit high budget filnms atleast 1M budget
df <- df[df$budget > 1000000,]

df$budget <- df$budget/1000000
df$revenue <- df$revenue/1000000


# roi
df$roi <- df$revenue/df$budget 
df$is_profit <- ifelse(df$roi > 1.1, "Yes", "No")

##
ggplot(df, aes(x=is_profit, fill = as.factor(is_profit))) + 
  geom_bar(stat = 'count', color="black", alpha = 0.7, position = "identity") +
  theme_minimal() +
  #facet_grid(prediction ~. ) +
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  scale_fill_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none") +
  xlab("has 10% ROI") +
  ggtitle("Expensive Movies (> $1M Budget)") +
  geom_bar(stat = "count") + 
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..), vjust= -0.5) +
  ylim(c(0,3000))
  #geom_text(aes(label=prediction), position=position_dodge(width=0.9), vjust=-0.25)
ggsave("is_profitable.png", width = 3.54, height = 4.65)

##
ggplot(df, aes(x=budget, y  = revenue, color = as.factor(is_profit))) + 
  geom_point(alpha = 0.7) +
  theme_minimal() +
  #facet_grid(prediction ~. ) +
  scale_color_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none") +
  ylab("Revenue (in $1M)") +
  xlab("Budget (in $1M)") +
  ggtitle("Successful Movies have >10% ROI")
  


###
ggplot(df, aes(x=popularity, fill = is_profit)) + 
  geom_histogram(color="black", bins = 100, alpha = 0.5, position = "identity") +
  theme_minimal() +
  facet_grid(is_profit ~., scales="free_y" ) +
  #scale_fill_manual(values=c("skyblue", "orange", "green")) +
  scale_fill_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none", strip.text.y = element_blank()) +
  xlim(c(0, 15)) +
  ggtitle("Popular Movies are More Profitable")
#ylim(c(0, 50))
ggsave("popularity.png", width = 5.54, height = 4.65)


###
ggplot(df, aes(x=runtime, fill = is_profit)) + 
  geom_histogram(color="black", bins = 100, alpha = 0.5, position = "identity") +
  theme_minimal() +
  facet_grid(is_profit ~., scales="free_y" ) +
  #scale_fill_manual(values=c("skyblue", "orange", "green")) +
  scale_fill_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none", strip.text.y = element_blank()) +
  xlim(c(50, 200)) +
  ggtitle("Runtimes Don't affect Movie Profitability")
#ylim(c(0, 50))
ggsave("runtime.png", width = 5.54, height = 4.65)


###
ggplot(df, aes(x=vote_count, fill = is_profit)) + 
  geom_histogram(color="black", bins = 100, alpha = 0.5, position = "identity") +
  theme_minimal() +
  facet_grid(is_profit ~., scales="free_y" ) +
  #scale_fill_manual(values=c("skyblue", "orange", "green")) +
  scale_fill_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none", strip.text.y = element_blank()) +
  xlim(c(0, 3000)) +
  ggtitle("More Vote Counts have More Profitable Movies") +
  xlab('vote count')
#ylim(c(0, 50))
ggsave("vote_count.png", width = 5.54, height = 4.65)

###
ggplot(df, aes(x=vote_average, fill = is_profit)) + 
  geom_histogram(color="black", bins = 100, alpha = 0.5, position = "identity") +
  theme_minimal() +
  facet_grid(is_profit ~., scales="free_y" ) +
  #scale_fill_manual(values=c("skyblue", "orange", "green")) +
  scale_fill_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none", strip.text.y = element_blank()) +
  xlim(c(0, 10)) +
  ggtitle("Flopped Movies have Worse Votes") +
  xlab('vote average')
#ylim(c(0, 50))
ggsave("vote_average.png", width = 5.54, height = 4.65)



##
ggplot(df, aes(x=release_year, fill = is_profit)) + 
  geom_histogram(color="black", bins = 110, alpha = 0.5, position = "identity") +
  theme_minimal() +
  facet_grid(is_profit ~., scales="free_y" ) +
  #scale_fill_manual(values=c("skyblue", "orange", "green")) +
  scale_fill_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none", strip.text.y = element_blank()) +
  #xlim(c(0, 10)) +
  xlab("release year") +
  ggtitle('1960s to 1980s were Profitable')
#ylim(c(0, 50))
ggsave("release_year.png", width = 5.54, height = 4.65)


## release month
df$release_date <- as.Date(df$release_date, format =  "%m/%d/%Y")
df$release_month <- format(df$release_date, "%m")

ggplot(df, aes(x=release_month, fill = is_profit)) + 
  geom_bar(color="black", alpha = 0.5, stat = 'count', position = "identity") +
  theme_minimal() +
  facet_grid(is_profit ~., scales="free_y" ) +
  #scale_fill_manual(values=c("skyblue", "orange", "green")) +
  scale_fill_manual(values=c("grey", "blue4")) +
  theme(legend.position = "none", strip.text.y = element_blank()) +
  #xlim(c(0, 10)) +
  xlab("release month") +
  ggtitle('Month June and December were More Profitable')
#ylim(c(0, 50))
ggsave("release_month.png", width = 5.54, height = 4.65)


## genre how to split
df$genres_2 <- (sapply(df$genres,gsub,pattern="\\|",replacement=" "))
library(tm)
library(dplyr)
genre <- Corpus(VectorSource(df[df$is_profit == "No",]$genres_2))
genre_dtm <- DocumentTermMatrix(genre)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing=FALSE) 
genre_wf <- data.frame(word=names(genre_freq), freq=genre_freq)
ggplot(genre_wf, aes(x=reorder(word,freq), y=freq))+ 
  geom_bar(stat="identity", fill = 'grey')+
  theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Black",face="bold"),legend.position="none")+
  #theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle("Movies have more Thriller-Horror Genres")+
  xlab("Genre")+
  ylab("No of Movies") + 
  coord_flip() +
  theme_minimal()
ggsave("genre_no.png", width = 5.54, height = 4.65)


genre <- Corpus(VectorSource(df[df$is_profit == "Yes",]$genres_2))
genre_dtm <- DocumentTermMatrix(genre)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing=FALSE) 
genre_wf <- data.frame(word=names(genre_freq), freq=genre_freq)
ggplot(genre_wf, aes(x=reorder(word,freq), y=freq))+ 
  geom_bar(stat="identity", fill = 'blue4')+
  theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Black",face="bold"),legend.position="none")+
  #theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle("Movies have more Comedy-Adventure Genres")+
  xlab("Genre")+
  ylab("No of Movies") + 
  coord_flip() +
  theme_minimal()
ggsave("genre_yes.png", width = 5.54, height = 4.65)
