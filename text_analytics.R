#######install/load packages#######

required.packages <- c("tidyverse", "magrittr", "tidytext", "tm")
new.packages <- required.packages[ !(required.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(required.packages, library, character.only = TRUE)


#######functions#######

code = function(wordlist, text) {
  
  out = ifelse(grepl(wordlist,text, ignore.case = TRUE, perl = TRUE),1,0)
  return (out)
  
}

es <- function(a,b){
  
  (mean(a)-mean(b))/
    (sqrt(((length(a)-1)*sd(a)+(length(b)-1)*sd(b))/
            (length(a)+length(b))))
}

#######set working directory#######
setwd("XX")

#######import/clean data#######
data <- read_csv("XX")

##specify text column
text <- colnames(data$XX)
data <- data[-which(is.na(data$text)) , ]

##input user information or subset however appropriate
data_subset <- subset(data ,select = c("user" , "text"))

##clean and pre-process data
indv_resp <- data$text
indv_resp <- tolower(na.omit(indv_resp))
indv_resp <- gsub("[\n\r]|<p>|</p>|[[:punct:]]|(sum)|\\?|\\.|\\,","",indv_resp, perl = TRUE)
indv_resp <- stringr::str_replace_all(indv_resp,"[^a-zA-Z\\s]", " ")
indv_resp <- stringr::str_replace_all(indv_resp,"[\\s]+", " ")
for (i in 1:length(indv_resp)){
  str <- indv_resp[i]
  str <- unlist(strsplit(str, " "))
  str <- subset(str, !(str %in% stop_words$word))
  str <- paste(str, collapse = " ")
  indv_resp[i] <- str
}
indv_resp <- indv_resp[indv_resp!=""]
indv_resp <- unique(indv_resp)


#######Create Document Term Matrix#######

doc_term_matrix <- dfm(indv_resp, stem=FALSE) 
doc_term_matrix <- as.matrix(doc_term_matrix)


#######Latent Semantic Analysis#######

# doc_term_matrix <- t(doc_term_matrix)
# doc_term_matrix <- cbind(doc_term_matrix, rowSums(doc_term_matrix))
# colnames(doc_term_matrix)[ncol(doc_term_matrix)] <- "sum"
# doc_term_matrix <- doc_term_matrix[ order(-doc_term_matrix[,"sum"]) , ]
# doc_term_matrix <- t(doc_term_matrix)

# doc_term_matrix_svd <- doc_term_matrix[1:1000,1:ncol(doc_term_matrix)-1]
# doc_term_matrix_svd <- doc_term_matrix_svd[ , !colSums(doc_term_matrix_svd) == 0  ]

# sort(svd$rotation[ svd$rotation[,1] > .001 , 1])
# sort(svd$rotation[svd$rotation[, 2] > .01 , 2])
# sort(svd$rotation[ svd$rotation[,3] > .01 , 3])
# sort(svd$rotation[ svd$rotation[,4] > .01 , 4])
# sort(svd$rotation[ svd$rotation[,5] > .1 , 5])
# sort(svd$rotation[ svd$rotation[,6] > .001 , 6])
# sort(svd$rotation[ svd$rotation[,7] > .1 , 7])
# sort(svd$rotation[ svd$rotation[,8] > .05 , 8])
# sort(svd$rotation[ svd$rotation[,9] > .05 , 9])
# sort(svd$rotation[ svd$rotation[,10] > .05 , 10])


#######Word Frequencies#######

tidy_data <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

word.freq <- tidy_data %>%
  count(word, sort = TRUE)
 
ggplot(word.freq[1:20,], aes(x=word, y=n, fill = word)) +
	geom_bar(stat="identity") + theme_minimal() + theme(legend.position="none")


#######Latent Dirichlet Allocation#######

doc_term_matrix <- doc_term_matrix[rowSums(doc_term_matrix)!=0 , ]
data_lda <- LDA(doc_term_matrix, k = 10)
tidy_data_lda <- tidy(data_lda)

top_terms <- tidy_data_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic", x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
  
  lda_gamma <- tidy(data_lda, matrix = "gamma")


ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

colnames(tidy_data_lda)[which(colnames(tidy_data_lda) == "term")] <- "word"

tidy_data_lda %>%
	arrange(desc(beta))



#######Sentiment Analysis#######

data_sentiments <- tidy_data_lda %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) #%>%
  #summarize(score = sum(score * n) / sum(n))

data_sentiments %>%
  #mutate(word = reorder(word, score)) %>%
  ggplot(aes(word, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average sentiment score")

sent.wordlist <- get_sentiments("afinn") %>%
	mutate(posneg = ifelse(score > 0 , "pos", "neg")) %>%
	arrange(desc(score)) %>%
	filter(!grepl("negative|like|ill|bomb|hindrance|problem|lobby|charged|no|yes", word))

positive5list <- unique(data_sentiments$word[data_sentiments$score==5])
positive4list <- unique(data_sentiments$word[data_sentiments$score==4])
positive3list <- unique(data_sentiments$word[data_sentiments$score==3])
positive2list <- unique(data_sentiments$word[data_sentiments$score==2])
positive2list <- positive2list[positive2list != "like"]
positive1list <- unique(data_sentiments$word[data_sentiments$score==1])
negative1list <- unique(data_sentiments$word[data_sentiments$score==-1])
negative1list <- negative1list[negative1list !="bomb"]
negative2list <- unique(data_sentiments$word[data_sentiments$score==-2])
negative2list <- negative2list[negative2list !="negative"]
negative2list <- negative2list[negative2list !="ill"]
negative2list <- negative2list[negative2list !="hindrance"]
negative2list <- negative2list[negative2list !="problem"]
negative2list <- negative2list[negative2list !="lobby"]
negative3list <- unique(data_sentiments$word[data_sentiments$score==-3])
negative3list <- negative3list[negative3list !="charged"]
negative4list <- unique(data_sentiments$word[data_sentiments$score==-4])

data$positive5 <- code(paste("\\b",paste(positive5list, collapse="\\b|\\b"),"\\b", sep=""), data$text)
data$positive4 <- code(paste("\\b",paste(positive4list, collapse="\\b|\\b"),"\\b", sep=""), data$text)
data$positive3 <- code(paste("\\b",paste(positive3list, collapse="\\b|\\b"),"\\b", sep=""), data$text)
data$positive2 <- code(paste("\\b",paste(positive2list, collapse="\\b|\\b"),"\\b", sep=""), data$text)
data$positive1 <- code(paste("\\b",paste(positive1list, collapse="\\b|\\b"),"\\b", sep=""), data$text)
data$negative1 <- code(paste("\\b",paste(negative1list, collapse="\\b|\\b"),"\\b", sep=""), data$text)
data$negative2 <- code(paste(paste("\\b",paste(negative2list, collapse="\\b|\\b"),"\\b", sep=""), "(?<!\\bno )(\\bproblem)", sep="|"), data$text)
data$negative3 <- code(paste("\\b",paste(negative3list, collapse="\\b|\\b"),"\\b", sep=""), data$text)
data$negative4 <- code(paste("\\b",paste(negative4list, collapse="\\b|\\b"),"\\b", sep=""), data$text)

pos5score <- as.data.frame(signif(aggregate(data$positive5, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
pos4score <- as.data.frame(signif(aggregate(data$positive4, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
pos3score <- as.data.frame(signif(aggregate(data$positive3, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
pos2score <- as.data.frame(signif(aggregate(data$positive2, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
pos1score <- as.data.frame(signif(aggregate(data$positive1, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
neg1score <- as.data.frame(signif(aggregate(data$negative1, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
neg2score <- as.data.frame(signif(aggregate(data$negative2, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
neg3score <- as.data.frame(signif(aggregate(data$negative3, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
neg4score <- as.data.frame(signif(aggregate(data$negative4, by = list(data$uniqueuser), FUN = sum)$x /as.data.frame(table(data$uniqueuser))$Freq , 3))
neg5score <-  0

sentimentscores <- cbind(as.data.frame(table(data$uniqueuser))$Var1, pos5score, pos4score, pos3score, pos2score, pos1score, neg1score, neg2score, neg3score, neg4score, neg5score)
colnames(sentimentscores) <- c("uniqueuser" , "pos5score", "pos4score", "pos3score", "pos2score", "pos1score", "neg1score", "neg2score", "neg3score", "neg4score", "neg5score")

data <- merge(data, sentimentscores, by = "uniqueuser")

data$sentimentscore <- data$pos5score*5 + data$pos4score*4 + data$pos3score*3 + data$pos2score*2 + data$pos1score + data$neg1score*-1 + data$neg2score*-2 + data$neg3score*-3 + data$neg4score*-4 + data$neg5score*-5

data$pos.sentimentscore <- data$pos5score*5 + data$pos4score*4 + data$pos3score*3 + data$pos2score*2 + data$pos1score

data$neg.sentimentscore <- data$neg1score*-1 + data$neg2score*-2 + data$neg3score*-3 + data$neg4score*-4 + data$neg5score*-5

sdata <- unique(data[c("uniqueuser", "underrepwomen", "sentimentscore")])
sdata.two <- unique(data[c("uniqueuser", "underrepwomen", "neg.sentimentscore", "pos.sentimentscore")])

sdata$underrepwomen <- as.factor(sdata$underrepwomen)
#sdata$underrepwomen<- relevel(sdata$underrepwomen, ref = 8)
summary(lm(sdata$sentimentscore ~ sdata$underrepwomen))

sdata.grouped <- aggregate(sdata$sentimentscore, by=list(sdata$underrepwomen), FUN = mean)
sdata.grouped$x <- signif(sdata.grouped$x, 3)

sdata.two.grouped.pos <- aggregate(sdata.two$pos.sentimentscore, by=list(sdata.two$underrepwomen), FUN = mean)
sdata.two.grouped.pos$type <- "pos"
sdata.two.grouped.neg <- aggregate(sdata.two$neg.sentimentscore, by=list(sdata.two$underrepwomen), FUN = mean)
sdata.two.grouped.neg$type <- "neg"

sdata.two.grouped <- rbind(sdata.two.grouped.pos,sdata.two.grouped.neg )
sdata.two.grouped$x <- signif(sdata.two.grouped$x, 3)

wilcox.test(sdata$sentimentscore[sdata$underrepwomen == "bhwomen"], sdata$sentimentscore[sdata$underrepwomen == "others"])
es(sdata$sentimentscore[sdata$underrepwomen == "bhwomen"], sdata$sentimentscore[sdata$underrepwomen == "others"])

wilcox.test(sdata.two$pos.sentimentscore[sdata.two$underrepwomen == "bhwomen"], sdata.two$pos.sentimentscore[sdata.two$underrepwomen == "others"])
wilcox.test(sdata.two$neg.sentimentscore[sdata.two$underrepwomen == "bhwomen"], sdata.two$neg.sentimentscore[sdata.two$underrepwomen == "others"])

rg.colors2 <- setNames(c( "#F8766D" , "#00BFC4"), levels(data$underrepwomen))

ggplot(sdata.two.grouped, aes(x=type, y = x, fill = Group.1 )) + geom_bar(stat= "identity", position="dodge") + guides(fill=guide_legend(title="")) + labs(x="sentiment type", y="sentiment score") + theme(text = element_text(size=15)) + scale_fill_manual(values = rg.colors2, labels = c("WOC", "others"))


ggplot(sdata.grouped, aes(x = Group.1, y = x, fill = Group.1)) + geom_bar(stat="identity") + geom_text(aes(label = x)) + scale_color_manual(values = rg.colors2)

ggplot(data = sdata, aes(x = sentimentscore, color = underrepwomen)) + geom_density(alpha = .3) + scale_color_manual(values = rg.colors2)
 
 # data.grouped <- aggregate(data$pos1score, by=list(data$racegender.grouped), FUN = mean)
 # data.grouped$x <- signif(data.grouped$x, 3)
 
  # ggplot(data.grouped, aes(x= Group.1, y = x, fill = Group.1))  + geom_bar(stat="identity") + geom_text(aes(label=x)) + scale_color_manual(values = rg.colors)


 data.long <- tidyr::gather(data[,c("underrepwomen", "pos5score", "pos4score", "pos3score", "pos2score", "pos1score", "neg1score", "neg2score", "neg3score", "neg4score", "neg5score")], key = sent_type, value = sent_score, -underrepwomen)
 
 data.long$sent_type <- as.factor(data.long$sent_type)
 data.long$sent_type <- factor(data.long$sent_type, levels=c("neg5score", "neg4score" , "neg3score","neg2score", "neg1score", "pos1score", "pos2score" , "pos3score", "pos4score","pos5score"))
 
data.long.grouped <- aggregate(data.long, by=list(data.long$underrepwomen, data.long$sent_type), FUN = mean)
  data.long.grouped$sent_score <- signif(data.long.grouped$sent_score, 3)
  data.long.grouped <- data.long.grouped[,-c(3,4)]
  colnames(data.long.grouped) <- c("underrepwomen", "sent_type","sent_score")
 
 
 
data.long.pos <- tidyr::gather(data[,c("underrepwomen", "pos5score", "pos4score", "pos3score", "pos2score", "pos1score")], key = pos, value = pos_score, -underrepwomen)

data.long.neg <- tidyr::gather(data[,c("underrepwomen", "neg1score", "neg2score", "neg3score", "neg4score", "neg5score")], key = neg, value = neg_score, -underrepwomen)

data.long.posneg <- cbind(data.long.pos, data.long.neg)

data.long.posneg <-  tidyr::gather(data.long.posneg[,c("underrepwomen", "pos_score", "neg_score")], key = sent_type, value = sent_score, -underrepwomen)



wilcox.test(data.long.posneg$sent_score[data.long.posneg$sent_type == "neg_score" & data.long.posneg$underrepwomen == "bhwomen"], data.long.posneg$sent_score[data.long.posneg$sent_type == "neg_score" & data.long.posneg$underrepwomen == "others"])

es(data.long.posneg$sent_score[data.long.posneg$sent_type == "neg_score" & data.long.posneg$underrepwomen == "bhwomen"], data.long.posneg$sent_score[data.long.posneg$sent_type == "neg_score" & data.long.posneg$underrepwomen == "others"])

wilcox.test(data.long.posneg$sent_score[data.long.posneg$sent_type == "pos_score" & data.long.posneg$underrepwomen == "bhwomen"], data.long.posneg$sent_score[data.long.posneg$sent_type == "pos_score" & data.long.posneg$underrepwomen == "others"])

es(data.long.posneg$sent_score[data.long.posneg$sent_type == "pos_score" & data.long.posneg$underrepwomen == "bhwomen"], data.long.posneg$sent_score[data.long.posneg$sent_type == "pos_score" & data.long.posneg$underrepwomen == "others"])

 

  
  # mean.scores <- as.data.frame(aggregate(data.long.grouped$sent_score, by=list(data.long.grouped$sent_type), FUN = mean))
  # mean.scores$racegender <- "Average"
  # colnames(mean.scores) <- c("sent_type", "sent_score", "racegender")
  # mean.scores <- mean.scores[,c(3,1,2)]
  
 # everyone <- rbind(mean.scores, data.long.grouped)

  ggplot(data.long.grouped, aes(x= sent_type, y = sent_score, fill = underrepwomen))  + geom_bar(stat= "identity",position = "dodge")  + scale_color_manual(values = c( "#C77CFF" , "#00BFC4")) 
 
 ggplot(data.long.posneg, aes(x= sent_type, y = sent_score, fill = underrepwomen))  + geom_bar(stat= "summary",position = "dodge", fun.y = "mean")  + scale_color_manual(values = c( "#C77CFF" , "#00BFC4"), drop = F) #+ geom_text(aes(label = sent_score))
  

#investigate meaning behind sentiment analysis
grams <- data %>%
	unnest_tokens(gram, text, token="ngrams", n = 2)

grams <- grams[,c("underrepwomen", "gram")]

listofwords <- sent.wordlist$word[sent.wordlist$posneg == "neg"]

grams_sent <- grams %>%
	separate(gram, c("word1","word2"), sep=" ") %>%
	filter(word1 %in% listofwords | word2 %in% listofwords)  %>%
	filter(underrepwomen %in% "bhwomen")

grams_sent_grouped <- grams_sent %>%
	unite(gram, word1, word2, sep=" ") %>%
	count(gram) %>%
	top_n(7, n) %>%
	#mutate(gram = fct_reorder(gram, n, .desc = FALSE)) %>%
	arrange(desc(n))

head(grams_sent_grouped, 10)

grams_sent_grouped$gram <- factor(grams_sent_grouped$gram, levels = grams_sent_grouped$gram[order(grams_sent_grouped$n)])
	levels(grams_sent_grouped$gram)
	
ggplot(grams_sent_grouped, aes(x=gram, y = n)) + geom_bar(stat="identity") + coord_flip() + theme(text = element_text(size=20)) + labs(y="negative bigram", x = "frequency")
#+ facet_wrap(~underrepwomen, ncol=2, scales ="free") 