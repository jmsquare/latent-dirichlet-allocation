
########################################
#          TOPIC MODELLING             #
########################################
install.packages("topicmodels")
install.packages("SnowballC")
library(SnowballC)
library(topicmodels)

path <- "C:/Users/JM/Documents/project"
quest <- read.csv("C:/Users/JM/Documents/questionnaire.csv",sep=";")

q1 <- quest["question1"]
question <- q1[!is.na(q1[,1]),1]
my.stopwords <- c()

###############################
#Generation of the DocumentTerm Matrix
###############################

DTM <- DocumentTermMatrix(generateCorpus(question,my.stopwords),control=list(tokenize=NGramTokenizer,stemming = function(x) wordStem(x, language = "french")))
rowTotals <- apply(DTM , 1, sum)
DTM.new   <- DTM[rowTotals> 0, ]    

################
#Latent Dirichlet allocation
###########################

lda <- LDA(DTM.new,7)
terms(lda,8)
TermDocumentMatrix()

######################
#Display using wordcloud
##########################
wordcloud.generate=function(corpus,min.freq=3,minWordLength=3,dictionnaire){
  doc.m = TermDocumentMatrix(corpus, control = list(minWordLength = 3,tokenize=NGramTokenizer))
  dm = as.matrix(doc.m)
  # calculate the frequency of words
  v = sort(rowSums(dm), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  d <- d[d$word %in% dict,]
  # Create palette
  pal <- brewer.pal(11, "Dark2")
  #Generate the wordcloud
  wc=wordcloud(d$word, 
               d$freq, 
               min.freq=5, 
               scale = c(2, 0.5), 
               colors = pal,
               max.words = 100,
               rot.per = 0.2,
               random.order = FALSE)
  #Display the wordcloud
  wc
}