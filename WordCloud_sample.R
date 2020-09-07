library(extrafont)
font_import()

par(family="NanumGothic")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(dplyr)
library(wordcloud2)


## Part 1. 영문 초록

setwd("/Users/jaesolshin/Google/R_Jaesol")

# Read the text file from internet
file <- "list_combined_josa.csv"
data <- read.csv(file, stringsAsFactors = FALSE, fileEncoding = "euc-kr")
names(data)

txt <- data$영문초록 
head(txt)

txt <- paste(txt, collapse=" ")
txt <- str_to_lower(txt)
# txt <- str_replace_all(txt,"effects","effect")
# txt <- str_replace_all(txt,"costs","cost")
# txt <- str_replace_all(txt,"markets","market")
# txt <- str_replace_all(txt,"factors","factor")
# txt <- str_replace_all(txt,"hospitals","hospital")
# txt <- str_replace_all(txt,"increases","increase")
# txt <- str_replace_all(txt,"investors","investor")
# txt <- str_replace_all(txt,"models","model")
# txt <- str_replace_all(txt,"policies","policy")
# txt <- str_replace_all(txt,"prices","price")
# txt <- str_replace_all(txt,"rates","rate")
# txt <- str_replace_all(txt,"returns","return")
# txt <- str_replace_all(txt,"significantly","significant")
# txt <- str_replace_all(txt,"trades","trade")
# txt <- str_replace_all(txt,"korean","korea")

docs <- Corpus(VectorSource(txt))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

# Remove your own stop word
docs <- tm_map(docs, removeWords, c("the","and","that","this","with","use","are","from","for","find","between","result","using","paper","also","elsevier","may","whether","show","examines","can","examine","two","found","investigates","suggest","first","based","among","one","however","study","studies")) 

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word=names(v), freq=v)
head(d, 150)

png(filename="abstract_eng1.png", width=1280, height=1280, units="px")

set.seed(1234)
wordcloud(words=d$word, freq=d$freq, min.freq=1,
          max.words=100, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))

dev.off()



## wordcloud2를 이용한 better graph
library(webshot)
library("htmlwidgets")
# webshot::install_phantomjs()

d.select<-d[1:100,]

set.seed(1234)
wordcloud2(d.select, fontFamily="Malgun Gothic", size = 0.5, minRotation=0, maxRotation=0)
my_graph <- wordcloud2(d.select, fontFamily="Malgun Gothic", size = 0.5, minRotation=0, maxRotation=0)

# save it in html
saveWidget(my_graph, "abstract_eng2.html", selfcontained=F)

# and in png or pdf
webshot("abstract_eng2.html", "abstract_eng2.png", delay=10, vwidth=480, vheight=480)





#########################
## Part 2. 국문 초록
#########################

useSejongDic()

# Read the text file from internet
file <- "list_combined_josa.csv"
# file <- "list_combined_econ.csv"
data <- read.csv(file,stringsAsFactors = FALSE, fileEncoding = "euc-kr")
names(data)

txt <- data$국문초록 
head(txt)

# txt<-data$abstract %>% str_replace_all("\n"," ")
# txt<-paste(txt,collapse=" ")
# txt<-str_replace_all(txt,"&#985172","")
# txt<-str_replace_all(txt,"&#985173","")
# txt<-str_replace_all(txt,";","")

# Encoding(txt)<-"UTF-8"
doc_temp <- sapply(txt, extractNoun, USE.NAMES = F)

doc_temp <- Filter(function(x) {nchar(x)>=2}, doc_temp)

doc_temp <- unlist(doc_temp)

head(doc_temp)

docs <- Corpus(VectorSource(doc_temp))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Remove your own stop word
docs <- tm_map(docs, removeWords, c("경우","가지","율의","주요","첫째","하게","하기","이후","고려","반면","들이","이상","cds","var","둘째","논문","결과","분석","연구"))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d %>% mutate(length=str_length(word)) # word는 factor변수라서, stringr::str_length를 써야 함.
d <- d %>% filter(length>=2)
head(d, 150)


png(filename="abstract_kor1.png",width=1280,height=1280,units="px")

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

dev.off()
class(d)


# wordcloud2를 이용한 better graph
library(webshot)
library("htmlwidgets")
# webshot::install_phantomjs()

d.select<-d[1:100,]

set.seed(1234)
wordcloud2(d.select,fontFamily="Malgun Gothic", size = 0.5, minRotation=0, maxRotation=0)
my_graph <- wordcloud2(d.select,fontFamily="Malgun Gothic", size = 0.5, minRotation=0, maxRotation=0)

# save it in html
saveWidget(my_graph,"abstract_kor2.html",selfcontained = F)

# and in png or pdf
webshot("abstract_kor2.html","abstract_kor2.png", delay=10, vwidth = 480, vheight=480)

