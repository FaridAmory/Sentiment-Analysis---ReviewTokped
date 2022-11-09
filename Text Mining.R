#Install Library yang dibutuhkan

install.packages("NLP")
install.packages("tm") # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # generate word cloud
install.packages("RColorBrewer") # color palettes
install.packages("ggplot2") # plot
install.packages("plyr")
install.packages("stringr")
install.packages("dplyr")



#Load 
library("ggplot2")
library("NLP")
library("tm") # 
library("SnowballC") 
library("wordcloud") 
library("RColorBrewer") 
library("plyr")
library("stringr")
library("dplyr")

#Read csv
filePath <- "tokopedia review 2018-2022.csv"
text <- readLines(filePath)

#Muat data sebagai corpus
#collection of text document over which would apply text mining to derive inferences.  
docs <- Corpus(VectorSource(text))
inspect(docs)

#Transformasi teks dengan fungsi tm map()
#karakter khusus -> spasi
toSpace <- content_transformer(function(x , pattern ) gsub(pattern, " ", x))
docs<-tm_map(docs, toSpace, "/")
docs<-tm_map(docs, toSpace, "@")
docs<-tm_map(docs, toSpace, "\\|")


#Start Data Cleaning
#Cleaning text 
#(tolower) (punctuation)
#(removenumber) (stripwhitespace)

#Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

#Remove number
docs <- tm_map(docs, removeNumbers)

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#Remove extra white spaces
docs <- tm_map(docs, stripWhitespace)

#check Data
View(docs)

#Stop Words

#Remove common words (no meaning)
myStopwords = readLines("id.stopwords.02.01.2016.txt")
myStopwordsEnglish = readLines("english.txt")

#Remove stop words from corpus
docs <- tm_map(docs, removeWords, myStopwords)
docs <- tm_map(docs, removeWords, myStopwordsEnglish)


#Tambahan stop words
docs <- tm_map(docs, removeWords, c("aplikasi", "app", "tokped", "tokopedia", "internet", "use", "like", "time", "hai", "alihalih"))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <-sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v), freq= v)
head(d, 50)

View(d)

head(d,25)


#Generate the word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


#Save cleaning text
dataframe <-data.frame(text=unlist(sapply(docs, '[')), stringsAsFactors = F)
write.csv(dataframe, "Hasil_Cleaning_Text.csv")

#Analisa
#Frequent term dalam term document matrix
findFreqTerms(dtm,lowfreq = 5)

#Find Asosiasi dari kata terkait
findAssocs(dtm, terms =  "buruk", corlimit = 0.3)

#Barplot  frekunsi kata
barplot(d[1:25,]$freq, las =3, names.arg = d[1:25,]$word, 
        col = "lightblue", main = "Most Frequent Words", 
        ylab = "Frequent Words")

barplot(d[1:25,]$freq, las =3, names.arg = d[1:25,]$word, cex.axis = 1,cex.names = 0.8, col = topo.colors(25),
        ylab = "Frequent Words")

#Sentiment Analysis
#Memilah kalimat positif, negatif, dan netral

kalimat2 <-read.csv("Hasil_Cleaning_Text.csv", header = TRUE)

#Proses Skoring
#proses loading file kata dan kalimat positif dan negatif menjadi variable

positif <- scan("positive.txt", what = "character", comment.char = ";")
negatif <- scan("negative.txt", what = "character", comment.char = ";")

#Penambahan kata positif dan negatif

kata.positif = c(positif, "baik", "luar biasa")
kata.negatif = c(negatif, "jelek","buruk", "buggy")

#Algoritma penilaian Sentimen Analysis

score.sentiment = function(kalimat2, kata.positif, kata.negatif, .progress= 'none')
{
  require(plyr)
  require(stringr)
  
  #vektor kalimat, plyr akan menangani daftar(per huruf)
  score = laply(kalimat2, function(kalimat, kata.positif, kata.negatif) {
  
  #Bersihkan kalimat dengan pengganti regular expression (regex)  
  kalimat = gsub ('[[:punct:]]', '', kalimat)
  kalimat = gsub ('[[:cntrl:]]', '', kalimat)
  kalimat = gsub ('\\d+', '', kalimat)
  
  #Ubah kehuruf kecil
  kalimat = tolower(kalimat)
  
  #dibagi menjadi kata-kata. str split ada didalm paket stringr
  list.kata = str_split(kalimat, '\\s+')
  
  kata2 = unlist (list.kata)
  
  #bandingkan dengan kamus
  positif.matches = match(kata2, kata.positif)
  negatif.matches = match(kata2, kata.negatif)
  
  #mengembalikan nilai
  positif.matches = !is.na(positif.matches)
  negatif.matches = !is.na(negatif.matches)
  
  score = sum(positif.matches) - (sum(negatif.matches))
  
  return(score)
 }, kata.positif, kata.negatif, .progress=.progress )
  
  score.df = data.frame(score=score, text= kalimat2)
}

hasil = score.sentiment(kalimat2$text, kata.positif, kata.negatif)
View(hasil)
  

#Convert Score to Sentiment
hasil$klasifikasi <- ifelse(hasil$score<0, "Negatif", ifelse(hasil$score==0, "Netral", "Positif"))
hasil$klasifikasi
View(hasil)

#Exchange row sequence
data <- hasil[c(3,1,2)]
View(data)
write.csv(data,file = "hasil_sentiment_pos_net_neg.csv")
  
#Remove null data  
data[data==""]<-NA
data<-data[complete.cases(data),]
write.csv(data,file = "Hasil_sentiment_pos_net_neg.csv")
View(data)

#plot perbandingan score  
qplot(data = data, klasifikasi, fill = score, bins = 30)

#plot sebaran score
qplot(data = data, score, fill = klasifikasi, bins = 30)
  
#Split word  
#Kalimat positif

data_positif = filter(data, klasifikasi == "Positif")
View(data_positif)
data_positif_text = data_positif["text"]
View(data_positif_text)
write.csv(data_positif_text, file = "Hasil_sentiment_positif.csv")

#Kalimat netral

data_netral = filter(data, klasifikasi == "Netral")
View(data_netral)
data_netral_text = data_netral["text"]
View(data_netral_text)
write.csv(data_netral_text, file = "Hasil_sentiment_netral.csv")
  

#Kalimat negatif

data_negatif = filter(data, klasifikasi == "Negatif")
View(data_negatif)
data_negatif_text = data_negatif["text"]
View(data_negatif_text)
write.csv(data_negatif_text, file = "Hasil_sentiment_negatif.csv")

#Analisa Ulasan positif

docs<-readLines("Hasil_sentiment_positif.csv")
docs<- Corpus(VectorSource(docs))

dtm <- TermDocumentMatrix(docs)
m <-as.matrix(dtm)
v<-sort(rowSums(m), decreasing = TRUE)
d<-data.frame(word = names(v), freq=v)

head(d,25)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 5)

#View Assosiasi
findAssocs(dtm, terms = "terbaru", corlimit = 0.3)
View(findAssocs(dtm, terms = "terbaru", corlimit = 0.3))

#Bar plot positif
k<-barplot(d[1:20,] $freq, las = 2, names.arg = d[1:20,] $word, cex.axis = 1,cex.names = 0.8, col = topo.colors(25), 
          main = "20 Most Frequent Words",
          ylab = "Frequent Words")

#Analisa Ulasan negatif

docs<-readLines("Hasil_sentiment_negatif.csv")
docs<- Corpus(VectorSource(docs))

dtm <- TermDocumentMatrix(docs)
m <-as.matrix(dtm)
v<-sort(rowSums(m), decreasing = TRUE)
d<-data.frame(word = names(v), freq=v)

head(d,25)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 5)

#View Assosiasi
findAssocs(dtm, terms = "buruk", corlimit = 0.3)
View(findAssocs(dtm, terms = "buruk", corlimit = 0.3))

#Bar plot positif
k<-barplot(d[1:20,] $freq, las = 2, names.arg = d[1:20,] $word, cex.axis = 1,cex.names = 0.8, col = topo.colors(25), 
           main = "20 Most Frequent Words",
           ylab = "Frequent Words")


#Analisa Ulasan netral

docs<-readLines("Hasil_sentiment_netral.csv")
docs<- Corpus(VectorSource(docs))

dtm <- TermDocumentMatrix(docs)
m <-as.matrix(dtm)
v<-sort(rowSums(m), decreasing = TRUE)
d<-data.frame(word = names(v), freq=v)

head(d,25)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 5)

#View Assosiasi
findAssocs(dtm, terms = "transaksi", corlimit = 0.3)
View(findAssocs(dtm, terms = "transaksi", corlimit = 0.3))

#Bar plot positif
k<-barplot(d[1:20,] $freq, las = 2, names.arg = d[1:20,] $word, cex.axis = 1,cex.names = 0.8, col = topo.colors(25), 
           main = "20 Most Frequent Words",
           ylab = "Frequent Words")

  