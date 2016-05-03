install.packages('tm', dependencies=T)
install.packages('RWeka')
install.packages('ggplot2', dependencies=T)
install.packages('XML', dependencies=T)
install.packages('plyr', dependencies=T)
install.packages('doBy', dependencies=T)
install.packages('RJSONIO', dependencies=T)

library(RJSONIO)
library(tm)
library(RWeka)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # in score verrà salvato l'output della funzione seguente. Ovvero una lista dei punteggi di ogni sentence in sentences
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # lo statement seguente estrae solo caratteri alfabetici eliminando anche i caratteri non ascii, tipicamente presenti nei tweets
    sentence = gsub('[^A-z ]','', sentence)
    # conversione in lowercase:
    sentence = tolower(sentence)
    # split in parole. str_split è nel package stringr
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    # Confrontiamo le parole dei tweet con i dizionari dei termini positivi e negativi
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() restituisce la posizione del termine abbinato o NA
    # noi vogliamo semplicemente TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # Semplicemente, i valori vero/falso saranno trattati come 1/0
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences) # la funzione deve ritornare, per ogni sentence in sentences, il testo della frase e il punteggio ottenuto
  return(scores.df) 
}

hu.liu.pos=scan('positive-words.txt',what='character',comment.char=';')
hu.liu.neg=scan('negative-words.txt',what='character',comment.char=';')
pos.words=c(hu.liu.pos,'upgrade')
neg.words=c(hu.liu.neg,'wtf','wait','waiting','epicfail')

review = read.csv("movie_review_mod.csv")
class(review)
colnames(review)
colnames(review)[2]<-'class'
colnames(review)[1]<-'text'
colnames(review)
review.text=as.character(review$text)
review.class=as.character(review$class)
length(review$text)
result=score.sentiment(head(review.text,5),pos.words,neg.words)
result

review.score=score.sentiment(review.text,pos.words,neg.words)
review.df=merge(review.score,review,by=c('text','text'))
colnames(review.df)
review.df
review.res<-(review.df$score>=0 & review.df$class=='pos')|(review.df$score<0 & review.df$class=='neg')
count(review.res)
count(review.res)$freq[2]/length(review.res)#accuratezza
review.tp<-count((review.df$score >=0 & review.df$class=='pos'))$freq[2]
review.fp<-count((review.df$score < 0 & review.df$class=='pos'))$freq[2]
review.tn<-count((review.df$score < 0 & review.df$class=='neg'))$freq[2]
review.fn<-count((review.df$score >=0 & review.df$class=='neg'))$freq[2]
pos.p=review.tp / (review.tp + review.fp)
pos.r=review.tp / (review.tp + review.fn)
neg.p=review.tn / (review.tn + review.fn)
neg.r=review.tn / (review.tn + review.fp)
pos.p
pos.r
neg.p
neg.r