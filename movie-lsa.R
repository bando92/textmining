install.packages("lsa")
install.packages("tm")
install.packages("RWeka")
library(tm)
library(lsa)
library(RWeka)
movie_rev <- read.arff("movie_review.arff")
fix( movie_rev ) #stampa a video il file -> da mettere nel foglio
rev <- movie_rev$review_text
rating <- movie_rev$review_class
#Analogia con string to word vector in weka -> devo creare la term document matrix
corpus <- Corpus(VectorSource(rev))
stoplist <- readLines("stopwords.txt")
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, PlainTextDocument)
tdmc <- TermDocumentMatrix(corpus)
tdmc #mostra info sulla tdmc
tdms <- removeSparseTerms( tdmc, 0.99 )
tdms
words <- rownames(tdms)
length(words)#quante sono le parole
tdm <- as.matrix(tdms)
tdmle <- lw_logtf(tdm)*(1-entropy(tdm))

# funzione che calcola la norma di un vettore
norm_vec <- function(x) sqrt(sum(x^2))
# calcola la norma di ogni termine della matrice termini-doc
norma_termini <- apply(tdmle, 1, norm_vec)

#lsa
lsar <- lsa(tdmle)
length(lsar$sk)#ha calcolato 611 autovalori
# grafico degli autovalori determinati in LSA mediante scomposizione SVD
plot( 1:611, lsar$sk, type="b" )
# calcolo delle 2 matrici di similarita' semantica: termini e documenti
tls <- lsar$tk %*% diag( lsar$sk )
dls <- lsar$dk %*% diag( lsar$sk )

# calcola la norma di ogni termine 
norma_termini_lsa <- apply(tls, 1, norm_vec)
# aggiunge la colonna delle norme alla matrice tls
tls_norma_termini = cbind(tls, norma_termini_lsa)
# subset della matrice dei termini con norma > 1.25 
tls_ridotto <- subset(tls_norma_termini, norma_termini_lsa > 1.5)[,-70]

# rappresentazione grafica dei termini e documenti dimensioni latenti 1 e 2
plot( tls, pch=20 )
text( tls, labels=words, cex=0.8, pos=1 )

# rappresentazione grafica dei termini ridotti e doc variabili latenti 1 e 2
plot( tls_ridotto, pch=21, cex=0.3 )
text( tls_ridotto, labels=words, cex=0.5, pos=1 )

# rappresentazione grafica dei termini e doc variabili latenti 2 e 3
plot( tls[,-1], pch=20 ) # -1 elimina la prima variabile latente
text( tls[,-1], labels=words, cex=0.8, pos=1 )
points( 0, 0, pch=20, cex=2, col="blue" ) # traccia il punto di origine del piano

plot( dls, pch=18, cex=0.8 )
plot( dls[,-1], pch=18, cex=0.8 )

# negative in rosso, positive in blue
levels(rating)
rating_color <- function(x) switch(x, "red", "green")
ratingcols <- sapply( rating, rating_color )

plot( dls[,-1], pch=10, cex=0.3, col=ratingcols )

# definizione e applicazione della funzione di normalizzazione dei vettori termini e documenti
# infatti i vettori tls e dls non sono normali poiche' ottenuti dal prodotto con sk (matrice autovalori) 
normrows <- function(x) x / apply(x,1,norm,"2")
tlsn <- normrows(tls)
dlsn <- normrows(dls)

#prova mia -> normrows a tls ridotto
tlsrn <- normrows(tls_ridotto)

# nuovo grafico con vettori termini e doc normalizzati
plot( dlsn[,-1], pch=20, cex=0.6, col=ratingcols )#cex era 0.8 non 0.4
text( tlsn[,-1], labels=words, cex=0.6 )
#prova mia qui sotto
plot( dlsn[,-1], pch=20, cex=0.6, col=ratingcols )
text( tlsrn[,-1], labels=words, cex=0.6 )

points( 0, 0, pch=20, cex=2, col="blue" )

plot.vs.neg <- table( tdm["script",]>0, rating=="neg")
dimnames(plot.vs.neg) <- list(script = c("No", "Si"), neg = c("No", "Si"))
chisqtest = chisq.test( plot.vs.neg, correct=FALSE )
chisqtest
chisqtest$expected
chisqtest$observed

associate(tls[,2:3], "script", threshold=0.99)

played_script.vs.neg <- table( tdm["script",]>0 & tdm["played",]>0, rating=="neg")
dimnames(played_script.vs.neg) <- list(played_script = c("No", "Si"), neg = c("No", "Si"))
chisq.test( played_script.vs.neg, correct=FALSE )
chisqtest = chisq.test( played_script.vs.neg, correct=FALSE )
chisqtest
chisqtest$expected
chisqtest$observed

psq <- "script played"
psv <- query( psq, words )
psle <- lw_logtf(psv) * ( 1-entropy(tdm) )
psls <- t(psle) %*% lsar$tk

# grafico dei termini e documenti nello spazio LSA relativo alle dimensioni 2 e 3
plot( dlsn[,-1], pch=20, cex=0.6, col=ratingcols )
text( tlsn[,-1], labels=words, cex=0.6 )
points( 0, 0, pch=20, cex=1, col="blue" )  # visualizzazione dell'origine 0,0 nel grafico
pslsn <- normrows( psls ) # normalizzazione vettore query
points(pslsn[2],pslsn[3],cex=1,pch=18,col="blue")# visualizzazione della query nel grafico

cosines <- function(X, Q) apply( X, 1, cosine, as.vector(Q) )
# definizione funzione che restituisce gli indici (i.e. posizioni) degli N valori più elevati in X 
top <- function(X, N) order( X, decreasing=TRUE )[1:N]

# indice dei 10 documenti che risultano semanticamente piu' simili alla query "script played"
# considerando solo la dimensione 2 e 3
top( cosines( dls[,2:3], psls[2:3] ), 10 )
# elenco dei 10 documenti sopra menzionati 
rev[top(cosines(dls[,2:3], psls[2:3]), 10)]

fordiff <- function(x) x[2:length(x)] - x[1:(length(x)-1)]
skd <- fordiff(lsar$sk)
skdd <- fordiff(skd)
skcurv <- skdd[1:20] / (1+(skd[1:20])^2)^1.5
plot( 1:20, skcurv, type="b" )

rev[top(cosines(dls[,1:4], psls[1:4]), 5)]

table(rating[top(cosines(dls[,1:4],psls[1:4]),10)])
table( rating )
table( rating[ top( cosines( dls[,1:4], psls[1:4] ), 1000 ) ] )
psq.vs.neg <- table(1:nrow(dls) %in% top(cosines(dls[,1:4], psls[1:4]), 1000), rating=="neg" )
dimnames(psq.vs.neg) <- list(ps = c("No", "Si"), rat = c("No", "Si"))
psq.vs.neg

chisq.test( psq.vs.neg, correct=FALSE )
chisqtest = chisq.test( psq.vs.neg, correct=FALSE )
chisqtest
chisqtest$expected
chisqtest$observed
