setwd("C:/Users/setup/Downloads/ML-Email-Code-master/ML-Email-Code-master/03-SPAM_Classification/code")
library(tm)
library(ggplot2)
spam.path <- "data/spam/"
spam2.path <- "data/spam_2/"
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"
get.msg <- function(path) {
  con <- file(path, open="rb", encoding="latin1")
  text <- readLines(con)
  # The message always begins after the first full line break
  msg <- text[seq(which(text=="")[1]+1,length(text),length.out=1)]
  close(con)
  return(paste(msg, collapse="/n"))
}
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!= "cmds")]
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path,p,sep ="/")))
head(all.spam)
get.tdm <- function(doc.vec) {
  control <- list(stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, 
                  minDocFreq = 2)
  doc.corpus <- Corpus(VectorSource(doc.vec))
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}
spam.tdm <- get.tdm(all.spam)
stopwords=TRUE
stopwords()
removePunctuation=TRUE
removeNumbers=TRUE
minDocFreq=2
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)), stringsAsFactors = FALSE)
names(spam.df) <- c("term","frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)
spam.occurrence <- sapply(1:nrow(spam.matrix), function(i) { length(which(spam.matrix[i,]>0))/ncol(spam.matrix) })
spam.density <- spam.df$frequency / sum(spam.df$frequency)
spam.df <- transform(spam.df, density = spam.density, occurrence = spam.occurrence)
as.matrix
rowSums
head(spam.df[with(spam.df, order(-occurrence)),])
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)], function(p) get.msg(paste(easyham.path,p,sep = "")))

easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts), as.numeric(easyham.counts)), stringsAsFactors = FALSE)
names(easyham.df) <- c("term","frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix), function(i) {length(which(easyham.matrix[i,]>0))/ncol(easyham.matrix)})
easyham.density <- easyham.df$frequency/sum(easyham.df$frequency)

easyham.df <- transform(easyham.df, density = easyham.density, occurrence = easyham.occurrence)
head(easyham.df[with(easyham.df, order(-occurrence)),])
classify.email <- function(path, training.df, prior = 0.5, c = 1e-6) {
  # Here, we use many of the support functions to get the
  # email text data in a workable format
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)
  # Now, we just perform the naive Bayes calculation
  if(length(msg.match) < 1) {
    return(prior*c^(length(msg.freq)))
  }
  else {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior*prod(match.probs)*c^(length(msg.freq)-length(msg.match)))
  }
}
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

hardham.spamtest <- sapply(hardham.docs, function(p) classify.email(paste(hardham.path,p,sep = ""), 
                                                                    training.df = spam.df))

hardham.hamtest <- sapply(hardham.docs, function(p) classify.email(paste(hardham.path,p,sep = ""), 
                                                                   training.df = easyham.df))

hardham.res <- ifelse(hardham.spamtest>hardham.hamtest,TRUE,FALSE)
summary(hardham.res)
spam.classifier <- function(path) {
  pr.spam <- classify.email(path, spam.df)
  pr.ham <- classify.email(path, easyham.df)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

# Classify them all!
easyham2.class <- suppressWarnings(lapply(easyham2.docs, function(p) spam.classifier(paste(easyham2.path,p,sep = ""))))
hardham2.class <- suppressWarnings(lapply(hardham2.docs, function(p) spam.classifier(paste(hardham2.path,p,sep = ""))))
spam2.class <- suppressWarnings(lapply(spam2.docs, function(p) spam.classifier(paste(spam2.path,p,sep = ""))))

# Create a single, final, data frame with all of the classification data in it
easyham2.matrix <- do.call(rbind, easyham2.class)
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")

hardham2.matrix <- do.call(rbind, hardham2.class)
hardham2.final <- cbind(hardham2.matrix, "HARDHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

class.matrix <- rbind(easyham2.final, hardham2.final, spam2.final)
class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)
head(class.df)

get.results <- function(bool.vector) {
  results <- c(length(bool.vector[which(bool.vector == FALSE)])/length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)])/length(bool.vector))
  return(results)
}
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, hardham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)
spam.classifier <- function(path) {
  pr.spam <- classify.email(path, spam.df, prior=0.2)
  pr.ham <- classify.email(path, easyham.df, prior=0.8)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, hardham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)
write.csv(spam.df, "data/spam_df.csv", row.names = FALSE)
write.csv(easyham.df, "data/easyham_df.csv", row.names = FALSE)
