########################
###Delta Project Code###
###Authors:            #
#Quinn Anderson        #  
#John Croft            #
#Brandon Stringfield   #
########################

################################################
###Installing all packages needed for Project###
################################################
install.packages('tm')
library(tm)
install.packages('plyr')
library(plyr)
install.packages('lattice')
library(lattice)
install.packages("wordcloud")
library(wordcloud)
install.packages("ggplot2")
library(ggplot2)
install.packages("maps") #may not need packages below here. 
library(maps)
install.packages('mapproj')
library(mapproj)
install.packages("gplots", dependencies = TRUE)
library(gplots)
install.packages("RColorBrewer", dependencies = TRUE)
library(RColorBrewer)
install.packages("tau")
library(tau)
install.packages("NLP")
library(NLP)

###########################
#Setting working directing
#Set appropriate file path
#Reading in Data
###########################

setwd("C:/Users/JohnM/Google Drive/Delta Project")#John
comments <-  read.txt ("DSC.csv", stringsAsFactors=FALSE)

###########################
#replacing missing Regions#
###########################
attach(comments)
comments$Region[Station == "BOS"] <- "East"
comments$Region[Station == "ATL"] <- "ATL"
comments$Region[Station == "HNL"] <- "West"
comments$Region[Station == "SCL"] <- "ATL"

################
#Creating Tiers#
################
##"ATL-A CPT" is actually "ATL-CPT"
##ATL-A17 = ATLA17 & -B10 = B1 & -B25 = B25 & -D12 = D12 & -D27 = D27
##BOS-A6 = BOS-A7 & -A18 = -SAT
##CVG = CVG-B14
##DFW = DFW10
##DTW-CPT = DTW-CEN
##HNL = HNL-DHC
##JFK-T2 = JFKCRT2
##LGA-D = LGA
##ORD-E = ORD
##MSP-F/G = MSG-FG
###############################################################################
comments$Tier[Crc.Loc.Cd=="ATLE15"|Crc.Loc.Cd=="JFK-T4"|Crc.Loc.Cd=="ATL-F"|
                Crc.Loc.Cd=="DTW-CEN"|Crc.Loc.Cd=="NRT-2"|Crc.Loc.Cd=="NRT-1"|
                Crc.Loc.Cd=="LAX"|Crc.Loc.Cd=="SEA"|Crc.Loc.Cd=="MSP-FG"|
                Crc.Loc.Cd=="LGA"|Crc.Loc.Cd=="LGA-C" | Crc.Loc.Cd=="ATL-E"] <- "Tier 1"


comments$Tier[Crc.Loc.Cd=="DTW-S"|Crc.Loc.Cd=="DTW-N"|Crc.Loc.Cd=="JFKCRT2"|
                Crc.Loc.Cd=="MSP-C"|Crc.Loc.Cd=="HNL"|Crc.Loc.Cd=="SLC"|
                Crc.Loc.Cd=="BOS-A6"|Crc.Loc.Cd=="EWR"|Crc.Loc.Cd=="PDX"|
                Crc.Loc.Cd=="PHL"|Crc.Loc.Cd=="DTW-C"|Crc.Loc.Cd=="CVG-B14"|
                Crc.Loc.Cd=="BOS-SAT"|Crc.Loc.Cd=="MCO"|Crc.Loc.Cd=="BNA"|
                Crc.Loc.Cd=="TPA"|Crc.Loc.Cd=="RDU"|Crc.Loc.Cd=="MKE"|
                Crc.Loc.Cd=="JAX"|Crc.Loc.Cd=="ATLA17"|Crc.Loc.Cd=="ATL-CPT"|
                Crc.Loc.Cd=="HNL-DHC"] <- "Tier 2"

comments$Tier[Crc.Loc.Cd=="ATL-C"|Crc.Loc.Cd=="ATL-T"|Crc.Loc.Cd=="ORD-E"|
                Crc.Loc.Cd=="DCA"|Crc.Loc.Cd=="FLL"|Crc.Loc.Cd=="MIA"|
                Crc.Loc.Cd=="DFW10"|Crc.Loc.Cd=="SFO"|Crc.Loc.Cd=="PBI"|
                Crc.Loc.Cd=="MSY"|Crc.Loc.Cd=="IND"|Crc.Loc.Cd=="SAN"|
                Crc.Loc.Cd=="MEM"|Crc.Loc.Cd=="ATLD12"|Crc.Loc.Cd=="ATLB10"|
                Crc.Loc.Cd=="ATLB25"|Crc.Loc.Cd=="ATLD27"| Crc.Loc.Cd== "SCL"] <- "Tier 3"

#################
#Creating Groups#
#################
comments$Group[Crc.Loc.Cd=="JFK-T4"|Crc.Loc.Cd=="DTW-CEN"|Crc.Loc.Cd=="MSP-FG"|
                 Crc.Loc.Cd=="ATLA17"|Crc.Loc.Cd=="LAX"|Crc.Loc.Cd=="ATLB25"|
                 Crc.Loc.Cd=="ATLE15"|Crc.Loc.Cd=="SLC"|Crc.Loc.Cd=="ATLB10"|
                 Crc.Loc.Cd=="LGA"|Crc.Loc.Cd=="ATL-F"|Crc.Loc.Cd=="SFO"|
                 Crc.Loc.Cd=="ATL-CPT"|Crc.Loc.Cd=="ATL-C"|Crc.Loc.Cd=="ATL-T"|
                 Crc.Loc.Cd=="ATLD27"|Crc.Loc.Cd=="SEA"|Crc.Loc.Cd=="ATLD12"|
                 Crc.Loc.Cd=="SCL"| Crc.Loc.Cd=="ATL-E"] <- "Group 1"

comments$Group[Crc.Loc.Cd=="MSP-C"|Crc.Loc.Cd=="ORD-E"|Crc.Loc.Cd=="MCO"|
                 Crc.Loc.Cd=="LGA-C"|Crc.Loc.Cd=="NRT-2"|Crc.Loc.Cd=="JFKCRT2"|
                 Crc.Loc.Cd=="DTW-C"|Crc.Loc.Cd=="DCA"|Crc.Loc.Cd=="CVG-B14"|
                 Crc.Loc.Cd=="TPA"|Crc.Loc.Cd=="NRT-1"] <- "Group 2"

comments$Group[Crc.Loc.Cd=="DTW-S"|Crc.Loc.Cd=="DTW-N"|Crc.Loc.Cd=="DFW10"|
                 Crc.Loc.Cd=="FLL"|Crc.Loc.Cd=="EWR"|Crc.Loc.Cd=="BOS-SAT"|
                 Crc.Loc.Cd=="MIA"|Crc.Loc.Cd=="RDU"|Crc.Loc.Cd=="BOS-A6"|
                 Crc.Loc.Cd=="BNA"|Crc.Loc.Cd=="PHL"|Crc.Loc.Cd=="PDX"|
                 Crc.Loc.Cd=="HNL-DHC"|Crc.Loc.Cd=="MKE"|Crc.Loc.Cd=="IND"|
                 Crc.Loc.Cd=="MSY"|Crc.Loc.Cd=="JAX"|Crc.Loc.Cd=="PBI"|
                 Crc.Loc.Cd=="SAN"|Crc.Loc.Cd=="MEM"| Crc.Loc.Cd=="HNL"|
                 Crc.Loc.Cd=="SCL"] <- "Group 3"


################################
###Color Coding by Categories###
################################

products<-c("food", "selection", "drink", "coffee","breakfast", "options", 
            "variety","choices", "salad", "soup", "chicken", "lunch", "dinner")

service<-c("bar","staff","service","desk","bartender","front","friendly", "wait")

facility<-c("club","sky", "comfortable", "clean", "place", "airport", "lounges", 
            "delta","lounge","club.","clubs","wifi","experience","room","crowded","skyclub",
            "seating","space","area","chairs","terminal")

policy<-c("wife","guest","pay","free","allow","keep", "membership")

#####################################
###Subsetting data files by Region###
#####################################
comments_atl <- subset(comments, Region=="ATL")
comments_east <- subset(comments, Region == "East")
comments_west <- subset(comments, Region == "West")
comments_mw <- subset(comments, Region == "Mid West")

#################################
#RUNNING TM PROGRAM FOR TOP WORD#
#################################

#grabbing text from comments
comments_txt <- paste(comments$Skc.Comments, collapse=" ") 

#setting up source and corpus
comments_source <- VectorSource(comments_txt)
corpus <- Corpus(comments_source)

#Corpus clean up
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

###Need to discuss with Delta about what specific words to remove
#e.g. 'delta' 'sky' 'club'
#corpus <- tm_map(corpus, removeWords, "club")

#Creating document term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

#observing high level frequencies.
freq <-colSums(dtm2)
freq <-sort(freq, decreasing=TRUE)
print(freq[1:100]) #printing top 100 words an counts.

###############################
###PLOTTING THE WORD CLOUDs####
###############################

##########################
###Word Cloud - OVERALL###
##########################
words <- names(freq)
pal2 <- c()

for (wrd in words[1:100]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}
wordcloud(words[1:100], freq[1:100], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

########################################################################################
################################Over All 2-word Phrases#################################
########################################################################################

t<- paste(comments$Skc.Comments, collapse=" ")

## Split into words:
w <- strsplit(t, " ", fixed = TRUE)[[1L]]
## Word tri-grams:
w2<- ngrams(w, 2L) #Break out sets of n-grams
str(w2)

## Word tri-grams pasted together:
w<- c(vapply(ngrams(w, 2L), paste, "", collapse = " ")) #combine n-gram sets back into 
str(w)
head(w)
w.freq<- count(w)
head(w.freq)

w.freq<-w.freq[order(w.freq$freq, decreasing=T),] 

print(w.freq[1:500,]) #Prints top bi-gram combos

#############################
###Word Cloud - ATL REGION###
#############################
comments_text_atl <- paste(comments_atl$Skc.Comments, collapse = " ")
comments_source_atl <- VectorSource(comments_text_atl)

corpus1 <- Corpus(comments_source_atl)
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 <- tm_map(corpus1, stripWhitespace)
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))
#corpus1 <- tm_map(corpus1, removeWords, "club")

dtm_atl <- DocumentTermMatrix(corpus1)
dtm2_atl <- as.matrix(dtm_atl)

freq_ATL <- colSums(dtm2_atl)
freq_ATL <- sort(freq_ATL, decreasing = TRUE)

words <- names(freq_ATL)

pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], freq_ATL[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

##############################
###Word Cloud - East REGION###
##############################
comments_text_east <- paste(comments_east$Skc.Comments, collapse = " ")
comments_source_east <- VectorSource(comments_text_east)

corpus1 <- Corpus(comments_source_east)
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 <- tm_map(corpus1, stripWhitespace)
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))
#corpus1 <- tm_map(corpus1, removeWords, "club")

dtm_east <- DocumentTermMatrix(corpus1)
dtm2_east <- as.matrix(dtm_east)

freq_east <- colSums(dtm2_east)
freq_east <- sort(freq_east, decreasing = TRUE)

words <- names(freq_east)

pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], freq_east[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

##############################
###Word Cloud - West REGION###
##############################
comments_text_west <- paste(comments_west$Skc.Comments, collapse = " ")
comments_source_west <- VectorSource(comments_text_west)

corpus1 <- Corpus(comments_source_west)
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 <- tm_map(corpus1, stripWhitespace)
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))
#corpus1 <- tm_map(corpus1, removeWords, "club")

dtm_west <- DocumentTermMatrix(corpus1)
dtm2_west <- as.matrix(dtm_west)

freq_west <- colSums(dtm2_west)
freq_west <- sort(freq_west, decreasing = TRUE)

words <- names(freq_west)

pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], freq_west[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

##################################
###Word Cloud - Mid West REGION###
##################################
comments_text_mw <- paste(comments_mw$Skc.Comments, collapse = " ")
comments_source_mw <- VectorSource(comments_text_mw)

corpus1 <- Corpus(comments_source_mw)
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 <- tm_map(corpus1, stripWhitespace)
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))
#corpus1 <- tm_map(corpus1, removeWords, "club")

dtm_mw <- DocumentTermMatrix(corpus1)
dtm2_mw <- as.matrix(dtm_mw)

freq_mw <- colSums(dtm2_mw)
freq_mw <- sort(freq_mw, decreasing = TRUE)

words <- names(freq_mw)

pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], freq_mw[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

####################################
###WORDCLOUDS FOR DIFFERENT TIERS###
####################################

#####################################
###Subsetting data files by Tiers####
#####################################
Tier1 <- subset(comments, Tier=="Tier 1")
Tier2 <- subset(comments, Tier=="Tier 2")
Tier3 <- subset(comments, Tier=="Tier 3")

##########################
#### TIER 1 WORDCLOUD ####
##########################

comments_tier1 <- paste(Tier1$Skc.Comments, collapse = " ")
comments_source_tier1 <- VectorSource(comments_tier1)
corpustier1 <- Corpus(comments_source_tier1)

corpustier1 <- tm_map(corpustier1, content_transformer(tolower))
corpustier1 <- tm_map(corpustier1, removePunctuation)
corpustier1 <- tm_map(corpustier1, stripWhitespace)
corpustier1 <- tm_map(corpustier1, removeWords, stopwords("english"))
#corpustier1 <- tm_map(corpustier1, removeWords, "club")

dtm_tier1 <- DocumentTermMatrix(corpustier1)
dtm2_tier1 <- as.matrix(dtm_tier1)

frequencytier1 <- colSums(dtm2_tier1)
frequencytier1 <- sort(frequencytier1, decreasing = TRUE)

words <- names(frequencytier1)
pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], frequencytier1[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)


##########################
#### TIER 2 WORDCLOUD ####
##########################

comments_tier2 <- paste(Tier2$Skc.Comments, collapse = " ")
comments_source_tier1 <- VectorSource(comments_tier2)
corpustier1 <- Corpus(comments_source_tier1)

corpustier1 <- tm_map(corpustier1, content_transformer(tolower))
corpustier1 <- tm_map(corpustier1, removePunctuation)
corpustier1 <- tm_map(corpustier1, stripWhitespace)
corpustier1 <- tm_map(corpustier1, removeWords, stopwords("english"))
#corpustier1 <- tm_map(corpustier1, removeWords, "club")

dtm_tier1 <- DocumentTermMatrix(corpustier1)
dtm2_tier1 <- as.matrix(dtm_tier1)

frequencytier1 <- colSums(dtm2_tier1)
frequencytier1 <- sort(frequencytier1, decreasing = TRUE)

words <- names(frequencytier1)
pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], frequencytier1[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

##########################
#### TIER 3 WORDCLOUD ####
##########################

comments_tier1 <- paste(Tier3$Skc.Comments, collapse = " ")
comments_source_tier1 <- VectorSource(comments_tier1)
corpustier1 <- Corpus(comments_source_tier1)

corpustier1 <- tm_map(corpustier1, content_transformer(tolower))
corpustier1 <- tm_map(corpustier1, removePunctuation)
corpustier1 <- tm_map(corpustier1, stripWhitespace)
corpustier1 <- tm_map(corpustier1, removeWords, stopwords("english"))
#corpustier1 <- tm_map(corpustier1, removeWords, "club")

dtm_tier1 <- DocumentTermMatrix(corpustier1)
dtm2_tier1 <- as.matrix(dtm_tier1)

frequencytier1 <- colSums(dtm2_tier1)
frequencytier1 <- sort(frequencytier1, decreasing = TRUE)

words <- names(frequencytier1)
pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], frequencytier1[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)


#####################################
###WORDCLOUDS FOR DIFFERENT GROUPS###
#####################################

######################################
###Subsetting data files by GROUPS####
######################################
Group1 <- subset(comments, Group=="Group 1")
Group2 <- subset(comments, Group=="Group 2")
Group3 <- subset(comments, Group=="Group 3")

###########################
#### GROUP 1 WORDCLOUD ####
###########################

comments_group1 <- paste(Group1$Skc.Comments, collapse = " ")
comments_source_group1 <- VectorSource(comments_group1)
corpusgroup1 <- Corpus(comments_source_group1)

corpusgroup1 <- tm_map(corpusgroup1, content_transformer(tolower))
corpusgroup1 <- tm_map(corpusgroup1, removePunctuation)
corpusgroup1 <- tm_map(corpusgroup1, stripWhitespace)
corpusgroup1 <- tm_map(corpusgroup1, removeWords, stopwords("english"))
#corpusgroup1 <- tm_map(corpusgroup1, removeWords, "club")

dtm_group1 <- DocumentTermMatrix(corpusgroup1)
dtm2_group1 <- as.matrix(dtm_group1)

frequencygroup1 <- colSums(dtm2_group1)
frequencygroup1 <- sort(frequencygroup1, decreasing = TRUE)

pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], frequencygroup1[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

###########################
#### GROUP 2 WORDCLOUD ####
###########################

comments_group1 <- paste(Group2$Skc.Comments, collapse = " ")
comments_source_group1 <- VectorSource(comments_group1)
corpusgroup1 <- Corpus(comments_source_group1)

corpusgroup1 <- tm_map(corpusgroup1, content_transformer(tolower))
corpusgroup1 <- tm_map(corpusgroup1, removePunctuation)
corpusgroup1 <- tm_map(corpusgroup1, stripWhitespace)
corpusgroup1 <- tm_map(corpusgroup1, removeWords, stopwords("english"))
#corpusgroup1 <- tm_map(corpusgroup1, removeWords, "club")

dtm_group1 <- DocumentTermMatrix(corpusgroup1)
dtm2_group1 <- as.matrix(dtm_group1)

frequencygroup1 <- colSums(dtm2_group1)
frequencygroup1 <- sort(frequencygroup1, decreasing = TRUE)

pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], frequencygroup1[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

###########################
#### GROUP 3 WORDCLOUD ####
###########################

comments_group1 <- paste(Group3$Skc.Comments, collapse = " ")
comments_source_group1 <- VectorSource(comments_group1)
corpusgroup1 <- Corpus(comments_source_group1)

corpusgroup1 <- tm_map(corpusgroup1, content_transformer(tolower))
corpusgroup1 <- tm_map(corpusgroup1, removePunctuation)
corpusgroup1 <- tm_map(corpusgroup1, stripWhitespace)
corpusgroup1 <- tm_map(corpusgroup1, removeWords, stopwords("english"))
#corpusgroup1 <- tm_map(corpusgroup1, removeWords, "club")

dtm_group1 <- DocumentTermMatrix(corpusgroup1)
dtm2_group1 <- as.matrix(dtm_group1)

frequencygroup1 <- colSums(dtm2_group1)
frequencygroup1 <- sort(frequencygroup1, decreasing = TRUE)

pal2 <- c()

for (wrd in words[1:40]){
  if (any(products==wrd)) {
    pal2<-c(pal2, "blue")
  }
  else if (any(facility==wrd)) {
    pal2<-c(pal2, "red")
  }
  else if (any(service==wrd)) {
    pal2<-c(pal2, "green")
  }
  else if (any(policy==wrd)) {
    pal2<-c(pal2, "orange")
  }
  else {
    pal2<-c(pal2, "black")
  }
}

wordcloud(words[1:40], frequencygroup1[1:40], scale=c(4,.5), min.freq = 100, max.words = Inf, 
          random.order = TRUE, rot.per = .1, colors = pal2, ordered.colors = TRUE)

##################
###HEAT MAPPING###
##################
R <-  read.csv ("Survey.csv", stringsAsFactors=FALSE)
attach(R)

#Creating state / Tier / Group variables
R$State[Station== "ATL" | Station == "SCL"] <- "GA"
R$State[Station== "BNA"|Station=="MEM"] <-"TN"
R$State[Station== "BOS"] <-"MA"
R$State[Station== "CVG"] <-"KY"
R$State[Station== "DCA"] <-"DC"
R$State[Station== "DFW"] <-"TX"
R$State[Station== "DTW"] <-"MI"
R$State[Station== "EWR"] <-"NJ"
R$State[Station== "FLL"|Station=="TPA"|Station=="MCO" |Station=="JAX"|Station=="MIA"|Station=="PBI"] <-"FL"
R$State[Station== "HNL"] <-"HI"
R$State[Station== "IND"] <- "IN"
R$State[Station== "JFK"|Station=="LGA"] <-"NY"
R$State[Station== "LAX"|Station=="SAN"|Station=="SFO"] <-"CA"
R$State[Station== "MKE"] <-"WI"
R$State[Station== "MSP"] <-"MN"
R$State[Station== "MSY"] <-"LA"
R$State[Station== "ORD"] <-"IL"
R$State[Station== "PDX"] <-"OR"
R$State[Station== "PHL"] <-"PA"
R$State[Station== "RDU"] <-"NC"
R$State[Station== "SEA"] <-"WA"
R$State[Station== "SLC"] <-"UT"

#Non-US Airports
R$State[Station== "NRT"] <-"Japan"
R$State[Station== "SCL"] <-"Chile"
#######################################

#creating Tiers
R$Tier[Crc.Loc.Cd=="ATLE15"|Crc.Loc.Cd=="JFK-T4"|Crc.Loc.Cd=="ATL-F"|
         Crc.Loc.Cd=="DTW-CEN"|Crc.Loc.Cd=="NRT-2"|Crc.Loc.Cd=="NRT-1"|
         Crc.Loc.Cd=="LAX"|Crc.Loc.Cd=="SEA"|Crc.Loc.Cd=="MSP-FG"|
         Crc.Loc.Cd=="LGA"|Crc.Loc.Cd=="LGA-C" | Crc.Loc.Cd=="ATL-E"] <- "Tier 1"


R$Tier[Crc.Loc.Cd=="DTW-S"|Crc.Loc.Cd=="DTW-N"|Crc.Loc.Cd=="JFKCRT2"|
         Crc.Loc.Cd=="MSP-C"|Crc.Loc.Cd=="HNL"|Crc.Loc.Cd=="SLC"|
         Crc.Loc.Cd=="BOS-A6"|Crc.Loc.Cd=="EWR"|Crc.Loc.Cd=="PDX"|
         Crc.Loc.Cd=="PHL"|Crc.Loc.Cd=="DTW-C"|Crc.Loc.Cd=="CVG-B14"|
         Crc.Loc.Cd=="BOS-SAT"|Crc.Loc.Cd=="MCO"|Crc.Loc.Cd=="BNA"|
         Crc.Loc.Cd=="TPA"|Crc.Loc.Cd=="RDU"|Crc.Loc.Cd=="MKE"|
         Crc.Loc.Cd=="JAX"|Crc.Loc.Cd=="ATLA17"|Crc.Loc.Cd=="ATL-CPT"|
         Crc.Loc.Cd=="HNL-DHC"] <- "Tier 2"

R$Tier[Crc.Loc.Cd=="ATL-C"|Crc.Loc.Cd=="ATL-T"|Crc.Loc.Cd=="ORD-E"|
         Crc.Loc.Cd=="DCA"|Crc.Loc.Cd=="FLL"|Crc.Loc.Cd=="MIA"|
         Crc.Loc.Cd=="DFW10"|Crc.Loc.Cd=="SFO"|Crc.Loc.Cd=="PBI"|
         Crc.Loc.Cd=="MSY"|Crc.Loc.Cd=="IND"|Crc.Loc.Cd=="SAN"|
         Crc.Loc.Cd=="MEM"|Crc.Loc.Cd=="ATLD12"|Crc.Loc.Cd=="ATLB10"|
         Crc.Loc.Cd=="ATLB25"|Crc.Loc.Cd=="ATLD27"| Crc.Loc.Cd== "SCL"] <- "Tier 3"


#Creating Groups
R$Group[Crc.Loc.Cd=="JFK-T4"|Crc.Loc.Cd=="DTW-CEN"|Crc.Loc.Cd=="MSP-FG"|
          Crc.Loc.Cd=="ATLA17"|Crc.Loc.Cd=="LAX"|Crc.Loc.Cd=="ATLB25"|
          Crc.Loc.Cd=="ATLE15"|Crc.Loc.Cd=="SLC"|Crc.Loc.Cd=="ATLB10"|
          Crc.Loc.Cd=="LGA"|Crc.Loc.Cd=="ATL-F"|Crc.Loc.Cd=="SFO"|
          Crc.Loc.Cd=="ATL-CPT"|Crc.Loc.Cd=="ATL-C"|Crc.Loc.Cd=="ATL-T"|
          Crc.Loc.Cd=="ATLD27"|Crc.Loc.Cd=="SEA"|Crc.Loc.Cd=="ATLD12"|
          Crc.Loc.Cd=="SCL"| Crc.Loc.Cd=="ATL-E"] <- "Group 1"

R$Group[Crc.Loc.Cd=="MSP-C"|Crc.Loc.Cd=="ORD-E"|Crc.Loc.Cd=="MCO"|
          Crc.Loc.Cd=="LGA-C"|Crc.Loc.Cd=="NRT-2"|Crc.Loc.Cd=="JFKCRT2"|
          Crc.Loc.Cd=="DTW-C"|Crc.Loc.Cd=="DCA"|Crc.Loc.Cd=="CVG-B14"|
          Crc.Loc.Cd=="TPA"|Crc.Loc.Cd=="NRT-1"] <- "Group 2"

R$Group[Crc.Loc.Cd=="DTW-S"|Crc.Loc.Cd=="DTW-N"|Crc.Loc.Cd=="DFW10"|
          Crc.Loc.Cd=="FLL"|Crc.Loc.Cd=="EWR"|Crc.Loc.Cd=="BOS-SAT"|
          Crc.Loc.Cd=="MIA"|Crc.Loc.Cd=="RDU"|Crc.Loc.Cd=="BOS-A6"|
          Crc.Loc.Cd=="BNA"|Crc.Loc.Cd=="PHL"|Crc.Loc.Cd=="PDX"|
          Crc.Loc.Cd=="HNL-DHC"|Crc.Loc.Cd=="MKE"|Crc.Loc.Cd=="IND"|
          Crc.Loc.Cd=="MSY"|Crc.Loc.Cd=="JAX"|Crc.Loc.Cd=="PBI"|
          Crc.Loc.Cd=="SAN"|Crc.Loc.Cd=="MEM"| Crc.Loc.Cd=="HNL"|
          Crc.Loc.Cd=="SCL"] <- "Group 3"


########################################################
#oBSERVING MISSING VALUES TO OMIT COLUMNS STRATEGICALLY#
########################################################
R.na <- is.na(R) #Created to observe missing per column. 
print(colSums(R.na)) #summarizes missing values above. 
#Dropping Col[16:18] 
#16 - 22,000 missing 
#17 - 1 or 0 score. Assumed not part of 4.35 target rating
#18 - 28,500 missing. Also 1 or 0 score. Assumed not part of 4.35 target rating. 



#################
#MATRIX HEAT MAP#
#################
R1<-(R[,6:15])
R2<-(R[,21:24])
R3<-cbind(R1,R2)
newR<- na.omit(R3) # Keeps 42,000+ rows 

colnames(newR) #Will need to print / make reference to these b/c I recode later. 

?heatmap

##By State##
agg<- data.frame(aggregate(newR[,1:10], list(newR$State), mean))
row.names(agg)<-agg$Group.1 #renaming rows as States
agg1<-subset(agg, select=-Group.1) #removing States column
colnames(agg1)<-1:ncol(agg1) #renaming columsn as #'s for output purposes. 
names(agg1) <- c("Overall", "Cleanliness", "Comfortable", "Staff", "Communication",
                 "Wifi", "Food Quality", "Food Quantity", "Food Variety", "Beverage Quality")

agg_m<-data.matrix(agg1)
#sorting by overall all experience: highest (red) to lowerest (white)
agg_m<-agg_m[order(agg_m[,1], decreasing=F),]
color=rev(heat.colors(256))
data_heatmap<-heatmap(agg_m, Rowv=NA, Colv=NA, col=color, scale="column", margins=c(3,3), cellnote = agg_m)

#d2<- heatmap.2(agg_m, Rowv=NA, Colv=NA, col=color, scale="column", margins=c(3,3), cellnote = agg_m)
agg_m<- round(agg_m,digits=2)
agg_m[,1]

##By Tier##
agg<- data.frame(aggregate(newR[,1:10], list(newR$Tier), mean))
row.names(agg)<-agg$Group.1 #renaming rows as States
agg1<-subset(agg, select=-Group.1) #removing States column
colnames(agg1)<-1:ncol(agg1) #renaming columsn as #'s for output purposes. 
names(agg1) <- c("Overall", "Cleanliness", "Comfortable", "Staff", "Communication",
                 "Wifi", "Food Quality", "Food Quantity", "Food Variety", "Beverage Quality")

agg_m<-data.matrix(agg1)
#sorting by overall all experience: highest (red) to lowerest (white)
agg_m<-agg_m[order(agg_m[,1], decreasing=F),]
color=rev(heat.colors(256))
data_heatmap<-heatmap(agg_m, Rowv=NA, Colv=NA, col=color, scale="column", margins=c(9,5))
agg_m<- round(agg_m,digits=2)
agg_m

##By Group##
agg<- data.frame(aggregate(newR[,1:10], list(newR$Group), mean))
row.names(agg)<-agg$Group.1 #renaming rows as States
agg1<-subset(agg, select=-Group.1) #removing States column
colnames(agg1)<-1:ncol(agg1) #renaming columsn as #'s for output purposes. 
names(agg1) <- c("Overall", "Cleanliness", "Comfortable", "Staff", "Communication",
                 "Wifi", "Food Quality", "Food Quantity", "Food Variety", "Beverage Quality")

agg_m<-data.matrix(agg1)
#sorting by overall all experience: highest (red) to lowerest (white)
agg_m<-agg_m[order(agg_m[,1], decreasing=T),]
color=rev(heat.colors(256))
data_heatmap<-heatmap(agg_m, Rowv=NA, Colv=NA, col=color, scale="column", margins=c(9,5))
agg_m<- round(agg_m,digits=2)
agg_m
