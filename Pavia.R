#In this session, we will use the package udpipe to do the following:
#-to annotate short texts and large corpora in different languages,
#-to load and analyze the existing UD corpora,
#-to extract different types of information from corpora in the UD format. 

#You can execute a line of R code by clicking on Run. 
#Alternatively, you can press Ctrl + Enter.
#The hashtagged lines are not executed.

#In order to use udpipe, you should install it first (see the instructions).
#After you have installed it, you should load it every time you start an R session.

library(udpipe)


###PART I. Annotation of simple texts and corpus query.
###Task 1. Annotation of English text.
#First, we should load the English model.

#If you have already loaded the English model, please ignore the line of code below.
#If you haven't, remove the hashtag and run this command:
#model_eng <- udpipe_download_model(language = "english")

ud_english <- udpipe_load_model(model_eng$file_model)

#We begin with a simple sentence, creating a character vector with the sentence. 
text <- "There's no place like home."
#We parse it with udpipe
x <- udpipe(text, object = ud_english)
#Let's look at the result 
x
#Some details for R users
is(x)

#What are the columns of this data frame?
colnames(x)

#Exercise1: Parse a sentence of your choice and check if the analysis is correct.

#Let's parse two sentences
text <- "Mama always said life was like a box of chocolates. You never know what you're gonna get."
x <- udpipe(text, object = ud_english)
x

# Now we'll parse the Universal Declaration of Human Rights. 
#Important: the text should be saved in the UTF-8 format (I recommend Notepad++ for working with text data)
#Running the code below will open a dialog window where you can select the file.
text <- scan(file = file.choose(), what = "character", sep  = "\n", encoding = "UTF-8")

#The function reads the text to R, splitting the text into paragraphs (sep = "\n")
#Look at the first six items of the Declaration.
head(text)

#Now we can parse the text.
x <- udpipe(text, object = ud_english)
head(x)

#How to save the output as a conllu file:
write.table(as_conllu(x), file = "UDHR_ud.conllu", sep = "\t", quote = F, row.names = F)


##Task 2. Data exploration and corpus query.

#How to compute token frequencies 
tokens_freq <- table(x$token)
tokens_freq
#Top 20
sort(tokens_freq, decreasing = TRUE)[1:20]

#The same for lemmas
lemmas_freq <- table(x$lemma)
sort(lemmas_freq, decreasing = TRUE)[1:20]

#Frequencies of noun lemmas only (top 20)
nouns_freq <- table(x$lemma[x$upos == "NOUN"])
sort(nouns_freq, decreasing = T)[1:20]

#Find the line with the word "wedlock". 
#Important: when making a subset of a 2-dimensional object, 
#the rows are selected on the left of the comma,
#the columns are selected on the right of the comma.

x[x$lemma == "wedlock",]

#Find only the sentence with the word "wedlock".
#Here, we don't need a comma because we make a subset of a one-dimensional object (i.e. the column Sentence)
x$sentence[x$lemma == "wedlock"]

#Exercise 2: find the sentences with the word "law"

#Find all plural nouns (tokens)
x$token[x$upos == "NOUN"& x$feats == "Number=Plur"] 

#If you forgot the values of POS, morphological features or dependencies,
#you can easily check them:
#Universal parts of speech
levels(factor(x$upos))
#Syntactic dependencies
levels(factor(x$dep_rel))
#Morphological features
levels(factor(x$feats))
#see universaldependencies.org for information about the meaning of all these labels.

#Exercise 3: find all tokens of adjectives in the comparative form, and the sentences where they occur

#Frequencies of parts of speech
pos_freq <- table(x$upos)
pos_freq
#sorted in descending order
sort(pos_freq, decreasing = TRUE)

#Frequencies of syntactic dependencies
dep_freq <- table(x$dep_rel)
dep_freq
#sorted in descending order
sort(dep_freq, decreasing = TRUE)

#Exercise 4: take a small text in English, read it in R, parse it and obtain the top 20 frequency lists of tokens, lemmas, parts of speech and syntactic dependencies.
#Note: use x1 or another name instead of x in your code. Otherwise, the parsed English text will be overwritten.

#Which and how many active and passive subjects are there?
x$lemma[x$dep_rel == "nsubj"]
length(which(x$dep_rel == "nsubj"))
x$lemma[x$dep_rel == "nsubj:pass"]
length(which(x$dep_rel == "nsubj:pass"))

#Combining different conditions. How many are nouns and how many are pronouns?
length(which(x$dep_rel == "nsubj"&x$upos == "NOUN"))
length(which(x$dep_rel == "nsubj"&x$upos == "PRON"))

#Exercise 5: how many passive subjects are nouns, and how many of them are pronouns?

#Let's extract the lemmas of all objects
x$lemma[x$dep_rel == "obj"]
#lemmas of all objects that are pronouns
x$lemma[x$dep_rel == "obj"&x$upos == "PRON"]
#Lemmas of all objects that are common nouns:
x$lemma[x$dep_rel == "obj"&x$upos == "NOUN"]
#How many such objects are there?
length(which(x$dep_rel == "obj"&x$upos == "NOUN"))

#Which cross-linguistic tendency do these results confirm?

#Lemmas of all objects that are common OR proper nouns:
x$lemma[x$dep_rel == "obj"&x$upos %in% c("NOUN", "PROPN")]

##Task 3. Extraction of information about word order from the English data.

#First, we need to turn the character vectors with IDs into numeric vectors.
#This is a purely technical procedure.
summary(x$token_id) #you can see that the token_id values are treated as character strings, not as numbers
summary(x$head_token_id) #the same for head_token_id

x$token_id <- as.numeric(x$token_id)
summary(x$token_id) #now you can see that these are numbers

x$head_token_id <- as.numeric(x$head_token_id)
summary(x$head_token_id)

#How many attributive adjectives ("amod" and ADJ) follow and precede the head?
length(which(x$dep_rel == "amod"&x$upos == "ADJ"&x$token_id < x$head_token_id))
length(which(x$dep_rel == "amod"&x$upos == "ADJ"&x$token_id > x$head_token_id))
#check the sentences with adjectives following the head. What do they have in common?
x$sentence[x$dep_rel == "amod"&x$upos == "ADJ"&x$token_id > x$head_token_id]

#Task 4. Parsing text in German

#If you have already downloaded the model, you can skip the next command.
#If not, remove the hashtag and run the code.
#model_ger <- udpipe_download_model(language = "german")

ud_german <- udpipe_load_model(model_ger$file_model)

text_ger <- "Du hast mich gefragt."
x <- udpipe(text_ger, object = ud_german)
x

#Exercise 6: parse a sentence in another language and interpret the output. Are there any errors?
#to see the available models, type:

?udpipe_download_model

#to choose between multiple models for the same language: 
#see https://universaldependencies.org/

########################################
###PART II: Order of head and amod in UD corpora

#select UD_English-EWT/en_ewt-ud-train.conllu from UD, version 2.4
eng_ud <- udpipe_read_conllu(file = file.choose())
nrow(eng_ud) #number of tokens (incl. punctuation)
colnames(eng_ud) #column names

#important: transform token_id and head_token_id into numeric vectors
eng_ud$token_id <- as.numeric(eng_ud$token_id)
eng_ud$head_token_id <- as.numeric(eng_ud$head_token_id)

#which and how many attributive adjectives precede the head?
eng_ud$token[eng_ud$dep_rel == "amod"&eng_ud$upos == "ADJ"&eng_ud$token_id < eng_ud$head_token_id]
length(which(eng_ud$dep_rel == "amod"&eng_ud$upos == "ADJ"&eng_ud$token_id < eng_ud$head_token_id))
#8084

#which and how many attributive adjectives follow the head?
eng_ud$token[eng_ud$dep_rel == "amod"&eng_ud$upos == "ADJ"&eng_ud$token_id > eng_ud$head_token_id]
length(which(eng_ud$dep_rel == "amod"&eng_ud$upos == "ADJ"&eng_ud$token_id > eng_ud$head_token_id))
#265

#Exercise 7: examine the contexts of individual adjectives, e.g. "fantastic".
#What kind of constructions can you identify?
eng_ud$sentence[eng_ud$dep_rel == "amod"&eng_ud$upos == "ADJ"&eng_ud$token_id > eng_ud$head_token_id&eng_ud$token == "fantastic"]

#Compute the proportions of amod - head and head - amod
prop.table(c(8084, 265))
#[1] 0.96825967 0.03174033
#Create a numeric vector with one element: proportion of amod - head
eng_amod_head <- prop.table(c(8084, 265))[1]
eng_amod_head

#The same for Italian ISDT (train)
ita_ud <- udpipe_read_conllu(file = file.choose())
nrow(ita_ud) #number of tokens (incl. punctuation)

#important: transform token_id and head_token_id into numeric vectors
ita_ud$token_id <- as.numeric(ita_ud$token_id)
#missing values: due to forms like degli, alla, etc. 
#we remove those lines:
ita_ud <- ita_ud[complete.cases(ita_ud$token_id),]

ita_ud$head_token_id <- as.numeric(ita_ud$head_token_id)

#which and how many attributive adjectives precede the head?
ita_ud$token[ita_ud$dep_rel == "amod"&ita_ud$upos == "ADJ"&ita_ud$token_id < ita_ud$head_token_id]
length(which(ita_ud$dep_rel == "amod"&ita_ud$upos == "ADJ"&ita_ud$token_id < ita_ud$head_token_id))
#4665
#which and how many attributive adjectives follow the head?
ita_ud$token[ita_ud$dep_rel == "amod"&ita_ud$upos == "ADJ"&ita_ud$token_id > ita_ud$head_token_id]
length(which(ita_ud$dep_rel == "amod"&ita_ud$upos == "ADJ"&ita_ud$token_id > ita_ud$head_token_id))
#10761

#Exercise 8: examine the contexts of individual adjectives, e.g. "vero".
#What kind of constructions can you identify?
ita_ud$sentence[ita_ud$dep_rel == "amod"&ita_ud$upos == "ADJ"&ita_ud$token_id < ita_ud$head_token_id&ita_ud$token == "vero"]

#Compute the proportions of amod - head and head - amod
prop.table(c(4665, 10761))
#[1] 0.3024115 0.6975885
ita_amod_head <- prop.table(c(4665, 10761))[1]
ita_amod_head

#Exercise 9: perform the same kind of analysis for another language. Are your expectations confirmed?

#After we have data about many languages, we can aggregate 
#and visualize the proportions, using the code below

amod_head <- c(eng_amod_head, ita_amod_head) #add other languages!
names(amod_head) <- c("English", "Italian") #add other names of languages!
dotchart(sort(amod_head), xlim = c(0, 1), xlab = "Proportion of amod first")

########################################
###PART III: Greenberg's Universal 25 in UD corpora
#"If the pronominal object follows the verb, so does the nominal object."

#First, we look for objects that are common or proper nouns, and which follow the head:
vo_nouns_eng <- length(which(eng_ud$dep_rel == "obj"&eng_ud$upos %in% c("NOUN", "PROPN")&eng_ud$token_id > eng_ud$head_token_id))
vo_nouns_eng
#7395
#the same, but the objects precede the head:
ov_nouns_eng <- length(which(eng_ud$dep_rel == "obj"&eng_ud$upos %in% c("NOUN", "PROPN")&eng_ud$token_id < eng_ud$head_token_id))
ov_nouns_eng
#45

#Now the same for pronominal objects following the head:
vo_pronouns_eng <- length(which(eng_ud$dep_rel == "obj"&eng_ud$upos == "PRON"&eng_ud$token_id > eng_ud$head_token_id))
vo_pronouns_eng
#2042
#And finally pronominal objects preceding the head:
ov_pronouns_eng <- length(which(eng_ud$dep_rel == "obj"&eng_ud$upos == "PRON"&eng_ud$token_id < eng_ud$head_token_id))
ov_pronouns_eng
#318

#Put the frequencies together in one numeric vector and provide them with names:
eng_vo <- c(vo_nouns_eng, ov_nouns_eng, vo_pronouns_eng, ov_pronouns_eng)
names(eng_vo) <- c("vo_nouns","ov_nouns", "vo_pronouns", "ov_pronouns")
eng_vo
#  vo_nouns    ov_nouns vo_pronouns ov_pronouns 
#   7395          45        2042         318 

#Let's repeat that for Yoruba

yoruba <- udpipe_read_conllu(file = file.choose())
head(yoruba)

yoruba$token_id <- as.numeric(yoruba$token_id)
summary(yoruba$token_id)

yoruba$head_token_id <- as.numeric(yoruba$head_token_id)
summary(yoruba$head_token_id)


vo_nouns_yor <- length(which(yoruba$dep_rel == "obj"&yoruba$upos %in% c("NOUN", "PROPN")&yoruba$token_id > yoruba$head_token_id))
vo_nouns_yor
ov_nouns_yor <- length(which(yoruba$dep_rel == "obj"&yoruba$upos %in% c("NOUN", "PROPN")&yoruba$token_id < yoruba$head_token_id))
ov_nouns_yor

vo_pronouns_yor <- length(which(yoruba$dep_rel == "obj"&yoruba$upos == "PRON"&yoruba$token_id > yoruba$head_token_id))
vo_pronouns_yor
ov_pronouns_yor <- length(which(yoruba$dep_rel == "obj"&yoruba$upos == "PRON"&yoruba$token_id < yoruba$head_token_id))
ov_pronouns_yor

yor_vo <- c(vo_nouns_yor, ov_nouns_yor, vo_pronouns_yor, ov_pronouns_yor)
names(yor_vo) <- c("vo_nouns","ov_nouns", "vo_pronouns", "ov_pronouns")

#Exercise 10: obtain the same frequencies for two or more other languages

#... after we have multiple languages, we can put everything together:
vo <- rbind(eng_vo, yor_vo) #add more languages...
vo <- as.data.frame(vo)

##compute and visualize the proportions of Noun + V and Pron + V:
vo$ov_nouns_prop <- vo$ov_nouns/(vo$vo_nouns + vo$ov_nouns)
vo$ov_pronouns_prop <- vo$ov_pronouns/(vo$vo_pronouns + vo$ov_pronouns)
vo$language <- c("eng", "yor") #Add more language names!
plot(vo$ov_nouns_prop, vo$ov_pronouns_prop, ylim = c(0, 1), xlim = c(0, 1), type = "n")
text(vo$ov_nouns_prop, vo$ov_pronouns_prop, labels = vo$language)

#Now let's stop and think: How can we reformulate the universal?

########################################
###PART IV: Greenberg's Universal 25 in film subtitles

library(readr) #the package readr should be installed first (see the instructions)

avatar_eng <- read_file(file = file.choose())
avatar_eng
#remove the timing information and italics
avatar_eng <- gsub("<i>", "", avatar_eng)
avatar_eng <- gsub("</i>", "", avatar_eng)
avatar_eng <- gsub("-->", "", avatar_eng)
avatar_eng <- gsub("\\b[0-9:,]+\\b", "", avatar_eng)
avatar_eng <- gsub("\r\n", "", avatar_eng)

ud_english <- udpipe_load_model(model_eng$file_model)

avatar_eng_ud <- udpipe(avatar_eng, object = ud_english)
head(avatar_eng_ud)
tail(avatar_eng_ud)

#transform token_id and head_token_id into numbers
avatar_eng_ud$token_id <- as.numeric(avatar_eng_ud$token_id)
avatar_eng_ud$head_token_id <- as.numeric(avatar_eng_ud$head_token_id)

#find all VO and OV patterns with nouns and pronouns separately

vo_nouns_eng <- length(which(avatar_eng_ud$dep_rel == "obj"&avatar_eng_ud$upos %in% c("NOUN", "PROPN")&avatar_eng_ud$token_id > avatar_eng_ud$head_token_id))
vo_nouns_eng
ov_nouns_eng <- length(which(avatar_eng_ud$dep_rel == "obj"&avatar_eng_ud$upos %in% c("NOUN", "PROPN")&avatar_eng_ud$token_id < avatar_eng_ud$head_token_id))
ov_nouns_eng

vo_pronouns_eng <- length(which(avatar_eng_ud$dep_rel == "obj"&avatar_eng_ud$upos == "PRON"&avatar_eng_ud$token_id > avatar_eng_ud$head_token_id))
vo_pronouns_eng
ov_pronouns_eng <- length(which(avatar_eng_ud$dep_rel == "obj"&avatar_eng_ud$upos == "PRON"&avatar_eng_ud$token_id < avatar_eng_ud$head_token_id))
ov_pronouns_eng

eng_vo <- c(vo_nouns_eng, ov_nouns_eng, vo_pronouns_eng, ov_pronouns_eng)
names(eng_vo) <- c("vo_nouns","ov_nouns", "vo_pronouns", "ov_pronouns")

##the same with Italian, Finnish and other versions

#Italian
avatar_ita <- read_file(file = file.choose())
avatar_ita
#remove the timing information and italics
avatar_ita <- gsub("<i>", "", avatar_ita)
avatar_ita <- gsub("</i>", "", avatar_ita)
avatar_ita <- gsub("-->", "", avatar_ita)
avatar_ita <- gsub("\\b[0-9:,]+\\b", "", avatar_ita)
avatar_ita <- gsub("\r\n", "", avatar_ita)

#if you haven't downloaded the model yet, run the line below:
#model_ita <- udpipe_download_model(language = "italian")

ud_italian <- udpipe_load_model(model_ita$file_model)

avatar_ita_ud <- udpipe(avatar_ita, object = ud_italian)
head(avatar_ita_ud)
tail(avatar_ita_ud)

#find all VO and OV patterns with nouns and pronouns separately
avatar_ita_ud$token_id <- as.numeric(avatar_ita_ud$token_id)
summary(avatar_ita_ud$token_id)

avatar_ita_ud$head_token_id <- as.numeric(avatar_ita_ud$head_token_id)
summary(avatar_ita_ud$head_token_id)

avatar_ita_ud <- avatar_ita_ud[complete.cases(avatar_ita_ud$token_id),]

vo_nouns_ita <- length(which(avatar_ita_ud$dep_rel == "obj"&avatar_ita_ud$upos %in% c("NOUN", "PROPN")&avatar_ita_ud$token_id > avatar_ita_ud$head_token_id))
vo_nouns_ita
ov_nouns_ita <- length(which(avatar_ita_ud$dep_rel == "obj"&avatar_ita_ud$upos %in% c("NOUN", "PROPN")&avatar_ita_ud$token_id < avatar_ita_ud$head_token_id))
ov_nouns_ita

vo_pronouns_ita <- length(which(avatar_ita_ud$dep_rel == "obj"&avatar_ita_ud$upos == "PRON"&avatar_ita_ud$token_id > avatar_ita_ud$head_token_id))
vo_pronouns_ita
ov_pronouns_ita <- length(which(avatar_ita_ud$dep_rel == "obj"&avatar_ita_ud$upos == "PRON"&avatar_ita_ud$token_id < avatar_ita_ud$head_token_id))
ov_pronouns_ita

ita_vo <- c(vo_nouns_ita, ov_nouns_ita, vo_pronouns_ita, ov_pronouns_ita)
names(ita_vo) <- c("vo_nouns","ov_nouns", "vo_pronouns", "ov_pronouns")

#Finnish
avatar_fin <- read_file(file = file.choose())
avatar_fin
#remove the timing information and italics
avatar_fin <- gsub("<i>", "", avatar_fin)
avatar_fin <- gsub("</i>", "", avatar_fin)
avatar_fin <- gsub("-->", "", avatar_fin)
avatar_fin <- gsub("\\b[0-9:,]+\\b", "", avatar_fin)
avatar_fin <- gsub("\r\n", "", avatar_fin)

#if you haven't downloaded the model yet, run the code line below:
#model_fin <- udpipe_download_model(language = "finnish")

ud_finnish <- udpipe_load_model(model_fin$file_model)

avatar_fin_ud <- udpipe(avatar_fin, object = ud_finnish)
head(avatar_fin_ud)
tail(avatar_fin_ud)

#find all VO and OV patterns with nouns and pronouns separately
avatar_fin_ud$token_id <- as.numeric(avatar_fin_ud$token_id)
summary(avatar_fin_ud$token_id)

avatar_fin_ud$head_token_id <- as.numeric(avatar_fin_ud$head_token_id)
summary(avatar_fin_ud$head_token_id)

avatar_fin_ud <- avatar_fin_ud[complete.cases(avatar_fin_ud$token_id),]

vo_nouns_fin <- length(which(avatar_fin_ud$dep_rel == "obj"&avatar_fin_ud$upos %in% c("NOUN", "PROPN")&avatar_fin_ud$token_id > avatar_fin_ud$head_token_id))
vo_nouns_fin
ov_nouns_fin <- length(which(avatar_fin_ud$dep_rel == "obj"&avatar_fin_ud$upos %in% c("NOUN", "PROPN")&avatar_fin_ud$token_id < avatar_fin_ud$head_token_id))
ov_nouns_fin

vo_pronouns_fin <- length(which(avatar_fin_ud$dep_rel == "obj"&avatar_fin_ud$upos == "PRON"&avatar_fin_ud$token_id > avatar_fin_ud$head_token_id))
vo_pronouns_fin
ov_pronouns_fin <- length(which(avatar_fin_ud$dep_rel == "obj"&avatar_fin_ud$upos == "PRON"&avatar_fin_ud$token_id < avatar_fin_ud$head_token_id))
ov_pronouns_fin

fin_vo <- c(vo_nouns_fin, ov_nouns_fin, vo_pronouns_fin, ov_pronouns_fin)
names(fin_vo) <- c("vo_nouns","ov_nouns", "vo_pronouns", "ov_pronouns")

#Exercise 11: check another language and obtain the frequencies. Share your results with the others.

#Exercise 12: visualize the frequencies in a scatter plot, as shown above.