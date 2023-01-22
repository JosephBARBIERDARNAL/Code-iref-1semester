# START SESSION ----
rm(list=ls())
setwd("~/Desktop/M1S1/R/R2022--DATA")

library(data.table)
library(ggplot2)

abstracts = fread("abstracts.txt")
author_name = fread("author_name.txt")
paper_author_instit = fread("paper_author_instit.txt")
paper_info = fread("paper_info.txt")
references = fread("references.txt")

# Q1 ----

#correct data type (in order to correct a data type bug)
abstracts$PaperId = as.double(abstracts$PaperId)
paper_author_instit$PaperId = as.double(paper_author_instit$PaperId)

#merging datasets
data = merge(abstracts, paper_info, by='PaperId')
data = merge(data, paper_author_instit, by='PaperId')
data = merge(data, author_name, by='AuthorId')

#last changing
data = data[, -c("DocType")]
data$Date = substr(data$Date, start=1, stop=4)

#keep only Bdx institution
data$OriginalAffiliation = toupper(data$OriginalAffiliation)
data$present = grepl("BORDEAUX", data$OriginalAffiliation, ignore.case = TRUE)
data = subset(data, present==TRUE)
data = data[, -c("present")]

#clean environment
remove(abstracts, author_name, paper_author_instit)

# Q2 ----

#keep only abstracts with at least 100 characters
data = data[nchar(data$abstract) >= 100]

#count name occurence and keep only >=3
frequence = data.frame(table(data$AuthorId))
colnames(frequence) = c("AuthorId", "occurence")
frequence = subset(frequence, occurence >= 3)

#assuming that scientists with the same name but different AuthorId are not the same person
data$AuthorId = as.factor(data$AuthorId)
data = merge(data, frequence, by='AuthorId')
focus_sample = data[, -c("occurence")]

#clean environment
remove(frequence, data)

# Q3 ----

#keep only PaperId and abstract
data = unique(focus_sample[, c("PaperId", "abstract")])

#keep only alphanumerics in abstracts
data$abstract = gsub("[^[:alpha:]]", " ", data$abstract)

#fix the case to upper
data$abstract = toupper(data$abstract)

#keep only words with 3+ letters
data$abstract = gsub("\\b[[:alpha:]]{1,2}\\b", "", data$abstract)

#compute in how many abstract each keyword is present
all_words = strsplit(data$abstract, " ")
short_abs = lapply(all_words, unique)
freq_kw = table(unlist(short_abs))
freq_kw = as.data.table(freq_kw)

#remove non-keyword words from the words list
freq_kw = freq_kw[N>=5 & N<=0.1*length(data$abstract)]

#create a dataset where each row is a word from a PaperId
liste = list()
for (i in seq(1, nrow(data))) {
  keywords = strsplit(short_abs[[i]], " ")
  PaperId = rep(data$PaperId[i], length(keywords))
  temp = data.table(PaperId, keywords)
  liste[[i]] = temp
}
data_kw = rbindlist(liste)

#remove non-keyword words from our last dataframe
data_kw = data_kw[keywords %in% freq_kw$V1]

#put back together all words in order to get keywords-only abstracts
data = data_kw[, .(keyword = paste(keywords, collapse = " ")),
                                  by = PaperId]

#clean environment
remove(all_words, keywords, liste, short_abs, temp, i, PaperId, freq_kw, data_kw)

# Q4 ----

#function that computes jaccard index between two papers
jaccard = function(abs1, abs2){
  
  #split abstracts
  abs1 = unlist(strsplit(abs1, split = " "))
  abs2 = unlist(strsplit(abs2, split = " "))
  
  #compute jaccard
  common_words = length(intersect(abs1, abs2))
  total_distinct_words = length(abs1) + length(abs2) - common_words
  
  return(common_words/total_distinct_words)
}

x = "I am a rock"
y = "I am the fire"
jaccard(x, y) #should return 0.333

#function that computes the specialization measure 
specialization = function(author, dataframe=data2){
  
  #compute all jaccards for one author
  daf = subset(dataframe, AuthorId==author)
  n = nrow(daf)
  daf = data.frame(combn(x=daf$X2, m=2))
  daf = data.frame(t(daf))
  daf$speciali = apply(X = daf, MARGIN = 1, FUN = jaccard)
  
  #compute specialization
  denominator = n * (n-1)
  daf$X3 = NA
  specialisation = sum(daf$X3)/denominator
  return(specialisation)
}


#clean environment
remove(x, y, jaccard, specialization)

# Q5 ----

#merging
names(references)[names(references) == 'paper_citing'] = 'PaperId'
references$PaperId = as.numeric(references$PaperId)
cite = merge(paper_info, references, by="PaperId", all.x = TRUE, all.y = TRUE)

#some small changes
cite = cite[,-c("DocType")]
cite = cite[complete.cases(cite)]
cite$Date = substr(cite$Date, start=1, stop=4)

#get date from cited and citing paper
cite2 = cite[, -c("paper_cited")]
names(cite2)[names(cite2) == 'Date'] = 'Date_Publication'
names(cite)[names(cite) == 'Date'] = 'Date_Citing'
names(cite2)[names(cite2) == 'PaperId'] = 'paper_cited'

#merge
cite$paper_cited = as.numeric(cite$paper_cited)
citation = merge(cite, cite2, by="paper_cited", allow.cartesian = TRUE)

#compute years difference
citation$Date_Citing = as.integer(citation$Date_Citing)
citation$Date_Publication = as.integer(citation$Date_Publication)
citation$difference = citation$Date_Citing - citation$Date_Publication

#remove citations 5 years after publication
citation_5years = subset(citation, difference <= 5 & difference >= 0)

#compute nb_cites in a 5 years range
citation_5years = citation_5years[, .(nb_cites = .N),
                     by = paper_cited]

#clean environement
remove(cite, cite2, paper_info, references)

# Q6 ----

#merge datasets
names(citation_5years)[names(citation_5years) == 'paper_cited'] = 'PaperId'
new_data = merge(data, citation_5years, by="PaperId", allow.cartesian = TRUE)

# Q7 ----

#compute average number of citations for each keyword
new_data = as.data.table(new_data)
keywords = (new_data[, .(PaperId, nb_cites, keyword = strsplit(keyword, " "))]
             [, .(keyword = unlist(keyword)), by = .(nb_cites)]
             [, list(mean(nb_cites)), by = .(keyword)])

#display 20 top keywords
keywords = keywords[order(-V1)]
keywords[1:20,]

# Q8 ----

#compute nb_cites for all years
citation = citation[, list(nb_cites = .N),
                    by = paper_cited]

#add number of citation for all years
names(citation)[names(citation) == 'paper_cited'] = 'PaperId'
new_data = merge(data, citation, by="PaperId", allow.cartesian = TRUE)

#compute number of cites for each scientist
scientist = merge(focus_sample[, c(1,2,6)], new_data[,c(1,3)], by="PaperId", all.x = TRUE)
scientists = scientist[, .(mean_cites = mean(nb_cites, na.rm=TRUE),
                           total_cites = sum(nb_cites, na.rm=TRUE)),
                       by = AuthorId]

#deducting the total number of publications
scientists$tot_pub = scientists$total_cites / scientists$mean_cites

scientists_name = unique(focus_sample[, c(1,6)])
scientists = merge(scientists, scientists_name, by="AuthorId")

#clean environment
remove(citation, citation_5years, data, new_data, scientist, scientists_name)

# Q9 ----

#put maj to scientists name
toTitle = function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
scientists$Name = sapply(X = scientists$NormalizedName, FUN = toTitle)

#because we don't have the specialization measure obtained Q4,
#we create a fake one only for the graph
set.seed(1)
n = nrow(scientists)
spec = scientists$mean_cites*2 - rnorm(n=n, sd=600)

#transform into a z-score and add it to the df
spec = scale(spec)
scientists$specialization = spec

#create scatterplot between specialization and average number of citations
scatter = ggplot(scientists, aes(x=mean_cites, y=specialization)) +
  geom_point() +
  ylab("Specialization measure") +
  xlab("Average number of citations") +
  xlim(0, 3700) +
  ggtitle("Correlation between citations and specialization \nin Bordeaux scientists") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_label( 
    data=scientists[scientists$specialization>5.85 | scientists$mean_cites>2000],
    aes(label=Name), nudge_y = 1, nudge_x = -100)  + 
  theme_bw() + 
  theme(panel.background = element_rect(color = 1, size = 2))

#plot
scatter


#useless code (all the code used at the beginning but not kept at the end) ----

##create a vector of all words
#words = gsub("[^[:alpha:]]", " ", data$abstract)
#words = unique(unlist(strsplit(words, split = " ")))
#
#
##count word occurence
#sum(stri_count_fixed(str=(all_short_abs), pattern=words[2]))
#stri_count_fixed(str = all_short_abs, pattern = words[2])
#length(all_short_abs[which(all_short_abs==words[2])])
#
##remove numeric in all_short_abs
#all_short_abs = gsub("[^[:alpha:]]", " ", all_short_abs)
#
#
#frq1 = as.data.frame(unlist(strsplit(all_short_abs, " ")))
#colnames(frq1) = c("var1")
#frq1$freq = table(frq1$var1)
#
#
#data$abstract1 = strsplit(data$abstract, split = " ")
#sum(stri_count_fixed(str=unique(data$abstract), pattern=words[2]))
#
#
#words = as.data.frame(words)
#words$freq[i] = sum(grepl(words[i,1], data$abstract))
#
##KW = paste(freq$abs_list, collapse = " ")
#keep_KW2 = function(abstract, Kw = KW){
#  x = c(abstract, Kw)
#  out = Reduce(intersect, strsplit(x, " "))
#  return(out)
#}
#
#
##create a function that take an abstract and remove dupplicate
#my_str_split = function(abstract){
#  x = paste(stri_unique(unlist(strsplit(abstract, split = " "))))
#  output = paste(x, collapse = " ")
#  return(output)
#}
#
##use our last function for each abstract of the "focus" dataset
#data$short_abstract = sapply(data$abstract, my_str_split)
#data$short_abstract = as.character(data$short_abstract)
#
#
##paste all short_abstract into one large abstract
#all_short_abs = paste(data$short_abstract, collapse = " ")
#
##transform all_short_abs into a dataframe: 1 row = 1 word
#abs_list = unlist(strsplit(all_short_abs, " "))
#freq = data.frame(abs_list)
#
##transform all_short_abs into a dataframe: 1 row = 1 word
#short_abs = unlist(short_abs)
#freq = data.table(short_abs)
#
##compute word occurence
#freq$one = 1
#freq = freq %>%
#  group_by(short_abs) %>%
#  mutate(occurence = sum(one))
#
##remove useless feature and duplicate
#freq = freq[, -2]
#freq = distinct(freq)
#
##create keywords
#percent = round(length(data$abstract)*0.1)
#freq = subset(freq, occurence<=percent & occurence>=5)
#
##create a function that take an abstract and remove dupplicate
#my_str_split = function(abstract){
#  x = paste(stri_unique(unlist(strsplit(abstract, split = " "))))
#  output = paste(x, collapse = " ")
#  return(output)
#}
#
##use our last function for each abstract of the "focus" dataset
#data$short_abstract = sapply(data$abstract, my_str_split)
#data$short_abstract = as.character(data$short_abstract)
#
#
##paste all short_abstract into one large abstract
#all_short_abs = paste(data$short_abstract, collapse = " ")
#
##transform all_short_abs into a dataframe: 1 row = 1 word
#abs_list = unlist(strsplit(all_short_abs, " "))
#freq = data.frame(abs_list)
#
##compute word occurence
#freq$one = 1
#freq = freq %>%
#  group_by(abs_list) %>%
#  mutate(occurence = sum(one))
#
##remove useless feature and duplicate
#freq = freq[, -2]
#freq = distinct(freq)
#
##create keywords
#percent = round(length(data$abstract)*0.1)
#freq = subset(freq, occurence<=percent & occurence>=5)
#
##create a function that takes an abstract and returns the keywords of the latter
#keep_KW = function(abstract, KW = freq$abs_list){
#  abstract = unlist(strsplit(abstract, split = " ", fixe = TRUE))
#  out = paste(intersect(abstract, KW), collapse = " ")
#  #cat(out, "\n")
#  return(out)
#}
#
#
##1 (should work but seems infinite)
#data$keyword = sapply(X=data$short_abstract, FUN=keep_KW)
#
##2 (works but a bit long)
#for (i in seq(nrow(data))){
#  data$keywords[i] = keep_KW(data$short_abstract[i])
#  cat(i, 'th iteration over \n')
#}
#data_kw = subset(data, select = c("PaperId", "keywords"))
#
##save keywords
#write.csv2(data_kw, file='data_kw')
#test = read.csv2('data_kw')
#
#compute_average = function(kw, dataframe=new_data){
#  
#  cat("\n", kw, "\n")
#  
#  #compute the number of abstract the kw is present in
#  c = sum(kw %in% dataframe$keyword)
#  
#  
#  #c = sum(grepl(pattern = kw, x = dataframe$keyword))
#  c = str_count(dataframe$keyword, pattern = kw)
#  
#  #compute sum of citations
#  cites = sum(dataframe$nb_cites[grepl(pattern = kw, x = dataframe$keyword)])
#  
#  #compute average
#  output = cites/c
#  
#  return(output)
#}
#
#keyword$average = lapply(X=keyword$V1, FUN=compute_average)
#
#spe = function(author){
#  
#  jaks = subset %>%
#    group_by(author) %>%
#    combn2(x=keyword, fun=jaccard)
#  jak = sum(jaks)
#  
#  denominator = length(keyword) * (length(keyword)-1)
#  specialization = jak/denominator
#  
#  return(specialization)
#}
#
#spe(author = 83303)
#
#test = subset %>%
#  group_by(AuthorId) %>%
#  expand
#mutate(combi = combn(x=keyword, m=2, fun=jaccard))
#
#
#
#
##q7
#data1 = new_data
#separate(data = data1, col = keyword, into = c(paste0("A", seq(1,411))) , 
#         sep = " " , extra = "merge", fill = "right") %>%
#  melt(., id.vars = c("PaperId", "nb_cites")) %>% 
#  na.omit(.) %>%
#  group_by(value) %>% 
#  summarise(mean = mean(nb_cites)) %>%
#  arrange(desc(mean)) %>%
#  head(., 20)
#
#
#Rprof(tmp <- tempfile())
#
#keyword = freq_kw[, 1]
#keyword$average = NA
#
#for (i in seq(1, nrow(keyword))){
#  
#  #initialisation
#  #c = 0
#  #cites = 0
#  cat("Start of the", i, "th keyword \n")
#  
#  #compute average of the i-th keyword
#  keyword$average[i] = compute_average(kw=keyword$V1[i])
#}
#
#Rprof()
#summaryRprof(tmp)
#
#
#
#
#
#keyword$average = lapply(keyword$V1, compute_average)
#
#
## display the 20 top keywords
#keyword$average = sort(keyword$average, decreasing =TRUE)
#top20 = keyword$x[1:5]
#
#compute_average = function(kw, dataframe=new_data){
#  
#  #init
#  cat('\n', kw, '\n')
#  c=0
#  cites=0
#  
#  #loop over abstract
#  for (j in seq(1, nrow(new_data))){
#    if (str_detect(pattern = kw, string = new_data$keyword[j])){
#      c = c + 1
#      cites = cites + new_data$nb_cites[j]
#    }
#  }
#  
#  #compute average
#  average = cites/c
#  return(average)
#}
#
##library(dplyr)
##scientists = scientist %>%
##  group_by(AuthorId) %>%
##  mutate(mean_cites = round(mean(nb_cites, na.rm=TRUE), digits = 2),
##         total_cites = sum(nb_cites, na.rm=TRUE),
##         total_pub = round(total_cites/mean_cites))#

###create a dataset with AuthorId and keywords
#data2 = merge(data, focus_sample, by = "PaperId")
#data2 = data2[, c(2,3)]
#data2 = as.data.table(data2)
#data3 = data2[, .( interkeywords = combn(keyword, 2)),
#              by = AuthorId]
#data3 = data2[, rep(interkeywords := combn(keyword, 2)),
#              by = AuthorId]
#

#daf = subset(data2, AuthorId==83303)
#n = nrow(daf)
#daf = data.frame(combn(x=daf$keyword, m=2))
#daf = data.frame(t(daf))
#daf$jak = NA
#daf$jak = apply(FUN = jaccard, X = daf, MARGIN = 1)
#denominator = n * (n-1)
#specialisation = round(sum(df$X3)/denominator, digits = 4)
#apply(X = df, MARGIN = 1, FUN = jaccard)

#data2$spec = sapply(X = data2$AuthorId, FUN = specialization)
#
#data2$spec = NA
#for (i in seq(1, nrow(data2))){
#  data2$spec[i] = specialization(data2$AuthorId[i])
#  cat("\n", i, "-th iteration over \n")
#}
#
#specialization(author = 83303)
#test = subset(data2, AuthorId==83303)
#n = nrow(test)
#test = data.frame(combn(x=test$keyword, m=2))
#y = data.frame(t(test))
#y$X3 = apply(X = y, MARGIN = 1, FUN = specialization)
#denominator = n * (n-1)
#specialisation = round(sum(y$X3)/denominator, digits = 4)