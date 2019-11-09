
library(RISMED)
library(tm)
res <- EUtilsSummary('chunhua weng', type='esearch', db='pubmed')
summary(res)
QueryId(res) #some extracts lack MeSH extractable terms; we took those out

fetch<-EUtilsGet(res)
mesh<-Mesh(fetch)
mesh

completed_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Index", "Terms"))
for (i in 1:length(mesh))
{
  if(mesh[i]!='NA'){
    tframe<-as.data.frame(mesh[i])
    #print(tframe)
    r=paste0( tframe$Heading, collapse=",")
    r_factor<-as.factor(r)
    #print(r_factor)
    #data_frame<-as.data.frame(i,r)
    #print(data_frame)
    #completed_df <-rbind(completed_df,data_frame)
    completed_df <- rbind(completed_df, data.frame("Index" = i, "Terms" = r_factor))
    
  }
  
}


completed_df
meshTermList<-as.list(completed_df["Terms"])
meshTermList
s <- strsplit(as.character(meshTermList$Terms), ',')
AllTerms<-data.frame(AllMeshTerms=unlist(s))

#####LDA part
library(textmineR)


# create a document term matrix 
dtm <- CreateDtm(doc_vec = completed_df$Terms, # character vector of documents
                 doc_names = completed_df$Index, # document names
                 ngram_window = c(1,3), # minimum and maximum n-gram length
                 #ngram_window = c(1),
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 #remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = TRUE # Turn off status bar for this demo
) # default is all available cpus on the system
dtm
dtm <- dtm[,colSums(dtm) >= 1]


# Fit a Latent Dirichlet Allocation model
# the number of topics is arbitrary here
set.seed(12345)

model <- FitLdaModel(dtm = dtm, 
                     k = 20,
                     iterations = 500, 
                     burnin = 50,#180
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE
) 

# probabilistic coherence, a measure of topic quality
# this measure can be used with any topic model, not just probabilistic ones
summary(model$coherence)
hist(model$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
head(t(model$top_terms))
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
head(t(model$prevalence))
# prevalence should be proportional to alpha
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")


model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)

head(model$labels)

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]

##### Trial 2: Using '_' terms
df_tt=as.data.frame(model$summary$top_terms)
df_tt
df_tt[1:10,]
df_topic=as.data.frame(df_tt[1:10,])
df_topic
colnames(df_topic)[1]<-c("Terms")
df_topic
#### Trial 3: Using model labels instead of '_' topic terms
df_tt=as.data.frame(model$summary$label_1)
df_tt
df_tt[1:10,]
df_topic=as.data.frame(df_tt[1:10,])
df_topic
colnames(df_topic)[1]<-c("Terms")
df_topic

##This function is used to find only bigrams and trigrams in topics term list
my_funct<-function(x){
  x=as.character(unlist(x))
  word_list=as.list(strsplit(x,","))[[1]]
  u_list=grep('_', word_list, value=TRUE)
  
}
out=apply(df_topic,1,my_funct )
## 'out' is our final LDA output term list. It consists of either '_' topic terms or label_terms based on the which approach we took
##above (either Trial:2 or Trial:3)
out

## 'AllTerms$AllMeshTerms' gives us extracted MeSH terms for all abstrats of a given author
## (Like a master dictionary) that we will use to compare our bigrams and trigrams with
AllTerms$AllMeshTerms

##wf is a list of all MeSH terms
wf<-as.list(AllTerms$AllMeshTerms)
##'unique_wf' gets rid of duplicate MeSH terms that may have been found in the abstracts
unique_wf<-unique(as.list(wf))

############### Trial : Fuzzy string matching. Finding the MeSh terms that correspond to the topic_terms
###determined by LDA
out_list<-list()
for (i in 1:10) ##(As we have set number of topics=10 for LDA)
{
  
  for (j in 1:length(out[[i]]))
  {  
    if (!is.null(out[[i]][j])){
      print(out[[i]][j])
      for (k in 1:length(unique_wf))
      {
        a_match<-agrep(out[[i]][j], unique_wf[[k]][1], max = 3, value = TRUE,ignore.case = TRUE)
        print(a_match)
        if (!is.null(a_match))
        {
          out_list<-append(out_list,a_match)
        }
      }
      
    } 
  }
  
}
##List of matched terms
out_list
##Removing duplicates
unique_terms=unique(out_list)
unique_terms



