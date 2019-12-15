library(RISmed)
library(tm)
library(sqldf)
library(textmineR) 

res <- EUtilsSummary('rita kukafka', type='esearch', db='pubmed')
summary(res)
fetch<-EUtilsGet(res)
#pubmed_data <- data.frame('Title'=ArticleTitle(fetch), 'Year'=YearPubmed(fetch))
#head(pubmed_data,4)
#write.csv(pubmed_data,"JRobert.csv", row.names = TRUE)

mesh<-Mesh(fetch)
year <- YearPubmed(fetch)

complete_df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), 
                        c("Heading", "Year", "Publication"))
for (i in 1:length(mesh)) {
  if(mesh[i]!='NA'){
    temp <- as.data.frame(mesh[i])
    mesh_year <- cbind(temp, Year = year[i])
    mesh_year <- cbind(mesh_year, Publication = i)
    complete_df <- rbind(complete_df, mesh_year)
  }
}

# automation of splitting into publication periods & aggregate MeSH
# set the time interval as 4 yrs
period <- (max(complete_df$Year) - min(complete_df$Year))/3
i <- 1
l <- list()
tick <- min(complete_df$Year)
while(i <= period+1) {
  l[[i]] <- tick
  i <- i + 1
  tick <- tick + 3
}

# Group by year range
complete_df$Y_range <- 0
for (i in 1:nrow(complete_df)) { 
  for (a in 1:(period)) {
    if (
      (complete_df[i,]$Year >= l[[a]]) & (complete_df[i,]$Year < l[[a+1]])) {
      complete_df[i,]$Y_range <- paste(toString(l[[a]]), toString(l[[a+1]]), 
                                       sep = "-", collapse = NULL)
    }
  }
  if ((complete_df[i,]$Year >= l[[a]]) & (complete_df[i,]$Year <= l[[a+1]])) {
    complete_df[i,]$Y_range <- paste(toString(l[[a]]), toString(l[[a+1]]), 
                                     sep = "-", collapse = NULL)
  }
}

group_by_year_range <- sqldf("SELECT Publication, Heading, Y_range
                             FROM complete_df
                             WHERE Y_range <> 0;")

# build data frame; list of MeSH terms per period
y<- unique(group_by_year_range$Y_range)
time_period_list<- list()
t_list<-list()
for (i in y) {
  year<-i
  period_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Index", "Terms"))
  temp <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Publication", "Heading"))
  for (j in 1:length(group_by_year_range$Heading)) {
    if (as.character(group_by_year_range[j,]$Y_range) == i) {
      temp <- rbind(temp, data.frame("Publication" = group_by_year_range[j,]$Publication, 
                                     "Heading" = group_by_year_range[j,]$Heading))
    }
  }
  print(i)
  # print(temp)
  
  pub <- unique(temp$Publication)
  df_by_pub <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Publication", "Terms"))
  for (k in pub) {
    by_publication <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Publication", "Heading"))
    for (h in 1:length(temp$Publication)) {
      if (temp[h,]$Publication == k) {
        by_publication <- rbind(by_publication, data.frame("Publication" = temp[h,]$Publication, 
                                                           "Heading" = temp[h,]$Heading))
      }
    }
    #print(by_publication)
    r=paste0(by_publication$Heading, collapse=",")
    r_factor<-as.factor(r)
    df_by_pub <- rbind(df_by_pub, data.frame("Publication" = k, "Terms" = r_factor))
  }
  #print(df_by_pub)
  
  # build data frame for all MeSH terms extracted (for later matching)
  meshTermList<-as.list(df_by_pub["Terms"])
  s <- strsplit(as.character(meshTermList$Terms), ',')
  AllTerms<-data.frame(AllMeshTerms=unlist(s))
  
  dtm <- CreateDtm(doc_vec = df_by_pub$Terms, 
                   doc_names = df_by_pub$Publication, 
                   ngram_window = c(1,3), 
                   stopword_vec = c(stopwords::stopwords("en"), 
                                    stopwords::stopwords(source = "smart")),
                   lower = TRUE, 
                   #remove_punctuation = TRUE, 
                   remove_numbers = TRUE,
                   verbose = TRUE )
  
  # sparse matrix, select for MeSH terms that occur at least once
  dtm <- dtm[,colSums(dtm) >= 1]
  set.seed(12345)
  
  model <- FitLdaModel(dtm = dtm, 
                       k = 20, 
                       iterations = 500, 
                       burnin = 50,
                       alpha = 0.1, 
                       beta = 0.05, 
                       optimize_alpha = TRUE,
                       calc_likelihood = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = TRUE) 
  
  # Get the top terms of each topic
  model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
  model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
  # textmineR has a naive topic labeling tool based on probable bigrams
  model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                              dtm = dtm,
                              M = 1)
  # put them together, with coherence into a summary table
  model$summary <- data.frame(topic = rownames(model$phi),
                              label = model$labels,
                              coherence = round(model$coherence, 3),
                              prevalence = round(model$prevalence,3),
                              top_terms = apply(model$top_terms, 2, function(x){
                                paste(x, collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
  #model$summary[order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]
  #model$labels
  
  # Using '_' terms
  df_tt=as.data.frame(model$summary$top_terms)
  df_topic=as.data.frame(df_tt[1:10,])
  colnames(df_topic)[1]<-c("Terms")
  
  ##This function is used to find only bigrams and trigrams in topics term list
  my_funct<-function(x){
    x=as.character(unlist(x))
    word_list=as.list(strsplit(x,","))[[1]]
    u_list=grep('_', word_list, value=TRUE)
  }
  out=apply(df_topic,1,my_funct )
  
  ## If there are no bi- or tri-grams in the topic terms, then use the topic-label as the term
  for (i in 1:length(out)) {
    if (as.character(out[i])=="character(0)") {
      out[i]=model$labels[i]
    }
  }
  
  ## back reference
  wf<-as.list(AllTerms$AllMeshTerms)
  unique_wf<-unique(as.list(wf))
  
  # Fuzzy string matching
  out_list<-list()
  for (i in 1:10) { 
    for (j in 1:length(out[[i]])) {  
      if (!is.null(out[[i]][j])) {
        #print(out[[i]][j])
        for (k in 1:length(unique_wf)) {
          a_match<-agrep(out[[i]][j], unique_wf[[k]][1], max = 3, value = TRUE,ignore.case = TRUE)
          #print(a_match)
          if (!is.null(a_match)) {
            out_list<-append(out_list,a_match)
            #print(out_list)
          }
        }
      } 
    }
  }
  ##List of matched terms
  #out_list
  ##Removing duplicates
  unique_terms=unique(out_list)
  #unique_terms
  time_period_list<-append(time_period_list,unique_terms)
  foo<-list(pub_year=year,terms=unique_terms)
  t_list<-append(t_list,foo)
}
#unique_terms
print(t_list)
