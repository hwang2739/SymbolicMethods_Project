library(RISmed)
library(sqldf)
library(wordcloud2)
library(ggplot2)
library(scales)
library(lubridate)
library(webshot)
library(htmlwidgets)
library(jpeg)
library(ggimage)

res <- EUtilsSummary('lena mamykina', type='esearch', db='pubmed')
fetch <- EUtilsGet(res)
mesh <- Mesh(fetch)
year <- YearPubmed(fetch)

complete_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Heading", "Type"))
for (i in 1:length(mesh)) {
  temp <- as.data.frame(mesh[i])
  mesh_year <- cbind(temp, Year = year[i])
  if(is.na(mesh_year[[1]][1]) == FALSE) { 
    complete_df <-rbind(complete_df,mesh_year) 
  }
}

descr_mesh<-complete_df[which(complete_df$Type=="Descriptor"),]
qual_mesh<-complete_df[which(complete_df$Type=="Qualifier"),]
# ---------------------------------------------------------------------------
# automation of splitting into publication periods & aggregate MeSH
# set the time interval as 4 yrs
period <- (max(descr_mesh$Year) - min(descr_mesh$Year))/4
i <- 1
l <- list()
tick <- min(descr_mesh$Year)
while(i <= period+1) {
  l[[i]] <- tick
  i <- i + 1
  tick <- tick + 4
}

# Group by year range
descr_mesh$Y_range <- 0
for (i in 1:nrow(descr_mesh)) { 
  for (a in 1:(period)) {
    if (
      (descr_mesh[i,]$Year >= l[[a]]) & (descr_mesh[i,]$Year < l[[a+1]])) {
      descr_mesh[i,]$Y_range <- paste(toString(l[[a]]), toString(l[[a+1]]), 
                                      sep = "-", collapse = NULL)
    }
  }
  if ((descr_mesh[i,]$Year >= l[[a]]) & (descr_mesh[i,]$Year <= l[[a+1]])) {
    descr_mesh[i,]$Y_range <- paste(toString(l[[a]]), toString(l[[a+1]]), 
                                    sep = "-", collapse = NULL)
  }
}

buzz_by_year_range <- sqldf("SELECT Heading, Y_range, COUNT(*) AS freq
                            FROM descr_mesh
                            GROUP BY Y_range, Heading
                            HAVING freq >= 3 AND Y_range <> 0;")
# ---------------------------------------------------------------------------
# MeSH timeline visualization
year_range <- sqldf("SELECT DISTINCT Y_range FROM buzz_by_year_range;")
r <- 1
fname_list <- list()
while (r <= length(year_range[[1]])) {
  buzz_words <- sqldf(paste0("SELECT Heading, freq
                    FROM buzz_by_year_range
                    WHERE Y_range = '", year_range[r,], "';"))
  words <- wordcloud2(buzz_words)
  fname <- paste0("words", year_range[r,])
  saveWidget(words, paste0(fname, ".html"), selfcontained = F)
  webshot(paste0(fname, ".html"), paste0(fname, ".jpeg"), delay =5)
  fname_list[r] = paste0(fname, ".jpeg")
  r = r + 1
}

# calculate the x ticks' positions & add the wc images
for (i in 1:round(period)){
  year_range$d[i] <- i/round(period)
  year_range$wc_img[i] <- fname_list[[i]]
  if (i%%2 != 0) { year_range$y_posi[i] = 0.4
  } else { year_range$y_posi[i] = -0.5 }
}

timeline_plot<-ggplot(year_range,aes(x=d, y=0)) +
  xlim(0, 1.1) +
  ylim(-1,1) +
  theme_classic() +
  geom_hline(yintercept=0, color = "black", size=0.3) +
  geom_point(aes(y=0), size=3) +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank()) +
  geom_text(aes(x=d,y=-0.1,label=Y_range), size=2.5,vjust=0.5, color='black') +
  geom_image(aes(y=y_posi, image = wc_img), size = 0.3)

print(timeline_plot)

# ---------------------------------------------------------------------------
# Timeline by different year range
# can pick 1 author and run the above code for year_interval = 3, 4, 5
# the best result should be used for generalization

