##Scraping the happiness index
##Source: http://www.uvm.edu/storylab/share/papers/mitchell2013a/AppendixCTable.html

library(rvest);library(XML);library(RCurl)
l <- readr::read_csv("Preliminary_Shiny/data/locations.csv")

b <- read_html("body_text.html")
b.t <- html_text(html_nodes(b,"td"))

df<- data.frame(matrix(NA,ncol=4,nrow=373))
names(df) <- c("rank","city","index","tweets")
j <- i <- 1
while(i <= 1492){
  df[j,]$rank <- as.numeric(b.t[i])
  df[j,]$city <- as.character(b.t[i+1])
  df[j,]$index <- as.numeric(b.t[i+2])
  df[j,]$tweets <- as.character(b.t[i+3])
  
  i <- i + 4
  j <- j + 1
}

for(i in 1:nrow(df)){
  df[i,]$city <- substr(df[i,]$city,1,unlist(gregexpr(",",df[i,]$city))+3)
}

df[,'state'] <- NA
for(i in 1:nrow(df)){
  df[i,]$state <- substr(df[i,]$city,unlist(gregexpr(",",df[i,]$city))+2,nchar(df[i,]$city))
  df[i,]$city <- substr(df[i,]$city,1,unlist(gregexpr(",",df[i,]$city))-1)
}

df$tweets <- as.numeric(gsub(pattern = ",",replacement = '',x = df$tweets))

l[,'index'] <- l[,'tweets'] <- NA
for(i in 1:nrow(l)){
  c <- l[i,]$city; s <- state.abb[match(l[i,]$state,state.name)]
  
  r.p <- df[which(grepl(c,df$city) & df$state==s),]
  if(nrow(r.p) >= 1){
    l[i,]$index <- r.p$index
    l[i,]$tweets <- r.p$tweets
  }
}
l.p2 <- l

write.csv(l.p,"locations_with_index.csv")
