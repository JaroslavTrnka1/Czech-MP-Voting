# Analysis of voting distance of Members of Parliament in Czech Republic, the Lower Chamber, 2017 - 2021
# Classical multidimensional scaling

library(dplyr)
library(tidyr)

temp <- tempfile()

# Downloading data from Czech Parliament website and their preparing for analysis. Final shape:
# Data frame called "celek" with following columns:
# "MP" - id number of MP
# "name" - name of MP
# "party" - MP's political membership
# "67018" - "76454" id numbers of votings

url1<-"https://www.psp.cz/eknih/cdrom/opendata/hl-2017ps.zip"
download.file(url1, temp)
voting<-read.csv(unz(temp, "hl2017h1.unl"), header = FALSE, sep = "|") %>% 
  select(!V4) %>%
  pivot_wider(names_from = V2, values_from = V3)
voting<-as.data.frame(voting)
colnames(voting)[1]<-"MP"

url2<-"https://www.psp.cz/eknih/cdrom/opendata/poslanci.zip"
download.file(url2, temp)
MP_id<-read.csv(unz(temp, "poslanec.unl"), sep = "|", header = FALSE,) %>%
  select(V1, V2, V4)
colnames(MP_id)<-c("MP", "person", "party_id")

persons<-read.csv(unz(temp,"osoby.unl"), sep = "|", header = FALSE) %>% select(V1, V4, V3)
persons<-cbind(persons$V1, paste(persons$V4, persons$V3))
colnames(persons)<-c("person", "name")

organs<-read.csv(unz(temp, "organy.unl"), sep = "|", header = FALSE) %>% select (V1, V4)
colnames(organs)<-c("party_id", "party")

MPS<-merge(MP_id, persons)%>%
  merge(organs)%>%
  select(MP, name, party) %>% 
  filter(MP >= min(voting$MP))

whole<-merge(MPS, voting)  # Final file with raw, uninterpreted data

unlink(temp)
rm(url1,url2)

# If not needed for other purposes, objects suitable for removing:  
# rm(organs)
# rm(persons)
# rm(MPS)
# rm(MP_id)
# rm(voting)
# rm(temp)

# Some of MPs left during their mandate, there are more MPs than the numer of seats in the Lower Chamber (which is 200).
# I have decided to interconnect and merge votes of preceding and successive (backup) MPs from the same party (of the "same seat").
# Going through the all possible pairs of MPs, the code searches for the best completion of votings.
# The best completion means: no overlap of voting, shortest intersect of missing votes (Na-s).
# Begins from the 201'st MP - the first of the backup MPs.
# New data frame of 200 MP mandates (with merged preceding-successive) is called "whole200".

for (v in 201:nrow(whole)) {
  record<-ncol(whole)
  best_fit<-c(0,0)
  for(u in which(whole$party == whole$party[v])[which(whole$party == whole$party[v])<=200]){
    if (whole$party[v]==whole$party[u]) {
      if (length(intersect(which(!is.na(whole[v,])), which(!is.na(whole[u,]))))==3) {
        if (length(intersect(which(is.na(whole[v,])), which(is.na(whole[u,]))))< record) {
          record<-sum(is.na(whole[v,]) * is.na(whole[u,]))
          best_fit<-c(u,v)
        }
      }
    }  
  }
  print(c("Merging successive mandates:",v , whole$name[best_fit[1]], whole$name[best_fit[2]]))
  whole[best_fit[1],which(is.na(whole[best_fit[1],]))]<-whole[best_fit[2],which(is.na(whole[best_fit[1],]))]
  whole$name[best_fit[1]]<-paste(whole$name[best_fit[1]], whole$name[best_fit[2]], sep = "/")
}
rm(record)
rm(best_fit)
rm(v)
rm(u)
whole200<-whole[1:200,]

# If not needed for other purposes, objects suitable for removing: 
# rm(whole)

# Two data frames for the analysis of mututal distance of MPs.
# "co_votes" contains detection of actively votings (Yes or No) - "V".
# "ag_votes" contains detection of agreement with voted suggestion (Yes - A, or both No - B).
# "@" is "abstain from voting", "K" is "absence", "W" is "voting before the oath" - all of them are interpreted as NAs.

co_votes<-whole200 %>% select(!c(name, party)) %>% pivot_longer(!MP)
co_votes$value[which(co_votes$value == "@")]<-NA
co_votes$value[which(co_votes$value == "W")]<-NA
co_votes$value[which(co_votes$value == "K")]<-NA
ag_votes<-pivot_wider(co_votes, names_from = name, values_from = value)
co_votes$value[which(co_votes$value == "A")]<-"V"
co_votes$value[which(co_votes$value == "B")]<-"V"
co_votes<-pivot_wider(co_votes, names_from = name, values_from = value)

# Function "proximity" is counting the amount of identical vote records between one MP and all af the others
proximity<-function(one, others){
  colSums(apply(others, 1, '==', one), na.rm = TRUE)
}

# Function "space" applies "proximity" to create a matrix-like table of mutual similarity between all of 200 MP mandates.
space<-function(MP_s){
  apply(MP_s, 1, proximity, MP_s)
}

# Applying "space" on the table of common voting and the table of agreement in voting.
common_votes<-space(co_votes[,2:ncol(co_votes)])
agreements<-space(ag_votes[,2:ncol(ag_votes)])

# Mutual distances of MPs are proportions of disagreement from the total of common votings.
distances<-(common_votes-agreements)/common_votes

# CMD scaling of distance matrix.
mp_positions<-cbind(whole200[,1:3],cmdscale(distances))
colnames(mp_positions)[4:5]<-c("X","Y")

# Graph..
library(plotly)
fig <- plot_ly(data = mp_positions, x = ~X, y = ~Y, text = ~paste(name, '<br>', party),color = ~party, 
               colors = c("navyblue","orange","yellow1","orangered4","dodgerblue2","black","brown3","grey70","tomato2"),
               size = 10)
fig
