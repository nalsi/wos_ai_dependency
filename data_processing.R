library(readr)
library(stringi)
library(stringr)
library(textstem)
library(koRpus)
library(qdapDictionaries)
library(tokenizers)
library(stringdist)

#### 1 Data collection
# data collected: 12/7/2017
# 29,195 papers downloaded from WoS by searching "artificial intelligence" without any other criteria
# Read data and merge into a single file

file <- list.files(path = "./ai_data",
                   pattern = "\\.csv")
data <- data.frame(matrix(vector(), 0, 66),
                   stringsAsFactors=F)

for (i in 1:length(file)) {
  data.1 <- read.csv(file = paste("./ai_data/",
                                  file[i],
                                  sep = ""),
                     stringsAsFactors = F)
  data <- rbind(data, data.1)
}

data <- data[which(duplicated(data$UT) == F),] # 29,184
data <- data[which(data$LA == "English"),] # 28,322
data$PY[which(data$PY == 31)] <- 2017
data <- data[data$PY > 1994,] # 24,396
data <- data[data$DT %in% c("Article", "Proceedings Paper", "Article; Proceedings Paper", "Review"),]
# 22743

hist(data$PY, xlim = c(1950, 2018), breaks = 3000)
for (i in 1:nrow(data)) {
  if (is.na(data$AB[i]) == T) {
    data$AB.1[i] = "N"
  } else if (nchar(data$AB[i]) < 1) {
    data$AB.1[i] = "N"
  } else {
    data$AB.1[i] = "Y"
  }
}
data.1 <- data[data$AB.1 == "Y",] # 22,339

write.csv(data.1, "revised_data.csv", row.names = F)
rm(list = ls())

#### 2 NLP preprocessing

#### 2.1 Extract DE terms (Run the whole section)

data <- read.csv("revised_data.csv", stringsAsFactors = F) # 22339
keywords.table.1 <- data.frame(keywords = character(0),
                               keywords.standard = character(0))
is.word  <- function(x) x %in% GradyAugmented
word.standardization <- function(x) {
  x <- as.character(x)
  x <- gsub("(-|/|\\[|\\]){1,}", " ", x, perl = F)
  x <- trimws(x)
  for (i in 1:length(x)) {
    x.1 <- x[i]
    if (str_count(x.1, "\\(") < 2) {
      x[i] <- gsub("\\(.*\\)", "", x.1, perl = T)
    } else {
      x[i] <- paste(substring(x.1, 
                              c(1, 
                                stri_locate_first_regex(x.1, "\\)")[1] + 1), 
                              c(stri_locate_first_regex(x.1, "\\(")[1]-1, 
                                stri_locate_last_regex(x.1, "\\(")[1]-1)), 
                    collapse = "")
    }
    if (str_count(x.1, "\\,") ==1 ) {
      part <- trimws(tail(strsplit(x.1, ",")[[1]], n = 1))
      part.1 <- trimws(head(strsplit(x.1, ",")[[1]], n = 1))
      if (is.word(part) == F | nchar(part) < 6) {
        x[i] <- ifelse(abbreviate(part.1, nchar(part)) == part,
                    part.1,
                    x.1)
      }
    }
  }
  x <- gsub("[[:punct:]]$", "", x)
  x <- gsub("\"", "", x)
  x <- trimws(x)
  x <- ifelse(str_count(x, " ") > 0, 
              stem_words(x), 
              x)
  x <- gsub(" ", "", x)
}

for (i in 1:nrow(data)) {
  keywords.1 <- strsplit(tolower(data$DE[i]), "; ")[[1]]
  keywords.1 <- gsub("^the ", "", keywords.1)
  keywords.1 <- gsub("^([[:digit:]]\\.){1,}[[:digit:]]*", "", keywords.1)
  keywords.1 <- gsub("\\, artificial intelligence|\\: artificial intelligence|artificial intelligence and |artificial intelligence & |ai and |\\(ai and\\)|\\(artificial intelligence\\)", "", keywords.1)
  keywords.1 <- gsub("^ai | ai |\\-ai ", "artificial intelligence ", keywords.1)
  keywords.1 <- trimws(keywords.1)
  keywords.1 <- keywords.1[which(str_count(keywords.1, " |/|-") > 0)]

  if (length(keywords.1) > 0) {
    keywords.2 <- word.standardization(keywords.1)
    keywords.df.1 <- data.frame(keywords = keywords.1,
                                keywords.standard = keywords.2,
                                stringsAsFactors = F)
    keywords.table.1 <- rbind(keywords.table.1,
                              keywords.df.1)
  }
}

keywords.summary <- data.frame(table(keywords.table.1$keywords.standard))
keywords.summary <- keywords.summary[keywords.summary$Freq > 1,]
keywords.table.2 <- keywords.table.1[keywords.table.1$keywords.standard %in% keywords.summary$Var1,]
keywords.table.2 <- keywords.table.2[keywords.table.2$keywords.standard != "",]
keywords.table.2 <- keywords.table.2[duplicated(keywords.table.2) == F,]
keywords.table.2 <- keywords.table.2[order(nchar(keywords.table.2$keywords), 
                                           decreasing = T),]
write.csv(keywords.table.2, "keywords.table.freq2.csv", row.names = F) # 23296
rm(list = ls())

#### 2.2 Preprocessing

data <- read.csv("revised_data.csv", stringsAsFactors = F) # 22339
keywords <- read.csv("keywords.table.freq2.csv",
                     colClasses = c("character", "character")) # 4945
abstract.table <- data.frame(id = character(0),
                             abstract = character(0))

for (i in 1:nrow(data)) {
  title <- as.character(data$TI[i])
  sen <- as.character(data$AB[i])
  sen <- paste(title, sen, sep = ". ")
  sen <- gsub("\\[.*?\\]", "", sen)
  sen <- gsub("\\(\\)|\\(s\\)", "", sen)
  #### think about multi-parenthesis scenario
  par <- regmatches(sen, 
                    gregexpr("\\(.*?(\\)|\\)\\))", sen, perl = T))[[1]]
  countchar <- function(x) {nchar(substr(par[x], 2, nchar(par[x])-1))}
  if (length(par) > 0) {
    for (j in 1:length(par)) {
      if (grepl("[[:digit:]]{4}", par[j]) == T | grepl("[[:digit:]]{2}\\([[:digit:]]{1,}.*?\\)", par[j]) == T) {
        sen <- gsub(gsub("([[:punct:]]){1}", "\\\\\\1", par[j], perl = T), 
                    "", 
                    sen)
        next
      } else if (grepl("[[:digit:]]", par[j]) == F & countchar(j) < 7 & countchar(j) > 2) {
        sen <- gsub(gsub("([[:punct:]]){1}", "\\\\\\1", par[j], perl = T), 
                    "", 
                    sen)
        next
      } else if (grepl(" ai$|ai\\-|\\-ai$", par[j]) == T) {
        sen <- gsub(gsub("([[:punct:]]){1}", "\\\\\\1", par[j], perl = T), 
                    "", 
                    sen)
        next
      } else if (countchar(j) == 2 & substr(par[j], 1, 1) != substr(par[j], 2, 2)) {
        sen <- gsub(gsub("([[:punct:]]){1}", "\\\\\\1", par[j], perl = T), 
                    "",
                    sen)
      } else if (grepl("[[:digit:]]{1, }\\-[[:digit:]]{1, }|[[:digit:]]{1, }\\.[[:digit:]]{1, }|[[:digit:]]{1, }//%", par[j]) == T) {
        sen <- gsub(gsub("([[:punct:]]){1}", "\\\\\\1", par[j], perl = T), 
                    "", 
                    sen)
        next
      } else if (grepl("[[:alpha:]]+(\\'|\\-)[[:digit:]]{2}", par[j]) == T) {
        sen <- gsub(gsub("([[:punct:]]){1}", "\\\\\\1", par[j], perl = T), 
                    "", 
                    sen)
        next
      } else if (grepl("n( {0,1})\\=( {0,1})[[:digit:]]{1,}", par[j]) == T) {
        sen <- gsub(gsub("([[:punct:]]){1}", "\\\\\\1", par[j], perl = T), 
                    "", 
                    sen)
        next
      }
    }
  }
  
  sen <- tolower(tokenize_sentences(sen)[[1]])
  sen <- subset(sen, grepl("artificial( |\\-)intelligence", sen) | grepl("( |\\(|^)ai( |\\)|$)", sen, perl = T))
  sen <- gsub("international joint conference on artificial intelligence", "JCArtificialIntelligence", sen)
  sen <- gsub("joint conference on artificial intelligence", "JCArtificialIntelligence", sen)
  sen <- gsub("mexican international conference on artificial intelligence", "MICArtificialIntelligence", sen)
  sen <- gsub("electronic transactions on artificial intelligence", "ETArtificialIntelligence", sen)
  sen <- gsub("computers and artificial intelligence journal", "CArtificialIntelligenceJ", sen)
  sen <- gsub("international conference on artificial intelligence and statistics", "ICArtificialIntelligenceS", sen)
  sen <- gsub("annals of mathematics and artificial intelligence\\, mathematics and informatic", "AMArtificialIntelligenceMI", sen)
  sen <- gsub("conference on uncertainty in artificial intelligence", "CUArtificialIntelligence", sen)
  sen <- gsub("aaai conference on artificial intelligence and digital entertainment", "AAAIArtificialIntelligence", sen)
  sen <- gsub("aaai conference on artificial intelligence", "AAAIArtificialIntelligence", sen)
  sen <- gsub("international conference on knowledge representation and reasoning", "ICKRR", sen)
  sen <- gsub("indian international conference on artificial intelligence", "IICArtificialIntelligence", sen)
  sen <- gsub("international conference on artificial intelligence planning and scheduling", "ICArtificialIntelligencePS", sen)
  sen <- gsub("international conference on artificial intelligence planning systems", "ICArtificialIntelligencePS", sen)
  sen <- gsub("ieee conference on artificial intelligence for applications", "IEEECArtificialIntelligenceA", sen)
  sen <- gsub("international conference on artificial intelligence applications and innovations", "ICArtificialIntelligenceAI", sen)
  sen <- gsub("international conference on artificial intelligence and law", "ICArtificialIntelligenceL", sen)
  sen <- gsub("innovative applications of artificial intelligence conference", "IArtificialIntelligenceAC", sen)
  sen <- gsub("international journal of artificial intelligence in education", "IJArtificialIntelligenceE", sen)
  sen <- gsub("international journal artificial intelligence in education", "IJArtificialIntelligenceE", sen)
  sen <- gsub("european conference on artificial intelligence", "ECArtificialIntelligence", sen)
  sen <- gsub("international conference on the application of artificial intelligence", "ICAartificialIntelligence", sen)
  sen <- gsub("national conference on artificial intelligence", "AAAIArtificialIntelligence", sen)
  sen <- gsub("international conference on artificial intelligence in medicine", "ArtificialIntelligenceME", sen)
  sen <- gsub("artificial intelligence in medicine europe", "ArtificialIntelligenceME", sen)
  sen <- gsub("conference on artificial intelligence and interactive digital entertainment", "CArtificialIntelligenceIDE", sen)
  sen <- gsub("international conference on artificial intelligence", "ICArtificialIntelligence", sen)
  sen <- gsub("association for the advancement of artificial intelligence", "AssoAArtificialIntelligence", sen)
  sen <- gsub("american association for artificial intelligence", "AmAArtificialIntelligence", sen)
  sen <- gsub("israeli association for artificial intelligence", "ISArtificialIntelligence", sen)
  sen <- gsub("international association for artificial intelligence and law", "ISArtificialIntelligenceL", sen)
  sen <- gsub(" aips", "ICArtificialIntelligencePS", sen)
  sen <- gsub(" aiide|aiide conference", "CArtificialIntelligenceIDE", sen)
  sen <- gsub(" ncai|ncai conference", "NCArtificialIntelligence", sen)
  sen <- gsub(" aaai conference", "AAAIArtificialIntelligence", sen)
  sen <- gsub('artificial( |\\-)intelligence series', "ArtificialIntelligence", sen)
  sen <- gsub('artificial( |\\-)intelligence (\\(ai\\)|\\(a\\.i\\.\\)|\\(al\\)|\\(at\\))', "ArtificialIntelligence", sen)
  sen <- gsub('artificial( |\\-)intelligence(, | )(ai|a\\.i\\.|al|at)($|[[:punct:]])', "ArtificialIntelligence ", sen)
  sen <- gsub('(ai|a\\.i\\.|al|at) \\(artificial( |\\-)intelligence\\)', "ArtificialIntelligence", sen)
  sen <- gsub('artificial( |\\-)intelligence', "ArtificialIntelligence", sen, perl = T)
  sen <- gsub('( |^)(ai|a.i.)( |$|\\-|\\/)', " ArtificialIntelligence ", sen)
  sen <- gsub('[[:punct:]]ArtificialIntelligence [[:punct:]]', " ArtificialIntelligence ", sen, perl = F)
  
  if (length(sen) > 0) {
    for (j in 1:length(sen)) {
      sen.1 <- sen[j]
      for (k in 1:nrow(keywords)) {
        old <- gsub("([[:punct:]])", "\\\\\\1", keywords$keywords[k])
        new <- keywords$keywords.standard[k]
        sen.1 <- gsub(old,
                        new,
                        sen.1)
      }
      abstract.df <- data.frame(id = as.character(data$UT[i]),
                                abstract = sen.1,
                                stringsAsFactors = F)
      abstract.table <- rbind(abstract.table,
                              abstract.df)
    }
  }
}

write.csv(abstract.table, "abstract.sample.new.csv", row.names = F)
rm(list = ls())


#### 2.2 Add sentence number

abstract <- read.csv("abstract.sample.new.csv", 
                     stringsAsFactors = F)
abstract.1 <- data.frame(id = character(0),
                         sentence.no = numeric(0),
                         abstract = character(0))
id.list <- as.character(unique(abstract$id))
for (i in 1:length(id.list)) {
  abstract.df <- abstract[which(abstract$id == id.list[i]),]
  abstract.df$sentence.no <- 1:nrow(abstract.df)
  abstract.df <- abstract.df[,c(1, 3, 2)]
  abstract.1 <- rbind(abstract.1,
                      abstract.df)
}

# write.csv(abstract.1, "abstract.sample.csv", row.names = F)
write.csv(abstract.1, "abstract.sample.new.csv", row.names = F)
rm(list = ls())

#### 2.3 NLP processing

library(coreNLP)
initCoreNLP()

abstract <- read.csv("abstract.sample.new.csv",
                 stringsAsFactors = F) # 22896 sentences from 17184 papers
output.final <- data.frame(id = character(0),
                           sentence = numeric(0),
                           term = character(0),
                           lemma = character(0),
                           govenor = character(0),
                           relation = character(0),
                           target = character(0),
                           POS = character(0),
                           NER = character(0),
                           stringsAsFactors = F)

ai.list <- c("ArtificialIntelligence", "DisArtificialIntelligence", "GamArtificialIntelligence")
for (i in 1:nrow(abstract)) {
  output <- data.frame(getDependency(annotateString(abstract$abstract[i])))
  token <- data.frame(getToken(annotateString(abstract$abstract[i])))
  ###### check this line later
  id.ai <- token$id[token$token %in% ai.list]
  output.1 <- output[(output$governorIdx %in% id.ai | output$dependentIdx %in% id.ai),]
  if (nrow(output.1) > 0) {
    for (m in 1:nrow(output.1)) {
      id <- abstract$id[i]
      order <- ifelse(sum(grepl("ArtificialIntelligence", output.1[m, 2:3]) == F) == 0,
                      1,
                      which(grepl("ArtificialIntelligence", output.1[m, 2:3]) == F))
      sentence <- abstract$sentence.no[i]
      term <- c(output.1[m, 2], output.1[m,3])[order]
      lemma <- token$lemma[which(token$sentence == output.1$sentence[m] & token$token == term)][1]
      governor <- ifelse(order == 1,
                         "governor", "dependent")
      target <- c(output.1[m, 2], output.1[m,3])[3-order]
      relation <- as.character(output.1$type[m])
      POS <- token$POS[which(token$sentence == output.1$sentence[m] & token$token == term)][1]
      NER <- token$NER[which(token$sentence == output.1$sentence[m] & token$token == term)][1]
      output.final <- rbind(output.final,
                            data.frame(id = id,
                                       sentence = sentence,
                                       term = term,
                                       lemma = lemma,
                                       governor = governor,
                                       relation = relation,
                                       target = target,
                                       POS = POS,
                                       NER = NER,
                                       stringsAsFactors = F))
    }
  }
}

write.csv(output.final, "nlp_result_final.csv", row.names = F) # 45440

# Depulicate and summarize
nlp_output <- read.csv("nlp_result_final.csv", stringsAsFactors = F) # 45440
summary <- data.frame(table(nlp_output$lemma))
nlp_output <- nlp_output[which(duplicated(nlp_output) == F),]
summary <- summary[order(summary$Freq, decreasing = T),]

write.csv(nlp_output, "nlp_result_final_dedup.csv", row.names = F) # 44676
write.csv(summary, "nlp_summary.csv", row.names = F)
rm(list = ls())
