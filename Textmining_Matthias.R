#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

## Umgebung leeren ## 

rm(list=ls(all=TRUE))

## Daten laden ## 
#path <- "YOURDATAPATH"
reviews <- read.csv('reviews.csv',colClasses=c("character","factor"))

## Pakete laden ## 

library(stringr)
library(dplyr)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

## Data Cleanup & Tokenization ## 

clean_string <- function(string){
  # Lowercase
  temp <- tolower(string)
  # Replace all numbers with "0"
  temp <- gsub('[\\.,]*[0-9]+', '0',string)
  temp <- gsub('0+', '0',temp)
  # Remove everything that is not a number or letter 
  temp <- stringr::str_replace_all(temp,"[^[A-Za-zÄÖÜäöüß0-9]\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

# converts list to a vector
long_format <- function(texts){                           
  doc_count <- length(texts)
  doc_id <- 1:length(texts)
  text_vec <- t(c(0,0))
  for (a in 1:doc_count){
    word_count <- length(texts[[a]])
    for (b in 1:word_count){
      text_vec <- rbind(text_vec,cbind(as.integer(doc_id[a]),texts[[a]][b]))
    }
  }
  text_vec <- text_vec[-which(text_vec[,1]=='0'),]
  return(as.matrix(text_vec))
}  

# final function 
text_cleaner <- function(texts){
  text_new <- sapply(texts,clean_string,simplify=TRUE,USE.NAMES=FALSE)
  text_new <- long_format(text_new)
  text_new <- as.data.frame(text_new)
  names(text_new) <- c("doc_id","cleaned_text")
  return(text_new)
}

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

## CISTEM Implementation in R

# first function 
first_function <- function(text){
  # replacements 
  text <- text %>%
    gsub(pattern = "ß", replacement="ss") %>%
    gsub(pattern = "sch", replacement="\\$") %>%
    gsub(pattern = "ch", replacement="\\(") %>%
    gsub(pattern = "ck", replacement="\\{") %>%
    gsub(pattern = "st", replacement="§") %>%
    gsub(pattern = "ü", replacement="u") %>%
    gsub(pattern = "ä", replacement="a") %>%
    gsub(pattern = "ö", replacement="o") %>%
    gsub(pattern = "ei", replacement="%") %>%
    gsub(pattern = "ie", replacement="&") %>%
    gsub(pattern = "au", replacement = "@") %>%
    gsub(pattern = "eu", replacement = "€") %>%
    gsub(pattern = "sp", replacement = "S")
  alphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  for ( k in 1:length(alphabet) ){
    text <- text %>% 
      gsub(pattern = paste(alphabet[k],alphabet[k],sep=""), replacement = paste(alphabet[k],"*",sep="") )
  }

  # remove "ge" in the beginning if wordlength > 6
  vector_length <- length(text)
  for (z in 1:vector_length){
    if( nchar(text[z]) > 7){
      text[z] <- text[z] %>% gsub(pattern = "^vor", replacement="")
    }
    if( nchar(text[z]) > 7){
      text[z] <- text[z] %>% gsub(pattern = "^ver", replacement="")
    }
    if( nchar(text[z]) > 7){
      text[z] <- text[z] %>% gsub(pattern = "^zer", replacement="")
    }
    if( nchar(text[z]) > 6){
      text[z] <- text[z] %>% gsub(pattern = "^ge", replacement="")
    }
    if( nchar(text[z]) > 6){
      text[z] <- text[z] %>% gsub(pattern = "^be", replacement="")
    }
    if( nchar(text[z]) > 6){
      text[z] <- text[z] %>% gsub(pattern = "^un", replacement="")
    }
  }
  # return output
  return(text)
}

# second function 
second_function <- function(text){
  # if wordlength > 5 
  vector_length <- length(text)
  
  for (t in 1:vector_length){
    if( nchar(text[t]) > 6){
      text[t] <- text[t] %>% gsub(pattern = "h%t$", replacement="")
    }
    if( nchar(text[t]) > 6){
      text[t] <- text[t] %>% gsub(pattern = "k%t$", replacement="")
    }
    if( nchar(text[t]) > 6){
      text[t] <- text[t] %>% gsub(pattern = "bar$", replacement="")
    }
    if( nchar(text[t]) > 6){
      text[t] <- text[t] %>% gsub(pattern = "li\\($", replacement="")
    }
    if( nchar(text[t]) > 6){
      text[t] <- text[t] %>% gsub(pattern = "tum$", replacement="")
    }
    if( nchar(text[t]) > 6){
      text[t] <- text[t] %>% gsub(pattern = "ung$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "&r$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "ig$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "i\\$$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "in$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "en$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "er$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "em$", replacement="")
    }
    if( nchar(text[t]) > 5){
      text[t] <- text[t] %>% gsub(pattern = "nd$", replacement="")
    } 
    if( nchar(text[t]) > 4){
      text[t] <- text[t] %>% gsub(pattern = "t$", replacement="")
    }
    if( nchar(text[t]) > 4){
      text[t] <- text[t] %>% gsub(pattern = "e$", replacement="")
    }
    if( nchar(text[t]) > 4){
      text[t] <- text[t] %>% gsub(pattern = "s$", replacement="")
    }
    if( nchar(text[t]) > 4){
      text[t] <- text[t] %>% gsub(pattern = "n$", replacement="")
    }
  }
  # return output 
  return(text)
}

# undo star replacement (part of third function)
star_replacer <- function(word){
  if ( any( str_detect(word,'\\*') ) ){
    star_count <- dim(str_locate_all(word, '\\*')[[1]])[1]
    for ( i in 1:star_count ){
      star_pos <- str_locate_all(word, '\\*')[[1]][1] 
      star_replacer_pos <- star_pos - 1
      star_replacer <- substr(word, star_replacer_pos, star_replacer_pos)
      substr(word, star_pos, star_pos) <- star_replacer
    }
  }
  return(word)
} 


# third function 
third_function <- function(text){
  text <- text %>%
    gsub(pattern = "^hat$", replacement = "habe") %>%
    gsub(pattern = "^hat\\*$", replacement = "habe") %>%
    gsub(pattern = "^ha§$", replacement = "habe") %>%
    gsub(pattern = "^habt$", replacement = "habe" )%>%
    gsub(pattern = "^gehab$", replacement = "habe" )
  
  text <- text %>%
    gsub(pattern = "^i§$", replacement = "sein")  %>%
    gsub(pattern = "^bin$", replacement = "sein")  %>%
    gsub(pattern = "^bi§$", replacement = "sein")  %>%
    gsub(pattern = "^sind$", replacement = "sein")  %>%
    gsub(pattern = "^s%d$", replacement = "sein")  %>%
    gsub(pattern = "^war$", replacement = "sein")  %>%
    gsub(pattern = "^war§$", replacement = "sein")  %>%
    gsub(pattern = "^ware$", replacement = "sein") %>%
    gsub(pattern = "^ward$", replacement = "sein") %>%
    gsub(pattern = "^werd$", replacement = "sein") %>%
    gsub(pattern = "^wurd$", replacement = "sein") %>%
    gsub(pattern = "^wurde§$", replacement = "sein")  %>%
    gsub(pattern = "^word$", replacement = "sein") %>%
    gsub(pattern = "^geword$", replacement = "sein") 
  
  #   gsub(pattern = "^wart$", replacement = "sein") 
  #   gsub(pattern = "^wars$", replacement = "sein") 
  
  text <- text %>% 
    gsub(pattern = "\\$", replacement="sch") %>% 
    gsub(pattern = "§", replacement="st") %>% 
    gsub(pattern = "\\(", replacement="ch") %>% 
    gsub(pattern = "\\{", replacement="ck") %>% 
    gsub(pattern = "%", replacement="ei") %>% 
    gsub(pattern = "&", replacement="ie") %>%
    gsub(pattern = "@", replacement = "au") %>%
    gsub(pattern = "€", replacement = "eu") %>%
    gsub(pattern = "S", replacement = "sp")
  text <- sapply(text,star_replacer)
  return(as.vector(text))
}

# final function 
cistem_stemmer <- function(text){
  text_new <- first_function(text)
  text_new_old <- rep(0,length(text_new))
  # second function loop
  while ( sum(text_new != text_new_old) != 0)  {
    text_new_old <- text_new
    text_new <- second_function(text_new)
  }
  # third function
  text_new <- third_function(text_new)
  return(text_new)
}

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

# apply final functions on reviews
reviews_new <- text_cleaner(reviews$text)
reviews_new$cleaned_text <- tolower(reviews_new$cleaned_text)
reviews_new$preprocessed_text <- cistem_stemmer(reviews_new$cleaned_text)
write.csv(reviews_new, "reviews_preprocessed.csv",row.names=FALSE)
