#' @title Function to remove some forms of pluralization. 
#' @description This function takes a character vector as input and removes some forms of pluralization
#' from the ends of the words. 
#' @param term.vec A character vector
#' 
#' @details The entries of the vector should be single words or short n-grams without punctuation as the 
#' function only looks at the ends of strings. In other words, if entries are a paragraph of text. Only the 
#' final words will get de-pluralized. (Even then, if the final character is a period, as would be the 
#' case with paragraphs, it's likely that nothing will be de-pluralized.)
#' @note WARNING: This does make mistakes for irregular words. You should check its results manually. It tends to fail spectacularly for words ending in "es".
#' @export
#' @examples
#' myvec <- c("banana", "bananas", "scientists", "large_armies")
#'
#' CorrectS(term.vec=myvec)
#'
#'       original   adjusted changed
#' 1       banana     banana   FALSE
#' 2      bananas     banana    TRUE
#' 3   scientists  scientist    TRUE
#' 4 large_armies large_army    TRUE

CorrectS <- function(term.vec){
	# makes some adjustments to pluralization
	# WARNING: This does make mistakes for irregular words. You should check its results manually.
    s.adjust <- gsub("sses$", "ss", term.vec) 
    keep.list <- s.adjust[ grepl("sis$|ss$|us$|species$", s.adjust) | nchar(term.vec) <= 3 | grepl( "_[a-zA-Z][a-zA-Z][a-zA-Z]$", s.adjust) ]
    
    s.adjust2 <- gsub("ies$", "y", s.adjust)
    s.adjust2 <- gsub("s$", "", s.adjust2)
    
    out.list <- s.adjust2
    out.list[ s.adjust %in% keep.list ] <- s.adjust[ s.adjust %in% keep.list ]
    
    result <- data.frame(original=term.vec, adjusted=out.list, changed=term.vec!=out.list, stringsAsFactors=FALSE)
    return(result)
}


CorrectS2 <- function(term.vec){
  # makes some adjustments to pluralization
  # WARNING: This does make mistakes for irregular words. You should check its results manually.
  
  # Order of the depluralization rules matters: 
  #    assesses "ss" replacement should happen before "es" replacement
  #    otherwise, assesses maps to asse
  
  deplurals <- 
    c("[^-]ss$" = "-s",
      "ies$" = "-y",
      "sses$" = "-ss",
      "zzes$" = "-zz",
      "([^z])zes$" = "\\1-z",
      "es$" = "-s",
      "([^s])s$" = "\\1-",
      "-" = "" #,
#      "^ass$" = "assess",
#      "^specy$" = "species"
    )
  
  terms.singular <- 
    data.frame(term = term.vec, stringsAsFactors = FALSE) %>% 
    mutate(replacement = str_replace_all(as.vector(term), deplurals),
           changed = term != replacement
    )
  
  return(terms.singular)
  
}

test.CorrectS2 <- function() {
  plurals <-
    c("arches", "atlases", "axes", "bashes", "benches", "biases", "botches", "boxes", "brushes", "bunches", "buses", "bushes", "canvases", "catches", "churches", "classes", "compasses", "crashes", "crosses", "daises", "dishes", "dresses", "equinoxes", "etches", "fetches", "fixes", "foxes", "gases", "grasses", "itches", "kisses", "larches", "lashes", "latches", "mantises", "marches", "marshes", "mashes", "masses", "matches", "mosses", "mixes", "passes", "patches", "poxes", "radishes", "sashes", "sketches", "starches", "stitches", "taxes", "touches", "trashes", "twitches", "vehicles", "wishes", "witches", "wrenches", 
      "buzzes", "fizzes", "klutzes", "quizzes", "topazes", "waltzes", 
      "alleys", "attorneys", "essays", "boys", "delays", "guys", "jays", "keys", "ospreys", "plays", "rays", "strays", "toys", "trays", "turkeys", "valleys", "ways", 
      "allies", "armies", "babies", "beauties", "berries", "cherries", "cities", "colonies", "countries", "dictionaries", "duties", "enemies", "fairies", "families", "ferries", "flies", "galleries", "histories", "injuries", "jellies", "kitties", "ladies", "lilies", "navies", "histories", "parties", "ponies", "replies", "secretaries", "skies", "spies", "stories", "studies", "symphonies", "theories", "trophies", "tries", "universities", "varieties", "victories",
      "asses", "assess", "assesses", "mathematics")
  
  known.good <-
    c("arch", "atlas", "axe", "bash", "bench", "bias", "botch", "box", "brush", "bunch", "bus", "bush", "canvas", "catch", "church", "class", "compass", "crash", "cross", "daisy", "dish", "dress", "equinox", "etch", "fetch", "fix", "fox", "gas", "grass", "itch", "kiss", "larch", "lash", "latch", "mantis", "march", "marsh", "mash", "mass", "match", "moss", "mix", "pass", "patch", "pox", "radish", "sash", "sketch", "starch", "stitch", "tax", "touch", "trash", "twitch", "vehicle", "wish", "witch", "wrench", 
      "buzz", "fizz", "klutz", "quiz", "topaz", "waltz", 
      "alley", "attorney", "essay", "boy", "delay", "guy", "jay", "key", "osprey", "play", "ray", "stray", "toy", "tray", "turkey", "valley", "way", 
      "ally", "army", "baby", "beauty", "berry", "cherry", "city", "colony", "country", "dictionary", "duty", "enemy", "fairy", "family", "ferry", "fly", "gallery", "history", "injury", "jelly", "kitty", "lady", "lily", "navy", "history", "party", "pony", "reply", "secretary", "sky", "spy", "story", "study", "symphony", "theory", "trophy", "try", "university", "variety", "victory",
      "ass", "assess", "assess", "mathematics")
  
  result <-
    CorrectS2(plurals) %>%
    mutate(known.good = known.good,
           correct = replacement == known.good
    )
  
  return(result)
}

time.CorrectS.CorrectS2 <- function() {

  terms <- 
    read.csv(stringsAsFactors = FALSE,
             header = FALSE,
             file = "https://raw.githubusercontent.com/gwkkwg/lift/master/data/wordlist.text")
  
  names(terms) <- c("term")
  
  terms <- rbind(terms, terms, terms, terms, terms, terms, terms, terms)
  terms <- rbind(terms, terms, terms, terms, terms, terms, terms, terms)
  terms <- rbind(terms, terms, terms, terms, terms, terms, terms, terms)
  terms <- rbind(terms, terms)
  
  system.time(CorrectS(as.vector(terms$term)))
  system.time(CorrectS2(as.vector(terms$term)))
  
}
