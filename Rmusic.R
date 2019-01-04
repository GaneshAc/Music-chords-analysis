

#install.packages("chorrrds")

library(chorrrds)

devtools::install_github("r-music/chorrrds")

ls("package:chorrrds")


library(tidyverse)
#install.packages("tidyverse")
#install.packages("tidytext")
library(tidytext)

set.seed(20191)


# Step 1: Getting the chords for some Janis Joplin songs

songs <- "janis-joplin" %>% 
  chorrrds::get_songs() %>% 
  dplyr::sample_n(5)        # Selecting a random sample of 5 pieces 

# Step 2: getting the chords for the selected songs
chords <- songs %>% 
  dplyr::pull(url) %>%                     
  purrr::map(chorrrds::get_chords) %>%     # Mapping the function over the 
  # selected urls
  purrr::map_dfr(dplyr::mutate_if, is.factor, as.character)   %>% 
  chorrrds::clean(message = FALSE)         # Cleans the dataset, in case
# strange elements, as music lyrics, 
# are present when they shouldn't

chords %>% slice(1:10) %>% knitr::kable()



chords <- chords %>% 
  tidyr::separate(music, c("artist", "music"), 
                  sep = "(?<=joplin) ", extra = "merge")


chords %>% slice(1:10) %>% knitr::kable()


chords %>% 
  dplyr::group_by(music) %>% 
  dplyr::count(chord) %>%
  dplyr::top_n(n, n = 3) %>%
  dplyr::mutate(prop = scales::percent(n/sum(n))) %>% 
  knitr::kable()



chords %>%
  dplyr::group_by(music) %>% 
  tidytext::unnest_tokens(bigram, chord, to_lower = FALSE,
                          token = "ngrams", n = 2) %>% 
  dplyr::count(bigram) %>% 
  dplyr::top_n(n, n = 2) %>%
  knitr::kable()

devtools::install_github("mattflor/chorddiag")
library(chorddiag)


#Chord Digram

comp <- chords %>% 
  dplyr::mutate(seq = lead(chord)) %>% 
  dplyr::filter(chord != seq) %>% 
  dplyr::group_by(chord, seq) %>%  
  dplyr::summarise(n = n())

mat <- tidyr::spread(comp, key = chord, value = n, fill = 0)  
mm <- as.matrix(mat[-19, -1]) 

# Building the chords diagram
chorddiag::chorddiag(mm, showTicks = FALSE,
                     palette = "Blues")
