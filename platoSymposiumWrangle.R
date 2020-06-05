# Adrian J Woods ----
# Plato Symposium Greek/English Dictionary ----
# 2020.06.03

# 1.0 Setup ---- 
# 1.1 Load some libraries ----

library(tidyverse)
library(tidytext)
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen) 



# 1.2 Import the Symposium ----
symposium_words_list <- read_rds("platoSymposiumWordList.rds")

# 2.0 Prepare Wordlist for WebSearch ----

# 2.1 Research ----
# wiktionary.org has a simple url for catalogueing words. That makes it 
# simple to add to my list of words from the Symposium. 

# 2.2 Glue URl ----
#This segment simply glues on the https to the front of each word. 
symposium_url_list <- symposium_words_list %>%
    # filter distinct words
    count(word, sort = TRUE) %>%
    select(word) %>%
    mutate(url = str_glue("https://en.wiktionary.org/wiki/{word}")) %>%
    arrange(word) %>%
    slice(-1:-4) %>%
    arrange(desc(word))

# 2.3 Sample List ----
# I need a working list, and a non-working list in order to 
# test out the function
working_sample_url_list <- symposium_url_list %>%
    slice(2, 4)

nonworking_sample_url_list <- symposium_url_list %>%
    slice(1:2)

# 3.0 Building Some Functions ----
# 3.1 Sample URL ----
# Saving this working url and nonworking for testing.
url <- "https://en.wiktionary.org/wiki/ᾤχετο"
url_broke <- "https://en.wiktionary.org/wiki/ᾠχόμην"
        # followed by a quick opening of the url - It works.
xopen(url)

# 3.2 Using chrome devleoper ----
# I work my way through the html to find 
# the html tags that hold the english definitions.
read_html(url) %>%
    html_nodes("#mw-content-text") %>%
    html_nodes("ol") %>%
# map will pull out just the text from the tag. 
    map(html_text) %>%
    unlist() %>%
# from experience I know there is some clean up that sometimes needs to happen
# str_replace_all does the trick. 
    str_replace_all("\n", " ")

# 3.3 Create Function ----

get_greek_definition <- function(url) {
    wiktionary_definition <- read_html(url) %>%
        html_nodes("#mw-content-text") %>%
        html_nodes("ol") %>%
        map(html_text) %>%
        unlist() %>%
        str_replace_all("\n", " ")
 
    tibble(
        definition = wiktionary_definition
    ) 
}

# 3.4 Test the function - IT Works! Progress
get_greek_definition(url)
    
# 3.5 Test function with my working sample list
# this gives me my tibble with the greek word, a url, and a nest definition
working_sample_url_list %>%
    mutate(definition = map(url, get_greek_definition))
    
# 3.6 Test function with nonworking samle url list
nonworking_sample_url_list %>%
    mutate(definition = map(url, get_greek_definition))
# this will give you a HTTP error 404. The reason we get this break
# is because the function breaks when it cannot find the requested html.
# fortunately R, naturally, has a fix for that

# 4.0 Safely Test ----
# 4.1 Here is how safely works ----
safely(read_html, 'empty page')(url)

# 4.2 take that line of code and create a function to test my urls ----
test_my_urls <- function(url) {
    safely(read_html, 'empty page')(url)
}

# 4.3 Let's see how the function works. ----
working_sample_url_list %>%
    mutate(definition = map(url, test_my_urls)) %>%
    select(- url) %>%
    unnest(cols = c(definition)) %>%
    view()

# The non working list will leave an Empty Page variable. This will be easy to navigate.
nonworking_sample_url_list %>%
    mutate(definition = map(url, test_my_urls)) %>%
    select(- url) %>%
    unnest(cols = c(definition)) %>%
    view()

# 5.0 Create a list of working and non working url's ----

working_symposium_url_list <- symposium_url_list %>%
    mutate(definition = map(url, test_my_urls))
# that took 25 minutes - Watch out!
# save this to an rds so we dont have to do that again.
working_symposium_url_list %>%
    write_rds("working_symposium_url_list.rds")

# working_symposium_url_list <- read_rds("working_symposium_url_list.rds")

# 5.1 Filter out the non working url's ----
# this list doesn't have the actual URL's so I will need to add a column of TRUE statemnts
# that way I can join with the original Symposium_url_list and then filter out the non working urls
list_working_urls <- working_symposium_url_list %>%
    select(- url) %>%
    unnest(cols = c(definition)) %>%
    #slice(1:20) %>%
    filter(str_detect(definition, "pointer")) %>%
    mutate(weblink_works = TRUE) %>%
    select(word, weblink_works)
    
    
# 5.3 Join the two list and filter for just the working urls ----
final_list_of_working_urls <- symposium_url_list %>%
    left_join(list_working_urls) %>%
    filter(weblink_works == TRUE) %>%
    select(word, url)

# 6.0 Ready to get Greek Definitions ----
symposium_lexicon <- final_list_of_working_urls %>%
    mutate(definition = map(url, get_greek_definition))

symposium_lexicon %>%
    write_rds("symposium_lexicon.rds")
 
symposium_lexicon_clean <- symposium_lexicon %>% 
    select(- url) %>%
    unnest(cols = c(definition)) 

# 7.0 Save the Final Product ----    
symposium_lexicon_clean %>%
    write_rds("symposium_lexicon_wiktionary.rds")
    
