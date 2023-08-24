
#................................................................................................. WhatsApp
# Installation of the whatsapp R package
# remotes::install_github("JBGruber/rwhatsapp")

# This R package allows to import your exported WhatsApp chat history. So what it does is to take a history file from the 'WhatsApp' messenger app (txt or zip) and
# return a formatted data.frame with descriptions of the used emojis. There is no connection to an API which means we can not access data of other users.
#
# ChatGPT: To access WhatsApp data of other users using an API in R or Python programming, it's important to note that WhatsApp does not officially provide a public API for accessing user data.
#          The WhatsApp's API is primarily intended for businesses to interact with their customers and is not designed for general data access.

#................................................................................................. facebook

# It seems the 'Rfacebook' R package does not work and is not actively maintained. Facebook changed
# the API policy in 2018 and now users should first register an application and apply for access which
# seems to take quite long for acceptance (https://github.com/pablobarbera/Rfacebook/issues/189#issuecomment-528585241)
#
# Downloading and processing of own facebook data is feasible: http://www.deeplytrivial.com/2018/06/working-with-your-facebook-data-in-r.html

#................................................................................................. tik-tok

# In the R package website is mentioned:
# https://github.com/JBGruber/traktok/issues

# The python package based on the 'reticulate' R package gave the following error after the installation of the required Python packages:
# "ImportError: cannot import name 'ChromeType' from 'webdriver_manager.core.utils' (/usr/local/lib/python3.8/dist-packages/webdriver_manager/core/utils.py)"
# sudo pip3 install browser-cookie3
# sudo pip3 install selenium
# sudo pip3 install webdriver-manager
# sudo pip3 install pyktok

# pyk = reticulate::import(module = 'pyktok', convert = TRUE)

#................................................................................................. twitter  [ !! it no longer works, see the "twitter_new_policy.png" file ]

# we need to create a token and have the keys, secrets and token saved in the .Renviron file of the home directory
twitter_token = rtweet::create_token(app = 'social_media',
                                     consumer_key = Sys.getenv("consumer_key"),
                                     consumer_secret = Sys.getenv("consumer_secret"),
                                     access_token = Sys.getenv("access_token"),
                                     access_secret = Sys.getenv("access_secret"),
                                     set_renv = FALSE)

if (Sys.getenv("consumer_key") == "" ||
    Sys.getenv("consumer_secret") == "" ||
    Sys.getenv("access_token") == "" ||
    Sys.getenv("access_secret") == "") {
  stop("The '.Renviron' file MUST include all necessary twitter credentials! Make sure that I have access to this file when I run the app!", call. = F)
}

# SEE:  https://stackoverflow.com/a/58790480 which explains the 'OR' of the next lines
TAGS = c("#stopwarinsudan","#NRC_EAY")
SEARCH = paste(TAGS, collapse=" OR ")
SEARCH = glue::glue("{SEARCH} -filter:retweets -filter:quote -filter:replies")

get_tweets = rtweet::tweet_search_all(query = SEARCH,
                                   n = 500,
                                   include_rts = FALSE,
                                   retryonratelimit = TRUE,
                                   lang = 'en',
                                   token = twitter_token,
                                   clean_tweets = TRUE,                     # clean_tweets: logical indicating whether to remove non-ASCII characters in text of tweets. defaults to FALSE  [ SEE: https://www.rdocumentation.org/packages/rtweet/versions/0.3.6/topics/search_tweets ]
                                   verbose = TRUE)
# to register an Application
rtweet::rtweet_app()

