library(twitteR)
library(rjson)
library(httr)

####
# I assume you have a file, config.R outside of the github project folder
# (thus not shared on the internet)
# which contains your twitter developer credentials in the form:
# api_key <- "your_api_key_here"
# api_secret <- "your_api_secret_here"
# access_token <- "your_access_token_here"
# access_token_secret <- "your_access_secret_here"
# so they are stored in active memory with 
source("../config.R")
####
# I'm not going to cache credientials:
options(httr_oauth_cache=F)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# get the nzparliament list of MP twitter handles
# code based on
# https://stackoverflow.com/questions/30063597/obtaining-twitter-screen-names-from-a-twitter-list
# but modified to actually work these days by swapping GET for POST
twowner <- "NZParliament"
twlist <- "mps"
api.url <- paste0("https://api.twitter.com/1.1/lists/members.json?slug=",
                  twlist, "&owner_screen_name=", twowner, "&count=200")
response <- GET(api.url, config(token=twitteR:::get_oauth_sig()))
response.list <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
mps_screennames <- sapply(response.list$users, function(i) i$screen_name)
twlist <- "parties"
api.url <- paste0("https://api.twitter.com/1.1/lists/members.json?slug=",
                  twlist, "&owner_screen_name=", twowner, "&count=200")
response <- GET(api.url, config(token=twitteR:::get_oauth_sig()))
response.list <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
parties_screennames <- sapply(response.list$users, function(i) i$screen_name)

seed_accounts <- c(mps_screennames, parties_screennames)
seed_details <- lapply(seed_accounts, getUser)
##getting the followers is going to take a while, because rate limits
followerdata <- lapply(seed_details, function(x){x$getFollowers(retryOnRateLimit=180)})



