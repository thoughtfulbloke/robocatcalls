# robocatcalls
some code for analysing the role of Twitter robot accounts in the 2017 New Zealand general election

Note, this code requires a config.R file outside the program directory, which sets you twitter developer credentials (because I am keeping that kind of personal information out of the code)

config.R should set:

```
api_key <- "your_api_key_here"
api_secret <- "your_api_secret_here"
access_token <- "your_access_token_here"
access_token_secret <- "your_access_secret_here"
```

I am using seed lists of New Zealand politicians and poltical parties to generate the followers of those accounts, so thanks to @NZParliament for providing them.
