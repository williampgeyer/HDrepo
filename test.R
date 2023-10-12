# # github setup
# from: https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r
# library(usethis)
# use_git()
# create_github_token()
# 
# library(gitcreds)
# gitcreds_set()
# 
# use_github()

# libraries
library(data.table)
library(ggplot2)

dt.test = data.table(x=rnorm(100),y=rnorm(100))
ggplot(dt.test,aes(x,y)) + geom_point()
