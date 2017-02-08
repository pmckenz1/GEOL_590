library(rvest)
library(stringr)

new_link <- "https://en.wikipedia.org/wiki/Special:Random"

title <- character(0)
for (o in 1:50) {
test.page<-read_html(new_link)
page.title.full<-as.character(html_nodes(test.page,"head title"))
title <- c(title, str_sub(page.title.full,str_locate(page.title.full,"<title>")[2]+1,str_locate(page.title.full," - Wiki")[1]-1))

node.vector <- logical(0)
for (i in 1:length(html_nodes(test.page,css="p"))) {
  node.vector <- c(node.vector,str_detect(as.character(html_nodes(test.page,css="p")[i]), "<b>"))
}

beginning.text<-as.character(html_nodes(test.page,css="p")[node.vector][1])

sub_start <- as.numeric(str_locate(beginning.text,' <a href=\\"/wiki')[1,2])-3 #excludes parantheses
sub_end <- min(str_locate_all(beginning.text,"</a>")[[1]][,1][which((str_locate_all(beginning.text,"</a>")[[1]][,1]-sub_start) > 0)])-1
first_link_long<-str_sub(beginning.text,sub_start,sub_end)

sub_start <- str_locate(first_link_long,"wiki/")[2]+1
sub_end <- min(str_locate_all(first_link_long," ")[[1]][,1][which(str_locate_all(first_link_long," ")[[1]][,1]-sub_start > 0)])-2

first_link <- str_sub(first_link_long,sub_start,sub_end)

new_link <- paste0("https://en.wikipedia.org/wiki/",first_link)
}
length(title) == length(levels(as.factor(title)))

test.page<-read_html("https://en.wikipedia.org/wiki/German_language")

grep("\\(<a",beginning.text)




external.links<-html_nodes(test.page,css="p a")[grep("/wiki",html_nodes(test.page,css="p a"))]

if (length(grep("Help:",external.links)) > 0) {
  external.links <- external.links[-grep("Help:",external.links)]
}

external.links[1]



