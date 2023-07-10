library(RSelenium)
library(rvest)
library(glue)

rD <- rsDriver(verbose = TRUE,
               chromever = "114.0.5735.90",
               port = 2031L)


remDr <- rD[["client"]]
remDr$navigate("https://www.praxisplan.at/suche?name=&specialization=&zip=&gender=&diploma=&specialty=&insurance=&week_day%5Bmonday%5D=false&week_day%5Btuesday%5D=false&week_day%5Bwednesday%5D=false&week_day%5Bthursday%5D=false&week_day%5Bfriday%5D=false&week_day%5Bsaturday%5D=false&week_day%5Bsunday%5D=false&part_of_day%5Bmorning%5D=false&part_of_day%5Bafternoon%5D=false&locale=&accessibility%5Bwheelchair%5D=false&accessibility%5Belevator%5D=false&accessibility%5Bground_level%5D=false&accessibility%5Bmeasuring_data%5D=false")

# Store the HTML of the first page
first_page_html = remDr$getPageSource()[[1]]
op_first = "~/Desktop/htmls/1.html"
write(first_page_html, file = op_first)

for(i in 2:900){
  file = glue("~/Desktop/htmls/{i}.html")

  # find next url
  next_link <- remDr$findElement(using = "css", ".is-primary:not(.mb-30px)")
  next_url = next_link$getElementAttribute("href")[[1]]

  # navigate to next page
  remDr$navigate(next_url)

  if(file.exists(file)) next

  # Save the current HTML to a file
  current_html <- xml2::read_html(remDr$getPageSource()[[1]])
  xml2::write_html(current_html, file)

  # next_button <- remDr$findElement(using = "css", ".is-primary:not(.mb-30px)")
  # next_button$clickElement()

  print(paste0("clicked ", i, " times"))

  Sys.sleep(3)
}
