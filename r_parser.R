library(pdftools)
library(tesseract)
library(stringr)


extract_text <- function(path){
  
  # function for reading machine readable pdf files
  
  text_from_pdf <- pdf_text(path)
  text <- strsplit(text_from_pdf[[1]], "\r\n")[[1]]
  
  df <- data.frame(text)
  if (grepl("£", text_from_pdf)){
    df2 <- data.frame(str_split_fixed(df$text, "£", 5))
  } else {
    df2 <- data.frame(str_split_fixed(df$text, " (?=[^ ]+$)", 5))
  }
  return(df2)
}


extract_text_from_image <- function(path){
  
  # function for reading pdf files created from image
  
  img_file <- pdftools::pdf_convert(path, format = 'tiff', pages = 1, dpi = 400)
  
  text_ocr <- ocr(img_file)
  unlink(img_file)
  text <- strsplit(text_ocr[[1]], "\n")[[1]]
  
  df <- data.frame(text)
  if (grepl("£", text_ocr)){
    df2 <- data.frame(str_split_fixed(df$text, "£", 5))
  } else{
    df2 <- data.frame(str_split_fixed(df$text, " (?=[^ ]+$)", 5))
  }
  return(df2)
  
}


add_category <- function(menu_table){
  
  #add category label to rows with items
  
  menu_table$category <- ''
  category = ''
  
  for (i in 1:length(menu_table[[1]])){
    if (menu_table[i, 2] == ''){
      category = as.character(menu_table[i, 1])
    } else {
      menu_table[i, 6] = category
    }
  }
  menu_table <-  menu_table[menu_table$X2 != '',]
  colnames(menu_table) <- c("item", "price1", "price2", "price3", "price4", "category")
  menu_table <- menu_table[c("category", "item", "price1", "price2", "price3", "price4")]
  menu_table <- data.frame(lapply(menu_table, trimws), stringsAsFactors = FALSE)
}


pdf_to_text <- function(path){
  
  #main function
  text_from_pdf <- extract_text(path)
  if (length(text_from_pdf$text == 0)){
    text_from_pdf <- extract_text_from_image(path)
  }
  finale_table <- add_category(text_from_pdf)
  return(finale_table)
}


path <- 'menus_dataset\\menu_links_crawled 4158.pdf'
result <- pdf_to_text(path)
write.csv(result, file = "4158.csv")