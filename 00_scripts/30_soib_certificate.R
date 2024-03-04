###     Script by Ashish Jha       ###
###     SoIB 2023 certificates     ###

# Load libraries
library(magick)
library(dplyr)
library(extrafont)
library(stringr)

loadfonts(device = "win", quiet = TRUE)

# Set the loop
Observers <- read.csv("India_eBirders_20230815.csv") %>%
  dplyr::select(Get_User_Display_Name, lists, Code, text_size)


for (i in 1:nrow(Observers)){

  selected_row = Observers[i, ]
  
  name = selected_row$Get_User_Display_Name
  list = paste(selected_row$lists, "checklists")
  
  text_size = selected_row$text_size
  
  # certificate base; can be created using Canva or similar
  path_cert_base <- "30_certificates/certificate_base.jpg"
  
  path_jpg <- paste0("Certificates/", "SoIB_", selected_row$Code, ".jpg")
  path_pdf <- paste0("Certificates/", "SoIB_", selected_row$Code, ".pdf")
  
  image <- image_read(path_cert_base) # Load Certificate
  Image1 <- image_annotate(image, name, size = text_size, color = "black", 
                           location="+0-300", gravity = "Center",
                           font = "Edwardian Script ITC")
  image_write(Image1, path = path_jpg, format = "JPEG")
  
  image <- image_read(path_jpg) # Load Certificate
  Image2 <- image_annotate(image, list, size = 70, color = "gray35", 
                           location = "+500+1070",
                           font = "Microsoft Sans Serif bold")
  image_write(Image2, path = path_jpg, format = "JPEG")
  
  image <- image_read(path_jpg) # Load Certificate
  Image3 <- image_annotate(image, "THANK YOU", size = 70, color = "gray35", 
                           location = "+500+1370",
                           font = "Microsoft Sans Serif")
  image_write(Image3, path = path_jpg, format = "JPEG")
  
  image <- image_read(path_jpg) # Load Certificate
  Image4 <- image_annotate(image, "THANK YOU", size = 70, color = "gray35", 
                           location = "+500+1370",
                           font = "Microsoft Sans Serif")
  image_write(Image4, path = path_pdf, format = "PDF")
  
}
  
