df <- tibble(A = c("A", "B"), B = 1, C = "abc")

# df %>% 
#   group_by(A) %>% 
#   group_modify(~ {
#     
#     df <- .x 
#     
#     # iterate over indexed vector elements
#     imap(soib_year_info("cat_years"), ~ {
#       
#       df <- if (!exists("df2", envir = .GlobalEnv)) df else df2
# 
#       assign("df2",
#              df %>% bind_cols(tibble(
#                !!paste0("sl", .y) := 10 + .y,
#                !!paste0("slse", .y) := 0 + .y
#              )),
#              envir = .GlobalEnv)
#       
#     })
#     
#     df2
#     
#   })


df %>% 
  group_by(A) %>% 
  group_modify(~ {
    
    df2 <- .x
    
    .x %>% 
      reframe(
        
        !!!imap(soib_year_info("cat_years"), ~ {
          col_name_sl <- paste0("sl", .y)
          col_name_slse <- paste0("slse", .y)
          
          print(df2$B)
          
          # Generate some placeholder values for demonstration
          # Replace these with your actual calculation for sl and slse
          tibble(
            !!col_name_sl := 10 + .y,
            !!col_name_slse := 0 + .y
          )
        }) %>% bind_cols()
        
      )
    
  })
