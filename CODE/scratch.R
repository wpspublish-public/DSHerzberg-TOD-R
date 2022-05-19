# apply new names to input files
input_files_ta <- map2(
  input_files_ta[1:2],
  list(
    new_names_input
  ), 
  ~ {
    n <- .y
    .x %>% 
      rename_with(
        ~
          n,
        everything()
      )}
) 


# apply new names to input files
input_files_ta <- map2(
  input_files_ta[1:12],
  c(
    rep(list(new_names_input), 12)
    # list(new_names_input_sre1, new_names_input_sre2), 
    # rep(list(new_names_input), 7)
  ), 
  ~ {
    n <- .y
    .x %>% 
      rename_with(
        ~
          n,
        everything()
      )}
) 



list(
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input_sre1, 
  new_names_input_sre2, 
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input,
  new_names_input
), 
