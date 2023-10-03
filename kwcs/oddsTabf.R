oddsTabf = function(...){
  arglist = list(...)
  mod1 = arglist[[1]]
  tt = map(arglist, oddf) %>%
    reduce(full_join, by=c("variables", "values"))
  vl = c(length(tt)-2)
  ys =  mod1$formula[2] %>% as.character() %>% str_replace(., "\\=\\=", "being reference of") %>%
    str_replace_all(., '\\"', "")
  tt = tt %>% setNames(c("Variables", "Values", paste0("Model.", as.roman(1:vl))))
  tt %>%  `rownames<-`(NULL) %>%
    group_by(Variables) %>%
    mutate(rank = row_number()) %>%
    mutate(Variables = ifelse(rank == min(rank), Variables, "")) %>%
    mutate_at(., vars(starts_with("Model")), ~replace(., is.na(.), "")) %>%  
    ungroup() %>% select(-rank) %>%
    addHtmlTableStyle(align = 'll') %>%
    htmlTable(
      caption = sprintf("Table. OR(95%%CI) for %s", ys)
      
    )
  
}