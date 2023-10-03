kwcsTabf = function(dat1, stratas, catVars, conVars){
  varOrder = tibble("variables"=c(catVars, conVars)) %>%
    mutate(order = row_number())
  
  catTab = dat1 %>%
    select(stratas, all_of(catVars)) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values")%>%
    group_by( variables, values) %>%
    count(!!sym(stratas)) %>%
    mutate(prob = n/sum(n), 
           smry= sprintf("%.0f (%.1f%%)", n, prob*100)
    ) %>%
    select(-n, -prob) %>%
    ungroup() %>%
    pivot_wider(names_from = stratas, values_from =smry) 
  
  conTab = 
    dat1 %>%
    select(stratas, all_of(conVars)) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values")%>%
    group_by( !!sym(stratas), variables) %>%
    summarise(avg = mean(values, na.rm =TRUE), 
              std = sd(values, na.rm =TRUE) 
    ) %>%
    mutate(smry  = sprintf("%.1f\u00b1%.1f", avg, std)) %>%
    select(stratas, variables, smry)%>%
    ungroup() %>%
    pivot_wider(names_from = stratas, values_from =smry) %>%
    mutate(values ="") 
  tabDat = rbind(catTab, conTab)
  
  
  catPvalue = 
    dat1 %>%
    select(stratas, catVars) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values")%>%
    group_by(variables, values) %>%
    count(!!sym(stratas)) %>%
    pivot_wider(names_from = stratas, values_from =n) %>%
    ungroup() %>%
    select(-values) %>%
    nest(dat = -variables) %>%
    mutate(
      fit = map(dat, 
                ~chisq.test(.x)), 
      tidied = map(fit, tidy)
    ) %>%
    unnest(tidied) %>%
    select(variables, p.value) %>%
    mutate(p.value = ifelse(p.value <0.001, "<0.001", sprintf("%.3f", p.value)))
  
  conPvalue=dat1 %>%
    mutate(stratas = !!sym(stratas)) %>%
    select(stratas, conVars) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values") %>%
    nest(dat = -variables) %>%
    mutate(
      fit   =map(dat, ~t.test(.$values ~ .$stratas)), 
      tidied=map(fit, tidy)
    ) %>%
    unnest(tidied) %>%
    select(variables, p.value) %>%
    mutate(p.value = ifelse(p.value <0.001, "<0.001", sprintf("%.3f", p.value)))
  
  tabPvalue = rbind(catPvalue, conPvalue)
  
  tab1 = tabDat %>%
    left_join(tabPvalue, by=c("variables")) %>%
    left_join(varOrder, by = c("variables")) %>%
    arrange(order, values) %>%
    group_by(variables) %>%
    mutate(ranks = row_number()) %>%
    mutate(p.value   = ifelse(ranks==min(ranks), p.value,   "")) %>% 
    mutate(variables = ifelse(ranks==min(ranks), variables, "")) %>%
    ungroup() %>%
    select(-order, -ranks)%>%
    mutate(values = str_replace(values, "[:digit:]\\.", ""))
  return(tab1)
}