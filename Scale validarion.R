library(psych)
library(tidyverse)
library(corrplot)

bulling_vars = c("ST038Q03NA", "ST038Q04NA", "ST038Q05NA", "ST038Q06NA", "ST038Q07NA", "ST038Q08NA")

bullying_data = 
  `Pisa Russia` %>%
  select(bulling_vars) %>%
  replace_with_na_all(condition = ~.x %in% c(5,7,8,9)) %>%
  drop_na()

psych::alpha(bullying_data)

#do PCA

one_pc = PCA(`Pisa Russia`[, c(bulling_vars)], scale.unit = TRUE, ncp = 1, graph = TRUE)

eig.val <- 
  get_eigenvalue(one_pc) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Dimension") %>%
  select(1,2)

fviz_eig(one_pc, addlabels = TRUE, ylim = c(0, 90))

var <- get_pca_var(one_pc)

var_dat = var$cos2 %>% as.matrix() 

corrplot(var_dat)
