rep_tab_reg <- function(object,
                        caption) {
  tab <- object
  i_head <- grep("E\\[y\\|X, c\\]|P\\[c\\|X\\]", tab[, 1])
  i_comp <- grep("Comp", tab[, 1])
  i_oth <- 1:nrow(tab)
  i_oth <- i_oth[!(i_oth %in% c(i_head, i_comp))]
  tab %>%
    kableExtra::kbl(row.names = FALSE,
                    format = "html",
                    caption = caption) %>%
    kableExtra::row_spec(nrow(tab), 
                         extra_css = "border-top: 1px solid grey; border-bottom-color: white; ") %>%
    kableExtra::row_spec(i_head, 
                         extra_css = "border-top: 1px solid grey; border-bottom: 1px solid grey; ") %>%
    kableExtra::row_spec(i_comp, 
                         extra_css = "border-top: 1px solid grey; ")
  
}