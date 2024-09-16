# This function will modify the output dimensions of the flextables
fit_flextable_wordpage <- function(ft, pgwidth = 7){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, 
                  width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}