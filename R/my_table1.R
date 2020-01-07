my_table1 <- function(varnames, factorVarnames, strata, df, missing = FALSE){


  tab_s <- CreateTableOne(vars=varnames,
                          strata = strata,
                          factorVars = factorVarnames,
                          includeNA = missing,
                          data = df)

  categor <- tab_s$CatTable
  cont <- tab_s$ContTable

  tab_o <- CreateTableOne(vars=varnames,
                          factorVars = factorVarnames,
                          includeNA = missing,
                          data = df)

  categor_o <- tab_o$CatTable
  cont_o <- tab_o$ContTable

  n <- nrow(unique(df[,strata]))

  cont[[n+1]]=cont_o$Overall
  categor[[n+1]]=categor_o$Overall
  names(categor)[n+1]='Overall'
  names(cont)[n+1]='Overall'

  tab_s$ContTable=cont
  tab_s$CatTable=categor

  cat("\n")

  tab_s%>%
    print(printToggle = F, missing = TRUE) %>%
    kable()


}

