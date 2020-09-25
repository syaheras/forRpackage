# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' GENERALIZED IBU_SIMPLE_CAP FUNCTION
## Created: March 2020
## Amended:
#' @author IBU Analytics
#' @description This is a function to capitalise the first letter of EVERY word.
#' @export
#' @rdname ibusimplecap
#' @param string = One or more character string to be put into the function
#' @return This function will return a character vector with the same length as the input vector.

ibu_simple_cap <- function(string) {
  ss = vector()
  for(i in 1:length(string)){
    s = strsplit(tolower(string[i]), " ")[[1]]
    ss[i] = paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "", collapse = " ")
  }
  return(ss)
}

#-------------------------------------------------------------------------------------------------------------------

#' GENERALIZED KOL_NAME FUNCTION
#' @description
#' \preformatted{
#' This is a function to format column names by :-
#'  1. lowering the case of the column names.
#'  2. trimming whitespace.
#'  3. substituting '.' with spaces in the columns.
#'  }
#' @export
#' @rdname kol_name
#' @param df - Dataframe to be formatted.

kol_name = function(df) {
  stopifnot("data.frame" %in% class(df))
  nama_kol = tolower(names(df))
  nama_kol = trimws(nama_kol)
  nama_kol = trimws(gsub("\\."," ", nama_kol))
  return(nama_kol)
}

#-------------------------------------------------------------------------------------------------------------------
#' GENERALIZED LINK_GREPPER FUNCTION
## Created: March 2020
## Amended:
#' @author IBU Analytics

#' @description This is a function to extract links filter the links needed using regex pattern.
#' \preformatted{
#' links
#' - A character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#' rules
#' - Regex pattern to search the wanted links.
#' }
#' @export
#' @return This function will return a character vector of links.

link_grepper = function(links,rules) {
  require(Rcrawler)
  l = list()
  for (i in 1:length(links)){
    cat("Now start at",i,"\n")
    li = LinkExtractor(links[i], IndexErrPages = c(200,404))$InternalLinks
    l[[i]] = grep(rules,li, ignore.case = T,  value = T)
    cat("Grepped",length(unlist(l[i])), "links\n")
    Sys.sleep(time = sample(seq(0.1,0.3, by = 0.001),1, replace = FALSE))
  }
  links = unlist(l)
  return(links)
}


#-------------------------------------------------------------------------------------------------------------------
#' GENERALIZED NUMERIC_CHECKER FUNCTION
## Created: 16 May 2019
## Amended:
#' @author IBU Analytics

#' @description This is a function to find all NA, NAN, NULL, and negative value inside numeric classes of dataframe.

#' @param df - Dataframe to be checked.
#' @param kelas - Classes of the columns.
#' @param negative - If TRUE, will run the is_non_negative function.
#' @param na - If TRUE, will run the is_not_na function.
#' @param nan - If TRUE, will run the is_not_nan function.
#' @param null - If TRUE, will run the is_not_null function.

#' @export
#' @rdname numeric_checker
#' @return This function will return a dataframe with the numeric_checker_list list.

numeric_checker = function(df,kelas,negative = TRUE,na = TRUE,nan = TRUE,null = TRUE){
  require(assertive)
  require(stringr)
  numeric_checker_list = list()
  kelas_numeric = names(df[kelas == "numeric"])
  if(negative == TRUE){
    numeric_checker_list = ibu_assertive_checker(df,kelas_numeric,is_non_negative,"num_negative_value",numeric_checker_list)
  }

  if(na == TRUE){
    numeric_checker_list = ibu_assertive_checker(df,kelas_numeric,is_not_na,"num_na",numeric_checker_list)
  }

  if(nan == TRUE){
    numeric_checker_list = ibu_assertive_checker(df,kelas_numeric,is_not_nan,"num_naN",numeric_checker_list)
  }

  if(null == TRUE){
    numeric_checker_list = ibu_assertive_checker(df,kelas_numeric,is_not_null,"num_null",numeric_checker_list)
  }
  basic_list = list(`time checked numeric` = Sys.time(),
                    `class_data_1` = paste0("Numeric: ",str_flatten(kelas_numeric,collapse = " "))
  )
  numeric_checker_list = append(basic_list,numeric_checker_list)
  return(as.data.frame(numeric_checker_list))
}

#-------------------------------------------------------------------------------------------------------------------

#' GENERALIZED PATH_CHECKER FUNCTION
## Created: 16 May 2019
## Amended:
#' @author IBU Analytics

#' @description
#' This is a function to check and create path if doesn't exist

#' @param
#' path = The path in which to be checked and created.
#' @export
#' @rdname path_checker
#' @return This function will not return an output,
#' @return it will just create a folder in the directory path given if it does not exist.

path_checker = function(path){
  if(dir.exists(path)== FALSE){
    dir.create(path)
  }else{}
}



