% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ibufunction.R
\name{link_grepper}
\alias{link_grepper}
\title{GENERALIZED LINK_GREPPER FUNCTION}
\usage{
link_grepper(links, rules)
}
\value{
This function will return a character vector of links.
}
\description{
This is a function to extract links filter the links needed using regex pattern.
\preformatted{
links
- A character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
rules
- Regex pattern to search the wanted links.
}
}
\author{
IBU Analytics
}
