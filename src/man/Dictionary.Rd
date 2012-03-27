\docType{data}
\name{Dictionary}
\alias{Dictionary}
\title{Dictionary object.
This object holds:
 * descriptions: a mapping of variable names to descriptions
 * codings: category names for categorical variables}
\format{proto object 
 $ cmatchFlattened   :function (., x.flat, varname, grpby.tag)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 81 21 100 2 28 9 81 100 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>  
 $ createCodings     :function (codings_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 249 19 267 2 26 9 249 267 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>  
 $ cmatch            :function (., x, varname)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 48 12 58 2 19 9 48 58 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>  
 $ codings           : list() 
 $ descriptions      : NULL 
 $ createDescriptions:function (descriptions_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 269 24 279 2 31 9 269 279 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>  
 $ cnamesLookup      :function (., vars)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 118 18 121 2 25 9 118 121 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>  
 $ dlookup           :function (., x)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 136 13 192 2 20 9 136 192 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>  
 $ new               :function (., descriptions_dataframe, codings_dataframe = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 234 9 247 2 16 9 234 247 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>  
 $ order_by_dlookup  :function (., ...)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 207 22 211 2 29 9 207 211 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07dd0364>}
\usage{
  Dictionary
}
\description{
  Dictionary object. This object holds: * descriptions: a
  mapping of variable names to descriptions * codings:
  category names for categorical variables
}
\keyword{datasets}

