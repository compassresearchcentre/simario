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
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>  
 $ createCodings     :function (codings_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 251 19 269 2 26 9 251 269 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>  
 $ cmatch            :function (., x, varname)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 48 12 58 2 19 9 48 58 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>  
 $ codings           : list() 
 $ descriptions      : NULL 
 $ createDescriptions:function (descriptions_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 271 24 281 2 31 9 271 281 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>  
 $ cnamesLookup      :function (., vars)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 119 18 122 2 25 9 119 122 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>  
 $ dlookup           :function (., x)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 138 13 194 2 20 9 138 194 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>  
 $ new               :function (., descriptions_dataframe, codings_dataframe = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 236 9 249 2 16 9 236 249 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>  
 $ order_by_dlookup  :function (., ...)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 209 22 213 2 29 9 209 213 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000fc2f370>}
\usage{
  Dictionary
}
\description{
  Dictionary object. This object holds: * descriptions: a
  mapping of variable names to descriptions * codings:
  category names for categorical variables
}
\keyword{datasets}

