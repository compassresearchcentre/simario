\docType{data}
\name{Dictionary}
\alias{Dictionary}
\title{Dictionary object.
This object holds:
 * descriptions: a mapping of variable names to descriptions
 * codings: category names for categorical variables}
\format{proto object 
 $ cmatchFlattened   :function (., x.flat, varname, grpby.tag)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 81 21 101 2 28 9 81 101 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>  
 $ createCodings     :function (codings_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 250 19 268 2 26 9 250 268 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>  
 $ cmatch            :function (., x, varname)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 48 12 58 2 19 9 48 58 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>  
 $ codings           : list() 
 $ descriptions      : NULL 
 $ createDescriptions:function (descriptions_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 270 24 280 2 31 9 270 280 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>  
 $ cnamesLookup      :function (., vars)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 119 18 122 2 25 9 119 122 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>  
 $ dlookup           :function (., x)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 137 13 193 2 20 9 137 193 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>  
 $ new               :function (., descriptions_dataframe, codings_dataframe = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 235 9 248 2 16 9 235 248 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>  
 $ order_by_dlookup  :function (., ...)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 208 22 212 2 29 9 208 212 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001589f8c8>}
\usage{
  Dictionary
}
\description{
  Dictionary object. This object holds: * descriptions: a
  mapping of variable names to descriptions * codings:
  category names for categorical variables
}
\keyword{datasets}

