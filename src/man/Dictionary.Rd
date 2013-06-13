\docType{data}
\name{Dictionary}
\alias{Dictionary}
\title{Dictionary object.
This object holds:
 * descriptions: a mapping of variable names to descriptions
 * codings: category names for categorical variables}
\format{proto object 
 $ cmatchFlattened   :function (., x.flat, varname, grpby.tag)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 82 21 102 2 28 9 82 102 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ createCodings     :function (codings_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 318 19 336 2 26 9 318 336 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ cmatch            :function (., x, varname)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 48 12 58 2 19 9 48 58 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ codings           : list() 
 $ descriptions      : NULL 
 $ createDescriptions:function (descriptions_dataframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 338 24 348 2 31 9 338 348 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ cnamesLookup      :function (., vars)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 120 18 123 2 25 9 120 123 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ dlookup_exists    :function (., x)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 201 20 261 2 27 9 201 261 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ dlookup           :function (., x)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 138 13 194 2 20 9 138 194 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ new               :function (., descriptions_dataframe, codings_dataframe = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 303 9 316 2 16 9 303 316 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>  
 $ order_by_dlookup  :function (., ...)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 276 22 280 2 29 9 276 280 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000170a7b18>}
\usage{
  Dictionary
}
\description{
  Dictionary object. This object holds: * descriptions: a
  mapping of variable names to descriptions * codings:
  category names for categorical variables
}
\keyword{datasets}

