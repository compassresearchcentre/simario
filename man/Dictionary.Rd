\docType{data}
\name{Dictionary}
\alias{Dictionary}
\title{Dictionary object.
This object holds:
 * descriptions: a mapping of variable names to descriptions
 * codings: category names for categorical variables}
\format{proto object 
 $ cmatchFlattened  :function (., x.flat, varname, grpby.tag)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 81 21 100 2 28 9 81 100 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>  
 $ cmatch           :function (., x, varname)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 48 12 58 2 19 9 48 58 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>  
 $ codings          : list() 
 $ loadDictionaryXLS:function (filedir, filename)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 308 23 319 2 30 9 308 319 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>  
 $ descriptions     : NULL 
 $ new_from_XLS     :function (., filedir, dict_filename, codings_filename = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 234 18 246 2 25 9 234 246 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>  
 $ cnamesLookup     :function (., vars)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 119 18 122 2 25 9 119 122 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>  
 $ loadCodingsXLS   :function (filedir, filename)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 267 20 287 2 27 9 267 287 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>  
 $ dlookup          :function (., x)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 138 13 194 2 20 9 138 194 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>  
 $ order_by_dlookup :function (., ...)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 209 22 213 2 29 9 209 213 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0857e668>}
\usage{
  Dictionary
}
\description{
  Dictionary object. This object holds: * descriptions: a
  mapping of variable names to descriptions * codings:
  category names for categorical variables
}
\keyword{datasets}

