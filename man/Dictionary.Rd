\docType{data}
\name{Dictionary}
\alias{Dictionary}
\title{Dictionary object.
This object holds:
 * a mapping of variable names to descriptions
 * codings (i.e: category names) for categorical variables}
\format{proto object 
 $ cmatchFlattened  :function (., x.flat, varname, grpby.tag)   
 $ cmatch           :function (., x, varname)   
 $ codings          : list() 
 $ loadDictionaryXLS:function (filedir, filename)   
 $ new_from_XLS     :function (., filedir, dict_filename, codings_filename)   
 $ cnamesLookup     :function (., vars)   
 $ loadCodingsXLS   :function (filedir, filename)   
 $ vardesc          : NULL 
 $ dlookup          :function (., x)   
 $ order_by_dlookup :function (., ...)}
\usage{
  Dictionary
}
\description{
  Dictionary object. This object holds: * a mapping of
  variable names to descriptions * codings (i.e: category
  names) for categorical variables
}
\keyword{datasets}

