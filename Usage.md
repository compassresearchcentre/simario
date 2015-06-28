# Dependencies #

Simario requires the following packages to be installed:
```
install.packages(c("plyr","xlsx","proto","abind", "Hmisc"))
```

# Startup #

The following warning messages can safely be ignored:

```
1: replacing previous import 'is.discrete' when loading 'plyr' 
2: replacing previous import 'summarize' when loading 'plyr' 
```

If however you see the following warning messages:
```
3: In grepl("\n", lines, fixed = TRUE) :
  input string 460 is invalid in this locale
4: In grepl("\n", lines, fixed = TRUE) :
  input string 460 is invalid in this locale
```

Then set your locale to "C" (which is the default for the C language and reflects North-American usage), as follows:
```
Sys.setlocale(locale="C")
```