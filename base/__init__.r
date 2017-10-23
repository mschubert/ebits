export_submodule('./operators')
export_submodule('./match')
export_submodule('./duplicated')
export_submodule('./expand_grid')
export_submodule('./intersect')
export_submodule('./util')
export_submodule('./functional')
#export_submodule('./lambda')
export_submodule('./indexing')
export_submodule('./vector')
export_submodule('./refactor')
export_submodule('./relevel')
export_submodule('./lnapply')

outer = import('./outer')$outer

omit = import('./omit')
list = import('./list')

library(dplyr) # in global env
#`%>%` = magrittr::`%>%`
`%$%` = magrittr::`%$%`


