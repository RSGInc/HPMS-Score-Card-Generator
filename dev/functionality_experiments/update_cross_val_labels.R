

library(data.table)
library(rprojroot)

root = find_root(is_git_root)

label_file   = file.path(root, 'resources/dat/cross_validation_labels.csv')
updates_file = file.path(root, 'dev/label_updates.csv')

xval_labels = fread(label_file)
updates     = fread(updates_file)

xval_labels = merge( 
  xval_labels,
  updates[, .(.id, description_new)], 
  all.x = TRUE 
  )