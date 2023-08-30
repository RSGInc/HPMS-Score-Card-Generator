
#' Find root directory for a Sharepoint project
#'
#' Find the root directory for a SharePoint (aka Teams) project sync'ed to
#' A local drive.  The function first looks for a directory named something like
#' "Resource Systems Group, Inc" and then searches for the project name within it.
#'
#' Probability of success is enhanced if you have synced the entirety of
#' "Transportation MR - Documents" to
#' `C:/Users/<user>/Resource Systems Group, Inc/Transportation MR - Documents`
#'
#' To do so, use the "Sync" button on [SharePoint](https://resourcesystemsgroupinc.sharepoint.com/sites/RSGTMR/Shared%20Documents/Forms/AllItems.aspx)
#'
#'
#' @param project_name The name of the project as seen in Teams.
#'   This is the name of the folder you are looking for. Partial names may also work.
#' @param root The root directory to start searching from. Defaults to C://Users//_user_
#' @return project_root
#' @export
#' @md
#' @examples
#' \dontrun{
#'   find_project_root('20007 Skagit HTS')
#'   find_project_root('20007')
#'   find_project_root('Skagit')}
find_project_root = function(
  project_name,
  root = file.path('C:/Users', Sys.info()['user'])){

  ls1 = fs::dir_ls(
    root,
    regexp = '($[.])|(AppData)',
    invert = TRUE,
    type = 'directory',
    fail = FALSE)

  rsg_string = 'Resource Systems Group'

  rsg_root = stringr::str_subset(ls1, rsg_string)

  if (length(rsg_root) == 0) {

    suppressWarnings({
      ls2 = sapply(ls1, function(x){
        result = fs::dir_ls(x, regexp = rsg_string, type = 'directory', fail = FALSE)
      })
    })

    rsg_root = unlist(ls2[sapply(ls2, length) > 0])
    names(rsg_root) = NULL

    if (length(rsg_root) == 0) {
      stop('Could not find directory matching "Resource Systems Group" in ', root, '.',
        '\nMaybe include "Documents" in root?')
    }

    if (length(rsg_root) > 1) {
      stop('Multiple matching directories: ', paste(rsg_root, collapse = ', '))
    }
  }

  ls3 = fs::dir_ls(rsg_root, recurse = TRUE, all = FALSE)
  ls3 = ls3[dir.exists(ls3)]

  ls4 = sapply(ls3, function(x){
    result = fs::dir_ls(x, regexp = project_name, fail = FALSE)
  }
  )

  project_root = unlist(ls4[sapply(ls4, length) > 0])
  names(project_root) = NULL

  return(project_root)

}

