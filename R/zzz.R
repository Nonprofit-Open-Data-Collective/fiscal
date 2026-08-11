# Declare the package data.table-aware.
#
# Several panel utilities (panel-dedupe.R, panel-describe.R) use data.table
# by-reference syntax (`:=`) and in-scope column access (`dt[get(id), ]`).
# data.table only enables that special evaluation when the *calling* package
# is registered as "aware" via cedta(). Because fiscal imports data.table
# through `data.table::` qualified calls rather than a NAMESPACE import, that
# flag is not set automatically, which caused inspect_duplicates() and
# panel_summary() to error with a cedta() failure. Setting this to TRUE is the
# canonical way to declare awareness.
.datatable.aware <- TRUE
