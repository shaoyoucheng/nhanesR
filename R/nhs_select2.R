# # Select column for nhs_read() result
# #
# # @param nhs result of nhs_read()
# # @param exact exact column names
# # @param grep fuzzy match
# # @param ex_grep exclude by fuzzy match
# #
# # @return selected list
# # @export
# #
# nhs_select <- function(nhs,exact=NULL,grep=NULL,ex_grep=NULL){
#     # exact
#     if (!is.null(exact)){
#         exact <- unique(tolower(exact))
#         jdg1 <- lapply(nhs, function(i) names(i) %in% exact)
#     }else{
#         (jdg1 <- lapply(nhs, function(i) rep(FALSE,ncol(i))))
#     }
#     # grep
#     if (!is.null(grep)){
#         pattern <- paste0(tolower(grep),collapse = '|')
#         jdg2 <- lapply(nhs, function(i) grepl(pattern,names(i)))
#     }else{
#         (jdg2 <- lapply(nhs, function(i) rep(FALSE,ncol(i))))
#     }
#
#     # ex_grep
#     if (!is.null(ex_grep)){
#         pattern <- paste0(tolower(ex_grep),collapse = '|')
#         jdg3 <- lapply(nhs, function(i) !grepl(pattern,names(i)))
#     }else{
#         (jdg3 <- lapply(nhs, function(i) rep(TRUE,ncol(i))))
#     }
#     r <- lapply(1:length(nhs), function(i){
#         jdg <- (jdg1[[i]] | jdg2[[i]]) & jdg3[[i]]
#         nhs[[i]][,jdg,drop=FALSE]
#     })
#     names(r) <- names(nhs)
#     r
# }
