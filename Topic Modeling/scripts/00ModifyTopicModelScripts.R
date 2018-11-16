library(idaTopicModels)
library(idaMakeDtm)


# Modify tommy's topic model functions

DepluralizeDtm <- function (dtm) {
  adjust.terms <- CorrectS(colnames(dtm))
  colnames(dtm) <- adjust.terms$adjusted
  unchanged <- dtm[, !colnames(dtm) %in% colnames(dtm)[duplicated(colnames(dtm))]]
  changed <- dtm[, colnames(dtm) %in% colnames(dtm)[duplicated(colnames(dtm))]]
  sfInit(parallel = TRUE, cpus = 4)
  sfExport("changed")
  sfLibrary(Matrix)
  term.indices <- sfLapply(unique(colnames(changed)), function(x) {
    grep(x, colnames(changed), fixed = TRUE)
  })
  sfStop()
  gc()
  sfInit(parallel = TRUE, cpus = 8)
  sfExport("changed")
  sfLibrary(Matrix)
  temp <- sfLapply(term.indices, function(y) {
    result <- Matrix(Matrix::rowSums(x = changed[, y]), sparse = TRUE)
  })
  sfStop()
  gc()
  batches <- seq(1, length(temp), by = 100)
  if (length(batches) > 1) {
    sfInit(parallel = TRUE, cpus = 8)
    sfExport("temp")
    sfLibrary(Matrix)
    temp2 <- sfLapply(batches, function(x) {
      do.call(cBind, temp[x:min(x + 99, length(temp))])
    })
    sfStop()
  }
  else {
    temp2 <- temp
  }
  if (length(temp2) > 100) {
    batches <- seq(1, length(temp2), by = 100)
    sfInit(parallel = TRUE, cpus = 8)
    sfExport("temp2")
    sfLibrary(Matrix)
    temp <- sfLapply(batches, function(x) {
      do.call(cBind, temp2[x:min(x + 99, length(temp2))])
    })
    sfStop()
    gc()
    temp <- do.call(cBind, temp)
  }
  else {
    temp <- do.call(cBind, temp2)
  }
  colnames(temp) <- unique(colnames(changed))
  dtm <- cBind(unchanged, temp)
  return(dtm)
}