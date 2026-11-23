# Send any plot a test draws to a null PDF device so no test writes
# Rplots.pdf into the working directory. CRAN-09 extends this file.
withr::local_pdf(NULL, .local_envir = testthat::teardown_env())
