context("Teste utils")

# Testes
test_that("Is gender", {
  expect_true(.is_gender("F"))
  expect_true(.is_gender("M"))
  expect_true(.is_gender("f"))
  expect_true(.is_gender("m"))
  expect_false(.is_gender("Q"))
})

test_that("Is estater", {
  expect_true(.is_estate("MG"))
  expect_true(.is_estate("TO"))
  expect_true(.is_estate("BA"))
  expect_true(.is_estate("RS"))
  expect_true(.is_estate("sp"))
  expect_false(.is_estate("TT"))
})

test_that("parameters_url", {
  parameters_list <- list("siglaSexo" = list("parameter_function" = "gender", "validate" = .is_gender),
                          "siglaUf" = list("parameter_function" = "estate", "validate" = .is_estate))
  gender <- "M"
  expect_equal("siglaSexo=M&siglaUf=MG", parameters_url(parameters_list, gender , estate = "MG"))
})

