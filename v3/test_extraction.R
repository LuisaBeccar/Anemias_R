

test_that("extraer_edad works with various formats", {
  # Test parentheses format
  expect_equal(
    extraer_edad_from_line("GARCIA MARIA (72) DNI: 12345"),
    72
  )
  
  # Test dash format
  expect_equal(
    extraer_edad_from_line("LOPEZ JUAN - 65 HC: 123"),
    65
  )
  
  # Test "años" format
  expect_equal(
    extraer_edad_from_line("PEREZ CARLOS 80 años DNI: 456"),
    80
  )
  
  # Test missing
  expect_true(
    is.na(extraer_edad_from_line("RODRIGUEZ ANA DNI: 789"))
  )
})

test_that("extraer_hb_inicial validates range", {
  # Valid HB
  contenido <- c("LABORATORIOS:", "12/05: 11.5/150/...")
  expect_equal(
    extraer_hb_inicial(contenido, 1)$valor,
    11.5
  )
  
  # Invalid HB (too high)
  contenido_invalid <- c("LABORATORIOS:", "12/05: 99.9/150/...")
  expect_true(
    is.na(extraer_hb_inicial(contenido_invalid, 1)$valor)
  )
})

test_that("extraer_nombre removes file extensions", {
  expect_equal(
    extraer_nombre_from_line("GARCIA MARIA EPICRISIS.DOCX (72)"),
    "GARCIA MARIA"
  )
  
  expect_equal(
    extraer_nombre_from_line("LOPEZ JUAN.DOCX - 65"),
    "LOPEZ JUAN"
  )
})

# Run tests
test_dir("tests")