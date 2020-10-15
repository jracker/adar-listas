# Questão 7

tw <- function(ur, temp_ar) {
  parte_1 <- temp_ar * atan(0.151977 * (ur + 8.313659)^0.5)
  parte_2 <- atan(temp_ar + ur) - atan(ur - 1.676331) + 0.00391838 * (ur)^(1.5) * atan(0.023101 * ur) - 4.686035
  resultado <- parte_1 + parte_2

  return(resultado)
}

(exerc_7 <- tw(30, 30))

# Questão 8

umid_solo <- function(si, cl, db, me) {
  x_14 <- -1.05501 + 0.0650857 * si
  x_15 <- -2.07588 + 0.0423954 * cl
  x_16 <- -6.03402 + 4.80572 * db
  x_17 <- -2.18409 + 8.84963 * me / 100
  z_9 <- 0.175202 + 1.18513 * x_17 - 0.0996042 * x_17^2 + 0.327915 * x_16 - 0.0758657 * x_16^2
  z_10 <- 0.929344 * z_9 + 0.132519 * x_14
  theta_10 <- 0.339255 + 0.112526 * z_10
  z_11 <- 0.191452 + 1.25652 * x_17 - 0.079098 * x_17^2 + 0.393814 * x_16 + 0.152095 * x_17 * x_16
  theta_33 <- 0.28951 + 0.103815 * z_11
  z_13 <- 0.235084 + 0.33033 * x_15 - 0.191838 * x_15^2 + 0.0543679 * x_15^3 + 0.977685 * x_17 + 0.304174 * x_15 * x_17 - 0.218857 * x_17^2 - 0.164373 * x_15 * x_17^2 + 0.0415057 * x_17^3 + 0.373361 * x_16 +
    0.0811861 * x_17 * x_16 - 0.0768087 * x_15 * x_17 * x_16
  theta_1500 <- 0.214008 + 0.0862945 * z_13
  resultados <- c(theta_10, theta_33, theta_1500)
  return(resultados)
}

valumid_solo <- umid_solo(13, 37, 1.3, 21)
(uspot_hidr10k <- valumid_solo[[1]])
(uscap_camp33k <- valumid_solo[[2]])
(usmur_perm15k <- valumid_solo[[3]])
