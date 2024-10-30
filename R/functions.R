# ~~~ Vidurkio PI pagal vid., SD, n ------------------------------------------

#' Vidurkio PI, suskaičiuotas pagal aprašomąsias statistikas
#'
#' Funkcija `ci_mean_t_stat()` skaičiuoja vidurkio pasikliautinajį intervalą (PI)
#' pagal *klasikinę* formulę su t (Stjudento) koeficientu, kai duotos
#' *aprašomosios statistikos* (vidurkis, standartinis nuokrypis, imties dydis).
#' Naudinga, kai tokie dydžiai būna pateikti mokslinėje literatūroje.
#'
#' @param mean_ Vektorius su kiekvienos grupės vidurkiais.
#' @param sd_   Vektorius su kiekvienos grupės standartiniu nuokrypiu.
#' @param n     Vektorius su kiekvienos grupės dydžiu.
#' @param group Grupės pavadinimas.
#'              Numatytoji reikšmė – tuščia eilutė (`""`).
#' @param conf.level Pasikliovimo lygmuo. Numatytoji reikšmė – 0.95.
#'
#' @note
#' Kiekvieno iš `mean_`, `sd_`, `n`, `group` ilgis (reikšmių skaičius) turi būti
#' (a) arba vieną reikšmė,
#' (b) arba sutapti su ilgiausiu šios argumentų grupės vektoriumi.
#'
#' Dėl aiškumo išnagrinėkite pavyzdžius.
#'
#' @return
#' Rezultatas – duomenų lentelė, su šiais stulpeliais:
#'
#' - `group`  (`<fct>`) – grupės pavadinimas;
#' - `mean`   (`<dbl>`) – vidurkio įvertis;
#' - `lwr.ci` (`<dbl>`) – apatinė vidurkio PI riba (lwr. atitinka „lower“);
#' - `upr.ci` (`<dbl>`) – viršutinė vidurkio PI riba (upr. atitinka „upper“);
#' - `sd`     (`<dbl>`)  – standartinis nuokrypis;
#' - `n`      (`<int>`)  – imties/grupės dydis.
#'
#' Skaičiavimai gali būti atlikti ir rezultatai pateikti daugiau nei vienai
#' grupei.
#'
#'
#' @examples
#' # Pavyzdžiai
#'
#' # Nurodant argumentų pavadinimus:
#' ci_mean_t_stat(mean_ = 362, sd_ = 35, n = 100)
#'
#' # Nenurodant argumentų pavadinimų:
#' ci_mean_t_stat(362, 35, 100)
#'
#'
#' # Skaičiavimai kelioms grupėms:
#' vidurkis     <- c(1, 2, 3)
#' st_nuokrypis <- c(3, 2, 3)
#' n            <- c(50, 20, 40)
#' grupe        <- c("A", "B", "C")
#'
#' ci_mean_t_stat(vidurkis, st_nuokrypis, n, grupe)
#'
#'
#' # Simuliacija su keliais imties dydžiais:
#' ci_mean_t_stat(mean_ = 362, sd_ = 35, n = c(10, 50, 100, 1000))
#'
#'
#' # Jei norite, kad rodytų daugiau skaitmenų po kablelio:
#' rez_pi <- ci_mean_t_stat(362, 35, 100)
#' as.data.frame(rez_pi)
#'
#' # Arba
#' # View(rez_pi)
#'
#' @importFrom stats qt
#' @export
ci_mean_t_stat <- function(mean_, sd_, n, group = "", conf.level = 0.95) {

  Q <- conf.level

  # Taikome formulę su t koeficientu:
  t <- qt(p = (1 - Q) / 2, df = n - 1, lower.tail = FALSE)
  paklaida <- t * sd_ / sqrt(n)

  apatine_riba   <- mean_ - paklaida
  virsutine_riba <- mean_ + paklaida

  # Sudėliojame rezultatus
  vidurkio_pi_t <-
    tibble::tibble(
      group   = forcats::as_factor(group),
      mean    = mean_,
      lwr.ci  = apatine_riba,
      upr.ci  = virsutine_riba,
      sd      = sd_,
      n       = as.integer(n)
    )

  vidurkio_pi_t
}

# ~~~ Vidurkio PI grupėms ----------------------------------------------------

#' Vidurkio PI, suskaičiuotas pagal duomenis
#'
#' Funkcija `ci_mean_t()` skaičiuoja vidurkio pasikliautinajį intervalą (PI)
#' pagal *klasikinę* formulę su t (Stjudento) koeficientu, kai duomenys
#' pateikti *duomenų lentelės pavidalu*. Ši funkcija yra patobulinta
#' [DescTools::MeanCI()], reaguojanti į [dplyr::group_by()], tad skaičiavimus
#' gali atlikti ir pogrupiams. Rezultatas – duomenų lentelė.
#'
#' @param .data Duomenų lentelė.
#' @param x     Stulpelio pavadinimas (be kabučių).
#' @param conf.level Pasikliovimo lygmuo. Numatytoji reikšmė – 0.95.
#' @param ... Kiti parametrai, kuriuos priima [DescTools::MeanCI()].
#'       Žiūrėti šios funkcijos dokumentaciją.
#'
#' @return
#' Rezultatas – duomenų lentelė, kurioje yra šie stulpeliai:
#'
#' - (jei yra) grupavimo kintamųjų pavadinimai;
#' - `mean` (`<dbl>`) – vidurkio įvertis;
#' - `lwr.ci`, `upr.ci` (`<dbl>`) – (lower CI, upper CI) apatinė ir viršutinė
#'   pasikliautinojo intervalo ribos.
#'
#' @examples
#' # Pavyzdžiai
#' data(npk, package = "datasets")
#' head(npk)
#'
#' # Kintamojo `yield` vidurkio PI skaičiavimas
#' ci_mean_t(npk, yield)
#'
#' # PI skaičiavimas naudojant jungimo operatorių
#' npk |> ci_mean_t(yield)
#'
#' # PI skaičiavimas grupuojant pagal vieną kintamąjį
#' npk |> dplyr::group_by(N) |> ci_mean_t(yield)
#'
#' # PI skaičiavimas grupuojant pagal 3 kintamuosius
#' npk |> dplyr::group_by(N, P, K) |> ci_mean_t(yield)
#'
#' @importFrom utils data
#' @export

ci_mean_t <- function(.data, x, conf.level = 0.95, ...) {

  vidurkio_pi <- function(x) {
    # Rezultatas turi būti duomenų lentelė
    DescTools::MeanCI(x, conf.level = conf.level, ...) |>
      t() |>
      as.data.frame()
  }

  # Rezultatas
  .data |>
    tidyr::nest(data = c(dplyr::everything(), -dplyr::group_vars(.data))) |>
    dplyr::mutate(
      ci = purrr::map(data, ~ dplyr::pull(., {{ x }}) |> vidurkio_pi())
    ) |>
    dplyr::select(-data) |>
    tidyr::unnest(ci) |>
    dplyr::ungroup()
}

# ~~~ Proporcijos PI ---------------------------------------------------------

#' Proporcijos PI: 2 grupės
#'
#' Dvireikšmio kintamojo mus dominančios reikšmės proporcijos pasikliautinojo
#' intervalo (PI) skaičiavimo funkcija, kuri yra patobulintas
#' [DescTools::BinomCI()] variantas. Rezultatas – duomenų lentelė.
#'
#' @details
#' Ši funkcija naudojama taip pat, kaip [DescTools::BinomCI()], tik numatytasis
#' metodas yra modifikuotasis Wilson metodas, o rezultatas – duomenų lentelė,
#' o ne vektorius. Todėl, pvz., rezultatą galima braižyti naudojant
#' \pkg{ggplot2}.
#'
#' @param x Mus dominančių
#'          *arba* mums palankių įvykių skaičius
#'          *arba* mus dominančios grupės dydis.
#'
#' @param n Įvykių skaičius iš viso. / Imties dydis.
#'
#' @param conf.level Pasikliovimo lygmuo. Numatytoji reikšmė – 0.95.
#'
#' @param method Skaičiavimo metodas (`"modified wilson"`, `"wilson"`,
#'        `"agresti-coull"` ir kiti variantai, aprašyti
#'        [DescTools::BinomCI()] dokumentacijoje).
#'
#' @param ... Kiti parametrai, kuriuos priima
#'        [DescTools::BinomCI()]. Žiūrėti šios funkcijos dokumentaciją.
#'
#' @return
#' Rezultatas – duomenų lentelė, kurios stulpeliai:
#'
#' - `est` (`<dbl>`) – proporcijos įvertis,
#' - `lwr.ci`, `upr.ci`  (`<dbl>`) – (lower CI, upper CI)
#'   apatinė ir viršutinė proporcijos pasikliautinojo intervalo ribos.
#' - `x` (`<int>`) – Mus dominančių įvykių skaičius / Mus dominančios grupės dydis.
#' - `n` (`<int>`) – Įvykių skaičius iš viso. / Imties dydis.
#'
#' @examples
#' x <- 54  # mus dominančių įvykių skaičius
#' n <- 80  # įvykių skaičius iš viso
#' ci_binom(x = 54, n = 80)
#'
#' # Simuliacija su skirtingais imties dydžiais
#' ci_binom(x = 54, n = c(80, 100, 512))
#'
#' # PI skaičiavimas kiekvienai grupei atskirai
#' y = c(23, 45)
#' ci_binom(y, n = sum(y))
#'
#'
#' @export

ci_binom <- function(x, n, method = "modified wilson", conf.level = 0.95, ...) {
  # Patikra
  len_x <- length(x)
  len_n <- length(n)
  checkmate::assert_integerish(x, lower = 0, min.len = 1)
  checkmate::assert_integerish(n, lower = 0, min.len = 1)
  if (!len_x %in% c(1, max(len_x, len_n))) {
    stop("lenght(x) must be either 1 or match length(n).")
  }
  if (!len_n %in% c(1, max(len_x, len_n))) {
    stop("lenght(n) must be either 1 or match length(x).")
  }

  # Skaičiavimai
  proporcijos_pi <- DescTools::BinomCI(
    x = x, n = n, conf.level = conf.level, method = method, ...
  )
  dplyr::bind_cols(
    tibble::as_tibble(proporcijos_pi),
    tibble::tibble(x = as.integer(x), n = as.integer(n))
  )
}


#' Proporcijos PI: 3 ar daugiau grupių
#'
#' Daugiareikšmio (k ≥ 3) kintamojo reikšmių proporcijų *vienu metu* skaičiuojamų
#' pasikliautinųjų intervalų (PI) skaičiavimo funkcija, kuri yra patobulintas
#' [DescTools::MultinomCI()] variantas. Rezultatas – duomenų lentelė.
#'
#' @details
#' Ši funkcija naudojama taip pat, kaip [DescTools::MultinomCI()], tik
#' numatytasis metodas yra Goodman metodas, o rezultatas – duomenų lentelė,
#' o ne vektorius.
#' Todėl rezultatą galima patogiai braižyti naudojant \pkg{ggplot2}.
#'
#' @param x Vektorius su grupių dydžiais.
#'          Geriausia, jei vektoriaus elementai turėtų prasmingu pavadinimus
#'          (žiūrėti pavyzdžius).
#'
#' @param conf.level Pasikliovimo lygmuo. Numatytoji reikšmė – 0.95.
#'
#' @param method Skaičiavimo metodas (`"goodman"`, `"sisonglaz"`, `"cplus1"`
#'        ir kiti variantai, aprašyti [DescTools::MultinomCI()] dokumentacijoje).
#' @param ... Kiti parametrai, kuriuos priima [DescTools::MultinomCI()].
#'        Žiūrėti šios funkcijos dokumentaciją.
#'
#' @param gr_colname Stulpelio pavadinimas (kabutėse), kuriame bus
#'        parašyti grupių pavadinimai. Numatytoji reikšmė yra `"group"`.
#'
#'
#' @return
#' Rezultatas – duomenų lentelė, kurios stulpeliai:
#'
#' - `group` arba kitas vartotojo pasirinktas pavadinimas stulpeliui su grupių
#'   pavadinimams, numatytoji reikšmė
#'   (`<fct>`).
#' - `est` (`<dbl>`) – proporcijos įvertis.
#' - `lwr.ci`, `upr.ci` (`<dbl>`) – (lower CI, upper CI) apatinė ir viršutinė
#'           proporcijos pasikliautinojo intervalo ribos.
#' - `x` (`<int>`) – Grupės dydis.
#' - `n` (`<int>`) – Įvykių skaičius iš viso. / Imties dydis.
#'
#' @examples
#' # Dažniai be pavadinimų
#' ci_multinom(c(20, 35, 54))
#'
#' # Nurodytas skaičiavimo metodas
#' ci_multinom(c(20, 35, 54), method = "goodman")
#'
#' # Dažniai su grupių pavadinimais
#' x <- c("dideli" = 20, "vidutiniai" = 35, "maži" = 54)
#' ci_multinom(x, method = "goodman")
#'
#' # Dažniai su grupių pavadinimais ir jungimo operatorius
#' c("dideli" = 20, "vidutiniai" = 35, "maži" = 54) |>
#'   ci_multinom()
#'
#' # Kitas metodas
#' c("dideli" = 33, "vidutiniai" = 35, "maži" = 30) |>
#'   ci_multinom(method = "sisonglaz")
#'
#'
#' @export

ci_multinom <- function(x, method = "goodman", conf.level = 0.95,
                        gr_colname = "group", ...) {
  checkmate::assert_integerish(x, lower = 0)

  x |>
    DescTools::MultinomCI(method = method, conf.level = conf.level, ...) |>
    as.data.frame() |>
    tibble::rownames_to_column(gr_colname) |>
    dplyr::mutate(
      {{ gr_colname }} := forcats::as_factor(.data[[gr_colname]]),
      x = as.integer(x),
      n = sum(x) |> as.integer()
    ) |>
    tibble::as_tibble()
}


# ~~~ PI savirankos metodu ----------------------------------------------------

#' Pasikliautinieji intervalai (PI) savirankos metodais
#'
#' Pasikliautinųjų intervalų (PI) skaičiavimas pasirinktu savirankos metodu
#' (angl. *statistical bootstrap*). Funkcija `ci_boot()` yra patobulintas
#' [DescTools::BootCI()] variantas. Rezultatas – duomenų lentelė.
#'
#' @details
#' Ši funkcija naudojama panašiai, kaip [DescTools::BootCI()], bet:
#'
#' - pirmas argumentas yra duomenų lentelė;
#' - argumentai `x` (ir, jei reikia, `y`) – stulpelių pavadinimai – nurodomi
#'   be kabučių;
#' - funkcija reaguoja į [dplyr::group_by()], tad skaičiavimus gali atlikti
#'   pogrupiams;
#' - rezultatas – duomenų lentelė.
#'   Todėl rezultatą galima patogiai braižyti naudojant \pkg{ggplot2}.
#'
#' @param .data Duomenų lentelė.
#' @param x,y   Stulpelio pavadinimas (be kabučių).
#' @param conf.level Pasikliovimo lygmuo. Numatytoji reikšmė – 0.95.
#' @param ... Kiti parametrai, kuriuos priima [DescTools::BootCI()], tarp kurių:
#' 1) `FUN` -- funkcija, kurios rezultatui skaičiuojami PI.
#' 2) `bci.method` -- intervalų sudarymo metodai:
#'    + `"perc"` -- procentilių metodas,
#'    + `"bca"`  -- koreguotasis procentilių metodas BCa
#'               (angl. bias-corrected and accelerated) – pastaba dėl metodo
#'               naudojimo žemiau,
#'    + kiti.
#' 3) `R` -- replikacijų (pakartojimų) skaičius.
#'           Įprastai turi būti tarp 1'000 ir 10'000.
#'
#' @return
#' Rezultatas – duomenų lentelė, su pasikliautinaisiais intervalais.
#' Stulpelių skaičius ir pavadinimai priklauso nuo funkcijos argumentų reikšmių
#' ir sugrupavimo:
#'
#' - Jei duomenų lentelė grupuotoji, pirmųjų stulpelių pavadinimai sutampa su
#'   grupavimo kintamųjų pavadinimais.
#' - Stulpelio pavadinimas, sutampantis su skaičiuojamos statistikos pavadinimu
#'   (argumento `FUN` reikšme).
#'   Jame yra skaičiuojamos statistikos įvertis.
#' - `lwr.ci`, `upr.ci` – (lower CI, upper CI) apatinė ir viršutinė
#'                        pasikliautinojo intervalo ribos.
#'
#' @note
#' **PASTABOS!**
#'
#'  1) Savirankos metodams *kiekvienos* grupės dydis turėtų būti **bent 20**.
#'
#'  2) Norėdami gauti atkartojamus rezultatus naudokite [set.seed()].
#'
#'  3) Naudojant `bci.method = "bca"` ir gavus įspėjimą
#'   „`extreme order statistics used as endpoints`“
#'   reiktų žinoti, kad BCa metodas šiai situacijai netinka ir reiktų
#'   naudoti kitą, pvz., `"perc"`
#'   (<https://rcompanion.org/handbook/E_04.html>).
#'
#' @examples
#' data(iris, package = "datasets")
#' head(iris)
#'
#' set.seed(1) # Atkartojamumui
#'
#' # Medianos PI iš 1000 pakartojimų,
#' # BCa metodas
#' ci_boot(iris, Petal.Length, FUN = median, R = 1000, bci.method = "bca")
#'
#' # Naudojamas jungimo operatorius
#' iris |>
#'   ci_boot(Petal.Length, FUN = median, R = 1000, bci.method = "bca")
#'
#' # PI skaičiavimas kiekvienai grupei atskirai
#' iris |>
#'   dplyr::group_by(Species) |>
#'   ci_boot(Petal.Length, FUN = median, R = 1000, bci.method = "bca")
#'
#' # Medianos PI iš 1000 pakartojimų, procentilių metodas
#' iris |>
#'   dplyr::group_by(Species) |>
#'   ci_boot(Petal.Length, FUN = median, R = 1000, bci.method = "perc")
#'
#' # PI skaičiavimas, nurodant funkcijos `median()`
#' # argumentą `na.rm = TRUE`
#' med_pi_gr <-
#'   iris |>
#'   dplyr::group_by(Species) |>
#'   ci_boot(
#'     Petal.Length,
#'     FUN = median, na.rm = TRUE,
#'     R = 1000, bci.method = "bca"
#'   )
#' med_pi_gr
#'
#' # Dviejų kintamųjų funkcijoms pavyzdys:
#' # Spearman koreliacijos koeficientas
#' # (method = "spearman" yra cor() argumentas)
#' spearman_pi_gr <-
#'   iris |>
#'   dplyr::group_by(Species) |>
#'   ci_boot(
#'     Petal.Length, Petal.Width,
#'     FUN = cor, method = "spearman",
#'     R = 1000, bci.method = "bca"
#'   )
#' spearman_pi_gr
#'
#' # Dviejų kintamųjų funkcijoms pavyzdys:
#' # Pearson koreliacijos koeficientas
#' # (method = "pearson" yra cor() argumentas)
#' pearson_pi_gr <-
#'   iris |>
#'   dplyr::group_by(Species) |>
#'   ci_boot(
#'     Petal.Length, Petal.Width,
#'     FUN = cor, method = "pearson",
#'     R = 1000, bci.method = "bca"
#'   )
#' pearson_pi_gr
#'
#' @export

ci_boot <- function(.data, x, y = NULL, conf.level = 0.95, ...) {

  ci_function <- function(x, y) {
    # Rezultatas turi būti duomenų lentelė
    DescTools::BootCI(x = x, y = y, conf.level = conf.level, ...) |>
      t() |>
      as.data.frame()
  }

  missing_y <- missing(y)

  .data |>
    tidyr::nest(data = c(dplyr::everything(), -dplyr::group_vars(.data))) |>
    dplyr::mutate(ci =
      purrr::map(data, ~ ci_function(
        x = dplyr::pull(., {{ x }}),
        y = if (missing_y) NULL else dplyr::pull(., {{ y }})
      ))
    ) |>
    dplyr::select(-data) |>
    tidyr::unnest(ci) |>
    dplyr::ungroup()
}
