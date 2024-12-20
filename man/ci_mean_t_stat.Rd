% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{ci_mean_t_stat}
\alias{ci_mean_t_stat}
\title{Vidurkio PI, suskaičiuotas pagal aprašomąsias statistikas}
\usage{
ci_mean_t_stat(mean_, sd_, n, group = "", conf.level = 0.95)
}
\arguments{
\item{mean_}{Vektorius su kiekvienos grupės vidurkiais.}

\item{sd_}{Vektorius su kiekvienos grupės standartiniu nuokrypiu.}

\item{n}{Vektorius su kiekvienos grupės dydžiu.}

\item{group}{Grupės pavadinimas.
Numatytoji reikšmė – tuščia eilutė (\code{""}).}

\item{conf.level}{Pasikliovimo lygmuo. Numatytoji reikšmė – 0.95.}
}
\value{
Rezultatas – duomenų lentelė, su šiais stulpeliais:
\itemize{
\item \code{group}  (\verb{<fct>}) – grupės pavadinimas;
\item \code{mean}   (\verb{<dbl>}) – vidurkio įvertis;
\item \code{lwr.ci} (\verb{<dbl>}) – apatinė vidurkio PI riba (lwr. atitinka „lower“);
\item \code{upr.ci} (\verb{<dbl>}) – viršutinė vidurkio PI riba (upr. atitinka „upper“);
\item \code{sd}     (\verb{<dbl>})  – standartinis nuokrypis;
\item \code{n}      (\verb{<int>})  – imties/grupės dydis.
}

Skaičiavimai gali būti atlikti ir rezultatai pateikti daugiau nei vienai
grupei.
}
\description{
Funkcija \code{ci_mean_t_stat()} skaičiuoja vidurkio pasikliautinajį intervalą (PI)
pagal \emph{klasikinę} formulę su t (Stjudento) koeficientu, kai duotos
\emph{aprašomosios statistikos} (vidurkis, standartinis nuokrypis, imties dydis).
Naudinga, kai tokie dydžiai būna pateikti mokslinėje literatūroje.
}
\note{
Kiekvieno iš \code{mean_}, \code{sd_}, \code{n}, \code{group} ilgis (reikšmių skaičius) turi būti
(a) arba vieną reikšmė,
(b) arba sutapti su ilgiausiu šios argumentų grupės vektoriumi.

Dėl aiškumo išnagrinėkite pavyzdžius.
}
\examples{
# Pavyzdžiai

# Nurodant argumentų pavadinimus:
ci_mean_t_stat(mean_ = 362, sd_ = 35, n = 100)

# Nenurodant argumentų pavadinimų:
ci_mean_t_stat(362, 35, 100)


# Skaičiavimai kelioms grupėms:
vidurkis     <- c(1, 2, 3)
st_nuokrypis <- c(3, 2, 3)
n            <- c(50, 20, 40)
grupe        <- c("A", "B", "C")

ci_mean_t_stat(vidurkis, st_nuokrypis, n, grupe)


# Simuliacija su keliais imties dydžiais:
ci_mean_t_stat(mean_ = 362, sd_ = 35, n = c(10, 50, 100, 1000))


# Jei norite, kad rodytų daugiau skaitmenų po kablelio:
rez_pi <- ci_mean_t_stat(362, 35, 100)
as.data.frame(rez_pi)

# Arba
# View(rez_pi)

}
