# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: GNU General Public License v3.0
# Last update: Tue Dec  4 11:12:47 2018
# --------------------------------------------------- #


.onAttach <- function(lib, pkg){
  packageStartupMessage("\nThe CoDa package is deprecated. Use MortalityForecast package instead.",
                        "\nhttps://github.com/mpascariu/MortalityForecast\n")
}
