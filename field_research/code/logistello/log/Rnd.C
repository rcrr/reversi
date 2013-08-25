// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "Rnd.H"

MLCG Rnd::gen;
RandomInteger Rnd::Number(&gen);
Uniform       Rnd::Uni01(0.0, 1.0, &gen);
