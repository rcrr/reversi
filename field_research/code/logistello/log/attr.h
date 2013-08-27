// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Bildschirmattribute */

#ifndef O_UMATTR
#define O_UMATTR

#ifdef xxx
#ifdef AMIGA

#define ATTR            "\2330;33;40m"
#define NORMAL          "\2330;31;40m"

#define BLACKMAN         "\2331;30;41m{}\2330;31;40m"
#define WHITEMAN         "{}"

#else

#define ATTR            ""
#define NORMAL          ""

#define BLACKMAN        "··"
#define WHITEMAN        "()"

#endif

#endif


#define BLACKMAN	"##"
#define WHITEMAN	"()"

#endif
