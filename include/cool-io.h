//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#ifndef COOL_IO_H
#define COOL_IO_H

//
// Cool files include this header to use the standard library's
// IO streams.
//

// By default use the ANSI standard iostream, etc.
#ifndef COOL_USE_OLD_HEADERS

#include <iostream>

using std::cerr;
using std::cout;
using std::endl;
using std::ostream;

#include <fstream>

using std::ofstream;

#include <iomanip>

using std::dec;
using std::oct;
using std::setfill;
using std::setw;

// Including the entire std namespace doesn't work well because of conflicts
// between e.g. std::plus and the plus AST node.
// using namespace std;

#else
// COOL_USE_OLD_HEADERS is defined

// I haven't actually tested this, but it ought to work
#include <fstream.h>
#include <iomanip.h>
#include <iostream.h>

#endif // COOL_USE_OLD_HEADERS

#endif // COOL_IO_H
