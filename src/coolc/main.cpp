#include <CLI/CLI.hpp>
#include <cstdio>
#include <format>
#include <fstream>
#include <iostream>
#include <print>

#include "cgen.hpp"
#include "cool-parse.hpp"
#include "cool-tree.hpp"
#include "options.hpp"
#include "semant.hpp"
#include "utilities.hpp"

extern int curr_lineno;
char* curr_filename = "<stdin>";
FILE* fin;
extern YYSTYPE cool_yylval;
extern Program ast_root;
extern Classes parse_results;
extern int omerrs;

extern int cool_yylex();
extern int cool_yyparse();

extern int yy_flex_debug;
extern int cool_yydebug;
int cgen_debug = 0;

// Forward declare yyrestart to initialize lexer
extern void yyrestart(FILE* input_file);

#include "cgen_gc.hpp"
Memmgr cgen_Memmgr = GC_NOGC;
Memmgr_Test cgen_Memmgr_Test = GC_NORMAL;
Memmgr_Debug cgen_Memmgr_Debug = GC_QUICK;

extern void dump_cool_token(ostream& out, int lineno, int token, YYSTYPE yylval);

int main(int argc, char** argv)
{
    CLI::App app {"coolc - The Cool Compiler"};

    auto& opts = CoolOptions::get();

    bool stop_lex = false, stop_parse = false, stop_semant = false;
    app.add_flag("--lex", stop_lex, "Stop after lexical analysis");
    app.add_flag("--parse", stop_parse, "Stop after parsing");
    app.add_flag("--semant", stop_semant, "Stop after semantic analysis");

    bool debug = false;
    app.add_flag("-d,--debug", debug, "Enable debug output for all phases");

    app.add_option("-o,--output", opts.output_file, "Output file name");

    std::string gc_choice = "none";
    bool gc_test = false, gc_debug = false;
    app.add_option("--gc", gc_choice, "Garbage collector: none, gen, scn (default: none)")
        ->check(CLI::IsMember({"none", "gen", "scn"}));
    app.add_flag("--gc-test", gc_test, "Enable GC test mode (stress test allocation)");
    app.add_flag("--gc-debug", gc_debug, "Enable GC debug mode (verbose GC output)");

    app.add_option("files", opts.input_files, "Input Cool source files")->required()->check(CLI::ExistingFile);

    CLI11_PARSE(app, argc, argv);

    if (gc_choice == "gen")
        cgen_Memmgr = GC_GENGC;
    else if (gc_choice == "scn")
        cgen_Memmgr = GC_SNCGC;
    else
        cgen_Memmgr = GC_NOGC;

    if (gc_test)
        cgen_Memmgr_Test = GC_TEST;
    if (gc_debug)
        cgen_Memmgr_Debug = GC_DEBUG;

    if (stop_lex)
        opts.stop_after = Phase::Lexer;
    else if (stop_parse)
        opts.stop_after = Phase::Parser;
    else if (stop_semant)
        opts.stop_after = Phase::Semant;
    else
        opts.stop_after = Phase::Cgen;

    // Initialize debug flags to off (flex debug defaults to on with %option debug)
    yy_flex_debug = 0;
    cool_yydebug = 0;
    
    if (debug)
    {
        yy_flex_debug = 1;
        cool_yydebug = 1;
        cgen_debug = 1;
    }

    for (const auto& filename : opts.input_files)
    {
        fin = std::fopen(filename.c_str(), "r");
        if (fin == nullptr)
        {
            std::println(std::cerr, "Error: Could not open input file {}", filename);
            return 1;
        }

        curr_lineno = 1;
        curr_filename = const_cast<char*>(filename.c_str());

        if (opts.stop_after == Phase::Lexer)
        {
            std::println("#name \"{}\"", filename);
            int token;
            while ((token = cool_yylex()) != 0)
            {
                dump_cool_token(std::cout, curr_lineno, token, cool_yylval);
            }
            std::fclose(fin);
            continue;
        }

        int parse_errors = cool_yyparse();
        std::fclose(fin);

        if (parse_errors != 0)
        {
            std::println(std::cerr, "Error: Parsing failed for {}", filename);
            return 1;
        }

        if (opts.stop_after == Phase::Parser)
        {
            ast_root->dump_with_types(std::cout, 0);
            continue;
        }

        ast_root->semant();

        if (opts.stop_after == Phase::Semant)
        {
            ast_root->dump_with_types(std::cout, 0);
            continue;
        }

        std::ostream* output = &std::cout;
        std::ofstream outfile;

        if (!opts.output_file.empty())
        {
            outfile.open(opts.output_file);
            if (!outfile.is_open())
            {
                std::println(std::cerr, "Error: Could not open output file {}", opts.output_file);
                return 1;
            }
            output = &outfile;
        }

        ast_root->cgen(*output);
    }

    return 0;
}
