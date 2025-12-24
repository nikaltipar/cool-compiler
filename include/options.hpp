#pragma once

#include <string>
#include <vector>

enum class Phase
{
    Lexer,  // Stop after lexing (output tokens)
    Parser, // Stop after parsing (output AST)
    Semant, // Stop after semantic analysis (output annotated AST)
    Cgen,   // Full compilation (output assembly)
};

struct CoolOptions
{
    Phase stop_after = Phase::Cgen;

    std::string output_file;

    std::vector<std::string> input_files;

    static CoolOptions& get()
    {
        static CoolOptions instance {};
        return instance;
    }

  private:
    CoolOptions() = default;
};
