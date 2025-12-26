#include <cstdio>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <regex>
#include <sstream>
#include <string>

#include "cgen_gc.hpp"
#include "cool-parse.hpp"
#include "cool-tree.hpp"
#include "test_helpers.hpp"
#include "utilities.hpp"

namespace fs = std::filesystem;

// Global variables required by the lexer/parser
extern int curr_lineno;
char* curr_filename {};
FILE* fin {};
extern YYSTYPE cool_yylval;
extern Program ast_root;

// Globals required by cgen (normally defined in main.cpp)
int cgen_debug = 0;
Memmgr cgen_Memmgr = GC_NOGC;
Memmgr_Test cgen_Memmgr_Test = GC_NORMAL;
Memmgr_Debug cgen_Memmgr_Debug = GC_QUICK;

// External lexer helpers
extern int yy_flex_debug;
extern void yyrestart(FILE* input_file);
extern void cool_yylex_reset();

// External parser helpers
extern int cool_yyparse();
extern int cool_yydebug;
extern int omerrs;
extern std::ostream* error_stream;

class ParserTest : public ::testing::Test
{
  public:
    inline const static fs::path test_data_dir {TEST_DATA_DIR};

  protected:
    std::ostringstream output {};
    std::string filename_storage {};

    static void SetUpTestSuite()
    {
        yy_flex_debug = 0;
        cool_yydebug = 0;
        ASSERT_TRUE(fs::exists(test_data_dir)) << "Test data directory not found: " << test_data_dir;
    }

    void SetUp() override
    {
        error_stream = &output;
        curr_lineno = 1;
        cool_yylval = YYSTYPE {};
        ast_root = nullptr;
        omerrs = 0;
    }

    void TearDown() override
    {
        if (fin)
        {
            std::fclose(fin);
            fin = nullptr;
        }
        curr_filename = nullptr;
    }

    std::string runParser(const fs::path& input_file)
    {
        fin = std::fopen(input_file.string().c_str(), "rb");
        if (!fin)
        {
            return "";
        }

        yyrestart(fin);
        cool_yylex_reset();

        // Terrible hack.
        filename_storage = fs::path(input_file).filename().string();
        curr_filename = filename_storage.data();

        int parse_errors = cool_yyparse();
        if (parse_errors == 0 && omerrs == 0 && ast_root != nullptr)
        {
            ast_root->dump_with_types(output, 0);
        }

        return output.str();
    }
};

class ParserFileTest : public ParserTest, public ::testing::WithParamInterface<fs::path>
{
};

TEST_P(ParserFileTest, CompareWithExpectedOutput)
{
    const fs::path input_file = ParserTest::test_data_dir / GetParam();
    const fs::path expected_file = fs::path(input_file).replace_extension(".cool.out");

    if (!fs::exists(expected_file))
    {
        GTEST_FAIL() << "No expected output file for file: " << input_file.filename().string();
    }

    const std::string actual {normalize(runParser(input_file))};
    const std::string expected {normalize(readFile(expected_file))};

    EXPECT_EQ(actual, expected) << "Parser output mismatch for: " << input_file.filename().string();
}

INSTANTIATE_TEST_SUITE_P(
    ParserTests,
    ParserFileTest,
    ::testing::ValuesIn(getTestFiles(ParserTest::test_data_dir)),
    [](const ::testing::TestParamInfo<fs::path>& info) { return getTestNameFromPath(info.param); }
);
