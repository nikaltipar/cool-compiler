#include <algorithm>
#include <array>
#include <cstdio>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <sstream>
#include <string>

#include "cgen_gc.hpp"
#include "cool-parse.hpp"
#include "cool-tree.hpp"
#include "semant.hpp"
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

class CgenTest : public ::testing::Test
{
  public:
    inline const static fs::path test_data_dir {TEST_DATA_DIR};
    inline const static uint8_t SPIM_HEADER_LINES {5};

    static std::vector<fs::path> getTestFiles(const fs::path& directory)
    {
        std::vector<fs::path> files {};

        if (!fs::exists(directory) || !fs::is_directory(directory))
        {
            return files;
        }

        try
        {
            for (const auto& entry : fs::directory_iterator(directory))
            {
                if (!entry.is_regular_file())
                {
                    continue;
                }

                fs::path filepath {entry.path()};

                if (filepath.extension() == ".cl")
                {
                    files.push_back(filepath.filename());
                }
            }
        }
        catch (const std::exception&)
        {
            return files;
        }

        std::ranges::sort(files);
        return files;
    }

  protected:
    std::ostringstream error_output {};
    std::string filename_storage {};

    static void SetUpTestSuite()
    {
        yy_flex_debug = 0;
        cool_yydebug = 0;
        ASSERT_TRUE(fs::exists(test_data_dir)) << "Test data directory not found: " << test_data_dir;

#ifdef _WIN32
        int ret = system("where spim >nul 2>&1");
#else
        int ret = system("which spim >/dev/null 2>&1");
#endif

        ASSERT_TRUE(ret == 0) << "SPIM not found in PATH.";
    }

    void SetUp() override
    {
        error_stream = &error_output;
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

    std::string runCodegen(const fs::path& input_file, const fs::path& output_asm_file)
    {
        fin = std::fopen(input_file.string().c_str(), "rb");
        if (!fin)
        {
            return "";
        }

        yyrestart(fin);
        cool_yylex_reset();

        filename_storage = fs::path(input_file).filename().string();

        curr_filename = filename_storage.data();

        int parse_errors = cool_yyparse();
        if (parse_errors != 0 || omerrs != 0 || ast_root == nullptr)
        {
            return error_output.str();
        }

        std::ostringstream semant_output {};
        ast_root->semant(semant_output);

        if (!semant_output.str().empty())
        {
            return semant_output.str();
        }

        std::ofstream asm_file {output_asm_file};
        if (!asm_file)
        {
            return "Error: Could not open output assembly file";
        }

        ast_root->cgen(asm_file);
        asm_file.close();

        return "";
    }

    std::string runAssembly(const fs::path& asm_file)
    {
        // Find the trap handler file
        fs::path trap_handler = fs::path(TEST_DATA_DIR).parent_path().parent_path() / "lib" / "trap.handler";

        std::string command = std::format(
            "timeout 5s spim -exception_file {} -file {} /dev/null 2>&1", trap_handler.string(), asm_file.string()
        );

        std::array<char, 128> buffer {};
        std::string result {};

#ifdef _WIN32
        command = std::format("spim -bare -file {} -file {} < NUL 2>&1", trap_handler.string(), asm_file.string());
        std::unique_ptr<FILE, decltype(&_pclose)> pipe(_popen(command.c_str(), "r"), _pclose);
#else
        std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(command.c_str(), "r"), pclose);
#endif

        if (!pipe)
        {
            return "Error: Could not execute spim";
        }

        while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr)
        {
            result += buffer.data();
        }

        return result;
    }
};

class CgenFileTest : public CgenTest, public ::testing::WithParamInterface<fs::path>
{
};

TEST_P(CgenFileTest, CompareWithExpectedOutput)
{
    const fs::path input_file = CgenTest::test_data_dir / GetParam();
    const fs::path expected_file = fs::path(input_file).replace_extension(".cl.out");

    if (!fs::exists(expected_file))
    {
        GTEST_SKIP() << "No expected output file for file: " << input_file.filename().string();
    }

    // Set memory manager based on test name
    std::string filename = input_file.filename().string();
    if (filename.find("gc") != std::string::npos || filename.find("GC") != std::string::npos)
    {
        cgen_Memmgr = GC_GENGC;
    }
    else
    {
        cgen_Memmgr = GC_NOGC;
    }

    const fs::path temp_asm_file = fs::temp_directory_path() / (input_file.stem().string() + "_temp.s");
    const std::string error_msg = runCodegen(input_file, temp_asm_file);

    if (!error_msg.empty())
    {
        if (fs::exists(temp_asm_file))
        {
            fs::remove(temp_asm_file);
        }
        GTEST_FAIL() << "Code generation failed for: " << input_file.filename().string() << "\n" << error_msg;
    }

    ASSERT_TRUE(fs::exists(temp_asm_file)) << "Assembly file was not generated for: " << input_file.filename().string();

    std::string actual = runAssembly(temp_asm_file);
    std::string expected = readFile(expected_file);

    auto skipFirstLines = [](const std::string& str, uint8_t num_skip) {
        size_t start {};
        for (int i = 0; i < num_skip; i++)
        {
            start = str.find('\n', start);
            if (start == std::string::npos)
            {
                return std::string {};
            }
            start += 1;
        }
        return str.substr(start);
    };
    actual = skipFirstLines(actual, SPIM_HEADER_LINES);
    expected = skipFirstLines(expected, SPIM_HEADER_LINES);

    if (fs::exists(temp_asm_file))
    {
        fs::remove(temp_asm_file);
    }

    EXPECT_EQ(actual, expected) << "Program output mismatch for: " << input_file.filename().string();
}

INSTANTIATE_TEST_SUITE_P(
    CgenTests,
    CgenFileTest,
    ::testing::ValuesIn(CgenTest::getTestFiles(CgenTest::test_data_dir)),
    [](const ::testing::TestParamInfo<fs::path>& info) { return getTestNameFromPath(info.param); }
);
