#include <gtest/gtest.h>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <regex>
#include <cstdio>

#include "cool-parse.hpp"
#include "utilities.hpp"

namespace fs = std::filesystem;

// Global variables required by the lexer (defined in main.cpp normally)
// TODO cleaner way to do this
int curr_lineno {1};
char* curr_filename {};
FILE* fin {};
YYSTYPE cool_yylval {};

// External lexer helpers
extern int yy_flex_debug;
extern int cool_yylex();
extern void dump_cool_token(std::ostream& out, int lineno, int token, YYSTYPE yylval);
extern void yyrestart(FILE* input_file);
extern void cool_yylex_reset();

class LexerTest : public ::testing::Test {
public:
    inline const static fs::path test_data_dir {TEST_DATA_DIR};
protected:
    static void SetUpTestSuite() {
        yy_flex_debug = 0;
        ASSERT_TRUE(fs::exists(test_data_dir)) 
            << "Test data directory not found: " << test_data_dir;
    }

    void SetUp() override {
        curr_lineno = 1;
    }

    void TearDown() override {
        if (fin) {
            std::fclose(fin);
            fin = nullptr;
        }
        curr_filename = nullptr;
    }

    std::string runLexer(const fs::path& input_file) {
        fin = std::fopen(input_file.string().c_str(), "rb");
        if (!fin) {
            return "";
        }

        yyrestart(fin);
        cool_yylex_reset();

        fs::path file_path {input_file};
        curr_filename = const_cast<char*>(file_path.string().c_str());

        std::ostringstream output {};
        
        output << "#name \"" << file_path.filename().string() << "\"\n";
        
        int token {};
        while ((token = cool_yylex()) != 0) {
            dump_cool_token(output, curr_lineno, token, cool_yylval);
        }
        
        return output.str();
    }

    static std::string readFile(const fs::path& path) {
        std::ifstream fstream {path};
        if (!fstream) { return ""; }
        std::stringstream buffer {};
        buffer << fstream.rdbuf();
        return buffer.str();
    }

    static std::string normalize(const std::string& text) {
        //TODO clean this
        std::string result = text;
        
        // Normalize line endings
        size_t pos = 0;
        while ((pos = result.find("\r\n", pos)) != std::string::npos) {
            result.replace(pos, 2, "\n");
        }
        
        // Trim trailing whitespace
        if (auto end = result.find_last_not_of(" \n\r"); end != std::string::npos) {
            result.erase(end + 1);
        } else {
            result.clear();
        }
        
        return result;
    }
};

class LexerFileTest : public LexerTest, public ::testing::WithParamInterface<fs::path> {};

TEST_P(LexerFileTest, CompareWithExpectedOutput) {
    const fs::path test_name = GetParam();
    const fs::path input_file = test_data_dir / test_name;
    const fs::path expected_file = fs::path(input_file).replace_extension(".cool.out");
    
    if (!fs::exists(expected_file))
    {
        GTEST_FAIL() << "No expected output file for: " << test_name;
    }
    
    const std::string actual {normalize(runLexer(input_file))};
    const std::string expected {normalize(readFile(expected_file))};
    
    EXPECT_EQ(actual, expected)
        << "Lexer output mismatch for: " << test_name;
}

std::vector<fs::path> getTestFiles() {
    std::vector<fs::path> files {};
    for (const auto& entry : fs::directory_iterator(LexerTest::test_data_dir)) {
        if (entry.is_regular_file()) {
            fs::path filename = entry.path().filename();
            if (filename.extension() == ".cool") {
                fs::path out_file = fs::path(entry.path()).replace_extension(".cool.out");
                if (fs::exists(out_file)) {
                    files.push_back(filename);
                }
            }
        }
    }
    
    std::sort(files.begin(), files.end());
    return files;
}

INSTANTIATE_TEST_SUITE_P(
    LexerTests,
    LexerFileTest,
    ::testing::ValuesIn(getTestFiles()),
    [](const ::testing::TestParamInfo<fs::path>& info) {
        auto name = info.param.filename().string();
        std::replace_if(name.begin(), name.end(), [](char c) { return c == '.' || c == '-'; }, '_');
        return name;
    }
);
