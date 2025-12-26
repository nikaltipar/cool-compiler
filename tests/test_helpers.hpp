#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

namespace fs = std::filesystem;

std::vector<fs::path> getTestFiles(const fs::path& directory)
{
    std::vector<fs::path> files {};

    for (const auto& entry : fs::directory_iterator(directory))
    {
        if (!entry.is_regular_file())
        {
            continue;
        }

        fs::path filepath {entry.path()};

        if (filepath.extension() == ".cool")
        {
            files.push_back(filepath.filename());
        }
    }

    std::sort(files.begin(), files.end());
    return files;
}

std::string normalize(const std::string& text)
{
    std::string result {};
    result.reserve(text.size());

    bool in_whitespace {false};
    for (char c : text)
    {
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
        {
            if (!in_whitespace && !result.empty())
            {
                result += ' ';
                in_whitespace = true;
            }
        }
        else
        {
            // Remove space after # (line number prefix) to match expected format
            if (in_whitespace && result.size() >= 2 && result.back() == ' ')
            {
                // Check if previous non-space chars form a line number like "#123 "
                size_t i {result.size() - 2};
                while (i > 0 && std::isdigit(static_cast<unsigned char>(result[i])))
                {
                    --i;
                }
                if (result[i] == '#')
                {
                    result.pop_back(); // Remove the space after line number
                }
            }
            result += c;
            in_whitespace = false;
        }
    }

    if (!result.empty() && result.back() == ' ')
    {
        result.pop_back();
    }

    return result;
}

std::string getTestNameFromPath(const fs::path& path)
{
    std::string name {path.filename().stem().string()};
    std::replace_if(name.begin(), name.end(), [](unsigned char c) { return !std::isalnum(c) && c != '_'; }, '_');
    return name;
}

std::string readFile(const fs::path& path)
{
    std::ifstream fstream {path};
    if (!fstream)
    {
        return "";
    }
    std::stringstream buffer {};
    buffer << fstream.rdbuf();
    return buffer.str();
}