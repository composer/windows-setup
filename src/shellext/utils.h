#pragma once
#include <windows.h>
#include <string>

namespace utils
{
    BOOLEAN DirectoryExists(const std::wstring& path);
    BOOLEAN FileExists(const std::wstring& filepath);
    BOOLEAN RegUserOpen(PHKEY pKey);
    std::wstring RegUserSubkey();
    std::wstring StringReplace(const std::wstring& str, const std::wstring& from,
        const std::wstring& to);
    std::wstring Trim(const std::wstring& str);
    std::wstring TrimRight(const std::wstring& str);
    std::wstring TrimLeft(const std::wstring& str);
}
