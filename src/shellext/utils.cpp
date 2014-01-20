#include "utils.h"
#include "types.h"

BOOLEAN utils::DirectoryExists(const std::wstring& path)
{
    BOOLEAN res = false;

    if (!path.empty())
    {
        DWORD attrib = GetFileAttributes(path.c_str());
        res = (attrib != INVALID_FILE_ATTRIBUTES) && (attrib & FILE_ATTRIBUTE_DIRECTORY);
    }

    return res;
}

BOOLEAN utils::FileExists(const std::wstring& filepath)
{
    BOOLEAN res = false;

    if (!filepath.empty())
    {
        DWORD attrib = GetFileAttributes(filepath.c_str());
        res = (attrib != INVALID_FILE_ATTRIBUTES) && ((attrib & FILE_ATTRIBUTE_DIRECTORY) == 0);
    }

    return res;
}

BOOLEAN utils::RegUserOpen(PHKEY pKey)
{
    std::wstring subkey = utils::RegUserSubkey();
    LONG res = RegOpenKeyEx(HKEY_CURRENT_USER, subkey.c_str(),
        0, KEY_READ, pKey);

    return ERROR_SUCCESS == res ? true : false; 
}

std::wstring utils::RegUserSubkey()
{
    std::wstring result(L"Software\\");
    result.append(COMPOSER_NAME);
    return result;
}

std::wstring utils::StringReplace(const std::wstring& str, const std::wstring& from,
    const std::wstring& to)
{
    if (from.empty())
        return str;
    
    std::wstring res;
    res.reserve(str.length());
    size_t start = 0, pos;

    while((pos = str.find(from, start)) != std::wstring::npos)
    {
        res += str.substr(start, pos - start);
        res += to;
        pos += from.length();
        start = pos;
    }

    res += str.substr(start);
    return res;
}


std::wstring utils::Trim(const std::wstring& str)
{
    return TrimLeft(TrimRight(str));
}


std::wstring utils::TrimRight(const std::wstring& str)
{
    std::wstring res = str;
    size_t pos = str.find_last_not_of(L" ");
    
    if (pos != std::wstring::npos)
        res.erase(pos + 1);

    return res;
}


std::wstring utils::TrimLeft(const std::wstring& str)
{
    std::wstring res = str;
    size_t pos = res.find_first_not_of(L" ");

    if (pos != std::wstring::npos)
        res.erase(0, pos);

    return res;
}
