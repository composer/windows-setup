#include "shared.h"
#include "types.h"
#include "utils.h"
#include <shlobj.h>
#include <strsafe.h>
#include <Shlwapi.h>
#include <uxtheme.h>
#pragma comment(lib, "shlwapi.lib")
#pragma comment(lib, "uxtheme.lib")

extern HMODULE g_Module;


DllShared::DllShared(void) :
    m_Admin(CS_UNKNOWN),
    m_MapHandle(NULL),
    m_MapMutex(NULL),
    m_MapDwData(NULL)
{
    m_BmpComposer.set = false;
    m_BmpComposer.hBmp = NULL;
    m_BmpUac.set = false;
    m_BmpUac.hBmp = NULL;
    m_ExeCmd.set = false;
    m_ExeSettings.set = false;
    MapOpen();
}

DllShared::~DllShared(void)
{
    MapClose();

    if (m_BmpComposer.hBmp)
        DeleteObject(m_BmpComposer.hBmp);

    if (m_BmpUac.hBmp)
        DeleteObject(m_BmpUac.hBmp);
}

std::wstring DllShared::GetExeCmd()
{
    if (!m_ExeCmd.set)
    {
        m_ExeCmd.set = true;
        std::wstring cmd = L"cmd.exe";
        wchar_t* ppszPath = NULL;

        if (SUCCEEDED(SHGetKnownFolderPath(FOLDERID_System, 0, NULL, &ppszPath)))
        {
            cmd.insert(0, ppszPath + std::wstring(L"\\"));  
            CoTaskMemFree(ppszPath);
        }

        m_ExeCmd.exe = cmd;
    }

    return m_ExeCmd.exe;
}

CSMENUVARS DllShared::GetMenuVars()
{
    CSMENUVARS result;
    CSOPTIONS options = GetOptions();

    result.isAdmin = CS_TRUE == GetIsAdmin();
    result.collapse = CS_TRUE == options.collapse;
    result.hBmpComposer = result.collapse ? GetBmpComposer() : NULL;

    result.elevate = !result.isAdmin && CS_TRUE == options.runas;
    result.hBmpUac = result.elevate ? GetBmpUac() : NULL;

    result.settings = GetExeSettings();

    return result;
}

HRESULT DllShared::ToggleRunas(BOOLEAN oldElevate)
{
    DWORD value = oldElevate ? CS_FALSE : CS_TRUE;
    HRESULT hr = RegSetRunas(value);
    
    if (SUCCEEDED(hr))
        hr = MapWrite(value);

    return hr;
}

HBITMAP DllShared::GetBmpComposer()
{
    if (!m_BmpComposer.set)
    {
        m_BmpComposer.set = true;
        std::wstring settings = GetExeSettings();
        HICON hIcon = NULL;

        if (1 == ExtractIconEx(settings.c_str(), 0, NULL, &hIcon, 1))
        {
            m_BmpComposer.hBmp = IconToBitmap(hIcon);
            DestroyIcon(hIcon);
        }
    }
    
    return m_BmpComposer.hBmp;
}

HBITMAP DllShared::GetBmpUac()
{
    if (!m_BmpUac.set)
    {
        m_BmpUac.set = true;
        SHSTOCKICONINFO sii = {0};
        sii.cbSize = sizeof(sii);

        if (SUCCEEDED(SHGetStockIconInfo(SIID_SHIELD, SHGSI_ICON | SHGSI_SMALLICON, &sii)))
        {
            m_BmpUac.hBmp = IconToBitmap(sii.hIcon);
            DestroyIcon(sii.hIcon);
        }

    }
    
    return m_BmpUac.hBmp;
}

std::wstring DllShared::GetExeSettings()
{
    if (!m_ExeSettings.set)
    {
        m_ExeSettings.set = true;
        std::wstring settings;    
        wchar_t module[MAX_PATH];

        if (0 != GetModuleFileName(g_Module, module, ARRAYSIZE(module)))
        {       
            if (PathRemoveFileSpec(module))
            {
                settings = module + std::wstring(L"\\settings.exe");

                if (!utils::FileExists(settings))
                    settings.clear();            
            }        
        }

        m_ExeSettings.exe = settings;
    }

    return m_ExeSettings.exe;
}

BOOLEAN DllShared::GetIsAdmin()
{
    if (CS_UNKNOWN != m_Admin)
        return m_Admin;

    m_Admin = CS_FALSE; 
    HANDLE hToken = NULL;
    PTOKEN_GROUPS pGroups = NULL;
    
    if (IsAdminWork(&hToken, &pGroups))
        m_Admin = CS_TRUE;

    // clean up
    if (hToken)
        CloseHandle(hToken);

    if (pGroups)
        free(pGroups);

    return m_Admin;
}

CSOPTIONS DllShared::GetOptions()
{
    CSOPTIONS result;

    if (SUCCEEDED(MapRead(&result)))
    {
        // See if any item is unknown. Note that it will be both or runas
        if (CS_UNKNOWN == result.collapse || CS_UNKNOWN == result.runas)
        {
            RegGetOptions(&result);
            MapWrite(result);
        }
    }
    else
    {
        RegGetOptions(&result);
    }
    
    return result;
}

HBITMAP DllShared::IconToBitmap(HICON hIcon)
{
    HBITMAP hBmp = NULL;
    HDC hdcDest = CreateCompatibleDC(NULL);

    if (hIcon && hdcDest)
    {
        long cx = GetSystemMetrics(SM_CXSMICON);
        long cy = GetSystemMetrics(SM_CYSMICON);
        RGBQUAD *bits = NULL;

        BITMAPINFO bmi;
        memset(&bmi, 0, sizeof(BITMAPINFO));
        bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
        bmi.bmiHeader.biPlanes = 1;
        bmi.bmiHeader.biCompression = BI_RGB;
        bmi.bmiHeader.biWidth = cx;
        bmi.bmiHeader.biHeight = cy;
        bmi.bmiHeader.biBitCount = 32;

        hBmp = CreateDIBSection(hdcDest, &bmi, DIB_RGB_COLORS, (VOID **)bits, NULL, 0);
        if (hBmp)
        { 
            SelectObject(hdcDest, hBmp);
            RECT rct;
            SetRect(&rct, 0, 0, cx, cy);

            BLENDFUNCTION bfAlpha = {AC_SRC_OVER, 0, 255, AC_SRC_ALPHA};
            BP_PAINTPARAMS paintParams;
            memset(&paintParams, 0, sizeof(BP_PAINTPARAMS));
            paintParams.cbSize = sizeof(BP_PAINTPARAMS);
            paintParams.dwFlags = BPPF_ERASE;
            paintParams.pBlendFunction = &bfAlpha;

            HDC hdcBuffer;
            HPAINTBUFFER hPaintBuffer = BeginBufferedPaint(hdcDest, &rct, BPBF_DIB, &paintParams, &hdcBuffer);
            if (hPaintBuffer)
            {
                DrawIconEx(hdcBuffer, 0, 0, hIcon, cx, cy, 0, NULL, DI_NORMAL);
                // Write the buffer contents to hdcDest
                EndBufferedPaint(hPaintBuffer, TRUE);
            }
        }

        DeleteDC(hdcDest);
    }

    return hBmp;
}

BOOLEAN DllShared::IsAdminWork(PHANDLE pToken, PTOKEN_GROUPS * pGroups)
{
    BOOLEAN result = false;
    DWORD i, dwSize = 0, dwElevation;
    SID_IDENTIFIER_AUTHORITY SidAuth = SECURITY_NT_AUTHORITY;
    PSID pSidAdmin = NULL;

    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, pToken))
        return result;

    GetTokenInformation(*pToken, TokenGroups, NULL, dwSize, &dwSize);

    if (ERROR_INSUFFICIENT_BUFFER != GetLastError())
        return result;

    *pGroups = (PTOKEN_GROUPS) malloc(dwSize);

    if (!*pGroups)
        return result;

    if (!GetTokenInformation(*pToken, TokenGroups, *pGroups, dwSize, &dwSize))
        return result;

    if (!AllocateAndInitializeSid(&SidAuth, 2, SECURITY_BUILTIN_DOMAIN_RID,
        DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, &pSidAdmin))
        return result;

    for (i = 0; i < (*pGroups)->GroupCount; i++)
    {
        if (EqualSid(pSidAdmin, (*pGroups)->Groups[i].Sid))
        {
            result = true;
            break;
        }
    }

    if (result)
    {
        dwSize = sizeof(dwElevation);
        if (GetTokenInformation(*pToken, TokenElevation, &dwElevation, dwSize, &dwSize))
            result = 0 != dwElevation; 
    }

    if (pSidAdmin)
        FreeSid(pSidAdmin);
            
    return result;
}


void DllShared::MapClose()
{
    if (NULL != m_MapDwData)
    {
        UnmapViewOfFile(m_MapDwData);
        m_MapDwData = NULL;
    }

    CloseHandle(m_MapMutex);
    CloseHandle(m_MapHandle);
}


void DllShared::MapOpen()
{
    BOOLEAN first = false;
    DWORD cb = sizeof(DWORD) * 2;

    // Create map mutex
    m_MapMutex = CreateMutex(NULL, false, COMPOSER_SHELL_MUTEX);

    if (NULL == m_MapMutex)
        return;
    
    // Create file-mapping
    m_MapHandle = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
        0, cb, COMPOSER_SHELL_MAP);

    if (NULL == m_MapHandle)
    {
        MapClose();
        return;
    }

    // See if we are the first
    first = ERROR_ALREADY_EXISTS != GetLastError();

    // Create view
    m_MapDwData = (PDWORD) MapViewOfFile(m_MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, cb);

    if (NULL == m_MapDwData)
    {
        MapClose();
    }
    else if (first)
    {
        // Write mapped option data to unknown values
        CSOPTIONS options;
        options.collapse = CS_UNKNOWN;
        options.runas = CS_UNKNOWN;
        MapWrite(options);
    }
}


HRESULT DllShared::MapRead(LPCSOPTIONS options)
{
    HRESULT hr = E_FAIL;
    
    if (!m_MapDwData)
        return hr;
    
    if (WAIT_OBJECT_0 == WaitForSingleObject(m_MapMutex, INFINITE))
    {
        options->collapse = *m_MapDwData;
        options->runas = *(m_MapDwData + 1); 
        ReleaseMutex(m_MapMutex);
        hr = S_OK;
    }

    return hr;
}

HRESULT DllShared::MapWrite(DWORD runas)
{
    HRESULT hr = E_FAIL;
    
    if (!m_MapDwData)
        return hr;    

    if (WAIT_OBJECT_0 == WaitForSingleObject(m_MapMutex, INFINITE))
    {
        *(m_MapDwData + 1) = runas;
        ReleaseMutex(m_MapMutex);
        hr = S_OK;
    }

    return hr;
}


HRESULT DllShared::MapWrite(CSOPTIONS options)
{
    HRESULT hr = E_FAIL;
    
    if (!m_MapDwData)
        return hr;

    if (WAIT_OBJECT_0 == WaitForSingleObject(m_MapMutex, INFINITE))
    {
        *m_MapDwData = options.collapse;
        *(m_MapDwData + 1) = options.runas;
        ReleaseMutex(m_MapMutex);
        hr = S_OK;
    }

    return hr;
}

void DllShared::RegGetOptions(LPCSOPTIONS options)
{
    options->collapse = CS_FALSE;
    options->runas = CS_FALSE;
    HKEY hKey = NULL;
    
    if (utils::RegUserOpen(&hKey))
    {
        DWORD value = 0;
        DWORD pcbData = sizeof(value);

        // CollapseMenu
        LONG res = RegGetValue(hKey, NULL, COMPOSER_REG_COLLAPSE, RRF_RT_REG_DWORD,
            NULL, &value, &pcbData);

        if (ERROR_SUCCESS == res)
            options->collapse = value;

        // Runas
        res = RegGetValue(hKey, NULL, COMPOSER_REG_RUNAS, RRF_RT_REG_DWORD,
            NULL, &value, &pcbData);

        if (ERROR_SUCCESS == res)
            options->runas = value;
       
        RegCloseKey(hKey);
    }
}

HRESULT DllShared::RegSetRunas(DWORD value)
{
    HKEY hKey = NULL;
    std::wstring subkey = utils::RegUserSubkey();

    LONG res = RegCreateKeyEx(HKEY_CURRENT_USER, subkey.c_str(),
        0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &hKey, NULL);

    if (ERROR_SUCCESS == res)
    {
        DWORD cbData = sizeof(value);
        res = RegSetKeyValue(hKey, NULL, COMPOSER_REG_RUNAS,
            REG_DWORD, &value, cbData);
        
        RegCloseKey(hKey);
    }

    return HRESULT_FROM_WIN32(res);
}
