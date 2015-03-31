#pragma once
#include <windows.h>
#include <string>

enum CSBOOL {
    CS_UNKNOWN = -1,
    CS_FALSE,
    CS_TRUE
};

// struct for holding file-mapped options
struct CSOPTIONS {
    DWORD collapse;
    DWORD runas;
};

typedef CSOPTIONS *LPCSOPTIONS;

// struct for holding menu var data
struct CSMENUVARS {
    BOOLEAN isAdmin;
    BOOLEAN collapse;
    BOOLEAN elevate;
    HBITMAP hBmpComposer;
    HBITMAP hBmpUac;
    std::wstring settings;
};

// struct for holding bitmap data
struct CSBMP {
    BOOLEAN set;
    HBITMAP hBmp;
};

// struct for holding exe data
struct CSEXE {
    BOOLEAN set;
    std::wstring exe;
};

class DllShared 
{
public:
    DllShared(void);
    ~DllShared(void);
    std::wstring GetExeCmd();
    CSMENUVARS GetMenuVars();
    HRESULT ToggleRunas(BOOLEAN oldElevate);
        
protected:

private:
    HBITMAP GetBmpComposer();
    HBITMAP GetBmpUac();
    std::wstring GetExeSettings();
    BOOLEAN GetIsAdmin();
    CSOPTIONS GetOptions();
    HBITMAP IconToBitmap(HICON hIcon);
    BOOLEAN IsAdminWork(PHANDLE pToken, PTOKEN_GROUPS * pGroups);
    void MapClose();
    void MapOpen();
    HRESULT MapRead(LPCSOPTIONS options);
    HRESULT MapWrite(DWORD runas);
    HRESULT MapWrite(CSOPTIONS options);
    void RegGetOptions(LPCSOPTIONS options);
    HRESULT RegSetRunas(DWORD value);
    
    CSBOOL m_Admin;
    CSBMP m_BmpComposer;
    CSBMP m_BmpUac;
    CSEXE m_ExeCmd;
    CSEXE m_ExeSettings;
    HANDLE m_MapHandle;
    HANDLE m_MapMutex;
    PDWORD m_MapDwData;    
};
