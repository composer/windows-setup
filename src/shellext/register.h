#pragma once
#include <windows.h>

const PCWSTR CS_REG_HANDLERS[] = {
    {L"AllFileSystemObjects"},
    {L"Directory\\Background"}
};

class ComposerShellReg
{
public:
    ComposerShellReg(HKEY rootKey);
    HRESULT Register(PCWSTR module);
    HRESULT Unregister();
private:
    HRESULT HandlersDoRegister(BOOL reg);
    HRESULT PathGetForHandler(PCWSTR handler, LPWSTR path, size_t cch);
    HRESULT PathGetForServer(LPWSTR path, size_t cch);
    HRESULT ServerRegister(PCWSTR module);
    HRESULT ServerUnregister();
    HRESULT SetKeyAndValue(PCWSTR subKey, PCWSTR valueName, PCWSTR data);

    HKEY m_RootKey;
    const PCWSTR m_ClassPath;
    wchar_t m_CLSIDStr[40];
};
