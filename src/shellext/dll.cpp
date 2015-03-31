#include <windows.h>
#include "types.h"
#include "factory.h"
#include "register.h"
#include "shared.h"

// Required globals
HKEY g_RegKey = NULL;
long g_DllRef = 0;
HMODULE g_Module = NULL;
DllShared g_Shared;


// Dll entry point.
BOOL APIENTRY DllMain(HMODULE module, DWORD reason, LPVOID reserved)
{
    switch (reason)
    {
    case DLL_PROCESS_ATTACH:
        g_Module = module;
        DisableThreadLibraryCalls(module);
        break;
    case DLL_THREAD_ATTACH:
        break;
    case DLL_THREAD_DETACH:
        break;
    case DLL_PROCESS_DETACH:
        break;
    }
    return TRUE;
}


// Create the class factory and query to the specific interface.
STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, void **ppv)
{
    HRESULT hr = CLASS_E_CLASSNOTAVAILABLE;

    if (IsEqualCLSID(COMPOSER_CLSID, rclsid))
    {
        hr = E_OUTOFMEMORY;

        ClassFactory *pClassFactory = new ClassFactory();
        if (pClassFactory)
        {
            hr = pClassFactory->QueryInterface(riid, ppv);
            pClassFactory->Release();
        }
    }

    return hr;
}


// Check if we can unload the component from the memory. 
STDAPI DllCanUnloadNow(void)
{
    return g_DllRef > 0 ? S_FALSE : S_OK;
}


// Register the COM server and the context menu handler. 
STDAPI DllRegisterServer(void)
{
    HRESULT hr;

    wchar_t module[MAX_PATH];
    if (GetModuleFileName(g_Module, module, ARRAYSIZE(module)) == 0)
    {
        hr = HRESULT_FROM_WIN32(GetLastError());
    }
    else
    {
        ComposerShellReg registry(g_RegKey);
        hr = registry.Register(module);
    }
    
    return hr;
}


// Unregister the COM server and the context menu handler.
STDAPI DllUnregisterServer(void)
{
    ComposerShellReg registry(g_RegKey);
    return registry.Unregister();
}


// Register or Unregister the COM server and the context menu handler
STDAPI DllInstall(BOOL bInstall, PCWSTR pszCmdLine)
{

    if (0 == wcscmp(L"admin", pszCmdLine))
    {
        g_RegKey = HKEY_LOCAL_MACHINE;
    }
    else if (0 == wcscmp(L"user", pszCmdLine))
    {
        g_RegKey = HKEY_CURRENT_USER;
    }
    else
    {
        g_RegKey = NULL;
        return HRESULT_FROM_WIN32(0x57);
    }

    if (bInstall)
    {
        return DllRegisterServer();
    }
    else
    {
        return DllUnregisterServer();
    }
}
