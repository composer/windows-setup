#define Release

; Set the release version in version.iss
#include "version.iss"

; The unique name you add to the Inno Setup IDE, using the Tools menu.
; Click Configure Sign Tools, then Add to enter your unique name, then
; enter $p as the command (the $p is replaced by the actual parameters).
#define SignTool "uniquename"

; The full path to `signtool.exe` enclosed in inner doube-quotes and outer single-quotes.
; This Microsoft tool is in the Windows SDK and can be found using the Visual Studio
; Developer Command Prompt and typing 'where signtool'.
#define SignExe '"path\to\signtool.exe"'

; SignSha1 and SignSha2 defines are used by the [Setup]: SignTool directives in build.iss.

; Use a sha1 timestamping service
#define SignSha1 "sign /a /fd sha1 /t http://timestamp.comodoca.com/authenticode"

; Append sha256 signature
#define SignSha2 "sign /a /fd sha256 /tr http://time.certum.pl/ /td sha256 /as"

#include "composer.iss"
