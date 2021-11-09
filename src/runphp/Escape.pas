unit Escape;

interface

function EscapeArg(Argument: String): String;
function EscapeQuotes(Argument: String): String;
function EscapeTrailingBackslashes(Argument: String): String;
function MatchChar(Needle, Haystack: String): Boolean;

implementation

{Escapes a Windows command line argument}
function EscapeArg(Argument: String): String;
begin

  {Escape double-quotes and double-up preceding backslashes}
  Result := EscapeQuotes(Argument);

  {Check for whitespace or an empty value}
  if MatchChar(#32#9, Result) or (Result = '') then
  begin
    {Double-up trailing backslashes and enclose in double-quotes}
    Result := EscapeTrailingBackslashes(Result);
    Result := '"' + Result + '"';
  end;

end;

{Escapes double-quotes and doubles-up preceding backslashes. Changed is set to
True if double-quotes were found.}
function EscapeQuotes(Argument: String): String;
var
  Index: Integer;
  Item: String;
  Remainder: String;

begin

  Result := '';
  Remainder := Argument;

  repeat
    Index := Pos('"', Remainder);

    if Index = 0 then
      Result := Result + Remainder
    else
    begin
      Item := Copy(Remainder, 1, Index - 1);
      Delete(Remainder, 1, Index);
      Result := Result + EscapeTrailingBackslashes(Item) + '\"';
    end;
  until Index = 0;

end;

{Backslash-escapes consecutive backslashes at the end of the argument.}
function EscapeTrailingBackslashes(Argument: String): String;
var
  Start: Integer;
  Count: Integer;

begin

  Result := Argument;
  Count := 0;

  for Start := Length(Argument) downto 1 do
  begin

    if Argument[Start] = '\' then
      Inc(Count)
    else
    begin

      if Count > 0 then
        Insert(StringOfChar('\', Count), Result, Start + 1);

      Break;
    end;
  end;

end;

{Returns True if a character in needle is found in Haystack}
function MatchChar(Needle, Haystack: String): Boolean;
var
  I: Integer;

begin

  Result := False;

  if Haystack = '' then
    Exit;

  for I := 1 to Length(Needle) do
  begin

    if Pos(Needle[I], Haystack) > 0 then
    begin
      Result := True;
      Exit;
    end;

  end;

end;

end.

