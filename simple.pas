{$MODE DELPHI}
{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

program Simple;

uses
  SysUtils,
  Generics.Collections,
  Generics.Defaults;

type
  TEntry = TPair<String, Integer>;

function Compare(constref Left, Right: TEntry): Integer;
begin
  Result := Right.Value - Left.Value;
end;

function CreateComparer(): IComparer<TEntry>;
begin
  Result := TComparer<TEntry>.Construct(Compare);
end;

var
  Line: String;
  WordArray: TStringArray;
  WordStr: String;
  Key: String;
  Dict: TDictionary<String, Integer>;
  Count: Integer;
  EntryArray: TArray<TEntry>;
  Entry: TEntry;
begin
  Dict := TDictionary<String, Integer>.Create();

  try
    while not EOF() do
    begin
      Readln(Line);

      if Length(Line) = 0 then
        continue;

      WordArray := Line.Split(' ');

      for WordStr in WordArray do
      begin
        if Length(WordStr) = 0 then
          continue;

        Key := Lowercase(WordStr);
        if Dict.TryGetValue(Key, Count) then
          Dict[Key] := Count + 1
        else
          Dict.Add(Key, 1);
      end;
    end;

    EntryArray := Dict.ToArray();
  finally
    Dict.Free();
  end;

  TArrayHelper<TEntry>.Sort(EntryArray, CreateComparer());

  for Entry in EntryArray do
    Writeln(Entry.Key, ' ', Entry.Value);
end.
