unit DumpParser;

interface

uses
  classes, SysUtils;

type

  callback = procedure (keys: TStrings; value: string);

  TDumpHandler = class (TObject)
    procedure OnElement (keys: array of string; value: string; depth: integer); virtual; abstract;
  end;

  procedure parse (text: string; DumpHandler: TDumpHandler);

  function split (s, token: string): TStrings;

implementation

  function split (s, token: string): TStrings;
  var
    p: integer;
  begin
    Result := TStringList.Create;
    while (true) do begin
      p := pos (token, s);
      if p = 0 then break;
      Result.Add (copy (s, 1, p - 1));
      delete (s, 1, p + length (token) - 1);
    end;
    Result.Add (s);
  end;

  procedure parse (text: string; DumpHandler: TDumpHandler);
  var
    n: integer;

    procedure swallow_blanks;
    begin
      while (n < length (text)) and (text [n] <= ' ') do inc (n);
    end;

    function get_string: string;
    begin

      Result := '';
      
      swallow_blanks;

      if text [n] = ''''

        then begin // quoted string

          inc (n); // step forward from quote

          while (n < length (text)) do begin

            if text [n] = ''''  // closing quote
              then begin
                inc (n);
                break;
              end
            else if (text [n] = '\') and (text [n + 1] = '''') // quoted quote
              then begin
                inc (n);
                Result := Result + '''';
              end
            else
              begin
                Result := Result + text [n];
              end;

            inc (n);

          end; // of loop

        end  // of quoted string branch

        else begin // bareword

          while (n < length (text)) and (text [n] in ['0' .. '9', 'a' .. 'z', '_', 'A'..'Z']) do begin
            Result := Result + text [n];
            inc (n);
          end;

//          inc (n);

        end;

    end;

  var
    KeyStack: array [1..10] of string;
    TypeStack:  array [1..10] of string;
    depth: integer;

  begin

    n := 1;
    depth := 0;

    while n < length (text) do begin

      swallow_blanks;

      case text [n] of

        '{': begin
          inc (depth);
          TypeStack [depth] := ('HASH');
          inc (n);
          KeyStack  [depth] := get_string;
          swallow_blanks;
          inc (n, 2);  // =>
        end;


        '[': begin
          inc (depth);
          TypeStack [depth] := 'ARRAY';
          KeyStack  [depth] := '0x0001';
          inc (n);
        end;

        ']', '}': begin
          dec (depth);
          inc (n);
        end;

        ',': begin

          inc (n);
          swallow_blanks;

          if text [n] in ['0' .. '9', 'a' .. 'z', '_', 'A'..'Z', ''''] then begin

            if TypeStack [depth] = 'HASH'

              then begin
                KeyStack [depth] := get_string;
                swallow_blanks;
                inc (n, 2);  // =>
              end

              else begin
                KeyStack [depth] := '0x' + inttohex (1 + strtoint (KeyStack [depth]), 4);
              end;

          end;

        end;

        else begin  //constant

           DumpHandler.OnElement (KeyStack, get_string, depth);

        end;

      end;

    end;

  end;



end.
