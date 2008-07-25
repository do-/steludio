(*
 *                         Super Object Toolkit
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@progdigy.com>
 *
 * This unit is inspired from the json c lib:
 *   Michael Clark <michael@metaparadigm.com>
 *   http://oss.metaparadigm.com/json-c/
 *
 *  CHANGES:
 *  v1.0
 *   + renamed class
 *   + interfaced object
 *   + added a new data type: the method
 *   + parser can now evaluate properties and call methods
 *   - removed obselet rpc class
 *   - removed "find" method, now you can use "parse" method instead
 *  v0.6
 *   + refactoring
 *  v0.5
 *   + new find method to get or set value using a path syntax
 *       ex: obj.s['obj.prop[1]'] := 'string value';
 *           obj.a['@obj.array'].b[n] := true; // @ -> create property if necessary
 *  v0.4
 *   + bug corrected: AVL tree badly balanced.
 *  v0.3
 *   + New validator partially based on the Kwalify syntax.
 *   + extended syntax to parse unquoted fields.
 *   + Freepascal compatibility win32/64 Linux32/64.
 *   + JavaToDelphiDateTime and DelphiToJavaDateTime improved for UTC.
 *   + new TJsonObject.Compare function.
 *  v0.2
 *   + Hashed string list replaced with a faster AVL tree
 *   + JsonInt data type can be changed to int64
 *   + JavaToDelphiDateTime and DelphiToJavaDateTime helper fonctions
 *   + from json-c v0.7
 *     + Add escaping of backslash to json output
 *     + Add escaping of foward slash on tokenizing and output
 *     + Changes to internal tokenizer from using recursion to
 *       using a depth state structure to allow incremental parsing
 *  v0.1
 *   + first release
 *)


unit superobject;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses Classes;

{$DEFINE SUPER_LARGE_INT}
{$DEFINE SUPER_EXTENDED_SYNTAX}
{$DEFINE SUPER_METHOD}

type
{$IFNDEF FPC}
  PtrInt = longint;
  PtrUInt = Longword;
{$ENDIF}
{$IFDEF SUPER_LARGE_INT}
  SuperInt = Int64;
{$ELSE}
  SuperInt = Integer;
{$ENDIF}

const
  SUPER_ARRAY_LIST_DEFAULT_SIZE = 32;
  SUPER_TOKENER_MAX_DEPTH = 32;

  SUPER_AVL_MAX_DEPTH = sizeof(longint) * 8;
  SUPER_AVL_MASK_HIGH_BIT = not ((not longword(0)) shr 1);

type
  // forward declarations
  TSuperObject = class;
  ISuperObject = interface;
  TSuperArray = class;

(* AVL Tree
 *  This is a "special" autobalanced AVL tree
 *  It use a hash value for fast compare
 *)

  TSuperAvlBitArray = set of 0..SUPER_AVL_MAX_DEPTH - 1;

  TSuperAvlSearchType = (stEQual, stLess, stGreater);
  TSuperAvlSearchTypes = set of TSuperAvlSearchType;
  TSuperAvlIterator = class;

  TSuperAvlEntry = class
  private
    FGt, FLt: TSuperAvlEntry;
    FBf: integer;
    FHash: Cardinal;
    FName: PChar;
    FPtr: Pointer;
    function GetValue: ISuperObject;
    procedure SetValue(const val: ISuperObject);
  public
    class function Hash(k: PChar): Cardinal; virtual;
    constructor Create(AName: PChar; Obj: Pointer); virtual;
    destructor Destroy; override;
    property Name: PChar read FName;
    property Ptr: Pointer read FPtr;
    property Value: ISuperObject read GetValue write SetValue;
  end;

  TSuperAvlTree = class
  private
    FRoot: TSuperAvlEntry;
    FCount: Integer;
    function balance(bal: TSuperAvlEntry): TSuperAvlEntry;
  protected
    procedure doDeleteEntry(Entry: TSuperAvlEntry; all: boolean); virtual;
    function CompareNodeNode(node1, node2: TSuperAvlEntry): integer; virtual;
    function CompareKeyNode(k: PChar; h: TSuperAvlEntry): integer; virtual;
    function Insert(h: TSuperAvlEntry): TSuperAvlEntry; virtual;
    function Search(k: PChar; st: TSuperAvlSearchTypes = [stEqual]): TSuperAvlEntry; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsEmpty: boolean;
    procedure Clear(all: boolean = false); virtual;
    procedure Delete(k: PChar);
    function GetEnumerator: TSuperAvlIterator;
    property count: Integer read FCount;
  end;

  TSuperTableString = class(TSuperAvlTree)
  protected
    procedure doDeleteEntry(Entry: TSuperAvlEntry; all: boolean); override;
  public
    function Put(k: PChar; Obj: ISuperObject): ISuperObject;
    function Get(k: PChar): ISuperObject;
    function GetValues: ISuperObject;
    function GetNames: ISuperObject;
  end;

  TSuperAvlIterator = class
  private
    FTree: TSuperAvlTree;
    FBranch: TSuperAvlBitArray;
    FDepth: longint;
    FPath: array[0..SUPER_AVL_MAX_DEPTH - 2] of TSuperAvlEntry;
  public
    constructor Create(tree: TSuperAvlTree); virtual;
    procedure Search(k: PChar; st: TSuperAvlSearchTypes = [stEQual]);
    procedure First;
    procedure Last;
    function GetIter: TSuperAvlEntry;
    procedure Next;
    procedure Prior;
    // delphi enumerator
    function MoveNext: boolean;
    property Current: TSuperAvlEntry read GetIter;
  end;

  TSuperObjectArray = array[0..(high(PtrInt) div sizeof(TSuperObject))-1] of ISuperObject;
  PSuperObjectArray = ^TSuperObjectArray;

{$IFDEF SUPER_METHOD}
  TSuperMethod = procedure(This, Params: ISuperObject; var Result: ISuperObject);
{$ENDIF}

  TSuperArray = class
  private
    FArray: PSuperObjectArray;
    FLength: Integer;
    FSize: Integer;
    function Expand(max: Integer): Integer;
  protected
    function GetO(const index: integer): ISuperObject;
    procedure PutO(const index: integer; Value: ISuperObject);
    function GetB(const index: integer): Boolean;
    procedure PutB(const index: integer; Value: Boolean);
    function GetI(const index: integer): SuperInt;
    procedure PutI(const index: integer; Value: SuperInt);
    function GetD(const index: integer): Double;
    procedure PutD(const index: integer; Value: Double);
    function GetS(const index: integer): string;
    procedure PutS(const index: integer; const Value: string);
{$IFDEF SUPER_METHOD}
    function GetM(const index: integer): TSuperMethod;
    procedure PutM(const index: integer; Value: TSuperMethod);
{$ENDIF}
    function GetA(const index: integer): TSuperArray;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(Data: ISuperObject): Integer;
    procedure Clear(all: boolean = false);
    property Length: Integer read FLength;

    property O[const index: integer]: ISuperObject read GetO write PutO; default;
    property B[const index: integer]: boolean read GetB write PutB;
    property I[const index: integer]: SuperInt read GetI write PutI;
    property D[const index: integer]: Double read GetD write PutD;
    property S[const index: integer]: string read GetS write PutS;
{$IFDEF SUPER_METHOD}
    property M[const index: integer]: TSuperMethod read GetM write PutM;
{$ENDIF}
    property A[const index: integer]: TSuperArray read GetA;
  end;

  TSuperWriter = class
  protected
    // abstact methods to overide
    function Append(buf: PChar; Size: Integer): Integer; overload; virtual; abstract;
    function Append(buf: PChar): Integer; overload; virtual; abstract;
    procedure Reset; virtual; abstract;
  public
    function Write(obj: ISuperObject; format: boolean; level: integer): Integer; virtual;
  end;

  TSuperWriterString = class(TSuperWriter)
  private
    FBuf: PChar;
    FBPos: integer;
    FSize: integer;
  protected
    function Append(buf: PChar; Size: Integer): Integer; overload; override;
    function Append(buf: PChar): Integer; overload; override;
    procedure Reset; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Data: PChar read FBuf;
    property Size: Integer read FSize;
    property Position: integer read FBPos;
  end;

  TSuperWriterStream = class(TSuperWriter)
  private
    FStream: TStream;
  protected
    function Append(buf: PChar; Size: Integer): Integer; override;
    function Append(buf: PChar): Integer; override;
    procedure Reset; override;
  public
    constructor Create(AStream: TStream); reintroduce; virtual;
  end;

  TSuperWriterFake = class(TSuperWriter)
  private
    FSize: Integer;
  protected
    function Append(buf: PChar; Size: Integer): Integer; override;
    function Append(buf: PChar): Integer; override;
    procedure Reset; override;
  public
    constructor Create; reintroduce; virtual;
    property size: integer read FSize;
  end;

  TSuperWriterSock = class(TSuperWriter)
  private
    FSocket: longint;
    FSize: Integer;
  protected
    function Append(buf: PChar; Size: Integer): Integer; override;
    function Append(buf: PChar): Integer; override;
    procedure Reset; override;
  public
    constructor Create(ASocket: longint); reintroduce; virtual;
    property Socket: longint read FSocket;
    property Size: Integer read FSize;
  end;

  TSuperTokenizerError = (
    teSuccess,
    teContinue,
    teDepth,
    teParseEof,
    teParseUnexpected,
    teParseNull,
    teParseBoolean,
    teParseNumber,
    teParseArray,
    teParseObjectKeyName,
    teParseObjectKeySep,
    teParseObjectValueSep,
    teParseString,
    teParseComment,
    teEvalObject,
    teEvalArray,
    teEvalMethod,
    teEvalInt
  );

  TSuperTokenerState = (
    tsEatws,
    tsStart,
    tsFinish,
    tsNull,
    tsCommentStart,
    tsComment,
    tsCommentEol,
    tsCommentEnd,
    tsString,
    tsStringEscape,
{$IFDEF SUPER_EXTENDED_SYNTAX}
    tsIdentifier,
{$ENDIF}
    tsEscapeUnicode,
    tsBoolean,
    tsNumber,
    tsArray,
    tsArrayAdd,
    tsArraySep,
    tsObjectFieldStart,
    tsObjectField,
{$IFDEF SUPER_EXTENDED_SYNTAX}
    tsObjectUnquotedField,
{$ENDIF}
    tsObjectFieldEnd,
    tsObjectValue,
    tsObjectValueAdd,
    tsObjectSep,

    tsEvalProperty,
    tsEvalArray,
    tsEvalMethod,

    tsParamValue,
    tsParamPut
  );

  PSuperTokenerSrec = ^TSuperTokenerSrec;
  TSuperTokenerSrec = record
    state, saved_state: TSuperTokenerState;
    obj: ISuperObject;
    current: ISuperObject;
    field_name: PChar;
    parent: ISuperObject;
    gparent: ISuperObject;
  end;

  TSuperTokenizer = class
  public
    str: PChar;
    pb: TSuperWriterString;
    depth, is_double, st_pos, char_offset: Integer;
    err:  TSuperTokenizerError;
    ucs_char: Cardinal;
    quote_char: char;
    stack: array[0..SUPER_TOKENER_MAX_DEPTH-1] of TSuperTokenerSrec;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ResetLevel(adepth: integer);
    procedure Reset;
  end;

  // supported object types
  TSuperType = (
    stNull,
    stBoolean,
    stDouble,
    stInt,
    stObject,
    stArray,
    stString
{$IFDEF SUPER_METHOD}
    ,stMethod
{$ENDIF}
  );

  TSuperValidateError = (
    veRuleMalformated,
    veFieldIsRequired,
    veInvalidDataType,
    veFieldNotFound,
    veUnexpectedField,
    veDuplicateEntry,
    veValueNotInEnum,
    veInvalidLength,
    veInvalidRange
  );

  TSuperFindOption = (
    foCreatePath,
    foPutValue
{$IFDEF SUPER_METHOD}
    ,foCallMethod
{$ENDIF}
  );

  TSuperFindOptions = set of TSuperFindOption;
  TSuperCompareResult = (cpLess, cpEqu, cpGreat, cpError);
  TSuperOnValidateError = procedure(sender: Pointer; error: TSuperValidateError; const objpath: string);

  ISuperObject = interface
  ['{4B86A9E3-E094-4E5A-954A-69048B7B6327}']
    function GetDataType: TSuperType;
    function GetProcessing: boolean;
    procedure SetProcessing(value: boolean);

    function GetO(const path: string): ISuperObject;
    procedure PutO(const path: string; Value: ISuperObject);
    function GetB(const path: string): Boolean;
    procedure PutB(const path: string; Value: Boolean);
    function GetI(const path: string): SuperInt;
    procedure PutI(const path: string; Value: SuperInt);
    function GetD(const path: string): Double;
    procedure PutD(const path: string; Value: Double);
    function GetS(const path: string): string;
    procedure PutS(const path: string; const Value: string);
{$IFDEF SUPER_METHOD}
    function GetM(const path: string): TSuperMethod;
    procedure PutM(const path: string; Value: TSuperMethod);
{$ENDIF}
    function GetA(const path: string): TSuperArray;

    // Writers
    function SaveTo(stream: TStream; format: boolean = false): integer; overload;
    function SaveTo(const FileName: string; format: boolean = false): integer; overload;
    function SaveTo(socket: longint; format: boolean = false): integer; overload;
    function CalcSize(format: boolean = false): integer;

    // convert
    function AsBoolean: Boolean;
    function AsInteger: SuperInt;
    function AsDouble: Double;
    function AsString: PChar;
    function AsArray: TSuperArray;
    function AsObject: TSuperTableString;
{$IFDEF SUPER_METHOD}
    function AsMethod: TSuperMethod;
{$ENDIF}
    function AsJSon(format: boolean = false): PChar;

    procedure Clear(all: boolean = false);

    property O[const path: string]: ISuperObject read GetO write PutO; default;
    property B[const path: string]: boolean read GetB write PutB;
    property I[const path: string]: SuperInt read GetI write PutI;
    property D[const path: string]: Double read GetD write PutD;
    property S[const path: string]: string read GetS write PutS;
{$IFDEF SUPER_METHOD}
    property M[const path: string]: TSuperMethod read GetM write PutM;
{$ENDIF}
    property A[const path: string]: TSuperArray read GetA;

{$IFDEF SUPER_METHOD}
    function call(const path: string; param: ISuperObject = nil): ISuperObject; overload;
    function call(const path, param: string): ISuperObject; overload;
{$ENDIF}
    // clone a node
    function Clone: ISuperObject;
    // merges tow objects of same type, if reference is true then nodes are not cloned
    procedure Merge(obj: ISuperObject; reference: boolean = false); overload;
    procedure Merge(const str: string); overload;

    // validate methods
    function Validate(const rules: string; const defs: string = ''; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;
    function Validate(rules: ISuperObject; defs: ISuperObject = nil; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;

    // compare
    function Compare(obj: ISuperObject): TSuperCompareResult; overload;
    function Compare(const str: string): TSuperCompareResult; overload;

    // the data type
    function IsType(AType: TSuperType): boolean;
    property DataType: TSuperType read GetDataType;
    property Processing: boolean read GetProcessing write SetProcessing;

    function GetDataPtr: Pointer;
    procedure SetDataPtr(const Value: Pointer);
    property DataPtr: Pointer read GetDataPtr write SetDataPtr;
  end;

  TSuperObject = class(TObject, ISuperObject)
  private
    FRefCount: Integer;
    FProcessing: boolean;
    FDataType: TSuperType;
    Fpb: TSuperWriterString;
    FDataPtr: Pointer;
    FO: record
      case TSuperType of
        stBoolean: (c_boolean: boolean);
        stDouble: (c_double: double);
        stInt: (c_int: SuperInt);
        stObject: (c_object: TSuperTableString);
        stArray: (c_array: TSuperArray);
        stString: (c_string: PChar);
{$IFDEF SUPER_METHOD}
        stMethod: (c_method: TSuperMethod);
{$ENDIF}
      end;
    function GetDataType: TSuperType;
    function GetDataPtr: Pointer;
    procedure SetDataPtr(const Value: Pointer);
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    function GetO(const path: string): ISuperObject;
    procedure PutO(const path: string; Value: ISuperObject);
    function GetB(const path: string): Boolean;
    procedure PutB(const path: string; Value: Boolean);
    function GetI(const path: string): SuperInt;
    procedure PutI(const path: string; Value: SuperInt);
    function GetD(const path: string): Double;
    procedure PutD(const path: string; Value: Double);
    function GetS(const path: string): string;
    procedure PutS(const path: string; const Value: string);
{$IFDEF SUPER_METHOD}
    function GetM(const path: string): TSuperMethod;
    procedure PutM(const path: string; Value: TSuperMethod);
{$ENDIF}
    function GetA(const path: string): TSuperArray;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;

    function GetProcessing: boolean;
    procedure SetProcessing(value: boolean);

    // Writers
    function SaveTo(stream: TStream; format: boolean = false): integer; overload;
    function SaveTo(const FileName: string; format: boolean = false): integer; overload;
    function SaveTo(socket: longint; format: boolean = false): integer; overload;
    function CalcSize(format: boolean = false): integer;
    function AsJSon(format: boolean = false): PChar;

    // parser  ... owned!
    class function Parse(s: PChar; partial: boolean = true; this: ISuperObject = nil; options: TSuperFindOptions = [];
       put: ISuperObject = nil; dt: TSuperType = stNull): ISuperObject;
    class function ParseEx(tok: TSuperTokenizer; str: PChar; len: integer; this: ISuperObject = nil;
      options: TSuperFindOptions = []; put: ISuperObject = nil; dt: TSuperType = stNull): ISuperObject;

    // constructors / destructor
    constructor Create(jt: TSuperType = stObject); overload; virtual;
    constructor Create(b: boolean); overload; virtual;
    constructor Create(i: SuperInt); overload; virtual;
    constructor Create(d: double); overload; virtual;
    constructor Create(p: PChar); overload; virtual;
    constructor Create(const s: string); overload; virtual;
{$IFDEF SUPER_METHOD}
    constructor Create(m: TSuperMethod); overload; virtual;
{$ENDIF}
    destructor Destroy; override;

    // convert
    function AsBoolean: Boolean;
    function AsInteger: SuperInt;
    function AsDouble: Double;
    function AsString: PChar;
    function AsArray: TSuperArray;
    function AsObject: TSuperTableString;
{$IFDEF SUPER_METHOD}
    function AsMethod: TSuperMethod;
{$ENDIF}
    procedure Clear(all: boolean = false); virtual;

    property O[const path: string]: ISuperObject read GetO write PutO; default;
    property B[const path: string]: boolean read GetB write PutB;
    property I[const path: string]: SuperInt read GetI write PutI;
    property D[const path: string]: Double read GetD write PutD;
    property S[const path: string]: string read GetS write PutS;
{$IFDEF SUPER_METHOD}
    property M[const path: string]: TSuperMethod read GetM write PutM;
{$ENDIF}
    property A[const path: string]: TSuperArray read GetA;

{$IFDEF SUPER_METHOD}
    function call(const path: string; param: ISuperObject = nil): ISuperObject; overload; virtual;
    function call(const path, param: string): ISuperObject; overload; virtual;
{$ENDIF}
    // clone a node
    function Clone: ISuperObject;
    // merges tow objects of same type, if reference is true then nodes are not cloned
    procedure Merge(obj: ISuperObject; reference: boolean = false); overload;
    procedure Merge(const str: string); overload;

    // validate methods
    function Validate(const rules: string; const defs: string = ''; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;
    function Validate(rules: ISuperObject; defs: ISuperObject = nil; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;

    // compare
    function Compare(obj: ISuperObject): TSuperCompareResult; overload;
    function Compare(const str: string): TSuperCompareResult; overload;

    // the data type
    function IsType(AType: TSuperType): boolean;
    property DataType: TSuperType read GetDataType;
    // a data pointer to link to something ele, a treeview for example
    property DataPtr: Pointer read GetDataPtr write SetDataPtr;
    property Processing: boolean read GetProcessing;
  end;

  TSuperObjectIter = record
    key: PChar;
    val: ISuperObject;
    Ite: TSuperAvlIterator;
  end;

function ObjectIsError(obj: TSuperObject): boolean;
function ObjectIsType(obj: ISuperObject; typ: TSuperType): boolean;
function ObjectGetType(obj: ISuperObject): TSuperType;

function ObjectFindFirst(obj: ISuperObject; var F: TSuperObjectIter): boolean;
function ObjectFindNext(var F: TSuperObjectIter): boolean;
procedure ObjectFindClose(var F: TSuperObjectIter);

function SO(const s: string = '{}'): ISuperObject;

function JavaToDelphiDateTime(const dt: int64): TDateTime;
function DelphiToJavaDateTime(const dt: TDateTime): int64;

implementation
uses sysutils,
{$IFDEF UNIX}
  baseunix, unix
{$ELSE}
  Windows
{$ENDIF}
{$IFDEF FPC}
  ,sockets
{$ELSE}
  ,WinSock
{$ENDIF};

{$IFDEF DEBUG}
var
  debugcount: integer = 0;
{$ENDIF}

const
  super_number_chars = '0123456789.+-e';
  super_number_chars_set = ['0'..'9','.','+','-','e'];
  super_hex_chars = '0123456789abcdef';
  super_hex_chars_set = ['0'..'9','a'..'f'];

{$IFDEF UNIX}
  {$linklib c}
  function sprintf(buffer, format: PChar; const args: array of const): longint; cdecl; external;
{$ENDIF}

{$ifdef MSWINDOWS}
  function sprintf(buffer, format: PChar): longint; varargs; cdecl; external 'msvcrt.dll';
{$endif}

{$IFDEF UNIX}
function GetTimeBias: integer;
var
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
begin
  fpGetTimeOfDay(@TimeVal, @TimeZone);
  Result := TimeZone.tz_minuteswest;
end;
{$ELSE}
function GetTimeBias: integer;
var
  tzi : TTimeZoneInformation;
begin
  if GetTimeZoneInformation (tzi) = TIME_ZONE_ID_DAYLIGHT then
    Result := tzi.Bias + tzi.DaylightBias else
    Result := tzi.Bias;
end;
{$ENDIF}

function JavaToDelphiDateTime(const dt: int64): TDateTime;
begin
  Result := 25569 + ((dt - (GetTimeBias * 60000)) / 86400000);
end;

function DelphiToJavaDateTime(const dt: TDateTime): int64;
begin
  Result := Round((dt - 25569) * 86400000) + (GetTimeBias * 60000);
end;

function FloatToStr(d: Double): string;
var
 buffer: array[0..255] of char;
 p1, p2: PChar;
begin
{$IFDEF UNIX}
 sprintf(buffer, '%lf', [d]);
{$ELSE}
 sprintf(buffer, '%lf', d);
{$ENDIF}
 p1 := @buffer[0];
 p2 := nil;
 while true do
 begin
   case p1^ of
     '.', '1'..'9': p2 := p1;
     '0':;
   else
     Break
   end;
   inc(p1);
 end;
 if p2 <> nil then p2[1] := #0;
 Result := buffer;
end;

function strdup(s: PChar): PChar;
var
  l: Integer;
begin
  if s <> nil then
  begin
    l := StrLen(s);
    GetMem(Result, l+1);
    move(s^, Result^, l);
    Result[l] := #0;
  end else
    Result := nil;
end;

function SO(const s: string): ISuperObject;
begin
  Result := TSuperObject.Parse(PChar(s));
end;

function ObjectIsError(obj: TSuperObject): boolean;
begin
  Result := PtrUInt(obj) > PtrUInt(-4000);
end;

function ObjectIsType(obj: ISuperObject; typ: TSuperType): boolean;
begin
  if obj <> nil then
    Result := typ = obj.DataType else
    Result := typ = stNull;
end;

function ObjectGetType(obj: ISuperObject): TSuperType;
begin
  if obj <> nil then
    Result := obj.DataType else
    Result := stNull;
end;

function ObjectFindFirst(obj: ISuperObject; var F: TSuperObjectIter): boolean;
var
  i: TSuperAvlEntry;
begin
  if ObjectIsType(obj, stObject) then
  begin
    F.Ite := TSuperAvlIterator.Create(obj.AsObject);
    F.Ite.First;
    i := F.Ite.GetIter;
    if i <> nil then
    begin
      f.key := i.Name;
      f.val := i.Value;
      Result := true;
    end else
      Result := False;
  end else
    Result := False;
end;

function ObjectFindNext(var F: TSuperObjectIter): boolean;
var
  i: TSuperAvlEntry;
begin
  F.Ite.Next;
  i := F.Ite.GetIter;
  if i <> nil then
  begin
    f.key := i.FName;
    f.val := i.Value;
    Result := true;
  end else
    Result := False;
end;

procedure ObjectFindClose(var F: TSuperObjectIter);
begin
  F.Ite.Free;
  F.val := nil;
end;

{ TSuperObject }

constructor TSuperObject.Create(jt: TSuperType);
begin
  inherited Create;
{$IFDEF DEBUG}
  InterlockedIncrement(debugcount);
{$ENDIF}

  FProcessing := false;
  FDataPtr := nil;
  FDataType := jt;
  case FDataType of
    stObject: FO.c_object := TSuperTableString.Create;
    stArray: FO.c_array := TSuperArray.Create;
  else
    FO.c_object := nil;
  end;
  Fpb := nil;
end;

constructor TSuperObject.Create(b: boolean);
begin
  Create(stBoolean);
  FO.c_boolean := b;
end;

constructor TSuperObject.Create(i: SuperInt);
begin
  Create(stInt);
  FO.c_int := i;
end;

constructor TSuperObject.Create(d: double);
begin
  Create(stDouble);
  FO.c_double := d;
end;

constructor TSuperObject.Create(p: PChar);
begin
  Create(stString);
  FO.c_string := strdup(p);
end;

destructor TSuperObject.Destroy;
begin
{$IFDEF DEBUG}
  InterlockedDecrement(debugcount);
{$ENDIF}
  case FDataType of
    stObject: FO.c_object.Free;
    stString: FreeMem(FO.c_string);
    stArray: FO.c_array.Free;
  end;
  if Fpb <> nil then Fpb.Free;
  inherited;
end;

function TSuperObject.IsType(AType: TSuperType): boolean;
begin
  Result := AType = FDataType;
end;

function TSuperObject.AsBoolean: boolean;
begin
  case FDataType of
    stBoolean: Result := FO.c_boolean;
    stInt: Result := (FO.c_int <> 0);
    stDouble: Result := (FO.c_double <> 0);
    stString: Result := (strlen(FO.c_string) <> 0);
  else
    Result := True;
  end;
end;

function TSuperObject.AsInteger: SuperInt;
var
  code: integer;
  cint: SuperInt;
begin
  case FDataType of
    stInt: Result := FO.c_int;
    stDouble: Result := round(FO.c_double);
    stBoolean: Result := ord(FO.c_boolean);
    stString:
      begin
        Val(FO.c_string, cint, code);
        if code = 0 then
          Result := cint else
          Result := 0;
      end;
  else
    Result := 0;
  end;
end;

function TSuperObject.AsDouble: Double;
var
  code: integer;
  cdouble: double;
begin
  case FDataType of
    stDouble: Result := FO.c_double;
    stInt: Result := FO.c_int;
    stBoolean: Result := ord(FO.c_boolean);
    stString:
      begin
        Val(FO.c_string, cdouble, code);
        if code = 0 then
          Result := cdouble else
          Result := 0.0;
      end;
  else
    Result := 0.0;
  end;
end;

function TSuperObject.AsString: PChar;
begin
  if FDataType = stString then
    Result := FO.c_string else
    Result := AsJSon(false);
end;

procedure TSuperObject.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
end;

procedure TSuperObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    raise Exception.Create('Invalid pointer');
end;

function TSuperObject.AsArray: TSuperArray;
begin
  if FDataType = stArray then
    Result := FO.c_array else
    Result := nil;
end;

function TSuperObject.AsObject: TSuperTableString;
begin
  if FDataType = stObject then
    Result := FO.c_object else
    Result := nil;
end;

function TSuperObject.AsJSon(format: boolean): PChar;
begin
  if(Fpb = nil) then
    Fpb := TSuperWriterString.Create else
    Fpb.Reset;

  if(Fpb.Write(self, format, 0) < 0) then
  begin
    Result := '';
    Exit;
  end;
  if Fpb.FBPos > 0 then
    Result := Fpb.FBuf else
    Result := '';
end;

class function TSuperObject.Parse(s: PChar; partial: boolean; this: ISuperObject;
  options: TSuperFindOptions; put: ISuperObject; dt: TSuperType): ISuperObject;
var
  tok: TSuperTokenizer;
  obj: ISuperObject;
begin
  tok := TSuperTokenizer.Create;
  obj := ParseEx(tok, s, -1, this, options, put, dt);
  if(tok.err <> teSuccess) or (not partial and (s[tok.char_offset] <> #0)) then
    Result := nil else
    Result := obj;
  tok.Free;
end;

class function TSuperObject.ParseEx(tok: TSuperTokenizer; str: PChar; len: integer;
  this: ISuperObject; options: TSuperFindOptions; put: ISuperObject; dt: TSuperType): ISuperObject;

  function hexdigit(x: char): byte;
  begin
    if x <= '9' then
      Result := byte(x) - byte('0') else
      Result := (byte(x) and 7) + 9;
  end;
  function min(v1, v2: integer): integer; begin if v1 < v2 then result := v1 else result := v2 end;

var
  obj: ISuperObject;
  c: char;
  utf_out: array[0..2] of byte;
{$IFDEF SUPER_METHOD}
  sm: TSuperMethod;
{$ENDIF}
  numi: SuperInt;
  numd: double;
  code: integer;
  TokRec: PSuperTokenerSrec;
{$IFDEF SUPER_EXTENDED_SYNTAX}
  evalstack: integer;
{$ENDIF}
const
  spaces = [' ',#8,#10,#13,#9];
  alphanum = ['-','_','a'..'z','A'..'Z','0'..'9'];
label out, redo_char;
begin
{$IFDEF SUPER_EXTENDED_SYNTAX}
  evalstack := 0;
{$ENDIF}
  obj := nil;
  Result := nil;
  TokRec := @tok.stack[tok.depth];

  tok.char_offset := 0;
  tok.err := teSuccess;

  repeat
    if (tok.char_offset = len) then
    begin
      if (tok.depth = 0) and (TokRec^.state = tsEatws) and
         (TokRec^.saved_state = tsFinish) then
        tok.err := teSuccess else
        tok.err := teContinue;
      goto out;
    end;

    c := str^;
redo_char:
    case TokRec^.state of
    tsEatws:
      begin
        if c in spaces then {nop} else
        if (c = '/') then
        begin
          tok.pb.Reset;
          tok.pb.Append(@c, 1);
          TokRec^.state := tsCommentStart;
        end else begin
          TokRec^.state := TokRec^.saved_state;
          goto redo_char;
        end
      end;

    tsStart:
      case c of
      '"',
      '''':
        begin
          TokRec^.state := tsString;
          tok.pb.Reset;
          tok.quote_char := c;
        end;
      '-':
        begin
          TokRec^.state := tsNumber;
          tok.pb.Reset;
          tok.is_double := 0;
          goto redo_char;
        end;

      '0'..'9':
        begin
{$IFDEF SUPER_EXTENDED_SYNTAX}
          if (tok.depth = 0) then
            case ObjectGetType(this) of
            stObject:
              begin
                TokRec^.state := tsIdentifier;
                TokRec^.current := this;
                goto redo_char;
              end;
          end;
{$ENDIF}
          TokRec^.state := tsNumber;
          tok.pb.Reset;
          tok.is_double := 0;
          goto redo_char;
        end;
      '{':
        begin
          TokRec^.state := tsEatws;
          TokRec^.saved_state := tsObjectFieldStart;
          TokRec^.current := TSuperObject.Create(stObject);
        end;
      '[':
        begin
          TokRec^.state := tsEatws;
          TokRec^.saved_state := tsArray;
          TokRec^.current := TSuperObject.Create(stArray);
        end;
{$IFDEF SUPER_METHOD}
      '(':
        begin
          if (tok.depth = 0) and ObjectIsType(this, stMethod) then
          begin
            TokRec^.current := this;
            TokRec^.state := tsParamValue;
          end;
        end;
{$ENDIF}
      'N',
      'n':
        begin
          TokRec^.state := tsNull;
          tok.pb.Reset;
          tok.st_pos := 0;
          goto redo_char;
        end;
      'T',
      't',
      'F',
      'f':
        begin
          TokRec^.state := tsBoolean;
          tok.pb.Reset;
          tok.st_pos := 0;
          goto redo_char;
        end;
      else
      {$IFDEF SUPER_EXTENDED_SYNTAX}
        TokRec^.state := tsIdentifier;
        tok.pb.Reset;
        goto redo_char;
      {$ELSE}
        tok.err := teParseUnexpected;
        goto out;
      {$ENDIF}
      end;

    tsFinish:
      begin
        if(tok.depth = 0) then goto out;
        obj := TokRec^.current;
        tok.ResetLevel(tok.depth);
        dec(tok.depth);
        TokRec := @tok.stack[tok.depth];
        goto redo_char;
      end;

    tsNull:
      begin
        tok.pb.Append(@c, 1);
        if (StrLComp('null', tok.pb.FBuf, min(tok.st_pos + 1, 4)) = 0) then
        begin
          if (tok.st_pos = 4) then
{$IFDEF SUPER_EXTENDED_SYNTAX}
          if (c in alphanum) then
            TokRec^.state := tsIdentifier else
{$ENDIF}
          begin
            TokRec^.current := nil;
            TokRec^.saved_state := tsFinish;
            TokRec^.state := tsEatws;
            goto redo_char;
          end;
        end else
        begin
{$IFDEF SUPER_EXTENDED_SYNTAX}
          TokRec^.state := tsIdentifier;
          tok.pb.FBuf[tok.st_pos] := #0;
          dec(tok.pb.FBPos);
          goto redo_char;
{$ELSE}
          tok.err := teParseNull;
          goto out;
{$ENDIF}
        end;
        inc(tok.st_pos);
      end;

    tsCommentStart:
      begin
        if(c = '*') then
        begin
          TokRec^.state := tsComment;
        end else
        if (c = '/') then
        begin
          TokRec^.state := tsCommentEol;
        end else
        begin
          tok.err := teParseComment;
          goto out;
        end;
        tok.pb.Append(@c, 1);
      end;

    tsComment:
      begin
        if(c = '*') then
          TokRec^.state := tsCommentEnd;
        tok.pb.Append(@c, 1);
      end;

    tsCommentEol:
      begin
        if (c = #10) then
          TokRec^.state := tsEatws else
          tok.pb.Append(@c, 1);
      end;

    tsCommentEnd:
      begin
        tok.pb.Append(@c, 1);
        if (c = '/') then
          TokRec^.state := tsEatws else
          TokRec^.state := tsComment;
      end;

    tsString:
      begin
        if (c = tok.quote_char) then
        begin
          TokRec^.current := TSuperObject.Create(tok.pb.Fbuf);
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (c = '\') then
        begin
          TokRec^.saved_state := tsString;
          TokRec^.state := tsStringEscape;
        end else
        begin
          tok.pb.Append(@c, 1);
        end
      end;

{$IFDEF SUPER_EXTENDED_SYNTAX}
    tsEvalProperty:
      begin
        if (TokRec^.current = nil) and (foCreatePath in options) then
        begin
          TokRec^.current := TSuperObject.Create(stObject);
          TokRec^.parent.AsObject.Put(tok.pb.Fbuf, TokRec^.current)
        end else
        if not ObjectIsType(TokRec^.current, stObject) then
        begin
          tok.err := teEvalObject;
          goto out;
        end;
        tok.pb.Reset;
        TokRec^.state := tsIdentifier;
        goto redo_char;
      end;

    tsEvalArray:
      begin
        if (TokRec^.current = nil) and (foCreatePath in options) then
        begin
          TokRec^.current := TSuperObject.Create(stArray);
          TokRec^.parent.AsObject.Put(tok.pb.Fbuf, TokRec^.current)
        end else
        if not ObjectIsType(TokRec^.current, stArray) then
        begin
          tok.err := teEvalArray;
          goto out;
        end;
        tok.pb.Reset;
        TokRec^.state := tsParamValue;
        goto redo_char;
      end;
{$IFDEF SUPER_METHOD}
    tsEvalMethod:
      begin
        if ObjectIsType(TokRec^.current, stMethod) and assigned(TokRec^.current.AsMethod) then
        begin
          tok.pb.Reset;
          TokRec^.state := tsParamValue;
          goto redo_char;
        end else
        begin
          tok.err := teEvalMethod;
          goto out;
        end;

      end;
{$ENDIF}
    tsParamValue:
      begin
        case c of
        ']':
          begin
            TokRec^.state := tsIdentifier;

          end;
        ')':
          begin
            TokRec^.state := tsIdentifier;

          end;
        else
          if (tok.depth >= SUPER_TOKENER_MAX_DEPTH-1) then
          begin
            tok.err := teDepth;
            goto out;
          end;
          inc(evalstack);
          TokRec^.state := tsParamPut;
          inc(tok.depth);
          tok.ResetLevel(tok.depth);
          TokRec := @tok.stack[tok.depth];
          goto redo_char;
        end;
      end;

    tsParamPut:
      begin
        dec(evalstack);
        TokRec^.obj := obj;
        tok.pb.Reset;
        TokRec^.saved_state := tsIdentifier;
        TokRec^.state := tsEatws;

        case ObjectGetType(TokRec^.current) of
          stArray:
            begin
              if c <> ']' then
              begin
                tok.err := teEvalArray;
                goto out;
              end;
            end;
{$IFDEF SUPER_METHOD}
          stMethod:
            begin
              if c <> ')' then
              begin
                tok.err := teEvalMethod;
                goto out;
              end;
            end;
{$ENDIF}
        end;
      end;

    tsIdentifier:
      begin

        if not (c in alphanum) then
        begin
          if (this = nil) then
          begin
            TokRec^.current := TSuperObject.Create(tok.pb.Fbuf);
            TokRec^.saved_state := tsFinish;
            TokRec^.state := tsEatws;
            goto redo_char;
          end else
          begin
           TokRec^.gparent := TokRec^.parent;
           if TokRec^.current = nil then
             TokRec^.parent := this else
             TokRec^.parent := TokRec^.current;

             case ObjectGetType(TokRec^.parent) of
               stObject:
                 case c of
                   '.':
                     begin
                       TokRec^.state := tsEvalProperty;
                       if tok.pb.FBPos > 0 then
                         TokRec^.current := TokRec^.parent.AsObject.Get(tok.pb.Fbuf);
                     end;
                   '[':
                     begin
                       TokRec^.state := tsEvalArray;
                       if tok.pb.FBPos > 0 then
                         TokRec^.current := TokRec^.parent.AsObject.Get(tok.pb.Fbuf);
                     end;
                   '(':
                     begin
                       TokRec^.state := tsEvalMethod;
                       if tok.pb.FBPos > 0 then
                         TokRec^.current := TokRec^.parent.AsObject.Get(tok.pb.Fbuf);
                     end;
                 else
                   if tok.pb.FBPos > 0 then
                     TokRec^.current := TokRec^.parent.AsObject.Get(tok.pb.Fbuf);
                   if (foPutValue in options) and (evalstack = 0) then
                   begin
                     TokRec^.parent.AsObject.Put(tok.pb.Fbuf, put);
                     TokRec^.current := put
                   end else
                   if (TokRec^.current = nil) and (foCreatePath in options) then
                   begin
                     TokRec^.current := TSuperObject.Create(dt);
                     TokRec^.parent.AsObject.Put(tok.pb.Fbuf, TokRec^.current);
                   end;
                   TokRec^.current := TokRec^.parent.AsObject.Get(tok.pb.Fbuf);
                   TokRec^.state := tsFinish;
                   goto redo_char;
                 end;
               stArray:
                 begin
                   if TokRec^.obj <> nil then
                   begin
                     if not ObjectIsType(TokRec^.obj, stInt) or (TokRec^.obj.AsInteger < 0) then
                     begin
                       tok.err := teEvalInt;
                       TokRec^.obj := nil;
                       goto out;
                     end;
                     numi := TokRec^.obj.AsInteger;
                     TokRec^.obj := nil;

                     TokRec^.current := TokRec^.parent.AsArray.GetO(numi);
                     case c of
                       '.':
                         if (TokRec^.current = nil) and (foCreatePath in options) then
                         begin
                           TokRec^.current := TSuperObject.Create(stObject);
                           TokRec^.parent.AsArray.PutO(numi, TokRec^.current);
                         end else
                         if (TokRec^.current = nil) then
                         begin
                           tok.err := teEvalObject;
                           goto out;
                         end;
                       '[':
                         begin
                           if (TokRec^.current = nil) and (foCreatePath in options) then
                           begin
                             TokRec^.current := TSuperObject.Create(stArray);
                             TokRec^.parent.AsArray.Add(TokRec^.current);
                           end else
                           if (TokRec^.current = nil) then
                           begin
                             tok.err := teEvalArray;
                             goto out;
                           end;
                           TokRec^.state := tsEvalArray;
                         end;
                       '(': TokRec^.state := tsEvalMethod;
                     else
                       if (foPutValue in options) and (evalstack = 0) then
                       begin
                         TokRec^.parent.AsArray.PutO(numi, put);
                         TokRec^.current := put;
                       end else
                         TokRec^.current := TokRec^.parent.AsArray.GetO(numi);
                       TokRec^.state := tsFinish;
                       goto redo_char
                     end;
                   end else
                   begin
                     case c of
                       '.':
                         begin
                           if (foPutValue in options) then
                           begin
                             TokRec^.current := TSuperObject.Create(stObject);
                             TokRec^.parent.AsArray.Add(TokRec^.current);
                           end else
                             TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1);
                         end;
                       '[':
                         begin
                           if (foPutValue in options) then
                           begin
                             TokRec^.current := TSuperObject.Create(stArray);
                             TokRec^.parent.AsArray.Add(TokRec^.current);
                           end else
                             TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1);
                           TokRec^.state := tsEvalArray;
                         end;
                       '(':
                         begin
                           if not (foPutValue in options) then
                             TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1) else
                             TokRec^.current := nil;

                           TokRec^.state := tsEvalMethod;
                         end;
                     else
                       if (foPutValue in options) and (evalstack = 0) then
                       begin
                         TokRec^.parent.AsArray.Add(put);
                         TokRec^.current := put;
                       end else
                         TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1);
                       TokRec^.state := tsFinish;
                       goto redo_char
                     end;
                   end;
                 end;
{$IFDEF SUPER_METHOD}
               stMethod:
                 case c of
                   '.':
                     begin
                       TokRec^.current := nil;
                       sm := TokRec^.parent.AsMethod;
                       sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                       TokRec^.obj := nil;
                     end;
                   '[':
                     begin
                       TokRec^.current := nil;
                       sm := TokRec^.parent.AsMethod;
                       sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                       TokRec^.state := tsEvalArray;
                       TokRec^.obj := nil;
                     end;
                   '(':
                     begin
                       TokRec^.current := nil;
                       sm := TokRec^.parent.AsMethod;
                       sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                       TokRec^.state := tsEvalMethod;
                       TokRec^.obj := nil;
                     end;
                 else
                   if not (foPutValue in options) or (evalstack > 0) then
                   begin
                     TokRec^.current := nil;
                     sm := TokRec^.parent.AsMethod;
                     sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                     TokRec^.obj := nil;
                     TokRec^.state := tsFinish;
                     goto redo_char
                   end else
                   begin
                     tok.err := teEvalMethod;
                     TokRec^.obj := nil;
                     goto out;
                   end;
                 end;
{$ENDIF}
             end;
          end;
        end else
        begin
          tok.pb.Append(@c, 1);
        end
      end;
{$ENDIF}

    tsStringEscape:
      case c of
      '"',
      '\',
      '/':
        begin
          tok.pb.Append(@c, 1);
          TokRec^.state := TokRec^.saved_state;
        end;
      'b',
      'n',
      'r',
      't':
        begin
          if(c = 'b') then tok.pb.Append(#8, 1)
          else if(c = 'n') then tok.pb.Append(#10, 1)
          else if(c = 'r') then tok.pb.Append(#13, 1)
          else if(c = 't') then tok.pb.Append(#9, 1);
          TokRec^.state := TokRec^.saved_state;
        end;
      'u':
        begin
          tok.ucs_char := 0;
          tok.st_pos := 0;
          TokRec^.state := tsEscapeUnicode;
        end;
      else
        tok.err := teParseString;
        goto out;
      end;

    tsEscapeUnicode:
      begin
        if (c in super_hex_chars_set) then
        begin
          inc(tok.ucs_char, (cardinal(hexdigit(c)) shl ((3-tok.st_pos)*4)));
          inc(tok.st_pos);
          if (tok.st_pos = 4) then
          begin
            if (tok.ucs_char < $80) then
            begin
              utf_out[0] := tok.ucs_char;
              tok.pb.Append(@utf_out, 1);
            end else
            if (tok.ucs_char < $800) then
            begin
              utf_out[0] := $c0 or (tok.ucs_char shr 6);
              utf_out[1] := $80 or (tok.ucs_char and $3f);
              tok.pb.Append(@utf_out, 2);
            end else
            begin
              utf_out[0] := $e0 or (tok.ucs_char shr 12);
              utf_out[1] := $80 or ((tok.ucs_char shr 6) and $3f);
              utf_out[2] := $80 or (tok.ucs_char and $3f);
              tok.pb.Append(@utf_out, 3);
            end;
            TokRec^.state := TokRec^.saved_state;
          end
        end else
        begin
          tok.err := teParseString;
          goto out;
        end
      end;

    tsBoolean:
      begin
        tok.pb.Append(@c, 1);
        if (StrLComp('true', tok.pb.FBuf, min(tok.st_pos + 1, 4)) = 0) then
        begin
          if (tok.st_pos = 4) then
{$IFDEF SUPER_EXTENDED_SYNTAX}
          if (c in alphanum) then
            TokRec^.state := tsIdentifier else
{$ENDIF}
          begin
            TokRec^.current := TSuperObject.Create(true);
            TokRec^.saved_state := tsFinish;
            TokRec^.state := tsEatws;
            goto redo_char;
          end
        end else
        if (StrLComp('false', tok.pb.FBuf, min(tok.st_pos + 1, 5)) = 0) then
        begin
          if (tok.st_pos = 5) then
{$IFDEF SUPER_EXTENDED_SYNTAX}
          if (c in alphanum) then
            TokRec^.state := tsIdentifier else
{$ENDIF}
          begin
            TokRec^.current := TSuperObject.Create(false);
            TokRec^.saved_state := tsFinish;
            TokRec^.state := tsEatws;
            goto redo_char;
          end
        end else
        begin
{$IFDEF SUPER_EXTENDED_SYNTAX}
          TokRec^.state := tsIdentifier;
          tok.pb.FBuf[tok.st_pos] := #0;
          dec(tok.pb.FBPos);
          goto redo_char;
{$ELSE}
          tok.err := teParseBoolean;
          goto out;
{$ENDIF}
        end;
        inc(tok.st_pos);
      end;

    tsNumber:
      begin
        if (c in super_number_chars_set) then
        begin
          tok.pb.Append(@c, 1);
          if (c in ['.','e']) then tok.is_double := 1;
        end else
        begin
          if (tok.is_double = 0) then
          begin
            val(tok.pb.FBuf, numi, code);
            if ObjectIsType(this, stArray) then
            begin
              if (foPutValue in options) and (evalstack = 0) then
              begin
                this.AsArray.PutO(numi, put);
                TokRec^.current := put;
              end else
                TokRec^.current := this.AsArray.GetO(numi);
            end else
              TokRec^.current := TSuperObject.Create(numi);

          end else
          if (tok.is_double <> 0) then
          begin
            val(tok.pb.FBuf, numd, code);
            TokRec^.current := TSuperObject.Create(numd);
          end else
          begin
            tok.err := teParseNumber;
            goto out;
          end;
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
          goto redo_char;
        end
      end;

    tsArray:
      begin
        if (c = ']') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        begin
          if(tok.depth >= SUPER_TOKENER_MAX_DEPTH-1) then
          begin
            tok.err := teDepth;
            goto out;
          end;
          TokRec^.state := tsArrayAdd;
          inc(tok.depth);
          tok.ResetLevel(tok.depth);
          TokRec := @tok.stack[tok.depth];
          goto redo_char;
        end
      end;

    tsArrayAdd:
      begin
        TokRec^.current.AsArray.Add(obj);
        TokRec^.saved_state := tsArraySep;
        TokRec^.state := tsEatws;
        goto redo_char;
      end;

    tsArraySep:
      begin
        if (c = ']') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (c = ',') then
        begin
          TokRec^.saved_state := tsArray;
          TokRec^.state := tsEatws;
        end else
        begin
          tok.err := teParseArray;
          goto out;
        end
      end;

    tsObjectFieldStart:
      begin
        if (c = '}') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (c in ['"', '''']) then
        begin
          tok.quote_char := c;
          tok.pb.Reset;
          TokRec^.state := tsObjectField;
        end else
{$IFDEF SUPER_EXTENDED_SYNTAX}
        if (c in alphanum) then
        begin
          TokRec^.state := tsObjectUnquotedField;
          tok.pb.Reset;
          goto redo_char;
        end else
{$ENDIF}
        begin
          tok.err := teParseObjectKeyName;
          goto out;
        end
      end;

    tsObjectField:
      begin
        if (c = tok.quote_char) then
        begin
          TokRec^.field_name := strdup(tok.pb.FBuf);
          TokRec^.saved_state := tsObjectFieldEnd;
          TokRec^.state := tsEatws;
        end else
        if (c = '\') then
        begin
          TokRec^.saved_state := tsObjectField;
          TokRec^.state := tsStringEscape;
        end else
        begin
          tok.pb.Append(@c, 1);
        end
      end;

{$IFDEF SUPER_EXTENDED_SYNTAX}
    tsObjectUnquotedField:
      begin
        if not (c in alphanum) then
        begin
          TokRec^.field_name := strdup(tok.pb.FBuf);
          TokRec^.saved_state := tsObjectFieldEnd;
          TokRec^.state := tsEatws;
          goto redo_char;
        end else
        if (c = '\') then
        begin
          TokRec^.saved_state := tsObjectField;
          TokRec^.state := tsStringEscape;
        end else
        begin
          tok.pb.Append(@c, 1);
        end
      end;
{$ENDIF}

    tsObjectFieldEnd:
      begin
        if (c = ':') then
        begin
          TokRec^.saved_state := tsObjectValue;
          TokRec^.state := tsEatws;
        end else
        begin
          tok.err := teParseObjectKeySep;
          goto out;
        end
      end;

    tsObjectValue:
      begin
        if (tok.depth >= SUPER_TOKENER_MAX_DEPTH-1) then
        begin
          tok.err := teDepth;
          goto out;
        end;
        TokRec^.state := tsObjectValueAdd;
        inc(tok.depth);
        tok.ResetLevel(tok.depth);
        TokRec := @tok.stack[tok.depth];
        goto redo_char;
      end;

    tsObjectValueAdd:
      begin
        TokRec^.current.AsObject.Put(TokRec^.field_name, obj);
        FreeMem(TokRec^.field_name);
        TokRec^.field_name := nil;
        TokRec^.saved_state := tsObjectSep;
        TokRec^.state := tsEatws;
        goto redo_char;
      end;

    tsObjectSep:
      begin
        if (c = '}') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (c = ',') then
        begin
          TokRec^.saved_state := tsObjectFieldStart;
          TokRec^.state := tsEatws;
        end else
        begin
          tok.err := teParseObjectValueSep;
          goto out;
        end
      end;
    end;
    inc(str);
    inc(tok.char_offset);
  until c = #0;

  if(TokRec^.state <> tsFinish) and
     (TokRec^.saved_state <> tsFinish) then
    tok.err := teParseEof;

 out:
  if(tok.err = teSuccess) then
  begin
{$IFDEF SUPER_METHOD}
    if (foCallMethod in options) and ObjectIsType(TokRec^.current, stMethod) and assigned(TokRec^.current.AsMethod) then
    begin
      sm := TokRec^.current.AsMethod;
      sm(TokRec^.parent, put, Result);
    end else
{$ENDIF}
    Result := TokRec^.current;
  end else
    Result := nil;
end;

procedure TSuperObject.PutO(const path: string; Value: ISuperObject);
begin
  Parse(PChar(path), true, self, [foCreatePath, foPutValue], Value);
end;

procedure TSuperObject.PutB(const path: string; Value: Boolean);
begin
  Parse(PChar(path), true, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;

procedure TSuperObject.PutD(const path: string; Value: Double);
begin
  Parse(PChar(path), true, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;

procedure TSuperObject.PutI(const path: string; Value: SuperInt);
begin
  Parse(PChar(path), true, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;

procedure TSuperObject.PutS(const path: string; const Value: string);
begin
  Parse(PChar(path), true, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;

function TSuperObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSuperObject.SaveTo(stream: TStream; format: boolean): integer;
var
  pb: TSuperWriterStream;
begin
  pb := TSuperWriterStream.Create(stream);
  if(pb.Write(self, format, 0) < 0) then
  begin
    pb.Reset;
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := stream.Size;
  pb.Free;
end;

function TSuperObject.CalcSize(format: boolean): integer;
var
  pb: TSuperWriterFake;
begin
  pb := TSuperWriterFake.Create;
  if(pb.Write(self, format, 0) < 0) then
  begin
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := pb.FSize;
  pb.Free;
end;

function TSuperObject.SaveTo(socket: Integer; format: boolean): integer;
var
  pb: TSuperWriterSock;
begin
  pb := TSuperWriterSock.Create(socket);
  if(pb.Write(self, format, 0) < 0) then
  begin
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := pb.FSize;
  pb.Free;
end;

constructor TSuperObject.Create(const s: string);
begin
  Create(stString);
  FO.c_string := strdup(PChar(s));
end;

procedure TSuperObject.Clear(all: boolean);
begin
  if FProcessing then exit;
  FProcessing := true;
  try
    case FDataType of
      stBoolean: FO.c_boolean := false;
      stDouble: FO.c_double := 0.0;
      stInt: FO.c_int := 0;
      stObject: FO.c_object.Clear(all);
      stArray: FO.c_array.Clear(all);
      stString:
        begin
          FreeMem(FO.c_string);
          FO.c_string := nil;
        end;
{$IFDEF SUPER_METHOD}
      stMethod: FO.c_method := nil;
{$ENDIF}
    end;
  finally
    FProcessing := false;
  end;
end;

function TSuperObject.Clone: ISuperObject;
var
  ite: TSuperObjectIter;
  arr: TSuperArray;
  j: integer;
begin
  case FDataType of
    stBoolean: Result := TSuperObject.Create(FO.c_boolean);
    stDouble: Result := TSuperObject.Create(FO.c_double);
    stInt: Result := TSuperObject.Create(FO.c_int);
    stString: Result := TSuperObject.Create(FO.c_string);
{$IFDEF SUPER_METHOD}
    stMethod: Result := TSuperObject.Create(FO.c_method);
{$ENDIF}
    stObject:
      begin
        Result := TSuperObject.Create(stObject);
        if ObjectFindFirst(self, ite) then
        with Result.AsObject do
        repeat
          Put(ite.key, ite.val.Clone);
        until not ObjectFindNext(ite);
        ObjectFindClose(ite);
      end;
    stArray:
      begin
        Result := TSuperObject.Create(stArray);
        arr := AsArray;
        with Result.AsArray do
        for j := 0 to arr.Length - 1 do
          Add(arr.GetO(j).Clone);
      end;
  else
    Result := nil;
  end;
end;

procedure TSuperObject.Merge(obj: ISuperObject; reference: boolean);
var
  prop1, prop2: ISuperObject;
  ite: TSuperObjectIter;
  arr: TSuperArray;
  j: integer;
begin
  if ObjectIsType(obj, FDataType) then
  case FDataType of
    stBoolean: FO.c_boolean := obj.AsBoolean;
    stDouble: FO.c_double := obj.AsDouble;
    stInt: FO.c_int := obj.AsInteger;
    stString:
      begin
        FreeMem(FO.c_string);
        FO.c_string := strdup(obj.AsString);
      end;
{$IFDEF SUPER_METHOD}
    stMethod: FO.c_method := obj.AsMethod;
{$ENDIF}
    stObject:
      begin
        if ObjectFindFirst(obj, ite) then
        with FO.c_object do
        repeat
          prop1 := FO.c_object.Get(ite.key);
          if (prop1 <> nil) and (ite.val <> nil) and (prop1.DataType = ite.val.DataType) then
            prop1.Merge(ite.val) else
            if reference then
              Put(ite.key, ite.val) else
              Put(ite.key, ite.val.Clone);
        until not ObjectFindNext(ite);
        ObjectFindClose(ite);
      end;
    stArray:
      begin
        arr := obj.AsArray;
        with FO.c_array do
        for j := 0 to arr.Length - 1 do
        begin
          prop1 := GetO(j);
          prop2 := arr.GetO(j);
          if (prop1 <> nil) and (prop2 <> nil) and (prop1.DataType = prop2.DataType) then
            prop1.Merge(prop2) else
            if reference then
              PutO(j, prop2) else
              PutO(j, prop2.Clone);
        end;
      end;
  end;
end;

procedure TSuperObject.Merge(const str: string);
begin
  Merge(TSuperObject.Parse(PChar(str)), true);
end;

class function TSuperObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSuperObject(Result).FRefCount := 1;
end;

function TSuperObject.GetO(const path: string): ISuperObject;
var
  p: PChar;
begin
  p := PChar(path);
  if p^ = '@' then
  begin
    inc(p);
    Result := Parse(p, true, self, [foCreatePath], nil, stObject);
  end else
    Result := Parse(PChar(path), true, self);
end;

function TSuperObject.GetA(const path: string): TSuperArray;
var
  p: PChar;
  obj: ISuperObject;
begin
  p := PChar(path);
  if p^ = '@' then
  begin
    inc(p);
    obj := Parse(p, true, self, [foCreatePath], nil, stArray);
    if obj <> nil then
      Result := obj.AsArray else
      Result := nil;
  end else
  begin
    obj := Parse(PChar(path), true, self);
    if obj <> nil then
      Result := obj.AsArray else
      Result := nil;
  end;
end;

function TSuperObject.GetB(const path: string): Boolean;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsBoolean else
    Result := false;
end;

function TSuperObject.GetD(const path: string): Double;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsDouble else
    Result := 0.0;
end;

function TSuperObject.GetI(const path: string): SuperInt;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsInteger else
    Result := 0;
end;

function TSuperObject.GetDataPtr: Pointer;
begin
  Result := FDataPtr;
end;

function TSuperObject.GetDataType: TSuperType;
begin
  Result := FDataType
end;

function TSuperObject.GetS(const path: string): string;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsString else
    Result := '';
end;

function TSuperObject.SaveTo(const FileName: string; format: boolean): integer;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmCreate);
  try
    Result := SaveTo(stream, format);
  finally
    stream.Free;
  end;
end;

function TSuperObject.Validate(const rules: string; const defs: string = ''; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean;
begin
  Result := Validate(TSuperObject.Parse(PChar(rules)), TSuperObject.Parse(PChar(defs)), callback, sender);
end;

function TSuperObject.Validate(rules: ISuperObject; defs: ISuperObject = nil; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean;
type
  TDataType = (dtUnknown, dtStr, dtInt, dtFloat, dtNumber, dtText, dtBool,
               dtMap, dtSeq, dtScalar, dtAny);
var
  datatypes: ISuperObject;
  names: ISuperObject;

  function FindInheritedProperty(const prop: PChar; p: ISuperObject): ISuperObject;
  var
    o: ISuperObject;
    e: TSuperAvlEntry;
  begin
    o := p[prop];
    if o <> nil then
      result := o else
      begin
        o := p['inherit'];
        if (o <> nil) and ObjectIsType(o, stString) then
          begin
            e := names.AsObject.Search(o.AsString);
            if (e <> nil) then
              Result := FindInheritedProperty(prop, e.Value) else
              Result := nil;
          end else
            Result := nil;
      end;
  end;

  function FindDataType(o: ISuperObject): TDataType;
  var
    e: TSuperAvlEntry;
    obj: ISuperObject;
  begin
    obj := FindInheritedProperty('type', o);
    if obj <> nil then
    begin
      e := datatypes.AsObject.Search(obj.AsString);
      if  e <> nil then
        Result := TDataType(e.Value.AsInteger) else
        Result := dtUnknown;
    end else
      Result := dtUnknown;
  end;

  procedure GetNames(o: ISuperObject);
  var
    obj: ISuperObject;
    f: TSuperObjectIter;
  begin
    obj := o['name'];
    if ObjectIsType(obj, stString) then
      names[obj.AsString] := o;

    case FindDataType(o) of
      dtMap:
        begin
          obj := o['mapping'];
          if ObjectIsType(obj, stObject) then
          begin
            if ObjectFindFirst(obj, f) then
            repeat
              if ObjectIsType(f.val, stObject) then
                GetNames(f.val);
            until not ObjectFindNext(f);
            ObjectFindClose(f);
          end;
        end;
      dtSeq:
        begin
          obj := o['sequence'];
          if ObjectIsType(obj, stObject) then
            GetNames(obj);
        end;
    end;
  end;

  function FindInheritedField(const prop: PChar; p: ISuperObject): ISuperObject;
  var
    o: ISuperObject;
    e: TSuperAvlEntry;
  begin
    o := p['mapping'];
    if ObjectIsType(o, stObject) then
    begin
      o := o.AsObject.Get(prop);
      if o <> nil then
      begin
        Result := o;
        Exit;
      end;
    end;

    o := p['inherit'];
    if ObjectIsType(o, stString) then
    begin
      e := names.AsObject.Search(o.AsString);
      if (e <> nil) then
        Result := FindInheritedField(prop, e.Value) else
        Result := nil;
    end else
      Result := nil;
  end;

  function InheritedFieldExist(const obj: ISuperObject; p: ISuperObject; const name: string = ''): boolean;
  var
   o: ISuperObject;
   e: TSuperAvlEntry;
   j: TSuperAvlIterator;
  begin
    Result := true;
    o := p['mapping'];
    if ObjectIsType(o, stObject) then
    begin
      j := TSuperAvlIterator.Create(o.AsObject);
      try
        j.First;
        e := j.GetIter;
        while e <> nil do
        begin
          if obj.AsObject.Search(e.Name) = nil then
          begin
            Result := False;
            if assigned(callback) then
              callback(sender, veFieldNotFound, name + '.' + e.Name);
          end;
          j.Next;
          e := j.GetIter;
        end;

      finally
        j.Free;
      end;
    end;

    o := p['inherit'];
    if ObjectIsType(o, stString) then
    begin
      e := names.AsObject.Search(o.AsString);
      if (e <> nil) then
        Result := InheritedFieldExist(obj, e.Value, name) and Result;
    end;
  end;

  function getInheritedBool(f: PChar; p: ISuperObject; default: boolean = false): boolean;
  var
    o: ISuperObject;
  begin
    o := FindInheritedProperty(f, p);
    case ObjectGetType(o) of
      stBoolean: Result := o.AsBoolean;
      stNull: Result := Default;
    else
      Result := default;
      if assigned(callback) then
        callback(sender, veRuleMalformated, f + '');
    end;
  end;

  procedure GetInheritedFieldList(list: ISuperObject; p: ISuperObject);
  var
   o: ISuperObject;
   e: TSuperAvlEntry;
   i: TSuperAvlIterator;
  begin
    Result := true;
    o := p['mapping'];
    if ObjectIsType(o, stObject) then
    begin
      i := TSuperAvlIterator.Create(o.AsObject);
      try
        i.First;
        e := i.GetIter;
        while e <> nil do
        begin
          if list.AsObject.Search(e.Name) = nil then
            list[e.Name] := e.Value;
          i.Next;
          e := i.GetIter;
        end;

      finally
        i.Free;
      end;
    end;

    o := p['inherit'];
    if ObjectIsType(o, stString) then
    begin
      e := names.AsObject.Search(o.AsString);
      if (e <> nil) then
        GetInheritedFieldList(list, e.Value);
    end;
  end;

  function CheckEnum(o: ISuperObject; p: ISuperObject; name: string = ''): boolean;
  var
    enum: ISuperObject;
    i: integer;
  begin
    Result := false;
    enum := FindInheritedProperty('enum', p);
    case ObjectGetType(enum) of
      stArray:
        for i := 0 to enum.AsArray.Length - 1 do
          if StrComp(o.AsString, enum.AsArray[i].AsString) = 0 then
          begin
            Result := true;
            exit;
          end;
      stNull: Result := true;
    else
      Result := false;
      if assigned(callback) then
        callback(sender, veRuleMalformated, '');
      Exit;
    end;

    if (not Result) and assigned(callback) then
      callback(sender, veValueNotInEnum, name);
  end;

  function CheckLength(len: integer; p: ISuperObject; const objpath: string): boolean;
  var
    length, o: ISuperObject;
  begin
    result := true;
    length := FindInheritedProperty('length', p);
    case ObjectGetType(length) of
      stObject:
        begin
          o := length.AsObject.Get('min');
          if (o <> nil) and (o.AsInteger > len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
          o := length.AsObject.Get('max');
          if (o <> nil) and (o.AsInteger < len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
          o := length.AsObject.Get('minex');
          if (o <> nil) and (o.AsInteger >= len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
          o := length.AsObject.Get('maxex');
          if (o <> nil) and (o.AsInteger <= len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
        end;
      stNull: ;
    else
      Result := false;
      if assigned(callback) then
        callback(sender, veRuleMalformated, '');
    end;
  end;

  function CheckRange(obj: ISuperObject; p: ISuperObject; const objpath: string): boolean;
  var
    length, o: ISuperObject;
  begin
    result := true;
    length := FindInheritedProperty('range', p);
    case ObjectGetType(length) of
      stObject:
        begin
          o := length.AsObject.Get('min');
          if (o <> nil) and (o.Compare(obj) = cpGreat) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
          o := length.AsObject.Get('max');
          if (o <> nil) and (o.Compare(obj) = cpLess) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
          o := length.AsObject.Get('minex');
          if (o <> nil) and (o.Compare(obj) in [cpGreat, cpEqu]) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
          o := length.AsObject.Get('maxex');
          if (o <> nil) and (o.Compare(obj) in [cpLess, cpEqu]) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
        end;
      stNull: ;
    else
      Result := false;
      if assigned(callback) then
        callback(sender, veRuleMalformated, '');
    end;
  end;


  function process(o: ISuperObject; p: ISuperObject; objpath: string = ''): boolean;
  var
    ite: TSuperAvlIterator;
    ent: TSuperAvlEntry;
    p2, o2, sequence: ISuperObject;
    s: PChar;
    i: integer;
    uniquelist, fieldlist: ISuperObject;
  begin
    Result := true;
    if (o = nil) then
    begin
      if getInheritedBool('required', p) then
      begin
        if assigned(callback) then
          callback(sender, veFieldIsRequired, objpath);
        result := false;
      end;
    end else
      case FindDataType(p) of
        dtStr:
          case ObjectGetType(o) of
            stString:
              begin
                Result := Result and CheckLength(strlen(o.AsString), p, objpath);
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtBool:
          case ObjectGetType(o) of
            stBoolean:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtInt:
          case ObjectGetType(o) of
            stInt:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtFloat:
          case ObjectGetType(o) of
            stDouble:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtMap:
          case ObjectGetType(o) of
            stObject:
              begin
                // all objects have and match a rule ?
                ite := TSuperAvlIterator.Create(o.AsObject);
                try
                  ite.First;
                  ent := ite.GetIter;
                  while ent <> nil do
                  begin
                    p2 :=  FindInheritedField(ent.Name, p);
                    if ObjectIsType(p2, stObject) then
                      result := process(ent.Value, p2, objpath + '.' + ent.Name) and result else
                    begin
                      if assigned(callback) then
                        callback(sender, veUnexpectedField, objpath + '.' + ent.Name);
                      result := false; // field have no rule
                    end;
                    ite.Next;
                    ent := ite.GetIter;
                  end;
                finally
                  ite.Free;
                end;

                // all expected field exists ?
                Result :=  InheritedFieldExist(o, p, objpath) and Result;
              end;
            stNull: {nop};
          else
            result := false;
            if assigned(callback) then
              callback(sender, veRuleMalformated, objpath);
          end;
        dtSeq:
          case ObjectGetType(o) of
            stArray:
              begin
                sequence := FindInheritedProperty('sequence', p);
                if sequence <> nil then
                case ObjectGetType(sequence) of
                  stObject:
                    begin
                      for i := 0 to o.AsArray.Length - 1 do
                        result := process(o.AsArray.GetO(i), sequence, objpath + '[' + inttostr(i) + ']') and result;
                      if getInheritedBool('unique', sequence) then
                      begin
                        // type is unique ?
                        uniquelist := TSuperObject.Create(stObject);
                        try
                          for i := 0 to o.AsArray.Length - 1 do
                          begin
                            s := o.AsArray.GetO(i).AsString;
                            if s <> nil then
                            begin
                              if uniquelist.AsObject.Search(s) = nil then
                                uniquelist[s] := nil else
                                begin
                                  Result := False;
                                  if Assigned(callback) then
                                    callback(sender, veDuplicateEntry, objpath + '[' + inttostr(i) + ']');
                                end;
                            end;
                          end;
                        finally
                          uniquelist := nil;
                        end;
                      end;

                      // field is unique ?
                      if (FindDataType(sequence) = dtMap) then
                      begin
                        fieldlist := TSuperObject.Create(stObject);
                        try
                          GetInheritedFieldList(fieldlist, sequence);
                          ite := TSuperAvlIterator.Create(fieldlist.AsObject);
                          try
                            ite.First;
                            ent := ite.GetIter;
                            while ent <> nil do
                            begin
                              if getInheritedBool('unique', ent.Value) then
                              begin
                                uniquelist := TSuperObject.Create(stObject);
                                try
                                  for i := 0 to o.AsArray.Length - 1 do
                                  begin
                                    o2 := o.AsArray.GetO(i);
                                    if o2 <> nil then
                                    begin
                                      s := o2.AsObject.Get(ent.Name).AsString;
                                      if s <> nil then
                                      if uniquelist.AsObject.Search(s) = nil then
                                        uniquelist[s] := nil else
                                        begin
                                          Result := False;
                                          if Assigned(callback) then
                                            callback(sender, veDuplicateEntry, objpath + '[' + inttostr(i) + '].' + ent.name);
                                        end;
                                    end;
                                  end;
                                finally
                                  uniquelist := nil;
                                end;
                              end;
                              ite.Next;
                              ent := ite.GetIter;
                            end;
                          finally
                            ite.Free;
                          end;
                        finally
                          fieldlist := nil;
                        end;
                      end;


                    end;
                  stNull: {nop};
                else
                  result := false;
                  if assigned(callback) then
                    callback(sender, veRuleMalformated, objpath);
                end;
                Result := Result and CheckLength(o.AsArray.Length, p, objpath);

              end;
          else
            result := false;
            if assigned(callback) then
              callback(sender, veRuleMalformated, objpath);
          end;
        dtNumber:
          case ObjectGetType(o) of
            stInt,
            stDouble:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtText:
          case ObjectGetType(o) of
            stInt,
            stDouble,
            stString:
              begin
                result := result and CheckLength(strlen(o.AsString), p, objpath);
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtScalar:
          case ObjectGetType(o) of
            stBoolean,
            stDouble,
            stInt,
            stString:
              begin
                result := result and CheckLength(strlen(o.AsString), p, objpath);
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtAny:;
      else
        if assigned(callback) then
          callback(sender, veRuleMalformated, objpath);
        result := false;
      end;
      Result := Result and CheckEnum(o, p, objpath)

  end;
var
  j: integer;

begin
  Result := False;
  datatypes := TSuperObject.Create(stObject);
  names := TSuperObject.Create;
  try
    datatypes.I['str'] := ord(dtStr);
    datatypes.I['int'] := ord(dtInt);
    datatypes.I['float'] := ord(dtFloat);
    datatypes.I['number'] := ord(dtNumber);
    datatypes.I['text'] := ord(dtText);
    datatypes.I['bool'] := ord(dtBool);
    datatypes.I['map'] := ord(dtMap);
    datatypes.I['seq'] := ord(dtSeq);
    datatypes.I['scalar'] := ord(dtScalar);
    datatypes.I['any'] := ord(dtAny);

    if ObjectIsType(defs, stArray) then
      for j := 0 to defs.AsArray.Length - 1 do
        if ObjectIsType(defs.AsArray[j], stObject) then
          GetNames(defs.AsArray[j]) else
          begin
            if assigned(callback) then
              callback(sender, veRuleMalformated, '');
            Exit;
          end;


    if ObjectIsType(rules, stObject) then
      GetNames(rules) else
      begin
        if assigned(callback) then
          callback(sender, veRuleMalformated, '');
        Exit;
      end;

    Result := process(self, rules);

  finally
    datatypes := nil;
    names := nil;
  end;
end;

function TSuperObject._AddRef: Integer; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TSuperObject._Release: Integer; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TSuperObject.Compare(const str: string): TSuperCompareResult;
begin
  Result := Compare(TSuperObject.Parse(PChar(str)));
end;

function TSuperObject.Compare(obj: ISuperObject): TSuperCompareResult;
  function GetIntCompResult(const i: int64): TSuperCompareResult;
  begin
    if i < 0 then result := cpLess else
    if i = 0 then result := cpEqu else
      Result := cpGreat;
  end;

  function GetDblCompResult(const d: double): TSuperCompareResult;
  begin
    if d < 0 then result := cpLess else
    if d = 0 then result := cpEqu else
      Result := cpGreat;
  end;

begin
  case DataType of
    stBoolean:
      case ObjectGetType(obj) of
        stBoolean: Result := GetIntCompResult(ord(FO.c_boolean) - ord(obj.AsBoolean));
        stDouble:  Result := GetDblCompResult(ord(FO.c_boolean) - obj.AsDouble);
        stInt:     Result := GetIntCompResult(ord(FO.c_boolean) - obj.AsInteger);
        stString:  Result := GetIntCompResult(StrComp(AsString, obj.AsString));
      else
        Result := cpError;
      end;
    stDouble:
      case ObjectGetType(obj) of
        stBoolean: Result := GetDblCompResult(FO.c_double - ord(obj.AsBoolean));
        stDouble:  Result := GetDblCompResult(FO.c_double - obj.AsDouble);
        stInt:     Result := GetDblCompResult(FO.c_double - obj.AsInteger);
        stString:  Result := GetIntCompResult(StrComp(AsString, obj.AsString));
      else
        Result := cpError;
      end;
    stInt:
      case ObjectGetType(obj) of
        stBoolean: Result := GetIntCompResult(FO.c_int - ord(obj.AsBoolean));
        stDouble:  Result := GetDblCompResult(FO.c_int - obj.AsDouble);
        stInt:     Result := GetIntCompResult(FO.c_int - obj.AsInteger);
        stString:  Result := GetIntCompResult(StrComp(AsString, obj.AsString));
      else
        Result := cpError;
      end;
    stString:
      case ObjectGetType(obj) of
        stBoolean,
        stDouble,
        stInt,
        stString:  Result := GetIntCompResult(StrComp(AsString, obj.AsString));
      else
        Result := cpError;
      end;
  else
    Result := cpError;
  end;
end;

{$IFDEF SUPER_METHOD}
function TSuperObject.AsMethod: TSuperMethod;
begin
  if FDataType = stMethod then
    Result := FO.c_method else
    Result := nil;
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
constructor TSuperObject.Create(m: TSuperMethod);
begin
  Create(stMethod);
  FO.c_method := m;
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
function TSuperObject.GetM(const path: string): TSuperMethod;
var
  p: PChar;
  v: ISuperObject;
begin
  p := PChar(path);
  if p^ = '@' then
  begin
    inc(p);
    v := Parse(p, true, Self, [foCreatePath], nil, stMethod);
  end else
    v := Parse(PChar(path), true, Self);

  if (v <> nil) and (ObjectGetType(v) = stMethod) then
    Result := v.AsMethod else
    Result := nil;

end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
procedure TSuperObject.PutM(const path: string; Value: TSuperMethod);
begin
  Parse(PChar(path), true, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
function TSuperObject.call(const path: string; param: ISuperObject): ISuperObject;
begin
  Result := Parse(PChar(path), true, Self, [foCallMethod], param);
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
function TSuperObject.call(const path, param: string): ISuperObject;
begin
  Result := Parse(PChar(path), true, Self, [foCallMethod], TSuperObject.Parse(PChar(param)));
end;
{$ENDIF}

function TSuperObject.GetProcessing: boolean;
begin
  Result := FProcessing;
end;

procedure TSuperObject.SetDataPtr(const Value: Pointer);
begin
  FDataPtr := Value;
end;

procedure TSuperObject.SetProcessing(value: boolean);
begin
  FProcessing := value;
end;

{ TSuperArray }

function TSuperArray.Add(Data: ISuperObject): Integer;
begin
  Result := FLength;
  PutO(Result, data);
end;

procedure TSuperArray.Clear(all: boolean);
var
  j: Integer;
begin
  for j := 0 to FLength - 1 do
    if FArray^[j] <> nil then
    begin
      if all then
        FArray^[j].Clear(all);
      FArray^[j] := nil;
    end;
  FLength := 0;
end;

constructor TSuperArray.Create;
begin
  inherited Create;
  FSize := SUPER_ARRAY_LIST_DEFAULT_SIZE;
  FLength := 0;
  GetMem(FArray, sizeof(Pointer) * FSize);
  FillChar(FArray^, sizeof(Pointer) * FSize, 0);
end;

destructor TSuperArray.Destroy;
begin
  Clear;
  FreeMem(FArray);
  inherited;
end;

function TSuperArray.Expand(max: Integer): Integer;
var
  new_size: Integer;
begin
  if (max < FSize) then
  begin
    Result := 0;
    Exit;
  end;
  if max < FSize shl 1 then
    new_size := FSize shl 1 else
    new_size := max;
  ReallocMem(FArray, new_size * sizeof(Pointer));
  FillChar(Pointer(PtrInt(FArray) + (FSize *sizeof(Pointer)))^, (new_size - FSize)*sizeof(Pointer), 0);
  FSize := new_size;
  Result := 0;
end;

function TSuperArray.GetO(const index: Integer): ISuperObject;
begin
  if(index >= FLength) then
    Result := nil else
    Result := FArray^[index];
end;

function TSuperArray.GetA(const index: integer): TSuperArray;
begin
  Result := GetO(index).AsArray;
end;

function TSuperArray.GetB(const index: integer): Boolean;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsBoolean else
    Result := false;
end;

function TSuperArray.GetD(const index: integer): Double;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsDouble else
    Result := 0.0;
end;

function TSuperArray.GetI(const index: integer): SuperInt;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsInteger else
    Result := 0;
end;

function TSuperArray.GetS(const index: integer): string;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsString else
    Result := '';
end;

procedure TSuperArray.PutO(const index: Integer; Value: ISuperObject);
begin
  if(Expand(index) <> 0) then
    Exit;
  FArray^[index] := value;
  if(FLength <= index) then FLength := index + 1;
end;

procedure TSuperArray.PutB(const index: integer; Value: Boolean);
begin
  PutO(index, TSuperObject.Create(Value));
end;

procedure TSuperArray.PutD(const index: integer; Value: Double);
begin
  PutO(index, TSuperObject.Create(Value));
end;

procedure TSuperArray.PutI(const index: integer; Value: SuperInt);
begin
  PutO(index, TSuperObject.Create(Value));
end;

procedure TSuperArray.PutS(const index: integer; const Value: string);
begin
  PutO(index, TSuperObject.Create(Value));
end;

{$IFDEF SUPER_METHOD}
function TSuperArray.GetM(const index: integer): TSuperMethod;
var
  v: ISuperObject;
begin
  v := GetO(index);
  if (ObjectGetType(v) = stMethod) then
    Result := v.AsMethod else
    Result := nil;
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
procedure TSuperArray.PutM(const index: integer; Value: TSuperMethod);
begin
  PutO(index, TSuperObject.Create(Value));
end;
{$ENDIF}

{ TSuperWriterString }

function TSuperWriterString.Append(buf: PChar; Size: Integer): Integer;
  function max(a, b: Integer): integer; begin if a > b then  Result := a else Result := b end;
begin
  Result := size;
  if Size > 0 then
  begin
    if (FSize - FBPos <= size) then
    begin
      FSize := max(FSize * 2, FBPos + size + 8);
      ReallocMem(FBuf, FSize);
    end;
    // fast move
    case size of
    1: FBuf[FBPos] := buf^;
    2: PWord(@FBuf[FBPos])^ := PWord(buf)^;
    4: PInteger(@FBuf[FBPos])^ := PInteger(buf)^;
    8: PInt64(@FBuf[FBPos])^ := PInt64(buf)^;
    else
      move(buf^, FBuf[FBPos], size);
    end;
    inc(FBPos, size);
    FBuf[FBPos] := #0;
  end;
end;

function TSuperWriterString.Append(buf: PChar): Integer;
begin
  Result := Append(buf, strlen(buf));
end;

constructor TSuperWriterString.Create;
begin
  inherited;
  FSize := 32;
  FBPos := 0;
  GetMem(FBuf, FSize);
end;

destructor TSuperWriterString.Destroy;
begin
  if FBuf <> nil then
    FreeMem(FBuf, FSize);
  inherited;
end;

procedure TSuperWriterString.Reset;
begin
  FBuf[0] := #0;
  FBPos := 0;
end;

{ TSuperWriterStream }

function TSuperWriterStream.Append(buf: PChar; Size: Integer): Integer;
begin
  Result := FStream.Write(buf^, Size);
end;

function TSuperWriterStream.Append(buf: PChar): Integer;
begin
  Result := FStream.Write(buf^, StrLen(buf));
end;

constructor TSuperWriterStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TSuperWriterStream.Reset;
begin
  FStream.Size := 0;
end;

{ TSuperWriterFake }

function TSuperWriterFake.Append(buf: PChar; Size: Integer): Integer;
begin
  inc(FSize, Size);
  Result := FSize;
end;

function TSuperWriterFake.Append(buf: PChar): Integer;
begin
  inc(FSize, Strlen(buf));
  Result := FSize;
end;

constructor TSuperWriterFake.Create;
begin
  inherited Create;
  FSize := 0;
end;

procedure TSuperWriterFake.Reset;
begin
  FSize := 0;
end;

{ TSuperWriterSock }

function TSuperWriterSock.Append(buf: PChar; Size: Integer): Integer;
begin
  Result := send(FSocket, buf^, size, 0);
  inc(FSize, Result);
end;

function TSuperWriterSock.Append(buf: PChar): Integer;
begin
  Result := send(FSocket, buf^, strlen(buf), 0);
  inc(FSize, Result);
end;

constructor TSuperWriterSock.Create(ASocket: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FSize := 0;
end;

procedure TSuperWriterSock.Reset;
begin
  FSize := 0;
end;

{ TSuperWriter }

function TSuperWriter.Write(obj: ISuperObject; format: boolean; level: integer): Integer;
  function Escape(str: PChar): Integer;
  var
    pos, start_offset: Integer;
    c: char;
    buf: array[0..5] of char;
  begin
    pos := 0; start_offset := 0;
    repeat
      c := str[pos];
      case c of
        #0: break;
        #8,#10,#13,#9,'"','\','/':
          begin
            if(pos - start_offset > 0) then
              Append(str + start_offset, pos - start_offset);
            if(c = #8) then Append('\b', 2)
            else if (c = #10) then Append('\n', 2)
            else if (c = #13) then Append('\r', 2)
            else if (c = #9) then Append('\t', 2)
            else if (c = '"') then Append('\"', 2)
            else if (c = '\') then Append('\\', 2)
            else if (c = '/') then Append('\/', 2);
            inc(pos);
            start_offset := pos;
          end;
      else
        if (c < ' ') then
        begin
          if(pos - start_offset > 0) then
            Append(str + start_offset, pos - start_offset);
          buf := '\u00';
          buf[4] := super_hex_chars[ord(c) shr 4];
          buf[5] := super_hex_chars[ord(c) and $f];
          Append(buf, 6);
          inc(pos);
          start_offset := pos;
        end else
          inc(pos);
      end;
    until c = #0;
    if(pos - start_offset > 0) then
      Append(str + start_offset, pos - start_offset);
    Result := 0;
  end;

  procedure Indent(i: shortint; r: boolean);
  begin
    inc(level, i);
    if r then
    begin
      Append(#10, 1);
      for i := 0 to level - 1 do
        Append(' ', 1);
    end;
  end;
var
  i,j: Integer;
  iter: TSuperObjectIter;
  s: string;
  val: ISuperObject;
begin
  if (obj = nil) or obj.Processing then
  begin
    Result := Append('null', 4);
    Exit;
  end;

  obj.Processing := true;
  try
    case ObjectGetType(obj) of
      stObject:
        if obj.AsObject.count > 0 then
        begin
          i := 0;
          Append('{', 1);
          if format then indent(1, false);
          if ObjectFindFirst(obj, iter) then
          repeat
  {$IFDEF SUPER_METHOD}
            if (iter.val = nil) or not ObjectIsType(iter.val, stMethod) then
            begin
  {$ENDIF}
              if (iter.val = nil) or (not iter.val.Processing) then
              begin
                if(i <> 0) then
                  Append(',', 1);
                if format then Indent(0, true);
                Append('"', 1);
                Escape(iter.key);
                if format then
                  Append('": ', 3) else
                  Append('":', 2);
                if(iter.val = nil) then
                  Append('null', 4) else
                  write(iter.val, format, level);
                inc(i);
              end;
  {$IFDEF SUPER_METHOD}
            end;
  {$ENDIF}
          until not ObjectFindNext(iter);
          ObjectFindClose(iter);
          if format then Indent(-1, true);
          Result := Append('}', 1);
        end else
          Result := Append('{}', 2);
      stBoolean:
        begin
          if (obj.AsBoolean) then
            Result := Append('true', 4) else
            Result := Append('false', 5);
        end;
      stInt:
        begin
          str(obj.AsInteger, s);
          Result := Append(PChar(s));
        end;
      stDouble:
        begin
          s := FloatToStr(obj.AsDouble);
          Result := Append(PChar(s));
        end;
      stString:
        begin
          Append('"', 1);
          Escape(obj.AsString);
          Append('"', 1);
          Result := 0;
        end;
      stArray:
        if obj.AsArray.Length > 0 then
        begin
          Append('[', 1);
          if format then Indent(1, true);
          i := 0;
          j := 0;
          while i < obj.AsArray.FLength do
          begin

            val :=  obj.AsArray.GetO(i);
  {$IFDEF SUPER_METHOD}
            if not ObjectIsType(val, stMethod) then
            begin
  {$ENDIF}
              if (val = nil) or (not val.Processing) then
              begin
                if (j <> 0) then
                  Append(',', 1);
                if(val = nil) then
                  Append('null', 4) else
                  write(val, format, level);
                inc(j);
              end;
  {$IFDEF SUPER_METHOD}
            end;
  {$ENDIF}
            inc(i);
          end;
          if format then Indent(-1, false);
          Result := Append(']', 1);
        end else
          Result := Append('[]', 2);
      stNull:
          Result := Append('null', 4);
    else
      Result := 0;
    end;
  finally
    obj.Processing := false;
  end;
end;

{ TSuperTokenizer }

constructor TSuperTokenizer.Create;
begin
  pb := TSuperWriterString.Create;
  Reset;
end;

destructor TSuperTokenizer.Destroy;
begin
  Reset;
  pb.Free;
  inherited;
end;

procedure TSuperTokenizer.Reset;
var
  i: integer;
begin
  for i := depth downto 0 do
    ResetLevel(i);
  depth := 0;
  err := teSuccess;
end;

procedure TSuperTokenizer.ResetLevel(adepth: integer);
begin
  stack[adepth].state := tsEatws;
  stack[adepth].saved_state := tsStart;
  stack[adepth].current := nil;
  FreeMem(stack[adepth].field_name);
  stack[adepth].field_name := nil;
  stack[adepth].obj := nil;
  stack[adepth].parent := nil;
  stack[adepth].gparent := nil;
end;

{ TSuperAvlTree }

constructor TSuperAvlTree.Create;
begin
  FRoot := nil;
  FCount := 0;
end;

destructor TSuperAvlTree.Destroy;
begin
  Clear;
  inherited;
end;

function TSuperAvlTree.IsEmpty: boolean;
begin
  result := FRoot = nil;
end;

function TSuperAvlTree.balance(bal: TSuperAvlEntry): TSuperAvlEntry;
var
  deep, old: TSuperAvlEntry;
  bf: integer;
begin
  if (bal.FBf > 0) then
  begin
    deep := bal.FGt;
    if (deep.FBf < 0) then
    begin
      old := bal;
      bal := deep.FLt;
      old.FGt := bal.FLt;
      deep.FLt := bal.FGt;
      bal.FLt := old;
      bal.FGt := deep;
      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf > 0) then
        begin
          old.FBf := -1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := 1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FGt := deep.FLt;
      deep.FLt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := -1;
        bal.FBf := 1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end else
  begin
    (* "Less than" subtree is deeper. *)

    deep := bal.FLt;
    if (deep.FBf > 0) then
    begin
      old := bal;
      bal := deep.FGt;
      old.FLt := bal.FGt;
      deep.FGt := bal.FLt;
      bal.FGt := old;
      bal.FLt := deep;

      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf < 0) then
        begin
          old.FBf := 1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := -1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FLt := deep.FGt;
      deep.FGt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := 1;
        bal.FBf := -1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end;
  Result := bal;
end;

function TSuperAvlTree.Insert(h: TSuperAvlEntry): TSuperAvlEntry;
var
  unbal, parentunbal, hh, parent: TSuperAvlEntry;
  depth, unbaldepth: longint;
  cmp: integer;
  unbalbf: integer;
  branch: TSuperAvlBitArray;
  p: Pointer;
begin
  inc(FCount);
  h.FLt := nil;
  h.FGt := nil;
  h.FBf := 0;
  branch := [];

  if (FRoot = nil) then
    FRoot := h
  else
  begin
    unbal := nil;
    parentunbal := nil;
    depth := 0;
    unbaldepth := 0;
    hh := FRoot;
    parent := nil;
    repeat
      if (hh.FBf <> 0) then
      begin
        unbal := hh;
        parentunbal := parent;
        unbaldepth := depth;
      end;
      if hh.FHash <> h.FHash then
      begin
        if hh.FHash < h.FHash then cmp := -1 else
        if hh.FHash > h.FHash then cmp := 1 else
          cmp := 0;
      end else
        cmp := CompareNodeNode(h, hh);
      if (cmp = 0) then
      begin
        Result := hh;
        //exchange data
        p := hh.Ptr;
        hh.FPtr := h.Ptr;
        h.FPtr := p;
        doDeleteEntry(h, false);
        dec(FCount);
        exit;
      end;
      parent := hh;
      if (cmp > 0) then
      begin
        hh := hh.FGt;
        include(branch, depth);
      end else
      begin
        hh := hh.FLt;
        exclude(branch, depth);
      end;
      inc(depth);
    until (hh = nil);

    if (cmp < 0) then
      parent.FLt := h else
      parent.FGt := h;

    depth := unbaldepth;

    if (unbal = nil) then
      hh := FRoot
    else
    begin
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      inc(depth);
      unbalbf := unbal.FBf;
      if (cmp < 0) then
        dec(unbalbf) else
        inc(unbalbf);
      if cmp < 0 then
        hh := unbal.FLt else
        hh := unbal.FGt;
      if ((unbalbf <> -2) and (unbalbf <> 2)) then
      begin
        unbal.FBf := unbalbf;
        unbal := nil;
      end;
    end;

    if (hh <> nil) then
      while (h <> hh) do
      begin
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        inc(depth);
        if (cmp < 0) then
        begin
          hh.FBf := -1;
          hh := hh.FLt;
        end else (* cmp > 0 *)
        begin
          hh.FBf := 1;
          hh := hh.FGt;
        end;
      end;

    if (unbal <> nil) then
    begin
      unbal := balance(unbal);
      if (parentunbal = nil) then
        FRoot := unbal
      else
      begin
        depth := unbaldepth - 1;
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        if (cmp < 0) then
          parentunbal.FLt := unbal else
          parentunbal.FGt := unbal;
      end;
    end;
  end;
  result := h;
end;

function TSuperAvlTree.Search(k: PChar; st: TSuperAvlSearchTypes): TSuperAvlEntry;
var
  cmp, target_cmp: integer;
  match_h, h: TSuperAvlEntry;
  ha: Cardinal;
begin
  ha := TSuperAvlEntry.Hash(k);

  match_h := nil;
  h := FRoot;

  if (stLess in st) then
    target_cmp := 1 else
    if (stGreater in st) then
      target_cmp := -1 else
      target_cmp := 0;

  while (h <> nil) do
  begin
    if h.FHash < ha then cmp := -1 else
    if h.FHash > ha then cmp := 1 else
      cmp := 0;

    if cmp = 0 then
      cmp := CompareKeyNode(k, h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        match_h := h;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and SUPER_AVL_MASK_HIGH_BIT) = 0 then
        match_h := h;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
  end;
  result := match_h;
end;

procedure TSuperAvlTree.Delete(k: PChar);
var
  depth, rm_depth: longint;
  branch: TSuperAvlBitArray;
  h, parent, child, path, rm, parent_rm: TSuperAvlEntry;
  cmp, cmp_shortened_sub_with_path, reduced_depth, bf: integer;
  ha: Cardinal;
begin
  ha := TSuperAvlEntry.Hash(k);
  cmp_shortened_sub_with_path := 0;
  branch := [];

  depth := 0;
  h := FRoot;
  parent := nil;
  while true do
  begin
    if (h = nil) then
      exit;
    if h.FHash < ha then cmp := -1 else
    if h.FHash > ha then cmp := 1 else
      cmp := 0;

    if cmp = 0 then
      cmp := CompareKeyNode(k, h);
    if (cmp = 0) then
      break;
    parent := h;
    if (cmp > 0) then
    begin
      h := h.FGt;
      include(branch, depth)
    end else
    begin
      h := h.FLt;
      exclude(branch, depth)
    end;
    inc(depth);
    cmp_shortened_sub_with_path := cmp;
  end;
  rm := h;
  parent_rm := parent;
  rm_depth := depth;

  if (h.FBf < 0) then
  begin
    child := h.FLt;
    exclude(branch, depth);
    cmp := -1;
  end else
  begin
    child := h.FGt;
    include(branch, depth);
    cmp := 1;
  end;
  inc(depth);

  if (child <> nil) then
  begin
    cmp := -cmp;
    repeat
      parent := h;
      h := child;
      if (cmp < 0) then
      begin
        child := h.FLt;
        exclude(branch, depth);
      end else
      begin
        child := h.FGt;
        include(branch, depth);
      end;
      inc(depth);
    until (child = nil);

    if (parent = rm) then
      cmp_shortened_sub_with_path := -cmp else
      cmp_shortened_sub_with_path := cmp;

    if cmp > 0 then
      child := h.FLt else
      child := h.FGt;
  end;

  if (parent = nil) then
    FRoot := child else
    if (cmp_shortened_sub_with_path < 0) then
      parent.FLt := child else
      parent.FGt := child;

  if parent = rm then
    path := h else
    path := parent;

  if (h <> rm) then
  begin
    h.FLt := rm.FLt;
    h.FGt := rm.FGt;
    h.FBf := rm.FBf;
    if (parent_rm = nil) then
      FRoot := h
    else
    begin
      depth := rm_depth - 1;
      if (depth in branch) then
        parent_rm.FGt := h else
        parent_rm.FLt := h;
    end;
  end;

  if (path <> nil) then
  begin
    h := FRoot;
    parent := nil;
    depth := 0;
    while (h <> path) do
    begin
      if (depth in branch) then
      begin
        child := h.FGt;
        h.FGt := parent;
      end else
      begin
        child := h.FLt;
        h.FLt := parent;
      end;
      inc(depth);
      parent := h;
      h := child;
    end;

    reduced_depth := 1;
    cmp := cmp_shortened_sub_with_path;
    while true do
    begin
      if (reduced_depth <> 0) then
      begin
        bf := h.FBf;
        if (cmp < 0) then
          inc(bf) else
          dec(bf);
        if ((bf = -2) or (bf = 2)) then
        begin
          h := balance(h);
          bf := h.FBf;
        end else
          h.FBf := bf;
        reduced_depth := integer(bf = 0);
      end;
      if (parent = nil) then
        break;
      child := h;
      h := parent;
      dec(depth);
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      if (cmp < 0) then
      begin
        parent := h.FLt;
        h.FLt := child;
      end else
      begin
        parent := h.FGt;
        h.FGt := child;
      end;
    end;
    FRoot := h;
  end;
  if rm <> nil then
  begin
    doDeleteEntry(rm, false);
    dec(FCount);
  end;
end;

procedure TSuperAvlTree.Clear(all: boolean);
var
  node1, node2: TSuperAvlEntry;
begin
  node1 := FRoot;
  while node1 <> nil do
  begin
    if (node1.FLt = nil) then
    begin
      node2 := node1.FGt;
      doDeleteEntry(node1, all);
    end
    else
    begin
      node2 := node1.FLt;
      node1.FLt := node2.FGt;
      node2.FGt := node1;
    end;
    node1 := node2;
  end;
  FRoot := nil;
  FCount := 0;
end;

function TSuperAvlTree.CompareKeyNode(k: PChar; h: TSuperAvlEntry): integer;
begin
  Result := StrComp(k, h.FName);
end;

function TSuperAvlTree.CompareNodeNode(node1, node2: TSuperAvlEntry): integer;
begin
  Result := StrComp(node1.FName, node2.FName);
end;

{ TSuperAvlIterator }

(* Initialize depth to invalid value, to indicate iterator is
** invalid.   (Depth is zero-base.)  It's not necessary to initialize
** iterators prior to passing them to the "start" function.
*)

constructor TSuperAvlIterator.Create(tree: TSuperAvlTree);
begin
  FDepth := not 0;
  FTree := tree;
end;

procedure TSuperAvlIterator.Search(k: PChar; st: TSuperAvlSearchTypes);
var
  h: TSuperAvlEntry;
  d: longint;
  cmp, target_cmp: integer;
  ha: Cardinal;
begin
  ha := TSuperAvlEntry.Hash(k);
  h := FTree.FRoot;
  d := 0;
  FDepth := not 0;
  if (h = nil) then
    exit;

  if (stLess in st) then
    target_cmp := 1 else
      if (stGreater in st) then
        target_cmp := -1 else
          target_cmp := 0;

  while true do
  begin
    if h.FHash < ha then cmp := -1 else
    if h.FHash > ha then cmp := 1 else
      cmp := 0;

    if cmp = 0 then
      cmp := FTree.CompareKeyNode(k, h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        FDepth := d;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and SUPER_AVL_MASK_HIGH_BIT) = 0 then
        FDepth := d;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
    if (h = nil) then
      break;
    if (cmp > 0) then
      include(FBranch, d) else
      exclude(FBranch, d);
    FPath[d] := h;
    inc(d);
  end;
end;

procedure TSuperAvlIterator.First;
var
  h: TSuperAvlEntry;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [];
  while (h <> nil) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FLt;
  end;
end;

procedure TSuperAvlIterator.Last;
var
  h: TSuperAvlEntry;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [0..SUPER_AVL_MAX_DEPTH - 1];
  while (h <> nil) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FGt;
  end;
end;

function TSuperAvlIterator.MoveNext: boolean;
begin
  if FDepth = not 0 then
    First else
    Next;
  Result := GetIter <> nil;
end;

function TSuperAvlIterator.GetIter: TSuperAvlEntry;
begin
  if (FDepth = not 0) then
  begin
    result := nil;
    exit;
  end;
  if FDepth = 0 then
    Result := FTree.FRoot else
    Result := FPath[FDepth - 1];
end;

procedure TSuperAvlIterator.Next;
var
  h: TSuperAvlEntry;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FGt else
      h := FPath[FDepth - 1].FGt;

    if (h = nil) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (not (FDepth in FBranch))
    else
    begin
      include(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FLt;
        if (h = nil) then
          break;
        exclude(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

procedure TSuperAvlIterator.Prior;
var
  h: TSuperAvlEntry;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FLt else
      h := FPath[FDepth - 1].FLt;
    if (h = nil) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (FDepth in FBranch)
    else
    begin
      exclude(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FGt;
        if (h = nil) then
          break;
        include(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

procedure TSuperAvlTree.doDeleteEntry(Entry: TSuperAvlEntry; all: boolean);
begin
  Entry.Free;
end;

function TSuperAvlTree.GetEnumerator: TSuperAvlIterator;
begin
  Result := TSuperAvlIterator.Create(Self);
end;

{ TSuperAvlEntry }

constructor TSuperAvlEntry.Create(AName: PChar; Obj: Pointer);
begin
  FName := strdup(AName);
  FPtr := Obj;
  FHash := Hash(FName);
end;

destructor TSuperAvlEntry.Destroy;
begin
  FreeMem(FName);
  inherited;
end;

function TSuperAvlEntry.GetValue: ISuperObject;
begin
  Result := ISuperObject(FPtr)
end;

class function TSuperAvlEntry.Hash(k: PChar): Cardinal;
var
  h: Cardinal;
begin
  h := 0;
  if k <> nil then
    while(k^ <> #0 ) do
    begin
      h := h*129 + byte(k^) + $9e370001;
      inc(k);
    end;
  Result := h;
end;

procedure TSuperAvlEntry.SetValue(const val: ISuperObject);
begin
  ISuperObject(FPtr) := val;
end;

{ TSuperTableString }

function TSuperTableString.GetValues: ISuperObject;
var
  ite: TSuperAvlIterator;
  obj: TSuperAvlEntry;
begin
  Result := TSuperObject.Create(stArray);
  ite := TSuperAvlIterator.Create(Self);
  try
    ite.First;
    obj := ite.GetIter;
    while obj <> nil do
    begin
      Result.AsArray.Add(obj.Value);
      ite.Next;
      obj := ite.GetIter;
    end;
  finally
    ite.Free;
  end;
end;

function TSuperTableString.GetNames: ISuperObject;
var
  ite: TSuperAvlIterator;
  obj: TSuperAvlEntry;
begin
  Result := TSuperObject.Create(stArray);
  ite := TSuperAvlIterator.Create(Self);
  try
    ite.First;
    obj := ite.GetIter;
    while obj <> nil do
    begin
      Result.AsArray.Add(TSuperObject.Create(obj.FName));
      ite.Next;
      obj := ite.GetIter;
    end;
  finally
    ite.Free;
  end;
end;

procedure TSuperTableString.doDeleteEntry(Entry: TSuperAvlEntry; all: boolean);
begin
  if Entry.Ptr <> nil then
  begin
    if all then Entry.Value.Clear(true);
    Entry.Value := nil;
  end;
  inherited;
end;

function TSuperTableString.Get(k: PChar): ISuperObject;
var
  e: TSuperAvlEntry;
begin
  e := Search(k);
  if e <> nil then
    Result := e.Value else
    Result := nil
end;

function TSuperTableString.Put(k: PChar; Obj: ISuperObject): ISuperObject;
var
  entry: TSuperAvlEntry;
begin
  entry := Insert(TSuperAvlEntry.Create(k, Pointer(obj)));
  if entry.FPtr <> nil then
  begin
    // success
    ISuperObject(entry.FPtr)._AddRef;
    Result := entry.Value;
  end else
    Result := nil;
end;

{$IFDEF DEBUG}
initialization

finalization
  Assert(debugcount = 0, 'Memory leak');
{$ENDIF}

end.

