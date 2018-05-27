{PICCore

Contains basic definitions applicable to all PIC microcontroller Cores
                                         Created by Tito Hinostroza   28/04/2018
}
unit PicCore;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc;
const
  PIC_MAX_PINES = 64;                //Max. number of pines for the package
type
  //Union to access bytes of a word
  TWordRec = record
    case byte of
      1 : (W : Word);
      {$IFDEF ENDIAN_LITTLE}
      2 : (L, H: Byte);
      {$ELSE}
      2 : (H, L: Byte);
      {$ENDIF}
    end;

  TPICCellState = (
     cs_impleGPR,   //Implemented. Can be used.
     cs_unimplem    //Not implemented.
  );

  TPICPinType = (
    pptVcc,    //Alimentación
    pptGND,    //Tierra
    pptControl,//Pin de control
    pptPort,   //Puerto Entrada/Salida
    pptUnused  //Pin no usado
  );

  { TPICPin }
  //Model for a phisycal pin of the PIC
  TPICPin = object
    nam: string;      //Eqtiueta o nombre
    typ: TPICPinType; //Tipo de pin
    add: word;        //Dirección en RAM
    bit: byte;        //Bit en RAM
    function GetLabel: string;
  end;
  TPICPinPtr = ^TPICPin;

type //Models for RAM memory

  { TPICRamCell }
  {Modela a una dirección lógica de la memoria RAM. Se ha taratdo de hacer una
  definición eficiente de esta estructura para facilitar la implementación de
  simuladores en tiempo real. Podemos usar un tamaño mediano para este registro,
  porque no esperamos tener muchas celdas de RAM (<1K).}
  TPICRamCellPtr = ^TPICRamCell;
  TPICRamCell = object
  private
    Fvalue  : byte;     //value of the memory
    function Getvalue: byte;
    procedure Setvalue(AValue: byte);
  public
    addr   : word;       //dirección física de memoria, en donde está la celda.
    name   : string;     //Name of the register (or variable)
    used   : boolean;    //Indicates if have been written
    shared : boolean;    //Used to share this register
    state  : TPICCellState; //Status of the cell
    property value: byte read Getvalue write Setvalue;
    property dvalue: byte read Fvalue write Fvalue;   //Direct access to "Fvalue".
    function AvailGPR: boolean;
  public  //Campos para deputación
    breakPnt  : boolean;  //Indicates if this cell have a Breakpoint
    {Be careful on the size of this record, because it's going to be multiplied by 64K}
  public     //Information of position in source code. Used for debug
    rowSrc    : word;     //Row number
    colSrc    : word;     //Column number
    idFile    : SmallInt; //Index to a file. No load the name to save space.
    {Estos campos de cadena ocupan bastante espacio, aún cuado están en NULL. Si se
    quisiera optimizar el uso de RAM, se podría pensar en codificar, varios campos en
    una sola cadena.}
    topLabel   : string;  //Label on the top of the cell.
    topComment : string;  //Comment on the top of the cell.
    sideComment: string;  //Right comment to code
  end;

  TPICRam = array of TPICRamCell;
  TPICRamPtr = ^TPICRam;
  TPICRutExplorRAM = procedure(offs, bnk: byte; regPtr: TPICRamCellPtr) of object;

type  //Models for Flash memory
  TPICFlashCell = record
    value     : word;     //Value of the memory (OpCode)
  end;

type

  { TPicCore }
  {Abcestor of all 8 bits PIC cores}
  TPicCore = class
  private
    FMaxFlash: integer;
    procedure SetMaxFlash(AValue: integer);
  public //Limits
    {This variables are set just one time. So they work as constant.}
    PICBANKSIZE : word;
    PICMAXRAM   : word;
    PICPAGESIZE : word;
    PICMAXFLASH : word;
  public   //General fields
    Model    : string;    //modelo de PIC
    frequen  : integer;   //frecuencia del reloj
    MaxFreq  : integer;   //máxima frecuencia del reloj
    //Propiedades que definen la arquitectura del PIC destino.
    NumBanks: byte;      //Número de bancos de RAM.
    NumPages: byte;      //Número de páginas de memoria Flash.
    MsjError: string;
    {Maximun numbers of Flash cells implemented (Generally used when the first Flash
    page is partially implemented). Applicable only when it's greater than zero}
    property MaxFlash: integer read FMaxFlash write SetMaxFlash;
  public   //Execution control
    nClck   : Int64;    //Contador de ciclos de reloj
    CommStop: boolean;  //Bandera para detener la ejecución
    OnExecutionMsg: procedure(message: string) of object;  //Genera mensaje en ejecución
  protected  //Generation of HEX files
    minUsed  : word;         //Dirección menor de la ROM usada
    maxUsed  : word;         //Dirección mayor de la ROM usdas
    hexLines : TStringList;  //Uusado para crear archivo *.hex
    function HexChecksum(const lin: string): string;
    procedure GenHexComm(comment: string);
    procedure GenHexExAdd(Data: word);
    procedure GenHexData(Address: word; Data: string);
    procedure GenHexEOF;
    function StrHexFlash(i1, i2: integer): string;
  public  //Memories
    ram    : TPICRam;   //memoria RAM
    iRam   : integer;   //puntero a la memoria RAM, para escribir cuando se ensambla o compila código.
    function DisassemblerAt(addr: word; out nBytesProc: byte; useVarName: boolean
      ): string; virtual; abstract; //Desensambla la instrucción actual
  public  //RAM memory functions
    procedure ClearMemRAM;
    procedure DisableAllRAM;
    procedure SetStatRAM(i1, i2: word; status0: TPICCellState);
    function SetStatRAMCom(strDef: string): boolean;
    function MapRAMtoPIN(strDef: string): boolean;
    function HaveConsecRAM(const i, n: word; maxRam: word): boolean; //Indica si hay "n" bytes libres
    procedure UseConsecRAM(const i, n: word);  //Ocupa "n" bytes en la posición "i"
    procedure SetSharedUnused;
    procedure SetSharedUsed;
  public  //ram memory functions
    function UsedMemRAM: word;  //devuelve el total de memoria ram usada
    procedure ClearMemFlash;
  public  //Pins fields
    Npins    : byte;      //Number of pins
    pines    : array[1..PIC_MAX_PINES] of TPICPin;
    procedure ResetPins;
    procedure SetPin(pNumber: integer; pLabel: string; pType: TPICPinType);
    function SetPinName(strDef: string): boolean;
  public  //RAM name managment
    function NameRAM(const addr: word): string;
    procedure SetNameRAM(const addr: word; const nam: string);  //Fija nombre a una celda de RAM
    procedure AddNameRAM(const addr: word; const nam: string);  //Agrega nombre a una celda de RAM
  public  //Execution control
    procedure AddBreakpoint(aPC: word);
    procedure ToggleBreakpoint(aPC: word);
    procedure Exec(aPC: word); virtual; abstract; //Ejecuta la instrucción en la dirección indicada.
    procedure Exec; virtual; abstract; //Ejecuta instrucción actual
    procedure ExecTo(endAdd: word); virtual; abstract; //Ejecuta hasta cierta dirección
    procedure ExecStep; virtual; abstract; //Execute one instruction considering CALL as one instruction
    procedure ExecNCycles(nCyc: integer; out stopped: boolean); virtual; abstract; //Ejecuta hasta cierta dirección
    procedure Reset; virtual; abstract;
    function ReadPC: dword; virtual; abstract;  //Defined DWORD to cover the 18F PC register
    procedure WritePC(AValue: dword); virtual; abstract;
  public  //Others
    procedure addTopLabel(lbl: string);  //Add a comment to the ASM code
    procedure addTopComm(comm: string; replace: boolean = true);  //Add a comment to the ASM code
    procedure addSideComm(comm: string; before: boolean); //Add lateral comment to the ASM code
    procedure addPosInformation(rowSrc, colSrc: word; idFile: byte);
  public  //Initialization
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TPICRamCell }
function TPICRamCell.Getvalue: byte;
begin
  Result := Fvalue;
end;
procedure TPICRamCell.Setvalue(AValue: byte);
begin
  Fvalue := AValue;
end;
function TPICRamCell.AvailGPR: boolean;
{Indica si el registro es una dirección disponible en la memoria RAM.}
begin
  Result := (state = cs_impleGPR);
end;

{ TPICPin }
function TPICPin.GetLabel: string;
{Devuelve una etiqueta para el pin}
begin
  case typ of
  pptUnused: Result := 'NC';
  else
    Result := nam;
  end;
end;

procedure TPicCore.SetMaxFlash(AValue: integer);
begin
  if FMaxFlash = AValue then Exit;
  FMaxFlash := AValue;
end;
{ TPicCore }
//Creación de archivo *.hex
function TPicCore.HexChecksum(const lin:string): string;
//Devuelve los caracteres en hexadecimal del Checksum, para el archivo *.hex
var
  i: Integer;
  chk: Integer;
  part: String;
begin
   i:=1;
   chk := 0;
   while i<length(lin) do begin
     part := copy(lin,i,2);
     chk := chk + StrToInt('$'+part);
     inc(i,2);
   end;
   chk := not chk;  //complemento a 1
   inc(chk);        //complemento a 2
   part := IntToHex(chk,4);  //a hexadecimal
   Result := copy(part, length(part)-1,2);  //recorta
end;
procedure TPicCore.GenHexComm(comment: string);
//Agrega una línea de comentario al archivo *.hex
begin
  hexLines.Add(';'+comment);
end;
procedure TPicCore.GenHexExAdd(Data: word);
//Agrega una línea de Extended Address al archivo *.hex
const RecordType = '04';
var
  ByteCount: Integer;
  lin: String;
begin
  ByteCount := 2;
  lin:= IntToHex(ByteCount,2) + '0000' + RecordType +  IntToHex(Data,4);
  hexLines.Add(':' + lin + HexChecksum(lin));
end;
procedure TPicCore.GenHexData(Address: word; Data: string);
//Agrega una línea de datos al archivo *.hex
const RecordType = '00';
var
  ByteCount: Integer;
  lin: String;
begin
  ByteCount := length(data) div 2;
  lin:= IntToHex(ByteCount,2) + IntToHex(Address*2,4) + RecordType +  Data;
  hexLines.Add(':'+lin + HexChecksum(lin));
end;
procedure TPicCore.GenHexEOF;
//Agrega una línea de Extended Address al archivo *.hex
begin
  hexLines.Add(':00000001FF');
end;
function  TPicCore.StrHexFlash(i1, i2: integer): string;
{Devuelve la cadena, de bytes hexadecimales de la memoria Flash, desde la posición
 i1 hasta i2.}
var
  i: Integer;
  tmp: String;
begin
  Result:='';
  for i:=i1 to i2 do begin
    tmp := IntToHex(ram[i].value,4);
    Result+=copy(tmp,3,2) + copy(tmp,1,2);  //se graba con los bytes invertidos
  end;
end;
//RAM memory functions
procedure TPicCore.ClearMemRAM;
{Limpia el contenido de la memoria}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    ram[i].dvalue := $00;
    ram[i].used := false;
    ram[i].name:='';
    ram[i].shared := false;
//    ram[i].state := cs_unimplem;  //por defecto se considera no implementado
  end;
end;
procedure TPicCore.DisableAllRAM;
{Inicia el estado de toda la memoria RAM física definida em el Modelo.
Solo debería usarse, para cuando se va a definir el hardware del dispositivo.}
var
  i: word;
begin
  for i:=0 to high(ram) do begin
    ram[i].addr     := i;
    ram[i].state    := cs_unimplem;
  end;
  //Inicia estado de pines
  for i:=1 to high(pines) do begin
    pines[i].typ := pptUnused;
  end;
end;
procedure TPicCore.SetStatRAM(i1, i2: word; status0: TPICCellState);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
}
var
  i: Integer;
begin
  for i:=i1 to i2 do begin  //verifica 1 a 1, por seguridad
    if i>PICMAXRAM-1 then continue;  //protection
    ram[i].state := status0;
  end;
end;
function TPicCore.SetStatRAMCom(strDef: string): boolean;
{Define el estado de la memoria RAM, usando una cadena de definición.
La cadena de definición, tiene el formato:
<comando 1>, <comando 2>, ...
Cada comando, tiene el formato:
<dirIni>-<dirFin>:<estado de memoria>
Un ejemplo de cadena de definición, es:
   '000-01F:IMP, 020-07F:NIM'
Si hay error, devuelve FALSE, y el mensaje de error en MsjError.
}
var
  coms: TStringList;
  add1, add2: longint;
  state: TPICCellState;
  staMem, com, str: String;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      if length(com)<>11 then begin
        MsjError := 'Memory definition syntax error: Bad string size.';
        exit(false);
      end;
      if com[4] <> '-' then begin
        MsjError := 'Memory definition syntax error: Expected "-".';
        exit(false);
      end;
      if com[8] <> ':' then begin
        MsjError := 'Memory definition syntax error: Expected ":".';
        exit(false);
      end;
      //Debe tener el formato pedido
      if not TryStrToInt('$'+copy(com,1,3), add1) then begin
        MsjError := 'Memory definition syntax error: Wrong address.';
        exit(false);
      end;
      if not TryStrToInt('$'+copy(com,5,3), add2) then begin
        MsjError := 'Memory definition syntax error: Wrong address.';
        exit(false);
      end;
      staMem := copy(com, 9, 3);
      case staMem of
      'IMP': state := cs_impleGPR;
      'NIM': state := cs_unimplem;
      else
        MsjError := 'Memory definition syntax error: Expected SFR or GPR';
        exit(false);
      end;
      //Ya se tienen los parámetros, para definir la memoria
      SetStatRAM(add1, add2, state);
    end;
  finally
    coms.Destroy;
  end;
end;
function TPicCore.MapRAMtoPIN(strDef: string): boolean;
{Mapea puertos de memoria RAM a pines físicos del dispositivo. Útil para la simulación
La cadena de definición, tiene el formato:
<dirección>:<comando 1>, <comando 2>, ...
Cada comando, tiene el formato:
<dirIni>-<dirFin>:<banco al que está mapeado>
Un ejemplo de cadena de definición, es:
   '005:0-17,1-18,2-1,3-2,4-3'
Si hay error, devuelve FALSE, y el mensaje de error en MsjError.
}
var
  coms: TStringList;
  add1, pin, bit: longint;
  com, str, ramName: String;
  pSep: SizeInt;
begin
  Result := true;
  //Obtiene dirección
  if length(strDef) < 4 then begin
    MsjError := 'Syntax error';
    exit(false);
  end;
  if strDef[4] <> ':' then begin
    MsjError := 'Expected "<3-digits address>"';
    exit(false);
  end;
  if not TryStrToInt('$'+copy(strDef,1,3), add1) then begin
    MsjError := 'Address format error.';
    exit(false);
  end;
  delete(strDef, 1, 4);  //quita la dirección
  //Obtiene lista de asociaciones
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));  //asociación
      if com='' then continue;
      pSep := pos('-',com);   //Posición de separador
      if pSep = 0 then begin
        MsjError := 'Expected "-".';
        exit(false);
      end;
      //Debe tener el formato pedido
//      debugln(com);
      if not TryStrToInt(copy(com,1,pSep-1), bit) then begin
        MsjError := 'Error in bit number.';
        exit(false);
      end;
      if not TryStrToInt(copy(com,pSep+1,length(com)), pin) then begin
        MsjError := 'Error in pin number.';
        exit(false);
      end;
      if (pin<0) or (pin>PIC_MAX_PINES) then begin
        MsjError := 'Pin number out of range.';
        exit(false);
      end;
      if pin>Npins then begin
        MsjError := 'Pin number out of range, for this device.';
        exit(false);
      end;
      //Ya se tiene el BIT y el PIN. Configura datos del PIN
      pines[pin].add := add1;
      pines[pin].bit := bit;
      pines[pin].typ := pptPort;
      ramName := ram[add1].name;
      if ramName='' then ramName := 'PORT';
      pines[pin].nam :=  ramName + '.' + IntToStr(bit);  //Nombre por defecto
    end;
  finally
    coms.Destroy;
  end;
end;
procedure TPicCore.ResetPins;
{Reset the pins of the device.}
var
  i: byte;
begin
  for i:=1 to Npins do begin
    pines[i].nam := ' ';
    pines[i].typ := pptUnused;
  end;
end;
procedure TPicCore.SetPin(pNumber: integer; pLabel: string; pType: TPICPinType);
begin
  if pNumber>PIC_MAX_PINES then exit;
  pines[pNumber].nam := pLabel;
  pines[pNumber].typ := pType;
end;
function TPicCore.SetPinName(strDef: string): boolean;
{Define the name for a specified Pin of the microcontroller, using a string.
"strDef" have the format:
<pin number>:<name of the pin>
On error this function return FALSE, and the error menssage in MsjError.
}
var
  com, pinName: String;
  pNumber: integer;
  pcol: SizeInt;
begin
  com := UpCase(trim(strDef));
  if com='' then exit;
  pcol := Pos(':', strDef);
  if pcol=0 then begin
    MsjError := 'SetPinName: Expected ":".';
    exit(false);
  end;
  //"com" must have the correct format
  if not TryStrToInt( copy(com, 1, pcol-1) , pNumber) then begin
    MsjError := 'SetPinName: Wrong Pin Number.';
    exit(false);
  end;
  pinName :=copy(com, pcol+1, 32);  //limited to 32
  SetPin(pNumber, pinName, pptControl);
end;
function TPicCore.HaveConsecRAM(const i, n: word; maxRam: word): boolean;
{Indica si hay "n" bytes consecutivos libres en la posicióm "i", en RAM.
La búsqueda se hace solo hasta la posición "maxRam"}
var
  c: Integer;
  j: word;
begin
  Result := false;
  c := 0;
  j := i;
  while (j<=maxRam) and (c<n) do begin
    if (ram[j].state <> cs_impleGPR) or (ram[j].used) then exit;
    inc(c);      //verifica siguiente
    inc(j);
  end;
  if j>maxRam then exit;  //no hay más espacio
  //Si llega aquí es porque estaban libres los bloques
  Result := true;
end;
procedure TPicCore.UseConsecRAM(const i, n: word);
{Marca "n" bytes como usados en la posición de memoria "i", en la RAM.
 Debe haberse verificado previamente que los parámetros son válidos, porque aquí no
 se hará ninguna verificación.}
var j: word;
begin
  for j:=i to i+n-1 do begin
    ram[j].used := true;  //todos los bits
  end;
end;
procedure TPicCore.SetSharedUnused;
{Marca las posiciones que estén en "shared", como no usadas, para que se puedan
usar nuevamente.}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].shared) then begin
      ram[i].used := false;  //pone en cero
    end;
  end;
end;
procedure TPicCore.SetSharedUsed;
{Marca las posiciones que estén en "shared", como usadas, para que no se puedan
usar nuevamente.}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].shared) then begin
      ram[i].used := true;  //pone en uno
    end;
  end;
end;
//FLASH memory functions
function TPicCore.UsedMemRAM: word;
var
  i: Integer;
begin
  Result := 0;
  for i:=$0000 to PICMAXFLASH-1 do begin
    if ram[i].used then inc(Result);
  end;
end;
procedure TPicCore.ClearMemFlash;
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    ram[i].value    := $3FFF;
    ram[i].used     := false;
    ram[i].breakPnt := false;
    ram[i].topLabel   := '';
    ram[i].sideComment:= '';
    ram[i].topComment := '';
    ram[i].idFile   := -1;  //Indica no inicializado
  end;
end;
//RAM name managment
function TPicCore.NameRAM(const addr: word): string;
{Devuelve el nombre de una celda de la memoria RAM.}
begin
  Result := ram[addr].name;
end;
procedure TPicCore.SetNameRAM(const addr: word; const nam: string
  );
{Escribe en el campo "name" de la RAM en la psoición indicada}
begin
   ram[addr].name:=nam;
end;
procedure TPicCore.AddNameRAM(const addr: word; const nam: string);
{Escribe en el campo "name" de la RAM en la psoición indicada. Si ya existía un nombre,
lo argega después de una coma.}
begin
  if ram[addr].name = '' then begin
    ram[addr].name:=nam;
  end else begin
    ram[addr].name+=','+nam;
  end;
end;
//Execution control
procedure TPicCore.AddBreakpoint(aPC: word);
//Agrega un punto de interrupción
begin
  if aPC>=PICMAXFLASH then exit;
  ram[aPC].breakPnt := true;
end;
procedure TPicCore.ToggleBreakpoint(aPC: word);
//COnmuta el estado del Punto de Interrupción, en la posición indicada
begin
  if aPC>=PICMAXFLASH then exit;
  ram[aPC].breakPnt := not ram[aPC].breakPnt;
end;
procedure TPicCore.addTopLabel(lbl: string);
begin
  ram[iRam].topLabel := lbl;
end;
procedure TPicCore.addTopComm(comm: string; replace: boolean);
{Agrega un comentario de línea al código en la posición de memoria actual}
begin
  if replace then begin
    ram[iRam].topComment := comm;
  end else begin
    ram[iRam].topComment := ram[iRam].topComment + comm;
  end;
end;
procedure TPicCore.addSideComm(comm: string; before: boolean);
{Agrega un comentario para que apareza al lado de la instrucción.
 "before" = TRUE -> Se debe llamar después de codificar la instrucción
 "before" = FALSE -> Se debe llamar antes de codificar la instrucción
 }
begin
  if before then begin
    if iRam= 0 then exit;
    ram[iRam-1].sideComment+=comm;   //se agrega al que pudiera haber
  end else begin
    if iRam= 0 then exit;
    ram[iRam].sideComment+=comm;   //se agrega al que pudiera haber
  end;
end;
procedure TPicCore.addPosInformation(rowSrc, colSrc: word; idFile: byte);
{Agrega information de la posición en el codigo fuente, a la posición actual de la
memoria flash.}
begin
  ram[iRam].rowSrc := rowSrc;
  ram[iRam].colSrc := colSrc;
  ram[iRam].idFile := idFile;
end;
//Initialization
constructor TPicCore.Create;
begin
  hexLines := TStringList.Create;
  frequen := 4000000;    //4MHz
end;
destructor TPicCore.Destroy;
begin
  hexLines.Destroy;
  inherited Destroy;
end;

initialization
end.

