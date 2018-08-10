{
Description
===========
Utilities for the 6502 CPU. The types here defined are intended to be used for:
* Assembling 6502 instructions.
* Disassembling 6502 instructions.
* Simulating the 6502 execution.
To simulate the CPU, it's assumed there are 64KB of RAM in a virtual system.
The main class TP6502 models a CPU6502 object including access to 64KB RAM.
The aim of this unit is to be used as base for assemblers, compilers and simulators.

                                         Created by Tito Hinostroza   19/05/2018
}

unit P6502utils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc, PicCore;
type  //Instructions set
  TP6502Inst = (
    i_ADC,  //add with carry
    i_AND,  //and (with accumulator)
    i_ASL,  //arithmetic shift left
    i_BCC,  //branch on carry clear
    i_BCS,  //branch on carry set
    i_BEQ,  //branch on equal (zero set)
    i_BIT,  //bit test
    i_BMI,  //branch on minus (negative set)
    i_BNE,  //branch on not equal (zero clear)
    i_BPL,  //branch on plus (negative clear)
    i_BRK,  //break / interrupt
    i_BVC,  //branch on overflow clear
    i_BVS,  //branch on overflow set
    i_CLC,  //clear carry
    i_CLD,  //clear decimal
    i_CLI,  //clear interrupt disable
    i_CLV,  //clear overflow
    i_CMP,  //compare (with accumulator)
    i_CPX,  //compare with X
    i_CPY,  //compare with Y
    i_DEC,  //decrement
    i_DEX,  //decrement X
    i_DEY,  //decrement Y
    i_EOR,  //exclusive or (with accumulator)
    i_INC,  //increment
    i_INX,  //increment X
    i_INY,  //increment Y
    i_JMP,  //jump
    i_JSR,  //jump subroutine
    i_LDA,  //load accumulator
    i_LDX,  //load X
    i_LDY,  //load Y
    i_LSR,  //logical shift right
    i_NOP,  //no operation
    i_ORA,  //or with accumulator
    i_PHA,  //push accumulator
    i_PHP,  //push processor status (SR)
    i_PLA,  //pull accumulator
    i_PLP,  //pull processor status (SR)
    i_ROL,  //rotate left
    i_ROR,  //rotate right
    i_RTI,  //return from interrupt
    i_RTS,  //return from subroutine
    i_SBC,  //subtract with carry
    i_SEC,  //set carry
    i_SED,  //set decimal
    i_SEI,  //set interrupt disable
    i_STA,  //store accumulator
    i_STX,  //store X
    i_STY,  //store Y
    i_TAX,  //transfer accumulator to X
    i_TAY,  //transfer accumulator to Y
    i_TSX,  //transfer stack pointer to X
    i_TXA,  //transfer X to accumulator
    i_TXS,  //transfer X to stack pointer
    i_TYA,  //transfer Y to accumulator
    //INVALID INSTRUCTION
    i_Inval
  );
  //Addressing Modes
  {Implicit Mode and Acumulator Mode are not considered here. We consider
   Only Modes with parameters.}
  TP6502AddMode = (
    aImplicit,  //Implied         : BRK
    aAcumulat,  //Acumulator      : ASL
    aImmediat,  //Immediate       : ORA #$B2
    aAbsolute,  //Absolute        : JMP $4032
    aZeroPage,  //Zero page       : LDA: $35
    aRelative,  //Relative        : BNE LABEL
    aIndirect,  //Indirect        : JMP ($1000)
    aAbsolutX,  //Absolute Indexed by X  : STA $1000, X
    aAbsolutY,  //Absolute Indexed by Y  : STA $1000, Y
    aZeroPagX,  //Zero page Indexed by X : LDA $10, X
    aZeroPagY,  //Zero page Indexed by Y : LDA $10, Y
    aIdxIndir,  //Indexed Indirect: LDA ($40,X)  Only for X
    aIndirIdx   //Indirect Indexed: LDA ($40),Y  Only for Y
  );
  TP6502AddModes = set of TP6502AddMode;
  //Instruction Information for each Address Mode
  TInstructInform = record
    Opcode: byte;   //Code for instruction
    nBytes: byte;   //Num. of bytes of the instruction.
    Cycles: byte;   //Num. of cycles the instruction takes.
    optCycles: string;  //Extra options in Num. of cycles.
  end;

  //Record for a 6502 instruction

  { TP6502Instruct }

  TP6502Instruct = object
  public
    //Name of the instruction
    name: string[3];
    //Address Modes supported
    addressModes: TP6502AddModes;
    //Information for each Address Mode supported
    instrInform: array[TP6502AddMode] of TInstructInform;
  public
    procedure Init(name0: string);
    procedure AddAddressMode(aMode: TP6502AddMode; Opcode, nBytes, nCycles: Byte;
      optCycles: string);
  end;

  //Indica el destino de la instrucción
  TPIC16destin = (
    toW = %00000000,    //al acumulador
    toF = %10000000     //a memoria
  );



const  //Constants of address and bit positions for some registers
  _STATUS = $03;
  _C      = 0;
  _Z      = 2;
  _RP0    = 5;
  _RP1    = 6;
//  _IRP   = 7;
type
  {Objeto que representa al hardware de un PIC de la serie 16}
  { TP6502 }
  TP6502 = class(TPicCore)
  public  //Campos para desensamblar instrucciones
    idIns: TP6502Inst;    //ID de Instrucción.
    modIns: TP6502AddMode; //Modo de direccionamiento
    parIns: word;        //Parámetro de instrucción. Válido solo en algunas instrucciones.
    b_   : byte;         //Bit destino. Válido solo en algunas instrucciones.
    k_   : word;         //Parámetro Literal. Válido solo en algunas instrucciones.
  private //Campos para procesar instrucciones
    function GetINTCON: byte;
    function GetINTCON_GIE: boolean;
    function GetSTATUS: byte;
    function GetSTATUS_C: boolean;
    function GetSTATUS_DC: boolean;
    function GetSTATUS_IRP: boolean;
    function GetSTATUS_Z: boolean;
    procedure SetINTCON_GIE(AValue: boolean);
    procedure SetSTATUS_C(AValue: boolean);
    procedure SetSTATUS_DC(AValue: boolean);
    procedure SetSTATUS_IRP(AValue: boolean);
    procedure SetSTATUS_Z(AValue: boolean);
    procedure SetFRAM(value: byte);
    function GetFRAM: byte;
  public  //Campos que modelan a los registros internos
    W        : byte;   //Registro de trabajo
    PC       : TWordRec; //PC as record to fast access for bytes
    PCLATH   : byte;   //Contador de Programa H
    STKPTR   : 0..7;   //Puntero de pila
    STACK    : array[0..7] of word;
    property STATUS: byte read GetSTATUS;
    property STATUS_Z: boolean read GetSTATUS_Z write SetSTATUS_Z;
    property STATUS_C: boolean read GetSTATUS_C write SetSTATUS_C;
    property STATUS_DC: boolean read GetSTATUS_DC write SetSTATUS_DC;
    property STATUS_IRP: boolean read GetSTATUS_IRP write SetSTATUS_IRP;
    property INTCON: byte read GetINTCON;
    property INTCON_GIE: boolean read GetINTCON_GIE write SetINTCON_GIE;
    property FRAM: byte read GetFRAM write SetFRAM;
  public  //Execution control
    function CurInstruction: TP6502Inst;
    procedure Exec(aPC: word); override; //Ejecuta la instrucción en la dirección indicada.
    procedure Exec; override; //Ejecuta instrucción actual
    procedure ExecTo(endAdd: word); override; //Ejecuta hasta cierta dirección
    procedure ExecStep; override; //Execute one instruction considering CALL as one instruction
    procedure ExecNCycles(nCyc: integer; out stopped: boolean); override; //Ejecuta hasta cierta dirección
    procedure Reset; override;
    function ReadPC: dword; override;
    procedure WritePC(AValue: dword); override;
  public  //Memories
    procedure Decode(const opCode: word);  //decodifica instrucción
    function Disassembler(const opCode, par1, par2: byte; out nBytesProc: byte;
      useVarName: boolean=false): string;  //Desensambla la instrucción actual
    function DisassemblerAt(addr: word; out nBytesProc: byte; useVarName: boolean
      ): string; override;
  public  //Funciones para la memoria RAM
    function GetFreeByte(out addr: word; shared: boolean): boolean;
    function GetFreeBytes(const size: integer; var addr: word): boolean;  //obtiene una dirección libre
    function TotalMemRAM: word; //devuelve el total de memoria RAM
    function UsedMemRAM: word;  //devuelve el total de memoria RAM usada
    procedure ExploreUsed(rutExplorRAM: TPICRutExplorRAM);    //devuelve un reporte del uso de la RAM
    function ValidRAMaddr(addr: word): boolean;  //indica si una posición de memoria es válida
  public  //Métodos para codificar instrucciones de acuerdo a la sintaxis
    procedure useRAM;
    procedure codAsmFD(const inst: TP6502Inst; addMode: TP6502AddMode; param: word);
    function codInsert(iRam0, nInsert, nWords: integer): boolean;
  public  //Métodos adicionales
    function FindOpcode(Op: string): TP6502Inst;  //busca Opcode
    procedure GenHex(hexFile: string; ConfigWord: integer = - 1);  //genera un archivo hex
    procedure DumpCode(lOut: TStrings; incAdrr, incCom, incVarNam: boolean);  //vuelva en código que contiene
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

var  //variables globales
  //mnemónico de las instrucciones
  PIC16InstName: array[low(TP6502Inst)..high(TP6502Inst)] of TP6502Instruct;

implementation

{ TP6502Instruct }

procedure TP6502Instruct.Init(name0: string);
//Initialize the instruction. Must be called before AddAddressMode().
begin
  name := name0;  //Set
  addressModes:= [];  //Clear
end;
procedure TP6502Instruct.AddAddressMode(aMode: TP6502AddMode;
          Opcode, nBytes, nCycles: Byte;
          optCycles: string);
{Add a new Address Mode including additional information.
"optCycles" is a flag and indicate aditional considerations on cycles:

'*'  -> Add 1 to cycles if page boundery is crossed

'**' -> Add 1 to cycles if branch occurs on same page
        Add 2 to cycles if branch occurs to different page
}
begin
  addressModes := addressModes + [aMode];  //Register Mode
  //Add information
  instrInform[aMode].Opcode:= Opcode;
  instrInform[aMode].nBytes:= nBytes;
  instrInform[aMode].Cycles:= nCycles;
  instrInform[aMode].optCycles := optCycles;
end;

{ TP6502 }
procedure TP6502.useRAM;
{Marca la posición actual, como usada, e incrementa el puntero iRam. Si hay error,
actualiza el campo "MsjError"}
begin
  //Protección de desborde
  if iRam >= CPUMAXRAM then begin
    MsjError := 'RAM Memory limit exceeded.';
    exit;
  end;
  ram[iRam].used := true;  //marca como usado
  inc(iRam);
end;
procedure TP6502.codAsmFD(const inst: TP6502Inst; addMode: TP6502AddMode; param: word);
{Codifica las instrucciones orientadas a registro, con sintAxis: NEMÓNICO f,d}
var
  rInst: TP6502Instruct;
begin
  rInst := PIC16InstName[inst];
  //Write OpCode
  ram[iRam].value := rInst.instrInform[addMode].Opcode;
  useRAM;  //marca como usado e incrementa puntero.
  //Codifica parámetros
  case addMode of
  aImplicit: begin
    //No parameters
    if addMode in rInst.addressModes then begin
      //It's OK
    end else begin
      MsjError:= 'Invalid Address Mode (Implicit)';
    end;
  end;
  aAcumulat: begin
    //No parameters
    if addMode in rInst.addressModes then begin
      //It's OK
    end else begin
      MsjError:= 'Invalid Address Mode (Acumulator)';
    end;
  end;
  aImmediat: begin
    if addMode in rInst.addressModes then begin
      //It's OK
    end else begin
      MsjError:= 'Invalid Address Mode (Immediate)';
    end;
    ram[iRam].value := lo(param);  //escribe parámetro
    useRAM;
  end;
  aAbsolute:begin
    ram[iRam].value := lo(param);
    useRAM;
    ram[iRam].value := hi(param);
    useRAM;
  end;
  aZeroPage:begin
    ram[iRam].value := lo(param);
    useRAM;
  end;
  aRelative:begin
    ram[iRam].value := lo(param);
    useRAM;
  end;
  aIndirect:begin
    ram[iRam].value := lo(param);
    useRAM;
    ram[iRam].value := hi(param);
    useRAM;
  end;
  aAbsolutX:begin
    ram[iRam].value := lo(param);
    useRAM;
    ram[iRam].value := hi(param);
    useRAM;
  end;
  aAbsolutY:begin
    ram[iRam].value := lo(param);
    useRAM;
    ram[iRam].value := hi(param);
    useRAM;
  end;
  aZeroPagX:begin
    ram[iRam].value := lo(param);
    useRAM;
  end;
  aZeroPagY:begin
    ram[iRam].value := lo(param);
    useRAM;
  end;
  aIdxIndir:begin
    ram[iRam].value := lo(param);
    useRAM;
  end;
  aIndirIdx:begin
    ram[iRam].value := lo(param);
    useRAM;
  end;
  else
    raise Exception.Create('Implementation Error.');
  end;
end;
function TP6502.codInsert(iRam0, nInsert, nWords: integer): boolean;
{Inserta en la posición iRam0, "nInsert" palabras, desplazando "nWords" palabras.
Al final debe quedar "nInsert" palabras de espacio libre en iRam0.
Si hay error devuelve FALSE.}
var
  i: Integer;
begin
  Result := True;  //By default
  if iRam+nInsert+nWords-1> CPUMAXRAM then begin
    //Overflow on address
    exit(false);
  end;
  for i:= iRam + nInsert + nWords -1 downto iRam + nWords do begin
    ram[i] := ram[i-nInsert];
  end;
end;
//procedure TP6502.BTFSC_sw_BTFSS(iRam0: integer);
//{Exchange instruction i_BTFSC to i_BTFSS, or viceversa, in the specified address.}
//begin
//  //Solo necesita cambiar el bit apropiado
//  ram[iRam0].value := ram[iRam0].value XOR %10000000000;
//end;
function TP6502.FindOpcode(Op: string): TP6502Inst;
{Busca una cádena que represente a una instrucción (Opcode). Si encuentra devuelve
 el identificador de instrucción . Si no encuentra devuelve "i_Inval". }
var
  idInst: TP6502Inst;
  tmp: String;
begin
  tmp := UpperCase(Op);
  for idInst := low(TP6502Inst) to high(TP6502Inst) do begin
    if PIC16InstName[idInst].name = tmp then begin
      Result := idInst;
      exit;
    end;
  end;
  //No encontró
  Result := i_Inval;
end;
//Campos para procesar instrucciones
function TP6502.GetSTATUS: byte;
begin
  Result := ram[_STATUS].dvalue;
end;
function TP6502.GetSTATUS_Z: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000100) <> 0;
end;
procedure TP6502.SetSTATUS_Z(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000100
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111011;
end;
function TP6502.GetSTATUS_C: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000001) <> 0;
end;
procedure TP6502.SetSTATUS_C(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000001
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111110;
end;
function TP6502.GetSTATUS_DC: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000010) <> 0;
end;
procedure TP6502.SetSTATUS_DC(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000010
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111101;
end;
function TP6502.GetSTATUS_IRP: boolean;
begin
  Result := (ram[_STATUS].dvalue and %10000000) <> 0;
end;
procedure TP6502.SetSTATUS_IRP(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %10000000
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %01111111;
end;
function TP6502.GetINTCON: byte;
begin
  Result := ram[$0B].dvalue;
end;
function TP6502.GetINTCON_GIE: boolean;
begin
  Result := (ram[$0B].dvalue and %10000000) <> 0;
end;
procedure TP6502.SetINTCON_GIE(AValue: boolean);
begin
  if AVAlue then ram[$0B].dvalue := ram[$0B].dvalue or  %10000000
            else ram[$0B].dvalue := ram[$0B].dvalue and %01111111;
end;
procedure TP6502.SetFRAM(value: byte);
{Escribe en la RAM; en la dirección global f_, el valor "value"
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
begin
   ram[parIns].value := value;
end;
function TP6502.GetFRAM: byte;
{Devuelve el valor de la RAM, de la posición global f_.
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
begin
  Result := ram[parIns].value;
end;
procedure TP6502.Decode(const opCode: word);
{Decode the value of "opCode" and update:
* "idIns" -> Instruction ID
* "modIns" -> Address Mode
}
var
  i : TP6502Inst;
  j : TP6502AddMode;
  inst: TP6502Instruct;
  iInfom: TInstructInform;
begin
  //Search the Opcode
  for i := low(TP6502Inst) to high(TP6502Inst) do begin
    inst := PIC16InstName[i];
    for j := low(TP6502AddMode) to high(TP6502AddMode) do begin
      iInfom := inst.instrInform[j];
      if iInfom.Opcode = opCode then begin
        idIns := i;
        modIns := j;
      end;
    end;
  end;
end;
function TP6502.Disassembler(const opCode, par1, par2: byte;
                             out nBytesProc: byte;
                             useVarName: boolean = false): string;
{Disassemble the instruction indicated in "Opcode". If the instruction is multibyte
it will be used "par1" and "par2" according to the intruction length, which is returned
in "nBytesProc".
Global variables used: "idIns", "modIns".
"useVarName" -> Flag to use the name of the variable instead of only the address.
                Valid only when a variAble name exists in his address.
}
var
  nemo: String;
  f: word;
begin
  Decode(opCode);   //Decode instruction. Update: "idIns", "modIns".
  nemo := trim(PIC16InstName[idIns].name) + ' ';
  case modIns of
  aImplicit: begin
    nBytesProc := 1;  //No parameters needed
  end;
  aAcumulat: begin
    nBytesProc := 1;  //No parameters needed
  end;
  aImmediat: begin
    Result := nemo + '#$'+IntToHex(par1,2);
    nBytesProc := 2;
  end;
  aAbsolute: begin
    Result := nemo + '$'+IntToHex(par1 + par2*256, 4);
    nBytesProc := 3;
  end;
  aZeroPage: begin
    Result := nemo + '$'+IntToHex(par1, 2);
    nBytesProc := 2;
  end;
  aRelative: begin
    Result := nemo + '$'+IntToHex(par1, 2);
    nBytesProc := 2;
  end;
  aIndirect: begin
    Result := nemo + '$('+IntToHex(par1 + par2*256, 4)+')';
    nBytesProc := 3;
  end;
  aAbsolutX: begin
    Result := nemo + '$'+IntToHex(par1 + par2*256, 4)+',X';
    nBytesProc := 3;
  end;
  aAbsolutY: begin
    Result := nemo + '$'+IntToHex(par1 + par2*256, 4)+',Y';
    nBytesProc := 3;
  end;
  aZeroPagX: begin
    Result := nemo + '$'+IntToHex(par1, 2)+',X';
    nBytesProc := 3;
  end;
  aZeroPagY: begin
    Result := nemo + '$'+IntToHex(par1, 2)+',Y';
    nBytesProc := 3;
  end;
  aIdxIndir: begin
    Result := nemo + '$('+IntToHex(par1, 2)+',X)';
    nBytesProc := 2;
  end;
  aIndirIdx: begin
    Result := nemo + '$('+IntToHex(par1, 2)+'),Y';
    nBytesProc := 3;
  end;
  end;
end;
function TP6502.DisassemblerAt(addr: word; out nBytesProc: byte;
                               useVarName: boolean): string;
{Disassembler the instruction located at "addr"}
var
  valOp: Word;
begin
  Result := Disassembler(ram[addr].value,
                         ram[addr+1].value,
                         ram[addr+2].value, nBytesProc, useVarName);
end;
function TP6502.CurInstruction: TP6502Inst;
{Resturn the instruction pointed by PC, in this moment.}
begin
  Decode(ram[PC.W].value);   //decode instruction
  Result := idIns;
end;
procedure TP6502.Exec;
{Execute the current instruction.}
begin
  Exec(PC.W);
end;
procedure TP6502.Exec(aPC: word);
{Ejecuta la instrución actual con dirección "pc".
Falta implementar las operaciones, cuando acceden al registro INDF, el Watchdog timer,
los contadores, las interrupciones}
var
  opc: byte;
  msk, resNib: byte;
  nCycles, nBytes: byte;
  resInt : integer;
begin
  //Decodifica instrucción
  opc := ram[aPC].value;
  Decode(opc);   //Decode instruction
  nCycles := PIC16InstName[idIns].instrInform[modIns].Cycles;
  nBytes  := PIC16InstName[idIns].instrInform[modIns].nBytes;

  case idIns of
  i_ADC:;  //add with carry
  i_AND:;  //and (with accumulator)
  i_ASL:;  //arithmetic shift left
  i_BCC:;  //branch on carry clear
  i_BCS:;  //branch on carry set
  i_BEQ:;  //branch on equal (zero set)
  i_BIT:;  //bit test
  i_BMI:;  //branch on minus (negative set)
  i_BNE:;  //branch on not equal (zero clear)
  i_BPL:;  //branch on plus (negative clear)
  i_BRK:;  //break / interrupt
  i_BVC:;  //branch on overflow clear
  i_BVS:;  //branch on overflow set
  i_CLC:;  //clear carry
  i_CLD:;  //clear decimal
  i_CLI:;  //clear interrupt disable
  i_CLV:;  //clear overflow
  i_CMP:;  //compare (with accumulator)
  i_CPX:;  //compare with X
  i_CPY:;  //compare with Y
  i_DEC:;  //decrement
  i_DEX:;  //decrement X
  i_DEY:;  //decrement Y
  i_EOR:;  //exclusive or (with accumulator)
  i_INC:;  //increment
  i_INX:;  //increment X
  i_INY:;  //increment Y
  i_JMP:;  //jump
  i_JSR:;  //jump subroutine
  i_LDA:;  //load accumulator
  i_LDX:;  //load X
  i_LDY:;  //load Y
  i_LSR:;  //logical shift right
  i_NOP:;  //no operation
  i_ORA:;  //or with accumulator
  i_PHA:;  //push accumulator
  i_PHP:;  //push processor status (SR)
  i_PLA:;  //pull accumulator
  i_PLP:;  //pull processor status (SR)
  i_ROL:;  //rotate left
  i_ROR:;  //rotate right
  i_RTI:;  //return from interrupt
  i_RTS:;  //return from subroutine
  i_SBC:;  //subtract with carry
  i_SEC:;  //set carry
  i_SED:;  //set decimal
  i_SEI:;  //set interrupt disable
  i_STA:;  //store accumulator
  i_STX:;  //store X
  i_STY:;  //store Y
  i_TAX:;  //transfer accumulator to X
  i_TAY:;  //transfer accumulator to Y
  i_TSX:;  //transfer stack pointer to X
  i_TXA:;  //transfer X to accumulator
  i_TXS:;  //transfer X to stack pointer
  i_TYA:;  //transfer Y to accumulator
  i_Inval: begin
      MsjError := 'Invalid Opcode';
    end;
  end;

{  i_ADDWF: begin
    resByte := FRAM;
    resWord := W + resByte;
    resNib := (W and $0F) + (resByte and $0F);
    if modIns = toF then begin
      FRAM := resWord and $FF;
    end else begin  //toW
      w := resWord and $FF;
    end;
    STATUS_Z := (resWord and $ff) = 0;
    STATUS_C := (resWord > 255);
    STATUS_DC := (resNib > 15);
  end;
  i_ANDWF: begin
    resByte := W and FRAM;
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_CLRF: begin
    FRAM := 0;
    STATUS_Z := true;
  end;
  i_CLRW: begin
    W := 0;
    STATUS_Z := true;
  end;
  i_COMF : begin
    resByte := not FRAM;
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_DECF : begin
    resByte := FRAM;
    if resByte = 0 then resByte := $FF else dec(resByte);
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_DECFSZ: begin
    resByte := FRAM;
    if resByte = 0 then resByte := $FF else dec(resByte);
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  i_INCF: begin
    resByte := FRAM;
    if resByte = 255 then resByte := 0 else inc(resByte);
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_INCFSZ: begin
    resByte := FRAM;
    if resByte = 255 then resByte := 0 else inc(resByte);
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  i_IORWF: begin
    resByte := W or FRAM;
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte <> 0;
  end;
  i_MOVF: begin
    resByte := FRAM;
    if modIns = toF then begin
      //no mueve, solo verifica
      STATUS_Z := (resByte = 0);
    end else begin  //toW
      w := resByte;
      STATUS_Z := (resByte = 0);
    end;
  end;
  i_MOVWF: begin
    FRAM := W;   //escribe a donde esté mapeado, (si está mapeado)
    if parIns = $02 then begin //Es el PCL
      PC.H := PCLATH;  //Cuando se escribe en PCL, se carga PCH con PCLATH
    end;
  end;
  i_NOP: begin
  end;
  i_RLF: begin
    resByte := FRAM;
    bit7 := resByte and $80; //guarda bit 7
    resByte := (resByte << 1) and $ff;  //desplaza
    //pone C en bit bajo
    if STATUS_C then begin  //C era 1
      resByte := resByte or $01;  //pone a 1 el bit 0
    end else begin          //C era 0
      //no es necesario agregarlo, porque por defecto se agrega 0
    end;
    //Actualiza C
    if bit7 = 0 then STATUS_C := false
                else STATUS_C := true;
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
  end;
  i_RRF: begin
    resByte := FRAM;
    bit0 := resByte and $01; //guarda bit 0
    resByte := resByte >> 1;  //desplaza
    //pone C en bit alto
    if STATUS_C then begin  //C era 1
      resByte := resByte or $80;  //pone a 1 el bit 0
    end else begin          //C era 0
      //no es necesario agregarlo, porque por defecto se agrega 0
    end;
    //Actualiza C
    if bit0 = 0 then STATUS_C := false
                else STATUS_C := true;
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
  end;
  i_SUBWF: begin
    resByte := FRAM;
    resInt := resByte - W;
    if modIns = toF then begin
      FRAM :=  resInt and $FF;
    end else begin  //toW
      w := resInt and $FF;
    end;
    STATUS_Z := (resInt = 0);
    if resInt < 0 then STATUS_C := false   //negativo
    else STATUS_C := true;
    resInt := (resByte and $0F) - (W and $0F);
    if resInt < 0 then STATUS_DC := false   //negativo
    else STATUS_DC := true;
  end;
  i_SWAPF: begin
    resByte := FRAM;
    FRAM := (resByte >> 4) or (resByte << 4);
  end;
  i_XORWF: begin
    resByte := W xor FRAM;
    if modIns = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte <> 0;
  end;
  //BIT-ORIENTED FILE REGISTER OPERATIONS
  i_BCF: begin
    msk := $1 << b_;
    msk := not msk;
    FRAM := FRAM and msk;
  end;
  i_BSF: begin
    msk := $1 << b_;
    FRAM := FRAM or msk;// b_
  end;
  i_BTFSC: begin
    msk := $1 << b_;
    if (FRAM and msk) = 0 then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  i_BTFSS: begin
    msk := $1 << b_;
    if (FRAM and msk) <> 0 then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  //LITERAL AND CONTROL OPERATIONS
  i_ADDLW: begin
    resWord := W + k_;
    resNib := (W and $0F) + (k_ and $0F);
    w := resWord and $FF;
    STATUS_Z := (resWord and $ff) = 0;
    STATUS_C := (resWord > 255);
    STATUS_DC := (resNib > 15);
  end;
  i_ANDLW: begin
    resByte := W and K_;
    w := resByte;
    STATUS_Z := resByte = 0;
  end;
  i_CALL: begin
    //Guarda dirección en Pila
    STACK[STKPTR] := PC.W;
    if STKPTR = 7 then begin
      //Desborde de pila
      STKPTR := 0;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on CALL OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR +1;
    end;
    PC.W := k_;  //Takes the 11 bits from k
    PC.H := PC.H or (PCLATH and %00011000);  //And complete with bits 3 and 4 of PCLATH
    Inc(nClck,2);   //This instruction takes two cicles
    exit;
  end;
  i_CLRWDT: begin
  end;
  i_GOTO: begin
    PC.W := k_;  //Takes the 11 bits from k
    PC.H := PC.H or (PCLATH and %00011000);  //And complete with bits 3 and 4 of PCLATH
    Inc(nClck,2);   //This instruction takes two cicles
    exit;
  end;
  i_IORLW: begin
    resByte := W or k_;
    w := resByte;
    STATUS_Z := resByte <> 0;
  end;
  i_MOVLW: begin
      W := k_;
  end;
  i_RETFIE: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETFIE OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PC.W := STACK[STKPTR];  //Should be 13 bits
    Inc(nClck);   //Esta instrucción toma un ciclo más
    //Activa GIE
    INTCON_GIE := true;
  end;
  i_RETLW: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETLW OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PC.W := STACK[STKPTR];  //Should be 13 bits
    Inc(nClck);   //Esta instrucción toma un ciclo más
    //Fija valor en W
    W := k_;
  end;
  i_RETURN: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETURN OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PC.W := STACK[STKPTR];  //Should be 13 bits
    Inc(nClck);   //Esta instrucción toma un ciclo más
  end;
  i_SLEEP: begin
  end;
  i_SUBLW: begin
    resInt := k_ - W;
    w := resInt and $FF;
    STATUS_Z := (resInt = 0);
    if resInt < 0 then STATUS_C := false   //negativo
    else STATUS_C := true;
    resInt := (k_ and $0F) - (W and $0F);
    if resInt < 0 then STATUS_DC := false   //negativo
    else STATUS_DC := true;
  end;
  i_XORLW: begin
    resByte := W xor k_;
    w := resByte;
    STATUS_Z := resByte <> 0;
  end;
  i_Inval: begin
    MsjError := 'Invalid Opcode';
  end;
  end;
}
  //Increase counters
  Inc(PC.W, nBytes);
  Inc(nClck, nCycles);
end;
procedure TP6502.ExecTo(endAdd: word);
{Ejecuta las instrucciones secuencialmente, desde la instrucción actual, hasta que el
contador del programa, sea igual a la dirección "endAdd".}
begin
  //Hace una primera ejecución, sin verificar Breakpoints
  Exec(PC.W);
  //Ejecuta cíclicamnente
  while PC.W <> endAdd do begin
    if ram[PC.W].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
      exit;
    end;
    //Ejecuta
    Exec(PC.W);
    //Debe haber una forma de salir si es un lazo infinito
    //if (nClck and $800000) = $800000 then begin
    //end;
  end;
end;
procedure TP6502.ExecStep;
begin
  if CurInstruction = i_JSR then begin
    ExecTo(PC.W+3);  //Ejecuta hasta la sgte. instrucción, salta el JSR
  end else begin
    Exec;
  end;
end;
procedure TP6502.ExecNCycles(nCyc: integer; out stopped: boolean);
{Ejecuta el número de ciclos indicados, o hasta que se produzca alguna condición
externa, que puede ser:
* Se encuentre un Punto de Interrupción.
* Se detecta la señal, de detenerse.
* Se genere algún error en la ejecución.
* Se ejecuta la instrucción i_SLEEP.
la bandera "stopped", indica que se ha detendio la ejecución sin completar la cantidad
de instrucciones requeridas.
Normalmente Se ejecutará el número de ciclos indicados, pero en algunos casos se
ejecutará un ciclo más, debido a que algunas instrucciones toman dos ciclos.}
var
  clkEnd: Int64;
  _pc: word;
begin
  clkEnd := nClck + nCyc;   //Valor final del contador
  while nClck < clkEnd do begin
    _pc := PC.W;
    if ram[_pc].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
      stopped := true;
      exit;
    end;
    if not ram[_pc].used then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for executing unused code.');
      stopped := true;
      exit;
    end;
    if CommStop then begin
      //Se detectó el comando STOP
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for STOP command.');
      stopped := true;
      exit;
    end;
    //Ejecuta
    Exec(_pc);
  end;
  stopped := false;
end;
procedure TP6502.Reset;
//Reinicia el dipsoitivo
var
  i: Integer;
begin
  PC.W   := 0;
  PCLATH := 0;
  W      := 0;
  STKPTR := 0;   //Posición inicial del puntero de pila
  nClck  := 0;   //Inicia contador de ciclos
  CommStop := false;  //Limpia bandera
  //Limpia solamente el valor inicial, no toca los otros campos
  for i:=0 to high(ram) do begin
    ram[i].dvalue := $00;
  end;
  ram[_STATUS].dvalue := %00011000;  //STATUS
end;
function TP6502.ReadPC: dword;
begin
  Result := PC.W;
end;
procedure TP6502.WritePC(AValue: dword);
begin
  PC.W := AValue;
end;
//Funciones para la memoria RAM
function TP6502.GetFreeByte(out addr: word; shared: boolean): boolean;
{Devuelve una dirección libre de la memoria RAM.
"Shared" indica que se marcará el bit como de tipo "Compartido", y se usa para el
caso en que se quiera comaprtir la misma posición para diversos variables.
Si encuentra espacio, devuelve TRUE.}
var
  i: Integer;
  maxRam: word;
begin
  Result := false;   //valor inicial
  maxRam := CPUMAXRAM;  //posición máxima
  //Realmente debería explorar solo hasta la dirección implementada, por eficiencia
  for i:=0 to maxRam-1 do begin
    if (ram[i].state = cs_impleGPR) and (not ram[i].used) then begin
      //Esta dirección está libre
      ram[i].used := true;   //marca como usado
      if shared then begin
        ram[i].shared := true;  //Marca como compartido
      end;
      addr := i;
      //Notar que la posición de memoria puede estar mapeada.
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TP6502.GetFreeBytes(const size: integer; var addr: word): boolean;
{Devuelve una dirección libre de la memoria RAM para ubicar un bloque
 del tamaño indicado. Si encuentra espacio, devuelve TRUE.
 El tamaño se da en bytes, pero si el valor es negativo, se entiende que es en bits.}
var
  i: word;
  maxRam: Word;
begin
  Result := false;  //valor por defecto
  if size=0 then exit;
  maxRam := CPUMAXRAM;
  for i:=0 to CPUMAXRAM-1 do begin  //verifica 1 a 1, por seguridad
    if HaveConsecRAM(i, size, maxRam) then begin
      //encontró del tamaño buscado
      UseConsecRAM(i, size);  //marca como usado
      addr := i;
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TP6502.TotalMemRAM: word;
{Devuelve el total de memoria RAM disponible}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to CPUMAXRAM - 1 do begin
    if ram[i].AvailGPR then begin
      Result := Result + 1;
    end;
  end;
end;
function TP6502.UsedMemRAM: word;
{Devuelve el total de memoria RAM usada}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to CPUMAXRAM - 1 do begin
    if ram[i].AvailGPR and (ram[i].used) then begin
      //Notar que "AvailGPR" asegura que no se consideran registros maepados
      Result := Result + 1;
    end;
  end;
end;
procedure TP6502.ExploreUsed(rutExplorRAM: TPICRutExplorRAM);
{Genera un reporte de uso de RAM}
var
  i: Integer;
begin
  for i := 0 to CPUMAXRAM - 1 do begin
    if ram[i].AvailGPR and (ram[i].used) then begin
      rutExplorRAM(i, 0, @ram[i]);
    end;
  end;
end;
function TP6502.ValidRAMaddr(addr: word): boolean;
{Indica si la dirección indicada es válida dentro del hardware del PIC}
begin
  if addr > CPUMAXRAM then exit(false);   //excede límite
  exit(true);
end;
procedure TP6502.GenHex(hexFile: string; ConfigWord: integer = -1);
{Genera el archivo *.hex, a partir de los datos almacenados en la memoria RAM.
Actualiza los campos, minUsed y maxUsed.}
var
  iHex: word;  //Índice para explorar
  addr: word;  //Dirección de inicio
  i: Integer;

begin
  hexLines.Clear;      //Se usará la lista hexLines
  //Prepara extracción de datos
  minUsed := CPUMAXRAM;
  maxUsed := 0;
  iHex := 0;
  //Busca dirección de inicio usada
  for i := 0 to CPUMAXRAM-1 do begin
    if ram[i].used then begin
      if i<minUsed then minUsed := i;
      if i>maxUsed then maxUsed := i;
      hexLines.Add('poke ' +  IntToStr(i) + ',' + IntToStr(ram[i].value));
    end;
  end;
  //hexLines.Add('Comentario');
  hexLines.SaveToFile(hexFile); //Genera archivo
end;
procedure TP6502.DumpCode(lOut: TStrings; incAdrr, incCom, incVarNam: boolean);
{Desensambla las instrucciones grabadas en el PIC.
 Se debe llamar despues de llamar a GenHex(), para que se actualicen las variables}
var
  i: Word;
  lblLin, comLat, comLin, lin: String;
  nBytes: byte;
begin
  //Se supone que minUsed y maxUsed, ya deben haber sido actualizados.
  i := minUsed;
  while i <= maxUsed do begin
    //Lee comentarios y etiqueta
    lblLin := ram[i].topLabel;
    comLat := ram[i].sideComment;
    comLin := ram[i].topComment;
    //Escribe etiqueta al inicio de línea
    if lblLin<>'' then lOut.Add(lblLin+':');
    //Escribe comentario al inicio de línea
    if incCom and (comLin<>'') then  begin
      lOut.Add(comLin);
    end;
    //Decodifica instrucción
    lin := DisassemblerAt(i, nBytes, incVarNam);  //Instrucción
    i := i + nBytes;
    //Verificas si incluye dirección física
    if incAdrr then  begin
      lin := '0x'+IntToHex(i,3) + ' ' + lin;
    end;
    //Verifica si incluye comentario lateral
    if incCom then begin
      lin := lin  + ' ' + comLat;
    end;
    lOut.Add('    ' + lin);
  end;
end;
constructor TP6502.Create;
begin
  inherited Create;
  //Default hardware settings
  CPUMAXRAM   := 4096; //Máx RAM memory
  SetLength(ram, CPUMAXRAM);
  //inicia una configuración común
  ClearMemRAM;
  SetStatRAM($020, $04F, cs_impleGPR);

  //Estado inicial
  iRam := 0;   //posición de inicio
end;
destructor TP6502.Destroy;
begin
  inherited Destroy;
end;
procedure InitTables;
begin
  ///////////////////////////////////////////////////////////////////
  ////////////////// Set instructions information ///////////////////
  // Based on the information from:
  //   http://www.masswerk.at/6502/6502_instruction_set.html
  ///////////////////////////////////////////////////////////////////

  ////////////////////////////// ADC ////////////////////////////////
  PIC16InstName[i_ADC].name := 'ADC';  //Add Memory to Accumulator with Carry
  PIC16InstName[i_ADC].AddAddressMode(aImmediat,$69,2,2,'');
  PIC16InstName[i_ADC].AddAddressMode(aZeroPage,$65,2,3,'');
  PIC16InstName[i_ADC].AddAddressMode(aZeroPagX,$75,2,4,'');
  PIC16InstName[i_ADC].AddAddressMode(aAbsolute,$6D,3,4,'');
  PIC16InstName[i_ADC].AddAddressMode(aAbsolutX,$7D,3,4,'*');
  PIC16InstName[i_ADC].AddAddressMode(aAbsolutY,$79,3,4,'*');
  PIC16InstName[i_ADC].AddAddressMode(aIdxIndir,$61,2,6,'');
  PIC16InstName[i_ADC].AddAddressMode(aIndirIdx,$71,2,5,'*');
  PIC16InstName[i_AND].name := 'AND';  //AND Memory with Accumulator
  PIC16InstName[i_AND].AddAddressMode(aImmediat,$29,2,2,'');
  PIC16InstName[i_AND].AddAddressMode(aZeroPage,$25,2,3,'');
  PIC16InstName[i_AND].AddAddressMode(aZeroPagX,$35,2,4,'');
  PIC16InstName[i_AND].AddAddressMode(aAbsolute,$2D,3,4,'');
  PIC16InstName[i_AND].AddAddressMode(aAbsolutX,$3D,3,4,'*');
  PIC16InstName[i_AND].AddAddressMode(aAbsolutY,$39,3,4,'*');
  PIC16InstName[i_AND].AddAddressMode(aIdxIndir,$21,2,6,'');
  PIC16InstName[i_AND].AddAddressMode(aIndirIdx,$31,2,5,'*');
  PIC16InstName[i_ASL].name := 'ASL';  //Shift Left One Bit (MemoryorAccumulator)
  PIC16InstName[i_ASL].AddAddressMode(aAcumulat,$0A,1,2,'');
  PIC16InstName[i_ASL].AddAddressMode(aZeroPage,$06,2,5,'');
  PIC16InstName[i_ASL].AddAddressMode(aZeroPagX,$16,2,6,'');
  PIC16InstName[i_ASL].AddAddressMode(aAbsolute,$0E,3,6,'');
  PIC16InstName[i_ASL].AddAddressMode(aAbsolutX,$1E,3,7,'');
  PIC16InstName[i_BCC].name := 'BCC';  //Branch on Carry Clear
  PIC16InstName[i_BCC].AddAddressMode(aRelative,$90,2,2,'**');
  PIC16InstName[i_BCS].name := 'BCS';  //Branch on Carry Set
  PIC16InstName[i_BCS].AddAddressMode(aRelative,$B0,2,2,'**');
  PIC16InstName[i_BEQ].name := 'BEQ';  //Branch on Result Zero
  PIC16InstName[i_BEQ].AddAddressMode(aRelative,$F0,2,2,'**');
  PIC16InstName[i_BIT].name := 'BIT';  //Test Bits in Memory with Accumulator
  PIC16InstName[i_BIT].AddAddressMode(aZeroPage,$24,2,3,'');
  PIC16InstName[i_BIT].AddAddressMode(aAbsolute,$2C,3,4,'');
  PIC16InstName[i_BMI].name := 'BMI';  //Branch on Result Minus
  PIC16InstName[i_BMI].AddAddressMode(aRelative,$30,2,2,'**');
  PIC16InstName[i_BNE].name := 'BNE';  //Branch on Result not Zero
  PIC16InstName[i_BNE].AddAddressMode(aRelative,$D0,2,2,'**');
  PIC16InstName[i_BPL].name := 'BPL';  //Branch on Result Plus
  PIC16InstName[i_BPL].AddAddressMode(aRelative,$10,2,2,'**');
  PIC16InstName[i_BRK].name := 'BRK';  //Force Break
  PIC16InstName[i_BRK].AddAddressMode(aImplicit,$00,1,7,'');
  PIC16InstName[i_BVC].name := 'BVC';  //Branch on Overflow Clear
  PIC16InstName[i_BVC].AddAddressMode(aRelative,$50,2,2,'**');
  PIC16InstName[i_BVS].name := 'BVS';  //Branch on Overflow Set
  PIC16InstName[i_BVS].AddAddressMode(aRelative,$70,2,2,'**');
  PIC16InstName[i_CLC].name := 'CLC';  //Clear Carry Flag
  PIC16InstName[i_CLC].AddAddressMode(aImplicit,$18,1,2,'');
  PIC16InstName[i_CLD].name := 'CLD';  //Clear Decimal Mode
  PIC16InstName[i_CLD].AddAddressMode(aImplicit,$D8,1,2,'');
  PIC16InstName[i_CLI].name := 'CLI';  //Clear Interrupt Disable Bit
  PIC16InstName[i_CLI].AddAddressMode(aImplicit,$58,1,2,'');
  PIC16InstName[i_CLV].name := 'CLV';  //Clear Overflow Flag
  PIC16InstName[i_CLV].AddAddressMode(aImplicit,$B8,1,2,'');
  PIC16InstName[i_CMP].name := 'CMP';  //Compare Memory with Accumulator
  PIC16InstName[i_CMP].AddAddressMode(aImmediat,$C9,2,2,'');
  PIC16InstName[i_CMP].AddAddressMode(aZeroPage,$C5,2,3,'');
  PIC16InstName[i_CMP].AddAddressMode(aZeroPagX,$D5,2,4,'');
  PIC16InstName[i_CMP].AddAddressMode(aAbsolute,$CD,3,4,'');
  PIC16InstName[i_CMP].AddAddressMode(aAbsolutX,$DD,3,4,'*');
  PIC16InstName[i_CMP].AddAddressMode(aAbsolutY,$D9,3,4,'*');
  PIC16InstName[i_CMP].AddAddressMode(aIdxIndir,$C1,2,6,'');
  PIC16InstName[i_CMP].AddAddressMode(aIndirIdx,$D1,2,5,'*');
  PIC16InstName[i_CPX].name := 'CPX';  //Compare Memory and Index X
  PIC16InstName[i_CPX].AddAddressMode(aImmediat,$E0,2,2,'');
  PIC16InstName[i_CPX].AddAddressMode(aZeroPage,$E4,2,3,'');
  PIC16InstName[i_CPX].AddAddressMode(aAbsolute,$EC,3,4,'');
  PIC16InstName[i_CPY].name := 'CPY';  //Compare Memory and Index Y
  PIC16InstName[i_CPY].AddAddressMode(aImmediat,$C0,2,2,'');
  PIC16InstName[i_CPY].AddAddressMode(aZeroPage,$C4,2,3,'');
  PIC16InstName[i_CPY].AddAddressMode(aAbsolute,$CC,3,4,'');
  PIC16InstName[i_DEC].name := 'DEC';  //Decrement Memory by One
  PIC16InstName[i_DEC].AddAddressMode(aZeroPage,$C6,2,5,'');
  PIC16InstName[i_DEC].AddAddressMode(aZeroPagX,$D6,2,6,'');
  PIC16InstName[i_DEC].AddAddressMode(aAbsolute,$CE,3,3,'');
  PIC16InstName[i_DEC].AddAddressMode(aAbsolutX,$DE,3,7,'');
  PIC16InstName[i_DEX].name := 'DEX';  //Decrement Index X by One
  PIC16InstName[i_DEX].AddAddressMode(aImplicit,$CA,1,2,'');
  PIC16InstName[i_DEY].name := 'DEY';  //Decrement Index Y by One
  PIC16InstName[i_DEY].AddAddressMode(aImplicit,$88,1,2,'');
  PIC16InstName[i_EOR].name := 'EOR';  //Exclusive-OR Memory with Accumulator
  PIC16InstName[i_EOR].AddAddressMode(aImmediat,$49,2,2,'');
  PIC16InstName[i_EOR].AddAddressMode(aZeroPage,$45,2,3,'');
  PIC16InstName[i_EOR].AddAddressMode(aZeroPagX,$55,2,4,'');
  PIC16InstName[i_EOR].AddAddressMode(aAbsolute,$4D,3,4,'');
  PIC16InstName[i_EOR].AddAddressMode(aAbsolutX,$5D,3,4,'*');
  PIC16InstName[i_EOR].AddAddressMode(aAbsolutY,$59,3,4,'*');
  PIC16InstName[i_EOR].AddAddressMode(aIdxIndir,$41,2,6,'');
  PIC16InstName[i_EOR].AddAddressMode(aIndirIdx,$51,2,5,'*');
  PIC16InstName[i_INC].name := 'INC';  //Increment Memory by One
  PIC16InstName[i_INC].AddAddressMode(aZeroPage,$E6,2,5,'');
  PIC16InstName[i_INC].AddAddressMode(aZeroPagX,$F6,2,6,'');
  PIC16InstName[i_INC].AddAddressMode(aAbsolute,$EE,3,6,'');
  PIC16InstName[i_INC].AddAddressMode(aAbsolutX,$FE,3,7,'');
  PIC16InstName[i_INX].name := 'INX';  //Increment Index X by One
  PIC16InstName[i_INX].AddAddressMode(aImplicit,$E8,1,2,'');
  PIC16InstName[i_INY].name := 'INY';  //Increment Index Y by One
  PIC16InstName[i_INY].AddAddressMode(aImplicit,$C8,1,2,'');
  PIC16InstName[i_JMP].name := 'JMP';  //Jump to New Location
  PIC16InstName[i_JMP].AddAddressMode(aAbsolute,$4C,3,3,'');
  PIC16InstName[i_JMP].AddAddressMode(aIndirect,$6C,3,5,'');
  PIC16InstName[i_JSR].name := 'JSR';  //Jump to New Location Saving Return Address
  PIC16InstName[i_JSR].AddAddressMode(aAbsolute,$20,3,6,'');
  PIC16InstName[i_LDA].name := 'LDA';  //Load Accumulator with Memory
  PIC16InstName[i_LDA].AddAddressMode(aImmediat,$A9,2,2,'');
  PIC16InstName[i_LDA].AddAddressMode(aZeroPage,$A5,2,3,'');
  PIC16InstName[i_LDA].AddAddressMode(aZeroPagX,$B5,2,4,'');
  PIC16InstName[i_LDA].AddAddressMode(aAbsolute,$AD,3,4,'');
  PIC16InstName[i_LDA].AddAddressMode(aAbsolutX,$BD,3,4,'*');
  PIC16InstName[i_LDA].AddAddressMode(aAbsolutY,$B9,3,4,'*');
  PIC16InstName[i_LDA].AddAddressMode(aIdxIndir,$A1,2,6,'');
  PIC16InstName[i_LDA].AddAddressMode(aIndirIdx,$B1,2,5,'*');
  PIC16InstName[i_LDX].name := 'LDX';  //Load Index X with Memory
  PIC16InstName[i_LDX].AddAddressMode(aImmediat,$A2,2,2,'');
  PIC16InstName[i_LDX].AddAddressMode(aZeroPage,$A6,2,3,'');
  PIC16InstName[i_LDX].AddAddressMode(aZeroPagY,$B6,2,4,'');
  PIC16InstName[i_LDX].AddAddressMode(aAbsolute,$AE,3,4,'');
  PIC16InstName[i_LDX].AddAddressMode(aAbsolutY,$BE,3,4,'*');
  PIC16InstName[i_LDY].name := 'LDY';  //Load Index Y with Memory
  PIC16InstName[i_LDY].AddAddressMode(aImmediat,$A0,2,2,'');
  PIC16InstName[i_LDY].AddAddressMode(aZeroPage,$A4,2,3,'');
  PIC16InstName[i_LDY].AddAddressMode(aZeroPagX,$B4,2,4,'');
  PIC16InstName[i_LDY].AddAddressMode(aAbsolute,$AC,3,4,'');
  PIC16InstName[i_LDY].AddAddressMode(aAbsolutX,$BC,3,4,'*');
  PIC16InstName[i_LSR].name := 'LSR';  //Shift One Bit Right (Memory orAccumulator)
  PIC16InstName[i_LSR].AddAddressMode(aAcumulat,$4A,1,2,'');
  PIC16InstName[i_LSR].AddAddressMode(aZeroPage,$46,2,5,'');
  PIC16InstName[i_LSR].AddAddressMode(aZeroPagX,$56,2,6,'');
  PIC16InstName[i_LSR].AddAddressMode(aAbsolute,$4E,3,6,'');
  PIC16InstName[i_LSR].AddAddressMode(aAbsolutX,$5E,3,7,'');
  PIC16InstName[i_NOP].name := 'NOP';  //No Operation
  PIC16InstName[i_NOP].AddAddressMode(aImplicit,$EA,1,2,'');
  PIC16InstName[i_ORA].name := 'ORA';  //OR Memory with Accumulator
  PIC16InstName[i_ORA].AddAddressMode(aImmediat,$09,2,2,'');
  PIC16InstName[i_ORA].AddAddressMode(aZeroPage,$05,2,3,'');
  PIC16InstName[i_ORA].AddAddressMode(aZeroPagX,$15,2,4,'');
  PIC16InstName[i_ORA].AddAddressMode(aAbsolute,$0D,3,4,'');
  PIC16InstName[i_ORA].AddAddressMode(aAbsolutX,$1D,3,4,'*');
  PIC16InstName[i_ORA].AddAddressMode(aAbsolutY,$19,3,4,'*');
  PIC16InstName[i_ORA].AddAddressMode(aIdxIndir,$01,2,6,'');
  PIC16InstName[i_ORA].AddAddressMode(aIndirIdx,$11,2,5,'*');
  PIC16InstName[i_PHA].name := 'PHA';  //Push Accumulator on Stack
  PIC16InstName[i_PHA].AddAddressMode(aImplicit,$48,1,3,'');
  PIC16InstName[i_PHP].name := 'PHP';  //Push Processor Status on Stack
  PIC16InstName[i_PHP].AddAddressMode(aImplicit,$08,1,3,'');
  PIC16InstName[i_PLA].name := 'PLA';  //Pull Accumulator from Stack
  PIC16InstName[i_PLA].AddAddressMode(aImplicit,$68,1,4,'');
  PIC16InstName[i_PLP].name := 'PLP';  //Pull Processor Status fromStack
  PIC16InstName[i_PLP].AddAddressMode(aImplicit,$28,1,4,'');
  PIC16InstName[i_ROL].name := 'ROL';  //Rotate One Bit Left (Memory orAccumulator)
  PIC16InstName[i_ROL].AddAddressMode(aAcumulat,$2A,1,2,'');
  PIC16InstName[i_ROL].AddAddressMode(aZeroPage,$26,2,5,'');
  PIC16InstName[i_ROL].AddAddressMode(aZeroPagX,$36,2,6,'');
  PIC16InstName[i_ROL].AddAddressMode(aAbsolute,$2E,3,6,'');
  PIC16InstName[i_ROL].AddAddressMode(aAbsolutX,$3E,3,7,'');
  PIC16InstName[i_ROR].name := 'ROR';  //Rotate One Bit Right (Memory or Accumulator)
  PIC16InstName[i_ROR].AddAddressMode(aAcumulat,$6A,1,2,'');
  PIC16InstName[i_ROR].AddAddressMode(aZeroPage,$66,2,5,'');
  PIC16InstName[i_ROR].AddAddressMode(aZeroPagX,$76,2,6,'');
  PIC16InstName[i_ROR].AddAddressMode(aAbsolute,$6E,3,6,'');
  PIC16InstName[i_ROR].AddAddressMode(aAbsolutX,$7E,3,7,'');
  PIC16InstName[i_RTI].name := 'RTI';  //Return from Interrupt
  PIC16InstName[i_RTI].AddAddressMode(aImplicit,$40,1,6,'');
  PIC16InstName[i_RTS].name := 'RTS';  //Return from Subroutine
  PIC16InstName[i_RTS].AddAddressMode(aImplicit,$60,1,6,'');
  PIC16InstName[i_SBC].name := 'SBC';  //Subtract Memory from Accumulator withBorrow
  PIC16InstName[i_SBC].AddAddressMode(aImmediat,$E9,2,2,'');
  PIC16InstName[i_SBC].AddAddressMode(aZeroPage,$E5,2,3,'');
  PIC16InstName[i_SBC].AddAddressMode(aZeroPagX,$F5,2,4,'');
  PIC16InstName[i_SBC].AddAddressMode(aAbsolute,$ED,3,4,'');
  PIC16InstName[i_SBC].AddAddressMode(aAbsolutX,$FD,3,4,'*');
  PIC16InstName[i_SBC].AddAddressMode(aAbsolutY,$F9,3,4,'*');
  PIC16InstName[i_SBC].AddAddressMode(aIdxIndir,$E1,2,6,'');
  PIC16InstName[i_SBC].AddAddressMode(aIndirIdx,$F1,2,5,'*');
  PIC16InstName[i_SEC].name := 'SEC';  //Set Carry Flag
  PIC16InstName[i_SEC].AddAddressMode(aImplicit,$38,1,2,'');
  PIC16InstName[i_SED].name := 'SED';  //Set Decimal Flag
  PIC16InstName[i_SED].AddAddressMode(aImplicit,$F8,1,2,'');
  PIC16InstName[i_SEI].name := 'SEI';  //Set Interrupt Disable Status
  PIC16InstName[i_SEI].AddAddressMode(aImplicit,$78,1,2,'');
  PIC16InstName[i_STA].name := 'STA';  //Store Accumulator in Memory
  PIC16InstName[i_STA].AddAddressMode(aZeroPage,$85,2,3,'');
  PIC16InstName[i_STA].AddAddressMode(aZeroPagX,$95,2,4,'');
  PIC16InstName[i_STA].AddAddressMode(aAbsolute,$8D,3,4,'');
  PIC16InstName[i_STA].AddAddressMode(aAbsolutX,$9D,3,5,'');
  PIC16InstName[i_STA].AddAddressMode(aAbsolutY,$99,3,5,'');
  PIC16InstName[i_STA].AddAddressMode(aIdxIndir,$81,2,6,'');
  PIC16InstName[i_STA].AddAddressMode(aIndirIdx,$91,2,6,'');
  PIC16InstName[i_STX].name := 'STX';  //Store Index X in Memory
  PIC16InstName[i_STX].AddAddressMode(aZeroPage,$86,2,3,'');
  PIC16InstName[i_STX].AddAddressMode(aZeroPagY,$96,2,4,'');
  PIC16InstName[i_STX].AddAddressMode(aAbsolute,$8E,3,4,'');
  PIC16InstName[i_STY].name := 'STY';  //Sore Index Y in Memory
  PIC16InstName[i_STY].AddAddressMode(aZeroPage,$84,2,3,'');
  PIC16InstName[i_STY].AddAddressMode(aZeroPagX,$94,2,4,'');
  PIC16InstName[i_STY].AddAddressMode(aAbsolute,$8C,3,4,'');
  PIC16InstName[i_TAX].name := 'TAX';  //Transfer Accumulator to IndexX
  PIC16InstName[i_TAX].AddAddressMode(aImplicit,$AA,1,2,'');
  PIC16InstName[i_TAY].name := 'TAY';  //Transfer Accumulator to IndexY
  PIC16InstName[i_TAY].AddAddressMode(aImplicit,$A8,1,2,'');
  PIC16InstName[i_TSX].name := 'TSX';  //Transfer Stack Pointer toIndex X
  PIC16InstName[i_TSX].AddAddressMode(aImplicit,$BA,1,2,'');
  PIC16InstName[i_TXA].name := 'TXA';  //Transfer Index X to Accumulator
  PIC16InstName[i_TXA].AddAddressMode(aImplicit,$8A,1,2,'');
  PIC16InstName[i_TXS].name := 'TXS';  //Transfer Index X to StackRegister
  PIC16InstName[i_TXS].AddAddressMode(aImplicit,$9A,1,2,'');
  PIC16InstName[i_TYA].name := 'TYA';  //Transfer Index Y to Accumulator
  PIC16InstName[i_TYA].AddAddressMode(aImplicit,$98,1,2,'');

  PIC16InstName[i_Inval].name := 'Inval';


end;
initialization
  InitTables;
end. //1550
