{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Float16

    Main purpose of this library is to provide routines for conversion from and
    to half precision (16bit) floating point numbers (Single -> Half, Half ->
    Single).
    It also provides functions for basic arithmetic and comparison, as well as
    overloaded operators when compiled using FPC. But note that these functions
    only converts arguments given as halfs into single-precision (32bit) floats
    and operates on them.
    
    F16C extension is used when symbol AllowF16CExtension is defined, PurePascal
    is not defined, and when (and only when) it is supported by the CPU and OS.

    Implemented Half should conform to IEEE 754-2008, meaning it has one sign
    bit (value is negative when sign bit is set, positive otherwise), 5 bits of
    biased exponent (exponent bias is 15) and 11 bit mantissa (10 bits
    explicitly stored, highest bit is assumed to be zero for denormal numbers,
    one otherwise)

      NOTE - type Half is declared in unit AuxTypes, not here.

  Version 1.0.3 (2020-11-09)

  Last change 2020-11-09

  ©2017-2020 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Float16

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID is required only when AllowF16CExtension symbol is defined and
  PurePascal symbol is not defined.

===============================================================================}
unit Float16;
{
  Float16Utils_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF Float16Utils_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF not(Defined(CPU386) or Defined(CPUX86_64) or Defined(CPUX64))}
  {$DEFINE PurePascal}
{$IFEND}

{$IFDEF ENDIAN_BIG}
  {$MESSAGE FATAL 'Big-endian architecture not supported'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE ObjFPC}{$MODESWITCH CLASSICPROCVARS+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
    {$DEFINE ASMSuppressSizeWarnings}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------

{
  AllowF16CExtension

  When defined, allows the use of F16C extension in ASM. The extension is used
  only when both CPU and OS supports it, otherwise pascal implementation is
  called instead.
  Has no meaning when PurePascal symbol is defined.

  Defined by default.
}
{$DEFINE AllowF16CExtension}

{
  H2S_Lookup

  When defined, pascal implementation of Half to Single conversion is done using
  large lookup table.
  This is faster than procedural conversion, but inclusion of this table
  increases size of the resulting binary by about 128KiB and prevents raising
  of any unmasked floating-point exception and indication of masked exceptions
  (result of the conversion is the same as if all exceptions would be masked).

  Not defined by default.
}
{.$DEFINE H2S_Lookup}

//------------------------------------------------------------------------------

// do not touch following...
{$IF not Defined(PurePascal) and Defined(AllowF16CExtension)}
  {$DEFINE F16U_ASM_IMPL}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes {contains declaration of type Half};

{-------------------------------------------------------------------------------
    Some predefined Half values
-------------------------------------------------------------------------------}
const
  Infinity: Half = ($00,$7C); // positive infinity
  NaN:      Half = ($00,$7E); // quiet NaN
  MaxHalf:  Half = ($FF,$7B); // 65504
  MinHalf:  Half = ($01,$00); // 5.96046e-8

{===============================================================================
    Library-specific exceptions - declaration
===============================================================================}
type
  EF16UException = class(Exception);

  EF16UInvalidFlag     = class(EF16UException);
  EF16UUnknownFunction = class(EF16UException);

{-------------------------------------------------------------------------------
    Library-specific exceptions - floating-point exceptions
-------------------------------------------------------------------------------}
(*
type
  EF16UFPUException = class(EF16UException)
  protected
    Function DefaultMessage: String; virtual; abstract;
  public
    constructor CreateDefMsg;
  end;
*)
{-------------------------------------------------------------------------------
    Library-specific exceptions - individual floating-point exception classes
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
================================================================================
                               Auxiliary routines
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Auxiliary routines - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - SSE status and control register (MXCSR) access
-------------------------------------------------------------------------------}
// some constants for MXCSR
const
  // MXCSR masks
  MXCSR_EXC_InvalidOP = UInt32($00000001);
  MXCSR_EXC_Denormal  = UInt32($00000002);
  MXCSR_EXC_DivByZero = UInt32($00000004);
  MXCSR_EXC_Overflow  = UInt32($00000008);
  MXCSR_EXC_Underflow = UInt32($00000010);
  MXCSR_EXC_Precision = UInt32($00000020);

  MXCSR_EMASK_InvalidOP = UInt32($00000080);
  MXCSR_EMASK_Denormal  = UInt32($00000100);
  MXCSR_EMASK_DivByZero = UInt32($00000200);
  MXCSR_EMASK_Overflow  = UInt32($00000400);
  MXCSR_EMASK_Underflow = UInt32($00000800);
  MXCSR_EMASK_Precision = UInt32($00001000);

  MXCSR_DenormalsAreZero = UInt32($00000040);
  MXCSR_FlushToZero      = UInt32($00008000);

  MXCSR_Rounding = UInt32($00006000); // bits 13..14

  MXCSR_SHIFT_Rounding = 13;

//------------------------------------------------------------------------------
// low-level access

Function GetMXCSR: UInt32;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetMXCSR(NewValue: UInt32);{$IFDEF CanInline} inline;{$ENDIF}

Function EmulatedMXCSR: Boolean;{$IFDEF CanInline} inline;{$ENDIF}

procedure InitMXCSR;{$IFDEF CanInline} inline;{$ENDIF}

Function GetMXCSRMask: UInt32;{$IFDEF CanInline} inline;{$ENDIF}
Function GetMXCSRSupportsDAZ: Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
// abstracted access

type
  TSSERoundingMode = (rmNearest,rmDown,rmUp,rmTruncate);

  TSSEException = (excInvalidOp,excDenormal,excDivByZero,excOverflow,
                   excUnderflow,excPrecision);

  TSSEExceptions = set of TSSEException;

  TSSEFlag = (flDenormalsAreZero,flFlushToZero);

  TSSEFlags = set of TSSEFlag;

Function GetSSERoundingMode: TSSERoundingMode;
Function SetSSERoundingMode(NewValue: TSSERoundingMode): TSSERoundingMode;

Function GetSSEExceptionMask(SSEException: TSSEException): Boolean;
Function SetSSEExceptionMask(SSEException: TSSEException; NewValue: Boolean): Boolean;

Function GetSSEExceptionMasks: TSSEExceptions;
Function SetSSEExceptionMasks(NewValue: TSSEExceptions): TSSEExceptions;

Function GetSSEExceptionState(SSEException: TSSEException): Boolean;
Function SetSSEExceptionState(SSEException: TSSEException; NewValue: Boolean): Boolean;

Function GetSSEExceptionStates: TSSEExceptions;
Function SetSSEExceptionStates(NewValue: TSSEExceptions): TSSEExceptions;

Function GetSSEFlag(Flag: TSSEFlag): Boolean;
Function SetSSEFlag(Flag: TSSEFlag; NewValue: Boolean): Boolean;

Function GetSSEFlags: TSSEFlags;
procedure SetSSEFlags(NewValue: TSSEFlags);

{-------------------------------------------------------------------------------
    Auxiliary routines - conversion functions
-------------------------------------------------------------------------------}

Function MapHalfToWord(Value: Half): UInt16;{$IFDEF CanInline} inline; {$ENDIF}
Function MapWordToHalf(Value: UInt16): Half;{$IFDEF CanInline} inline; {$ENDIF}

// for gebugging purposes...
procedure Fce_HalfToSingle_Pas(HalfPtr,SinglePtr: Pointer); register;
procedure Fce_SingleToHalf_Pas(SinglePtr,HalfPtr: Pointer); register;

Function HalfToSingle(Value: Half): Single;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}
Function SingleToHalf(Value: Single): Half;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}

procedure HalfToSingle4x(Input,Output: Pointer);{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}
procedure SingleToHalf4x(Input,Output: Pointer);{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}

{-------------------------------------------------------------------------------
    Number information functions
-------------------------------------------------------------------------------}

Function IsZero(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}
Function IsNaN(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}
Function IsInfinite(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}
Function IsNormal(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}  // returns false on zero
Function IsDenormal(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}

{-------------------------------------------------------------------------------
    Sign-related functions
-------------------------------------------------------------------------------}

type
  TValueSign = -1..1;

Function Sign(const Value: Half): TValueSign;
Function Abs(const Value: Half): Half;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}
Function Neg(const Value: Half): Half;{$IF Defined(CanInline) and Defined(FPC)} inline; {$IFEND}

{-------------------------------------------------------------------------------
    Comparison functions
-------------------------------------------------------------------------------}

Function IsEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function IsLess(const A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function IsGreater(const A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function IsLessOrEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function IsGreaterOrEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}

type
  TValueRelationship = -1..1; // to preven problems (because delphi vs. FPC)

Function CompareValue(const A,B: Half; Epsilon: Half): TValueRelationship;{$IFDEF CanInline} inline; {$ENDIF} overload;
Function CompareValue(const A,B: Half): TValueRelationship;{$IFDEF CanInline} inline; {$ENDIF} overload;

Function SameValue(const A,B: Half; Epsilon: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF} overload;
Function SameValue(const A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF} overload;

{-------------------------------------------------------------------------------
    Arithmetic functions
-------------------------------------------------------------------------------}

Function Add(const A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}
Function Subtract(const A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}
Function Multiply(const A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}
Function Divide(const A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}

{$IFDEF FPC}
{-------------------------------------------------------------------------------
    Overloaded operators (FPC only)
-------------------------------------------------------------------------------}

// assignment operators
operator := (Value: Half): Single;{$IFDEF CanInline} inline; {$ENDIF}
operator := (Value: Single): Half;{$IFDEF CanInline} inline; {$ENDIF}

// explicit assignment operators
operator explicit (Value: Half): Single;{$IFDEF CanInline} inline; {$ENDIF}
operator explicit (Value: Single): Half;{$IFDEF CanInline} inline; {$ENDIF}

// comparison operators
operator = (A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
operator > (A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
operator < (A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
operator >= (A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
operator <= (A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
operator <> (A,B: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}

// unary operators
operator + (A: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}
operator - (A: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}

// arithmetic operators
operator + (A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}
operator - (A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}
operator * (A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}
operator / (A,B: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}

{$ENDIF}

{===============================================================================
    Unit implementation routines
===============================================================================}

type
  TFloat16Functions = (fnGetMXCSR,fnSetMXCSR,fnHalfToSingle,fnSingleToHalf,
                       fnHalfToSingle4x,fnSingleToHalf4x);

{
  Returns true when selected function is currently set to asm-implemented code,
  false otherwise.
}
Function Float16FunctionIsAsm(Func: TFloat16Functions): Boolean;

{
  Routes selected function to pascal implementation.
}
procedure Float16FunctionPas(Func: TFloat16Functions);

{
  Routes selected function to assembly implementation.
  Does nothing when PurePascal symbol is defined or AllowF16CExtension symbol
  is not defined.
}
procedure Float16FunctionAsm(Func: TFloat16Functions);

{
  Calls Float16FunctionAsm for selected function when AssignASM is set to true,
  otherwise it calls Float16FunctionPas.
}
procedure Float16FunctionAssign(Func: TFloat16Functions; AssignASM: Boolean);

implementation

uses
{$IF Defined(AllowF16CExtension) and not Defined(PurePascal)}
  SimpleCPUID,
{$IFEND}
  Math;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{-------------------------------------------------------------------------------
    Internal constants
-------------------------------------------------------------------------------}

const
  F16_MASK_SIGN = UInt16($8000);  // sign bit
  F16_MASK_EXP  = UInt16($7C00);  // exponent
  F16_MASK_FRAC = UInt16($03FF);  // fraction/mantissa
  F16_MASK_NSGN = UInt16($7FFF);  // non-sign bits
  F16_MASK_FHB  = UInt16($0200);  // highest bit of the mantissa
{$IFNDEF FPC} // I don't want to deal with nonsensical warnings about unused constants
  F16_MASK_INTB = UInt16($0400);  // otherwise implicit integer bit of the mantissa
{$ENDIF}

  F32_MASK_SIGN = UInt32($80000000);
  F32_MASK_EXP  = UInt32($7F800000);
  F32_MASK_FRAC = UInt32($007FFFFF);
{$IFNDEF FPC}
  F32_MASK_NSGN = UInt32($7FFFFFFF);
{$ENDIF}
  F32_MASK_FHB  = UInt32($00400000);
  F32_MASK_INTB = UInt32($00800000);

{-------------------------------------------------------------------------------
================================================================================
                               Auxiliary routines
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Auxiliary routines - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - internal functions
-------------------------------------------------------------------------------}

{
  WARNING - MXCSR_MASK is initialized once and only once, at the unit
            initialization. It must not be written into later at any cost, that
            would break thread safety.
            Therefore consider this variable to be read-only.
}
var
  MXCSR_MASK: UInt32;

//------------------------------------------------------------------------------

{$IFDEF F16U_ASM_IMPL}
Function MXCSR_MASK_Load(Mem: Pointer): UInt32; register; assembler;
asm
{
  - FXSAVE does not check for pending FPU exceptions, therefore FWAIT to be sure
  - state saved by FXSAVE can contain MXCSR_MASK provided by the CPU (it if is
    zero, CPU is not providing it)
  - position of the mask is the same in all modes (offset 28) - no need to
    branch for individual modes
}
    FWAIT
    FXSAVE  [Mem]
    MOV     EAX,  dword ptr [Mem + 28]  
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure MXCSR_MASK_Init(EmulatedImpl: Boolean);
{$IFDEF F16U_ASM_IMPL}
var
  Buff: Pointer;
  Mask: UInt32;
begin
If not EmulatedImpl then
  begin
    // memory for FXSAVE must be 16-byte aligned and intialized to all-zero
    Buff := AllocMem(528{512 + 16 for alignement});
    try
      If (PtrUInt(Buff) and PtrUInt($F)) = 0 then
        Mask := MXCSR_MASK_Load(Buff)
      else
        Mask := MXCSR_MASK_Load(Pointer((PtrUInt(Buff) + 16) and not PtrUInt($F)));
    finally
      FreeMem(Buff,528);
    end;
    If Mask <> 0 then
      MXCSR_MASK := Mask
    else
      MXCSR_MASK := $0000FFBF;  // default mask, note that DAZ bit is not supported
  end
else MXCSR_MASK := $0000FFFF;  // DAZ bit is supported in pascal emulation
end;
{$ELSE}
begin
MXCSR_MASK := $0000FFFF;  // DAZ bit supported
end;
{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - SSE status and control register (MXCSR) access
-------------------------------------------------------------------------------}

threadvar
  Pas_MXCSR:  UInt32;
  MXCSRInit:  Boolean;

//------------------------------------------------------------------------------

{$IFDEF F16U_ASM_IMPL}

Function GetMXCSR_Aam: UInt32; register; assembler;
var
  Temp: UInt32;
asm
    STMXCSR   dword ptr [Temp]
    MOV       EAX,  dword ptr [Temp]
    AND       EAX,  dword ptr [MXCSR_MASK]
end;

//------------------------------------------------------------------------------

procedure SetMXCSR_Asm(NewValue: UInt32); register; assembler;
var
  Temp: UInt32;
asm
    AND       NewValue, dword ptr [MXCSR_MASK]
    MOV       dword ptr [Temp], NewValue
    LDMXCSR   dword ptr [Temp]
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function GetMXCSR_Pas: UInt32; register;
begin
If not MXCSRInit then
  begin
  {
    rounding set to nearest, masked precission, underflow and denormal
    exceptions, DAZ and FTZ flags are clear, exception states are all clear
  }
    Pas_MXCSR := $00001900;
    MXCSRInit := True;
  end;
Result := Pas_MXCSR and MXCSR_MASK;
end;

//------------------------------------------------------------------------------

procedure SetMXCSR_Pas(NewValue: UInt32); register;
begin
Pas_MXCSR := NewValue and MXCSR_MASK;
MXCSRInit := True;
end;

//------------------------------------------------------------------------------

var
  Var_GetMXCSR: Function: UInt32; register;
  Var_SetMXCSR: procedure(NewValue: UInt32); register;

//------------------------------------------------------------------------------

Function GetMXCSR: UInt32;
begin
Result := Var_GetMXCSR;
end;

//------------------------------------------------------------------------------

procedure SetMXCSR(NewValue: UInt32);
begin
Var_SetMXCSR(NewValue);
end;

//------------------------------------------------------------------------------

Function EmulatedMXCSR: Boolean;
begin
{$IFDEF F16U_ASM_IMPL}
If Assigned(@Var_SetMXCSR) then
  Result := @Var_SetMXCSR = @SetMXCSR_Pas
else
  raise EF16UUnknownFunction.Create('EmulatedMXCSR: Unassigned routing.');
{$ELSE}
Result := True;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure InitMXCSR;
begin
SetMXCSR($00001900);
end;

//------------------------------------------------------------------------------

Function GetMXCSRMask: UInt32;
begin
Result := MXCSR_MASK;
end;

//------------------------------------------------------------------------------

Function GetMXCSRSupportsDAZ: Boolean;
begin
Result := (MXCSR_MASK and MXCSR_DenormalsAreZero) <> 0;
end;

//==============================================================================

Function GetSSERoundingMode: TSSERoundingMode;
begin
case (GetMXCSR and MXCSR_Rounding) shr MXCSR_SHIFT_Rounding of
  1:  Result := rmDown;
  2:  Result := rmUp;
  3:  Result := rmTruncate;
else
  Result := rmNearest;
end;
end;

//------------------------------------------------------------------------------

Function SetSSERoundingMode(NewValue: TSSERoundingMode): TSSERoundingMode;
var
  Num:  UInt32;
begin
Result := GetSSERoundingMode;
case NewValue of
  rmDown:     Num := 1;
  rmUp:       Num := 2;
  rmTruncate: Num := 3;
else
 {rmNearest}
  Num := 0;
end;
SetMXCSR((GetMXCSR and not MXCSR_Rounding) or (Num shl MXCSR_SHIFT_Rounding));
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionMask(SSEException: TSSEException): Boolean;
begin
case SSEException of
  excInvalidOp: Result := (GetMXCSR and MXCSR_EMASK_InvalidOP) <> 0;
  excDenormal:  Result := (GetMXCSR and MXCSR_EMASK_Denormal) <> 0;
  excDivByZero: Result := (GetMXCSR and MXCSR_EMASK_DivByZero) <> 0;
  excOverflow:  Result := (GetMXCSR and MXCSR_EMASK_Overflow) <> 0;
  excUnderflow: Result := (GetMXCSR and MXCSR_EMASK_Underflow) <> 0;
  excPrecision: Result := (GetMXCSR and MXCSR_EMASK_Precision) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionMask: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionMask(SSEException: TSSEException; NewValue: Boolean): Boolean;

  procedure SetFlag(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;

begin
Result := GetSSEExceptionMask(SSEException);
case SSEException of
  excInvalidOp: SetFlag(MXCSR_EMASK_InvalidOP);
  excDenormal:  SetFlag(MXCSR_EMASK_Denormal);
  excDivByZero: SetFlag(MXCSR_EMASK_DivByZero);
  excOverflow:  SetFlag(MXCSR_EMASK_Overflow);
  excUnderflow: SetFlag(MXCSR_EMASK_Underflow);
  excPrecision: SetFlag(MXCSR_EMASK_Precision);
else
  raise EF16UInvalidFlag.CreateFmt('SetSSEExceptionMask: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionMasks: TSSEExceptions;
var
  MXCSR:  UInt32;
  i:      TSSEException;
begin
Result := [];
MXCSR := GetMXCSR;
For i := Low(TSSEException) to High(TSSEException) do
  case i of
    excInvalidOp: If (MXCSR and MXCSR_EMASK_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (MXCSR and MXCSR_EMASK_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (MXCSR and MXCSR_EMASK_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (MXCSR and MXCSR_EMASK_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (MXCSR and MXCSR_EMASK_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (MXCSR and MXCSR_EMASK_Precision) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionMasks: Invalid SSE exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionMasks(NewValue: TSSEExceptions): TSSEExceptions;
var
  MXCSR:  UInt32;

  procedure SetFlag(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
Result := GetSSEExceptionMasks;
MXCSR := GetMXCSR;
SetFlag(MXCSR_EMASK_InvalidOP,excInvalidOp in NewValue);
SetFlag(MXCSR_EMASK_Denormal,excDenormal in NewValue);
SetFlag(MXCSR_EMASK_DivByZero,excDivByZero in NewValue);
SetFlag(MXCSR_EMASK_Overflow,excOverflow in NewValue);
SetFlag(MXCSR_EMASK_Underflow,excUnderflow in NewValue);
SetFlag(MXCSR_EMASK_Precision,excPrecision in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionState(SSEException: TSSEException): Boolean;
begin
case SSEException of
  excInvalidOp: Result := (GetMXCSR and MXCSR_EXC_InvalidOP) <> 0;
  excDenormal:  Result := (GetMXCSR and MXCSR_EXC_Denormal) <> 0;
  excDivByZero: Result := (GetMXCSR and MXCSR_EXC_DivByZero) <> 0;
  excOverflow:  Result := (GetMXCSR and MXCSR_EXC_Overflow) <> 0;
  excUnderflow: Result := (GetMXCSR and MXCSR_EXC_Underflow) <> 0;
  excPrecision: Result := (GetMXCSR and MXCSR_EXC_Precision) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionState: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionState(SSEException: TSSEException; NewValue: Boolean): Boolean;

  procedure SetFlag(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;

begin
Result := GetSSEExceptionMask(SSEException);
case SSEException of
  excInvalidOp: SetFlag(MXCSR_EXC_InvalidOP);
  excDenormal:  SetFlag(MXCSR_EXC_Denormal);
  excDivByZero: SetFlag(MXCSR_EXC_DivByZero);
  excOverflow:  SetFlag(MXCSR_EXC_Overflow);
  excUnderflow: SetFlag(MXCSR_EXC_Underflow);
  excPrecision: SetFlag(MXCSR_EXC_Precision);
else
  raise EF16UInvalidFlag.CreateFmt('SetSSEExceptionState: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionStates: TSSEExceptions;
var
  MXCSR:  UInt32;
  i:      TSSEException;
begin
Result := [];
MXCSR := GetMXCSR;
For i := Low(TSSEException) to High(TSSEException) do
  case i of
    excInvalidOp: If (MXCSR and MXCSR_EXC_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (MXCSR and MXCSR_EXC_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (MXCSR and MXCSR_EXC_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (MXCSR and MXCSR_EXC_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (MXCSR and MXCSR_EXC_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (MXCSR and MXCSR_EXC_Precision) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionStates: Invalid SSE exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionStates(NewValue: TSSEExceptions): TSSEExceptions;
var
  MXCSR:  UInt32;

  procedure SetFlag(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
Result := GetSSEExceptionMasks;
MXCSR := GetMXCSR;
SetFlag(MXCSR_EXC_InvalidOP,excInvalidOp in NewValue);
SetFlag(MXCSR_EXC_Denormal,excDenormal in NewValue);
SetFlag(MXCSR_EXC_DivByZero,excDivByZero in NewValue);
SetFlag(MXCSR_EXC_Overflow,excOverflow in NewValue);
SetFlag(MXCSR_EXC_Underflow,excUnderflow in NewValue);
SetFlag(MXCSR_EXC_Precision,excPrecision in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

Function GetSSEFlag(Flag: TSSEFlag): Boolean;
begin
case Flag of
  flDenormalsAreZero: Result := (GetMXCSR and MXCSR_DenormalsAreZero) <> 0;
  flFlushToZero:      Result := (GetMXCSR and MXCSR_FlushToZero) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEFlag(Flag: TSSEFlag; NewValue: Boolean): Boolean;

  procedure SetFlag(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;
  
begin
Result := GetSSEFlag(Flag);
case Flag of
  flDenormalsAreZero: SetFlag(MXCSR_DenormalsAreZero);
  flFlushToZero:      SetFlag(MXCSR_FlushToZero);
else
  raise EF16UInvalidFlag.CreateFmt('SetSSEFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function GetSSEFlags: TSSEFlags;
var
  MXCSR:  UInt32;
  i:      TSSEFlag;
begin
Result := [];
MXCSR := GetMXCSR;
For i := Low(TSSEFlag) to High(TSSEFlag) do
  case i of
    flDenormalsAreZero: If (MXCSR and MXCSR_DenormalsAreZero) <> 0 then Include(Result,i);
    flFlushToZero:      If (MXCSR and MXCSR_FlushToZero) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetX87Flags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

procedure SetSSEFlags(NewValue: TSSEFlags);
var
  MXCSR:  UInt32;

  procedure SetFlag(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
MXCSR := GetMXCSR;
SetFlag(MXCSR_DenormalsAreZero,flDenormalsAreZero in NewValue);
SetFlag(MXCSR_FlushToZero,flFlushToZero in NewValue);
SetMXCSR(MXCSR);
end;

{-------------------------------------------------------------------------------
    Conversion functions
-------------------------------------------------------------------------------}

procedure Fce_HalfToSingle_Pas(HalfPtr,SinglePtr: Pointer); register;
{$IFDEF H2S_Lookup}
  {$INCLUDE '.\Float16.inc'}
begin
PUInt32(SinglePtr)^ := H2S_Lookup[PUInt16(HalfPtr)^ and F16_MASK_NSGN] or
                       UInt32(PUInt16(HalfPtr)^ and F16_MASK_SIGN) shl 16;
end;
{$ELSE}
var
  Sign:           UInt16;
  Exponent:       Int32;  // biased exponent (true exponent + 15)
  Mantissa:       UInt16;
  MantissaShift:  Integer;

  Function HighZeroCount(aValue: UInt16): Integer;
  begin
    If aValue <> 0 then
      begin
        Result := 0;
        while (aValue and UInt16($8000)) = 0  do
          begin
            aValue := UInt16(aValue shl 1);
            Inc(Result);
          end;
      end
    else Result := 16;
  end;

begin
(*
Sign := PUInt16(HalfPtr)^ and F16_MASK_SIGN;
Exponent := Int32((PUInt16(HalfPtr)^ and F16_MASK_EXP) shr 10);
Mantissa := PUInt16(HalfPtr)^ and F16_MASK_FRAC;
case Exponent of

        // zero exponent - zero or denormal
    0:  If Mantissa <> 0 then
          begin
          {
            denormal, normalize...

            ...shift mantissa left so that its highest set bit will be shifted
            to implicit integer bit (bit 23), also correct exponent to reflect
            this change
          }
            MantissaShift := HighZeroCount(Mantissa) + 8;
            PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                                   UInt32(UInt32(Exponent - MantissaShift + 126) shl 23) or
                                  (UInt32(UInt32(Mantissa) shl MantissaShift) and F32_MASK_FRAC);
          end
        // zero, return signed zero
        else PUInt32(SinglePtr)^ := UInt32(Sign shl 16);

        // max exponent - infinity or NaN
  $1F:  If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and F16_MASK_FHB) = 0 then
              begin
                // signaled NaN
                If GetSSEFlag(flMInvalidOp) then
                  // quiet signed NaN with mantissa
                  PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or (F32_MASK_EXP or F32_MASK_FHB) or
                                         UInt32(UInt32(Mantissa) shl 13)
                else
                  // signaling NaN
                  raise EInvalidOp.Create('Invalid floating point operation');
              end
            // quiet signed NaN with mantissa
            else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP or
                                        UInt32(UInt32(Mantissa) shl 13);
          end
        // infinity - return signed infinity
        else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP;
        
else
  // normal number
  PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                         UInt32(UInt32(Exponent + 112) shl 23) or
                         UInt32(UInt32(Mantissa) shl 13);
end;
*)
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Pas(SinglePtr,HalfPtr: Pointer); register;
var
  Sign:       UInt32;
  Exponent:   Int32;  // biased exponent (true exponent + 127)
  Mantissa:   UInt32;

  Function ShiftMantissa(Value: UInt32; Shift: Byte): UInt32;
  var
    Mask:     UInt32;
    Low,High: UInt32;
  begin
    If (Shift > 0) and (Shift < 25) then
      begin
        Mask := UInt32($FFFFFFFF) shr (32 - Shift);
        If (Value and Mask) <> 0 then
          begin
            Low := Value and not Mask;
            High := Low + (Mask + 1);
            case GetSSERoundingMode of
              rmDown,
              rmUp,
              rmTruncate: raise Exception.Create('not implemented yet');
            else
             {rmNearest}
              If (Value - Low) > (High - Value) then
                Result := High shr Shift
              else If (Value - Low) < (High - Value) then
                Result := Low shr Shift
              else
                begin
                  // select the one with clear lowest bit
                  If High and (Mask + 1) = 0 then
                    Result := High shr Shift
                  else
                    Result := Low shr Shift;
                end;
            end;
          end
        else Result := Value shr Shift;
      end
    // following cases should not happen, but whatever...  
    else If Shift >= 25 then
      Result := 0
    else
      Result := Value;
  end;

begin
(*
Sign := PUInt32(SinglePtr)^ and F32_MASK_SIGN;
Exponent := (PUInt32(SinglePtr)^ and F32_MASK_EXP) shr 23;
Mantissa := PUInt32(SinglePtr)^ and F32_MASK_FRAC;
case Exponent of

        // exponent of zero - zero or denormal
    0:  If Mantissa <> 0 then
          begin
            // denormal
            If GetSSEFlag(flMUnderflow) then
              // convert to signed zero
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16)
            else
              // signal underflow
              raise EUnderflow.Create('Floating point underflow');
          end
        // return signed zero
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16);

        // exponent 1..101 (-126..-26 unbiased) - too small to be represented in half even as denormal
   1..
  $65:  If GetSSEFlag(flMUnderflow) then
          begin
            { do not delete yet
            If ((RoundMode = 1{down}) and (Sign <> 0)) or
               ((RoundMode = 2{up}) and (Sign = 0)) then
              // convert to smallest representable number
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16(1)
            else
            }
              // convert to signed zero
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
          end
        // signal underflow
        else raise EUnderflow.Create('Floating point underflow');

        // exponent 102..112 (-25..-15 unbiased) - exponent still too small to be represented in half,
        // but the result can be denormalized (implicit exponent of -14, explicit 0)
  $66..
  $70:  If GetSSEFlag(flMUnderflow) then
          // denormalizing
          PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16(ShiftMantissa(Mantissa or F32_MASK_INTB,$7E - Exponent))
        else
          // signal underflow
          raise EUnderflow.Create('Floating point underflow');

        // exponent 143..254 (+16..+127 unbiased) - too large to be represented
        // in half (resulting exponent would be larger than 15)
  $8F..
  $FE:  If GetSSEFlag(flMOverflow) then
          begin
            {
            If (RoundMode = 3{trunc}) or
               ((RoundMode = 1{down}) and (Sign = 0)) or
               ((RoundMode = 2{up}) and (Sign <> 0)) then
              // convert to largest representable number
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16($7BFF)
            else
            }
              // convert to signed infinity
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP;
          end
        // signal overflow
        else raise EOverflow.Create('Floating point overflow');

        // max exponent - infinity or NaN
  $FF:  If Mantissa <> 0 then
          begin
            // not a number (NaN)
            If (Mantissa and F32_MASK_FHB) = 0 then
              begin
                // signalled NaN
                If GetSSEFlag(flMInvalidOP) then
                  // quiet signed NaN with truncated mantissa
                  PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP or F16_MASK_FHB or
                                       UInt16(Mantissa shr 13)
                else
                  // signaling NaN
                  raise EInvalidOp.Create('Invalid floating point operation');
              end
            // quiet signed NaN with truncated mantisssa
            else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP or
                                      UInt16(Mantissa shr 13);
          end
        // signed infinity
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP;
        
else
  // exponent 113..142 (-14..+15 unbiased) - representable numbers, normalized value
  Mantissa := ShiftMantissa(Mantissa,13);
  // check if mantisa overflowed, if so, increase exponent to compensate (mantissa will be zero)
  If Mantissa > F16_MASK_FRAC then
    Inc(Exponent);
  PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or
                       UInt16((Exponent - 112) shl 10) or
                       UInt16(Mantissa and F16_MASK_FRAC);
end;
*)
end;

//==============================================================================

procedure Fce_HalfToSingle4x_Pas(HalfPtr,SinglePtr: Pointer); register;
begin
Fce_HalfToSingle_Pas(HalfPtr,SinglePtr);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Fce_HalfToSingle_Pas(Pointer(PtrUInt(HalfPtr) + 2),Pointer(PtrUInt(SinglePtr) + 4));
Fce_HalfToSingle_Pas(Pointer(PtrUInt(HalfPtr) + 4),Pointer(PtrUInt(SinglePtr) + 8));
Fce_HalfToSingle_Pas(Pointer(PtrUInt(HalfPtr) + 6),Pointer(PtrUInt(SinglePtr) + 12));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf4x_Pas(SinglePtr,HalfPtr: Pointer); register;
begin
Fce_SingleToHalf_Pas(SinglePtr,HalfPtr);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Fce_SingleToHalf_Pas(Pointer(PtrUInt(SinglePtr) + 4),Pointer(PtrUInt(HalfPtr) + 2));
Fce_SingleToHalf_Pas(Pointer(PtrUInt(SinglePtr) + 8),Pointer(PtrUInt(HalfPtr) + 4));
Fce_SingleToHalf_Pas(Pointer(PtrUInt(SinglePtr) + 12),Pointer(PtrUInt(HalfPtr) + 6));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

{$IFNDEF PurePascal}

{$IFDEF ASMSuppressSizeWarnings}
  {$WARN 2087 OFF}  //  Suppresses warnings on following $WARN
  {$WARN 7121 OFF}  //  Warning: Check size of memory operand "op: memory-operand-size is X bits, but expected [Y bits]"
{$ENDIF}

procedure Fce_HalfToSingle_Asm(Input,Output: Pointer); register; assembler;
asm
    MOVZX   EAX,  word ptr [Input]
    MOVD    XMM0, EAX

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVSS   dword ptr [Output], XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Asm(Input,Output: Pointer); register; assembler;
asm
    MOVSS   XMM0, dword ptr [Input]

    // $04 - rounding set in MXCSR is used  
    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVD    EAX,  XMM0
    MOV     word ptr [Output],  AX
end;

//------------------------------------------------------------------------------

procedure Fce_HalfToSingle4x_Asm(Input,Output: Pointer); register; assembler;
asm
    MOVSD   XMM0, qword ptr [Input]

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVUPS  dqword ptr [Output],  XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingletoHalf4x_Asm(Input,Output: Pointer); register; assembler;
asm
    MOVUPS  XMM0, dqword ptr [Input]

    // $04 - rounding set in MXCSR is used 
    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVSD   qword ptr [Output],   XMM0
end;

{$IFDEF ASMSuppressSizeWarnings}
  {$WARN 7121 ON}
  {$WARN 2087 ON}
{$ENDIF}

{$ENDIF}

//==============================================================================

var
  Var_HalfToSingle:   procedure(Input,Output: Pointer); register;
  Var_SingleToHalf:   procedure(Input,Output: Pointer); register;
  Var_HalfToSingle4x: procedure(Input,Output: Pointer); register;
  Var_SingleToHalf4x: procedure(Input,Output: Pointer); register;

//==============================================================================

Function MapHalfToWord(Value: Half): UInt16;
begin
Result := UInt16(Addr(Value)^);
end;

//------------------------------------------------------------------------------

Function MapWordToHalf(Value: UInt16): Half;
begin
Result := Half(Addr(Value)^);
end;

//------------------------------------------------------------------------------

Function HalfToSingle(Value: Half): Single;
begin
Var_HalfToSingle(@Value,@Result);
end;

//------------------------------------------------------------------------------

Function SingleToHalf(Value: Single): Half;
begin
Var_SingleToHalf(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure HalfToSingle4x(Input,Output: Pointer);
begin
Var_HalfToSingle4x(Input,Output);
end;

//------------------------------------------------------------------------------

procedure SingleToHalf4x(Input,Output: Pointer);
begin
Var_SingleToHalf4x(Input,Output);
end;

{-------------------------------------------------------------------------------
    Number information functions
-------------------------------------------------------------------------------}

Function IsZero(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// bits other than sign are zero
Result := _Value and F16_MASK_NSGN = 0;
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// max exponent and non-zero mantissa
Result := ((_Value and F16_MASK_EXP) = F16_MASK_EXP) and ((_Value and F16_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// max exponent and zero mantissa
Result := ((_Value and F16_MASK_EXP) = F16_MASK_EXP) and ((_Value and F16_MASK_FRAC) = 0);
end;

//------------------------------------------------------------------------------

Function IsNormal(const Value: Half): Boolean;
var
  _Value:   UInt16 absolute Value;
  Exponent: UInt16;
begin
// non-zero less than max exponent, non-zero mantissa
Exponent := (_Value and F16_MASK_EXP) shr 10;
Result := (Exponent > 0) and (Exponent < $1F) and ((_Value and F16_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// zero exponent, non-zero mantissa
Result := ((_Value and F16_MASK_EXP) = 0) and ((_Value and F16_MASK_FRAC) <> 0);
end;

{-------------------------------------------------------------------------------
    Sign-related functions
-------------------------------------------------------------------------------}

Function Sign(const Value: Half): TValueSign;
var
  _Value: UInt16 absolute Value;
begin
If (_Value and F16_MASK_NSGN) <> 0 then
  begin
    If (_Value and F16_MASK_SIGN) <> 0 then
      Result := -1
    else
      Result := 1;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function Abs(const Value: Half): Half;
var
  _Value:   UInt16 absolute Value;
  _Result:  UInt16 absolute Result;
begin
_Result := _Value and F16_MASK_NSGN;
end;

//------------------------------------------------------------------------------

Function Neg(const Value: Half): Half;
var
  _Value:   UInt16 absolute Value;
  _Result:  UInt16 absolute Result;
begin
_Result := _Value xor F16_MASK_SIGN;
end;

{-------------------------------------------------------------------------------
    Comparison functions
-------------------------------------------------------------------------------}

Function IsEqual(const A,B: Half): Boolean;
var
  _A: UInt16 absolute A;
  _B: UInt16 absolute B;
begin
Result := _A = _B;
end;

//------------------------------------------------------------------------------

Function IsLess(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) < HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsGreater(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) > HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsLessOrEqual(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) <= HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsGreaterOrEqual(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) >= HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function CompareValue(const A,B: Half; Epsilon: Half): TValueRelationship;
begin
Result := TValueRelationship(Math.CompareValue(HalfToSingle(A),HalfToSingle(B),HalfToSingle(Epsilon)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(const A,B: Half): TValueRelationship;
begin
Result := TValueRelationship(Math.CompareValue(HalfToSingle(A),HalfToSingle(B),0.0));
end;

//------------------------------------------------------------------------------

Function SameValue(const A,B: Half; Epsilon: Half): Boolean;
begin
Result := Math.SameValue(HalfToSingle(A),HalfToSingle(B),HalfToSingle(Epsilon));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameValue(const A,B: Half): Boolean;
begin
Result := Math.SameValue(HalfToSingle(A),HalfToSingle(B),0.0);
end;

{-------------------------------------------------------------------------------
    Arithmetic functions
-------------------------------------------------------------------------------}

Function Add(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) + HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Subtract(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) - HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Multiply(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) * HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Divide(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) / HalfToSingle(B));
end;

{$IFDEF FPC}
{-------------------------------------------------------------------------------
    Overloaded operators (FPC only)
-------------------------------------------------------------------------------}

operator := (Value: Half): Single;
begin
Result := HalfToSingle(Value);
end;

//------------------------------------------------------------------------------

operator := (Value: Single): Half;
begin
Result := SingleToHalf(Value);
end;

//==============================================================================

operator explicit (Value: Half): Single;
begin
Result := HalfToSingle(Value);
end;

//------------------------------------------------------------------------------

operator explicit (Value: Single): Half;
begin
Result := SingleToHalf(Value);
end;

//==============================================================================

operator = (A,B: Half): Boolean;
begin
Result := IsEqual(A,B);
end;

//------------------------------------------------------------------------------

operator > (A,B: Half): Boolean;
begin
Result := IsGreater(A,B);
end;

//------------------------------------------------------------------------------

operator < (A,B: Half): Boolean;
begin
Result := IsLess(A,B);
end;

//------------------------------------------------------------------------------

operator >= (A,B: Half): Boolean;
begin
Result := IsGreaterOrEqual(A,B);
end;

//------------------------------------------------------------------------------

operator <= (A,B: Half): Boolean;
begin
Result := IsLessOrEqual(A,B);
end;

//------------------------------------------------------------------------------

operator <> (A,B: Half): Boolean;
begin
Result := not IsEqual(A,B);
end;

//==============================================================================

operator + (A: Half): Half;
begin
Result := A;
end;

//------------------------------------------------------------------------------

operator - (A: Half): Half;
begin
Result := Neg(A);
end;

//==============================================================================

operator + (A,B: Half): Half;
begin
Result := Add(A,B);
end;

//------------------------------------------------------------------------------

operator - (A,B: Half): Half;
begin
Result := Subtract(A,B);
end;

//------------------------------------------------------------------------------

operator * (A,B: Half): Half;
begin
Result := Multiply(A,B);
end;

//------------------------------------------------------------------------------

operator / (A,B: Half): Half;
begin
Result := Divide(A,B);
end;

{$ENDIF}

{===============================================================================
    Unit implementation management
===============================================================================}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function Float16FunctionIsAsm(Func: TFloat16Functions): Boolean;
begin
{$IFDEF PurePascal}
Result := False;
{$ELSE}
case Func of
  fnHalfToSingle:   Result := @Var_HalfToSingle = @Fce_HalfToSingle_Asm;
  fnSingleToHalf:   Result := @Var_SingleToHalf = @Fce_SingleToHalf_Asm;
  fnHalfToSingle4x: Result := @Var_HalfToSingle4x = @Fce_HalfToSingle4x_Asm;
  fnSingleToHalf4x: Result := @Var_SingleToHalf4x = @Fce_SingleToHalf4x_Asm;
else
  raise EF16UUnknownFunction.CreateFmt('Unknown function %d.',[Ord(Func)]);
end;
{$ENDIF}
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure Float16FunctionPas(Func: TFloat16Functions);
begin
case Func of
  fnHalfToSingle:   Var_HalfToSingle := Fce_HalfToSingle_Pas;
  fnSingleToHalf:   Var_SingleToHalf := Fce_SingleToHalf_Pas;
  fnHalfToSingle4x: Var_HalfToSingle4x := Fce_HalfToSingle4x_Pas;
  fnSingleToHalf4x: Var_SingleToHalf4x := Fce_SingleToHalf4x_Pas;
else
  raise EF16UUnknownFunction.CreateFmt('Unknown function %d.',[Ord(Func)]);
end;
end;
 
//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure Float16FunctionAsm(Func: TFloat16Functions);
begin
{$IFNDEF PurePascal}
case Func of
  fnHalfToSingle:   Var_HalfToSingle := Fce_HalfToSingle_Asm;
  fnSingleToHalf:   Var_SingleToHalf := Fce_SingleToHalf_Asm;
  fnHalfToSingle4x: Var_HalfToSingle4x := Fce_HalfToSingle4x_Asm;
  fnSingleToHalf4x: Var_SingleToHalf4x := Fce_SingleToHalf4x_Asm;
else
  raise EF16UUnknownFunction.CreateFmt('Unknown function %d.',[Ord(Func)]);
end;
{$ENDIF}
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
 
//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure Float16FunctionAssign(Func: TFloat16Functions; AssignASM: Boolean);
begin
{$IFNDEF PurePascal}
If AssignASM then
  Float16FunctionPas(Func)
else
{$ENDIF}
  Float16FunctionAsm(Func);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    Unit initialization
-------------------------------------------------------------------------------}

procedure LoadDefaultFunctions;
begin
Var_HalfToSingle := Fce_HalfToSingle_Pas;
Var_SingleToHalf := Fce_SingleToHalf_Pas;
Var_HalfToSingle4x := Fce_HalfToSingle4x_Pas;
Var_SingleToHalf4x := Fce_SingleToHalf4x_Pas;
end;

//------------------------------------------------------------------------------

procedure Initialize;
begin
LoadDefaultFunctions;
{$IFDEF F16U_ASM_IMPL}
with TSimpleCPUID.Create do
try
  If Info.SupportedExtensions.F16C and Info.SupportedExtensions.SSE2 then
    begin
      Var_HalfToSingle := Fce_HalfToSingle_Asm;
      Var_SingleToHalf := Fce_SingleToHalf_Asm;
      Var_HalfToSingle4x := Fce_HalfToSingle4x_Asm;
      Var_SingleToHalf4x := Fce_SingleToHalf4x_Asm;
      MXCSR_MASK_Init(False);
    end
  else MXCSR_MASK_Init(True);
finally
  Free;
end;
{$ELSE}
MXCSR_MASK_Init(True);
{$ENDIF}
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

end.
