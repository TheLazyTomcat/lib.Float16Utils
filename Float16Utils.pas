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

  �2017-2020 Franti�ek Milt

  Contacts:
    Franti�ek Milt: frantisek.milt@gmail.com

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
unit Float16Utils;
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

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
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
  If also means the conversion does not honor settings from MXCSR (rounding,
  DAZ and FTZ modes) - the table contains values as if rounding was se to
  nearest and both DAZ and FTZ modes were disabled.

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
  PosOne:   Half = ($00,$3C); // +1.0
  NegOne:   Half = ($00,$BC); // -1.0
  Zero:     Half = ($00,$00); // (+)0

  FLOAT16_EXPONENTBIAS = 15;

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
type
  EF16UFPUException = class(EF16UException)
  protected
    Function DefaultMessage: String; virtual; abstract;
  public
    constructor CreateDefMsg;
  end;

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual floating-point exception classes
-------------------------------------------------------------------------------}
type
  EF16UInvalidOp = class(EF16UFPUException) // invalid operation/operand
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UDenormal = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UDivByZero = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UOverflow = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UUnderflow = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UPrecision = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

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
  MXCSR_EFLAG_InvalidOP = UInt32($00000001);
  MXCSR_EFLAG_Denormal  = UInt32($00000002);
  MXCSR_EFLAG_DivByZero = UInt32($00000004);
  MXCSR_EFLAG_Overflow  = UInt32($00000008);
  MXCSR_EFLAG_Underflow = UInt32($00000010);
  MXCSR_EFLAG_Precision = UInt32($00000020);

  MXCSR_EMASK_InvalidOP = UInt32($00000080);
  MXCSR_EMASK_Denormal  = UInt32($00000100);
  MXCSR_EMASK_DivByZero = UInt32($00000200);
  MXCSR_EMASK_Overflow  = UInt32($00000400);
  MXCSR_EMASK_Underflow = UInt32($00000800);
  MXCSR_EMASK_Precision = UInt32($00001000);

  MXCSR_DenormalsAreZeros = UInt32($00000040);
  MXCSR_FlushToZero       = UInt32($00008000);

  MXCSR_Rounding = UInt32($00006000); // bits 13..14

  MXCSR_SHIFT_Rounding = 13;

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Low-level access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}
{
  GetMXCSR

  Returns current value of MXCSR register.
}
Function GetMXCSR: UInt32;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{
  SetMXCSR

  Sets MXCSR register to a passed value.
}
procedure SetMXCSR(NewValue: UInt32);{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{
  EmulatedMXCSR

  Returns true when a real MXCSR register is used, false when operating on an
  emulated local implementation.
}
Function EmulatedMXCSR: Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{
  Sets MXCSR register to $00001900 - denormal, underflow and precision
  exceptions are masked (others are unmasked), rounding is set to nearest,
  DAZ and FTZ bits are cleared.

  Call this routine only when MXCSR is NOT emulated (ie. a real CPU register is
  used) and the program is compiled so that SSE is not used as a primary mean
  of floating point arithmetics and/or is not automatically initialized (if the
  MXCSR equals to $00001F80 - a default value - you can safely assume it was
  not properly initialized).

  WARNING - the initialization must be done in each execution thread.
}
procedure InitMXCSR;{$IFDEF CanInline} inline;{$ENDIF}

{
  GetMXCSRMask

  Returns a bitmask used when reading and writing the MXCSR register. Zeroes
  are marking reserved bits, ones are marking used bits.

  This value is only informative, the masking is done automatically in calls to
  functions GetMXCSR and SetMXCSR.
}
Function GetMXCSRMask: UInt32;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{
  GetMXCSRSupportsDAZ

  Returns true when DAZ bit, and therefore denormals-are-zeros mode, is
  supported by the used implementation of MXCSR (be it true SSE register or
  an emulation). False when not supported.
}
Function GetMXCSRSupportsDAZ: Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Abstracted access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}

type
  TSSERoundingMode = (rmNearest,rmDown,rmUp,rmTruncate);

  TSSEException = (excInvalidOp,excDenormal,excDivByZero,excOverflow,
                   excUnderflow,excPrecision);

  TSSEExceptions = set of TSSEException;

  TSSEFlag = (flDenormalsAreZeros,flFlushToZero);

  TSSEFlags = set of TSSEFlag;

const
  AllSSEExceptions = [excInvalidOp,excDenormal,excDivByZero,excOverflow,
                      excUnderflow,excPrecision];

//------------------------------------------------------------------------------
{
  GetSSERoundingMode

  Returns current value of rounding mode from MXCSR.
}
Function GetSSERoundingMode: TSSERoundingMode;

{
  SetSSERoundingMode

  Sets rounding mode to a selected NewValue and returns previous value of
  rounding mode.
}
Function SetSSERoundingMode(NewValue: TSSERoundingMode): TSSERoundingMode;

//------------------------------------------------------------------------------
{
  GetSSEExceptionMask

  Returns current value of selected exception mask bit.
}
Function GetSSEExceptionMask(SSEException: TSSEException): Boolean;

{
  SetSSEExceptionMask

  Sets value of selected exception mask bit in MXCSR to a NewValue and returns
  previous value of this bit.

  When the bit is set (true), the slected exception will be masked and not
  raised on its occurence.
  When clear (false), the exception is unmasked and can be raised.
}
Function SetSSEExceptionMask(SSEException: TSSEException; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetSSEExceptionMasks

  Returns status of all exception mask bits in MXCSR. When the bit is set, the
  exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function GetSSEExceptionMasks: TSSEExceptions;

{
  SetSSEExceptionMasks

  Sets new value of all exception mask bits in MXCSR. If an exception is
  included in the NewValue, the mask bit will be set, when it is not included,
  the mask bit will be cleared.

  Returns previous state of all exception mask bits.
}
Function SetSSEExceptionMasks(NewValue: TSSEExceptions): TSSEExceptions;

//------------------------------------------------------------------------------
{
  GetSSEExceptionFlag

  Returns current value of selected exception flag bit.
}
Function GetSSEExceptionFlag(SSEException: TSSEException): Boolean;

{
  SetSSEExceptionFlag

  Sets value of selected exception flag bit in MXCSR to a NewValue and returns
  previous value of this bit.
}
Function SetSSEExceptionFlag(SSEException: TSSEException; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetSSEExceptionFlags

  Returns status of all exception flag bits in MXCSR. When the bit is set,
  the exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function GetSSEExceptionFlags: TSSEExceptions;

{
  SetSSEExceptionFlags

  Sets new value of all exception flag bits in MXCSR. If an exception is
  included in the NewValue, the flag bit will be set, when it is not included,
  the flag bit will be cleared.

  Returns previous state of all exception flag bits.
}
Function SetSSEExceptionFlags(NewValue: TSSEExceptions): TSSEExceptions;

//------------------------------------------------------------------------------
{
  GetSSEFlag

  Returns current value of selected flag bit.
}
Function GetSSEFlag(Flag: TSSEFlag): Boolean;

{
  SetSSEFlag

  Sets value of selected flag bit in MXCSR to a NewValue and returns previous
  value of this bit.
}
Function SetSSEFlag(Flag: TSSEFlag; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetSSEFlags

  Returns status of all flag bits in MXCSR. When the bit is set, the flag is
  included in the result, when it is clear, the flag is excluded from the
  result.
}
Function GetSSEFlags: TSSEFlags;

{
  SetSSEFlags

  Sets new value of all flag bits in MXCSR. If a flag is included in the
  NewValue, the bit will be set, when it is not included, the bit will be
  cleared.

  Returns previous state of all flag bits.
}
procedure SetSSEFlags(NewValue: TSSEFlags);

//------------------------------------------------------------------------------
{
  ClearSSEExceptions

  Clears (sets to 0) lower 6 bits of MXCSR - that is, all exception flag bits.
}
procedure ClearSSEExceptions;{$IFDEF CanInline} inline;{$ENDIF}

{
  RaiseSSEExceptions(MXCSR)

  Raises first encountered exception according to flags set in the passed MXCSR.

  The exception flag bits are traversed one by one and, when a set bit is
  encountered, it is cleared and a corresponding exception is raised.
  Only one exception is raised in each call, even when multiple bits are set.
  The order in which the bits are traversed and therefore the order of
  exception raising is:

    InvalidOP
    Denormal
    DivByZero
    Underflow
    Overflow
    Precision
}
procedure RaiseSSEExceptions(var MXCSR: UInt32); overload;

{
  RaiseSSEExceptions

  Operates exactly the same as the first overload, but directly on the current
  MXCSR (be it real register or emulation).
}
procedure RaiseSSEExceptions; overload;

{-------------------------------------------------------------------------------
    Auxiliary routines - conversion functions
-------------------------------------------------------------------------------}
{
  MapHalfToWord

  Directly maps type half to a 16bit unsigned integer - no convesion is done.
}
Function MapHalfToWord(Value: Half): UInt16;{$IFDEF CanInline} inline;{$ENDIF}
{
  MapHalfToWord

  Directly maps 16bit unsigned integer to type half - no convesion is done.
}
Function MapWordToHalf(Value: UInt16): Half;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure HalfToSingle(HalfPtr,SinglePtr: Pointer);{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND} overload;
procedure SingleToHalf(SinglePtr,HalfPtr: Pointer);{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND} overload;

//------------------------------------------------------------------------------

Function HalfToSingle(Value: Half): Single;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND} overload;
Function SingleToHalf(Value: Single): Half;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND} overload;

//------------------------------------------------------------------------------
{
  Followng two functions are expecting pointers to packed vector or four
  singles (SinglePtr) and packed vector or four halfs (HalfPtr).
}
procedure HalfToSingle4x(HalfPtr,SinglePtr: Pointer);{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
procedure SingleToHalf4x(SinglePtr,HalfPtr: Pointer);{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{-------------------------------------------------------------------------------
================================================================================
                               Number information
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Number information - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Number information - number class
-------------------------------------------------------------------------------}

Function IsZero(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
Function IsDenormal(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
Function IsNaN(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
Function IsInfinite(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
Function IsNormal(const Value: Half): Boolean;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}  // returns false on zero

{-------------------------------------------------------------------------------
    Number information - sign-related
-------------------------------------------------------------------------------}
type
  TValueSign = -1..1;

Function Sign(const Value: Half): TValueSign;
Function Abs(const Value: Half): Half;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
Function Neg(const Value: Half): Half;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{-------------------------------------------------------------------------------
================================================================================
                              Comparison functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Comparison functions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Comparison functions - basic comparison
-------------------------------------------------------------------------------}

Function IsEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsLess(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsGreater(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsLessOrEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsGreaterOrEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Comparison functions - ordered comparison
-------------------------------------------------------------------------------}
type
  TValueRelationship = -1..1; // to preven problems (because delphi vs. FPC)

Function CompareValue(const A,B: Half; Epsilon: Half): TValueRelationship;{$IFDEF CanInline} inline;{$ENDIF} overload;
Function CompareValue(const A,B: Half): TValueRelationship;{$IFDEF CanInline} inline;{$ENDIF} overload;

Function SameValue(const A,B: Half; Epsilon: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF} overload;
Function SameValue(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF} overload;

{-------------------------------------------------------------------------------
================================================================================
                              Arithmetic functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Arithmetic functions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Arithmetic functions - basic arithmetic
-------------------------------------------------------------------------------}

Function Add(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function Subtract(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function Multiply(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function Divide(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                            Type half decode/encode
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Type half decode/encode - declaration
===============================================================================}
{
  DecodeFloat16
  DecodeHalf

  When BiasedExp is set to true, the returned exponent is exponent as it is
  stored in the value, that is, biased. When false, the returned exponent is
  unbiased (its true value).

  When IntBit is set to true, the returned mantissa contains the integer bit
  (bit 11) inferred from the number class (0 for denormals and zero,
  1 otherwise). When false, the integer bit is masked-out and is zero,
  irrespective of actual value.
}
procedure DecodeFloat16(const Value: Half; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
procedure DecodeHalf(const Value: Half; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}

{
  EncodeFloat16
  EncodeHalf

  When BiasedExp is true, it indicates that the passed exponent is already
  biased and will be stored as is. When false, the passed exponent will be
  biased before storing.

  Integer bit, when passed in the mantissa, is ignored - it is implied for
  half-precision float.
}
Function EncodeFloat16(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Half;
Function EncodeHalf(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Half;{$IFDEF CanInline} inline;{$ENDIF}


{$IFDEF FPC}
{-------------------------------------------------------------------------------
================================================================================
                              Operators overloading
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Operators overloading - declaration
===============================================================================}
{
  Operators overloading is currently implemented only for FPC.
}

// assignment operators
operator := (Value: Half): Single;{$IFDEF CanInline} inline;{$ENDIF}
operator := (Value: Single): Half;{$IFDEF CanInline} inline;{$ENDIF}

// explicit assignment operators
operator explicit (Value: Half): Single;{$IFDEF CanInline} inline;{$ENDIF}
operator explicit (Value: Single): Half;{$IFDEF CanInline} inline;{$ENDIF}

// comparison operators
operator = (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator > (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator < (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator >= (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator <= (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator <> (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

// unary operators
operator + (A: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator - (A: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

// arithmetic operators
operator + (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator - (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator * (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator / (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                         Unit implementation management
================================================================================
-------------------------------------------------------------------------------}
{
  WARNING - be wery careful when changing the selected implementation, as there
            is absolutely no thread-safety protection

  For full description of this section, please refer to the same section in
  BitOps library (github.com/TheLazyTomcat/Lib.BitOps), file BitOps.pas.
}

type
  TUIM_Float16Utils_Function = (fnMXCSRAccess,
                                fnHalfToSingle,fnSingleToHalf,
                                fnHalfToSingle4x,fnSingleToHalf4x);

  TUIM_Float16Utils_Implementation = (imNone,imPascal,imAssembly);

  TUIM_Float16Utils_Implementations = set of TUIM_Float16Utils_Implementation;

//------------------------------------------------------------------------------

{
  Returns which implementations are available for the selected function.
}
Function UIM_Float16Utils_AvailableFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;

{
  Returns which implementations are supported and can be safely selected for
  a given function.
}
Function UIM_Float16Utils_SupportedFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;

{
  Returns value indicating what implementation of the selected function is
  executed when calling the function.
}
Function UIM_Float16Utils_GetFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementation;

{
  Routes selected function to a selected implementation.

  Returned value is the previous routing.

  NOTE - when routing for fnMXCSRAccess, both GetMXCSR and SetMXCSR are set to
         the same implementation - sanity precaution, so the two functions do
         not operate on different domains

  NOTE - when asm implementation cannot be used, and you still select it,
         the function will be routed to pascal version

  WARNING - when selecting imNone as an implementation for some function, the
            routing is set to nil, and because the routing mechanism, for the
            sake of speed, does not check validity, it will result in an
            exception when calling this function

  WANRING - when selecting unsupported implementation, calling the function will
            almost certainly result in an system exception (invalid
            instruction).
}
Function UIM_Float16Utils_SetFuncImpl(Func: TUIM_Float16Utils_Function; NewImpl: TUIM_Float16Utils_Implementation): TUIM_Float16Utils_Implementation;

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
  F16_MASK_INTB = UInt16($0400);  // otherwise implicit integer bit of the mantissa

  F32_MASK_SIGN = UInt32($80000000);
  F32_MASK_EXP  = UInt32($7F800000);
  F32_MASK_FRAC = UInt32($007FFFFF);
{$IFNDEF FPC}
  F32_MASK_NSGN = UInt32($7FFFFFFF);
{$ENDIF}
  F32_MASK_FHB  = UInt32($00400000);
  F32_MASK_INTB = UInt32($00800000);

{===============================================================================
    Library-specific exceptions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Library-specific exceptions - floating-point exceptions
-------------------------------------------------------------------------------}

constructor EF16UFPUException.CreateDefMsg;
begin
Create(DefaultMessage);
end;

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual floating-point exception classes
-------------------------------------------------------------------------------}

Function EF16UInvalidOp.DefaultMessage: String;
begin
Result := 'Invalid floating point operand';
end;

//------------------------------------------------------------------------------

Function EF16UDenormal.DefaultMessage: String;
begin
Result := 'Denormal floating point operand';
end;

//------------------------------------------------------------------------------

Function EF16UDivByZero.DefaultMessage: String;
begin
Result := 'Floating point division by zero';
end;

//------------------------------------------------------------------------------

Function EF16UOverflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic overflow';
end;

//------------------------------------------------------------------------------

Function EF16UUnderflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic underflow';
end;

//------------------------------------------------------------------------------

Function EF16UPrecision.DefaultMessage: String;
begin
Result := 'Inexact floating point result';
end;

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

{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
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
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      If (PtrUInt(Buff) and PtrUInt($F)) = 0 then
        Mask := MXCSR_MASK_Load(Buff)
      else
        Mask := MXCSR_MASK_Load(Pointer((PtrUInt(Buff) + 16) and not PtrUInt($F)));
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
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
{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - SSE status and control register (MXCSR) access
-------------------------------------------------------------------------------}

threadvar
  Pas_MXCSR:  UInt32;
  MXCSRInit:  Boolean;  // compiler initializes this to false

//------------------------------------------------------------------------------

{$IFDEF F16U_ASM_IMPL}

Function Fce_GetMXCSR_Asm: UInt32; register; assembler;
var
  Temp: UInt32;
asm
    STMXCSR   dword ptr [Temp]
    MOV       EAX,  dword ptr [Temp]
  {$IFDEF x64}
    AND       EAX,  dword ptr [RIP + MXCSR_MASK]
  {$ELSE}
    AND       EAX,  dword ptr [MXCSR_MASK]
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Fce_SetMXCSR_Asm(NewValue: UInt32); register; assembler;
var
  Temp: UInt32;
asm
 {$IFDEF x64}
    AND       NewValue, dword ptr [RIP + MXCSR_MASK]
{$ELSE}
    AND       NewValue, dword ptr [MXCSR_MASK]
 {$ENDIF}
    MOV       dword ptr [Temp], NewValue
    LDMXCSR   dword ptr [Temp]
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function Fce_GetMXCSR_Pas: UInt32; register;
begin
If not MXCSRInit then
  begin
  {
    rounding set to nearest, masked precission, underflow and denormal
    exceptions, DAZ and FTZ flags are clear, exception states are all clear

    note - hardware initialization value is $00001F80
  }
    Pas_MXCSR := $00001900;
    MXCSRInit := True;
  end;
Result := Pas_MXCSR and MXCSR_MASK;
end;

//------------------------------------------------------------------------------

procedure Fce_SetMXCSR_Pas(NewValue: UInt32); register;
begin
Pas_MXCSR := NewValue and MXCSR_MASK;
MXCSRInit := True;
end;

//------------------------------------------------------------------------------

var
  Var_GetMXCSR: Function: UInt32; register;
  Var_SetMXCSR: procedure(NewValue: UInt32); register;

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Low-level access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}

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
  Result := UIM_Float16Utils_GetFuncImpl(fnMXCSRAccess) = imPascal
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
Result := (MXCSR_MASK and MXCSR_DenormalsAreZeros) <> 0;
end;

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Abstracted access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}

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

  procedure SetBit(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;

begin
Result := GetSSEExceptionMask(SSEException);
case SSEException of
  excInvalidOp: SetBit(MXCSR_EMASK_InvalidOP);
  excDenormal:  SetBit(MXCSR_EMASK_Denormal);
  excDivByZero: SetBit(MXCSR_EMASK_DivByZero);
  excOverflow:  SetBit(MXCSR_EMASK_Overflow);
  excUnderflow: SetBit(MXCSR_EMASK_Underflow);
  excPrecision: SetBit(MXCSR_EMASK_Precision);
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

  procedure SetBit(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
Result := GetSSEExceptionMasks;
MXCSR := GetMXCSR;
SetBit(MXCSR_EMASK_InvalidOP,excInvalidOp in NewValue);
SetBit(MXCSR_EMASK_Denormal,excDenormal in NewValue);
SetBit(MXCSR_EMASK_DivByZero,excDivByZero in NewValue);
SetBit(MXCSR_EMASK_Overflow,excOverflow in NewValue);
SetBit(MXCSR_EMASK_Underflow,excUnderflow in NewValue);
SetBit(MXCSR_EMASK_Precision,excPrecision in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionFlag(SSEException: TSSEException): Boolean;
begin
case SSEException of
  excInvalidOp: Result := (GetMXCSR and MXCSR_EFLAG_InvalidOP) <> 0;
  excDenormal:  Result := (GetMXCSR and MXCSR_EFLAG_Denormal) <> 0;
  excDivByZero: Result := (GetMXCSR and MXCSR_EFLAG_DivByZero) <> 0;
  excOverflow:  Result := (GetMXCSR and MXCSR_EFLAG_Overflow) <> 0;
  excUnderflow: Result := (GetMXCSR and MXCSR_EFLAG_Underflow) <> 0;
  excPrecision: Result := (GetMXCSR and MXCSR_EFLAG_Precision) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionFlag: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionFlag(SSEException: TSSEException; NewValue: Boolean): Boolean;

  procedure SetBit(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;

begin
Result := GetSSEExceptionFlag(SSEException);
case SSEException of
  excInvalidOp: SetBit(MXCSR_EFLAG_InvalidOP);
  excDenormal:  SetBit(MXCSR_EFLAG_Denormal);
  excDivByZero: SetBit(MXCSR_EFLAG_DivByZero);
  excOverflow:  SetBit(MXCSR_EFLAG_Overflow);
  excUnderflow: SetBit(MXCSR_EFLAG_Underflow);
  excPrecision: SetBit(MXCSR_EFLAG_Precision);
else
  raise EF16UInvalidFlag.CreateFmt('SetSSEExceptionFlag: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionFlags: TSSEExceptions;
var
  MXCSR:  UInt32;
  i:      TSSEException;
begin
Result := [];
MXCSR := GetMXCSR;
For i := Low(TSSEException) to High(TSSEException) do
  case i of
    excInvalidOp: If (MXCSR and MXCSR_EFLAG_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (MXCSR and MXCSR_EFLAG_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (MXCSR and MXCSR_EFLAG_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (MXCSR and MXCSR_EFLAG_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (MXCSR and MXCSR_EFLAG_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (MXCSR and MXCSR_EFLAG_Precision) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionFlags: Invalid SSE exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionFlags(NewValue: TSSEExceptions): TSSEExceptions;
var
  MXCSR:  UInt32;

  procedure SetBit(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
Result := GetSSEExceptionFlags;
MXCSR := GetMXCSR;
SetBit(MXCSR_EFLAG_InvalidOP,excInvalidOp in NewValue);
SetBit(MXCSR_EFLAG_Denormal,excDenormal in NewValue);
SetBit(MXCSR_EFLAG_DivByZero,excDivByZero in NewValue);
SetBit(MXCSR_EFLAG_Overflow,excOverflow in NewValue);
SetBit(MXCSR_EFLAG_Underflow,excUnderflow in NewValue);
SetBit(MXCSR_EFLAG_Precision,excPrecision in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

Function GetSSEFlag(Flag: TSSEFlag): Boolean;
begin
case Flag of
  flDenormalsAreZeros:  Result := (GetMXCSR and MXCSR_DenormalsAreZeros) <> 0;
  flFlushToZero:        Result := (GetMXCSR and MXCSR_FlushToZero) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEFlag(Flag: TSSEFlag; NewValue: Boolean): Boolean;

  procedure SetBit(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;
  
begin
Result := GetSSEFlag(Flag);
case Flag of
  flDenormalsAreZeros:  SetBit(MXCSR_DenormalsAreZeros);
  flFlushToZero:        SetBit(MXCSR_FlushToZero);
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
    flDenormalsAreZeros:  If (MXCSR and MXCSR_DenormalsAreZeros) <> 0 then Include(Result,i);
    flFlushToZero:        If (MXCSR and MXCSR_FlushToZero) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetX87Flags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

procedure SetSSEFlags(NewValue: TSSEFlags);
var
  MXCSR:  UInt32;

  procedure SetBit(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
MXCSR := GetMXCSR;
SetBit(MXCSR_DenormalsAreZeros,flDenormalsAreZeros in NewValue);
SetBit(MXCSR_FlushToZero,flFlushToZero in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

procedure ClearSSEExceptions;
begin
SetMXCSR(GetMXCSR and $FFFFFFC0);
end;

//------------------------------------------------------------------------------

procedure RaiseSSEExceptions(var MXCSR: UInt32);
begin
If (MXCSR and MXCSR_EFLAG_InvalidOP) <> 0 then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_InvalidOP;
    raise EF16UInvalidOp.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Denormal) <> 0 then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Denormal;
    raise EF16UDenormal.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_DivByZero) <> 0 then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_DivByZero;
    raise EF16UDivByZero.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Overflow) <> 0 then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Overflow;
    raise EF16UOverflow.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Underflow) <> 0 then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Underflow;
    raise EF16UUnderflow.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Precision) <> 0 then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Precision;
    raise EF16UPrecision.CreateDefMsg;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RaiseSSEExceptions;
var
  MXCSR:  UInt32;
begin
MXCSR := GetMXCSR;
If (MXCSR and MXCSR_EFLAG_InvalidOP) <> 0 then
  begin
    SetMXCSR(MXCSR and not MXCSR_EFLAG_InvalidOP);
    raise EF16UInvalidOp.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Denormal) <> 0 then
  begin
    SetMXCSR(MXCSR and not MXCSR_EFLAG_Denormal);
    raise EF16UDenormal.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_DivByZero) <> 0 then
  begin
    SetMXCSR(MXCSR and not MXCSR_EFLAG_DivByZero);
    raise EF16UDivByZero.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Overflow) <> 0 then
  begin
    SetMXCSR(MXCSR and not MXCSR_EFLAG_Overflow);
    raise EF16UOverflow.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Underflow) <> 0 then
  begin
    SetMXCSR(MXCSR and not MXCSR_EFLAG_Underflow);
    raise EF16UUnderflow.CreateDefMsg;
  end;
If (MXCSR and MXCSR_EFLAG_Precision) <> 0 then
  begin
    SetMXCSR(MXCSR and not MXCSR_EFLAG_Precision);
    raise EF16UPrecision.CreateDefMsg;
  end;
end;

{-------------------------------------------------------------------------------
    Auxiliary routines - conversion functions
-------------------------------------------------------------------------------}

procedure Fce_HalfToSingle_Pas(HalfPtr,SinglePtr: Pointer); register;
{$IFDEF H2S_Lookup}
  {$INCLUDE '.\Float16Utils.inc'}
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

  Function HighZeroCount(Value: UInt16): Integer;
  begin
    If Value <> 0 then
      begin
        Result := 0;
        while (Value and UInt16($8000)) = 0  do
          begin
            Value := UInt16(Value shl 1);
            Inc(Result);
          end;
      end
    else Result := 16;
  end;

begin
Sign := PUInt16(HalfPtr)^ and F16_MASK_SIGN;
Exponent := Int32((PUInt16(HalfPtr)^ and F16_MASK_EXP) shr 10);
Mantissa := PUInt16(HalfPtr)^ and F16_MASK_FRAC;
case Exponent of

        // zero exponent - zero or denormal
    0:  If Mantissa <> 0 then
          begin
            // denormal
            If not GetSSEFlag(flDenormalsAreZeros) then
              begin
                // DAZ mode inactive
                If GetSSEExceptionMask(excDenormal) then
                  begin
                    {$message 'P359 - half denormals not signaled?'}
                    SetSSEExceptionFlag(excDenormal,True);
                  {
                    normalize...

                    ...shift mantissa left so that its highest set bit will be
                    shifted to implicit integer bit (bit 23), also correct
                    exponent to reflect this change
                  }
                    MantissaShift := HighZeroCount(Mantissa) + 8;
                    PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                                           UInt32(UInt32(126 - MantissaShift) shl 23) or
                                          (UInt32(UInt32(Mantissa) shl MantissaShift) and F32_MASK_FRAC);
                  end
                else raise EF16UDenormal.CreateDefMsg;
              end
            // DAZ mode - return signed zero
            else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16);
          end
        // return signed zero
        else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16);

        // max exponent - infinity or NaN
  $1F:  If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and F16_MASK_FHB) = 0 then
              begin
                // signaled NaN
                If GetSSEExceptionMask(excInvalidOp) then
                  begin
                    SetSSEExceptionFlag(excInvalidOp,True);
                    // quiet signed NaN with mantissa
                    PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP or
                                           F32_MASK_FHB or UInt32(UInt32(Mantissa) shl 13)
                  end
                // signaling NaN
                else raise EF16UInvalidOp.CreateDefMsg;
              end
            // quiet signed NaN with mantissa
            else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP or
                                        UInt32(UInt32(Mantissa) shl 13);
          end
        // return signed infinity
        else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP;
        
else
  // normal number
  PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                         UInt32(UInt32(Exponent + 112{127 - 15}) shl 23) or
                         UInt32(UInt32(Mantissa) shl 13);
end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Pas(SinglePtr,HalfPtr: Pointer); register;
var
  Sign:         UInt32;
  Exponent:     Int32;  // biased exponent (true exponent + 127)
  Mantissa:     UInt32;
  RoundMode:    TSSERoundingMode;
  ResultTemp:   UInt16;
  BitsLost:     Boolean;
  ManOverflow:  Boolean;

  Function ShiftMantissa(Value: UInt32; Shift: Byte; out DataLoss: Boolean): UInt32;
  var
    Mask:     UInt32;
    Low,High: UInt32;
  begin
    DataLoss := False;
    If (Shift > 0) and (Shift < 25) then
      begin
        Mask := UInt32($FFFFFFFF) shr (32 - Shift);
        If (Value and Mask) <> 0 then
          begin
            DataLoss := True;
            Low := Value and not Mask;
            High := Low + (Mask + 1);
            case GetSSERoundingMode of
              rmDown:     If Sign <> 0 then
                            Result := High shr Shift
                          else
                            Result := Low shr Shift;
              rmUp:       If Sign <> 0 then
                            Result := Low shr Shift
                          else
                            Result := High shr Shift;
              rmTruncate: Result := Low shr Shift;
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

  procedure ExceptionSetOrRaise(SSEException: TSSEException);
  begin
    If not GetSSEExceptionMask(SSEException) then
      case SSEException of
        excDenormal:  raise EF16UDenormal.CreateDefMsg;
        excDivByZero: raise EF16UDivByZero.CreateDefMsg;
        excOverflow:  raise EF16UOverflow.CreateDefMsg;
        excUnderflow: raise EF16UUnderflow.CreateDefMsg;
        excPrecision: raise EF16UPrecision.CreateDefMsg;
      else
       {excInvalidOp}
        raise EF16UInvalidOP.CreateDefMsg;
      end
    else SetSSEExceptionFlag(SSEException,True);
  end;

begin
RoundMode := GetSSERoundingMode;
Sign := PUInt32(SinglePtr)^ and F32_MASK_SIGN;
Exponent := (PUInt32(SinglePtr)^ and F32_MASK_EXP) shr 23;
Mantissa := PUInt32(SinglePtr)^ and F32_MASK_FRAC;
case Exponent of

        // exponent of zero - zero or denormal
    0:  If Mantissa <> 0 then
          begin
            // non-zero mantissa - denormals
            If not GetSSEFlag(flDenormalsAreZeros) then
              begin
                // DAZ mode inactive
                If GetSSEExceptionMask(excDenormal) then
                  begin
                    SetSSEExceptionFlag(excDenormal,True);
                    If ((RoundMode = rmUp) and (Sign = 0)) or
                       ((RoundMode = rmDown) and (Sign <> 0)) then
                      // return signed smallest representable number
                      ResultTemp := UInt16(Sign shr 16) or UInt16(1)
                    else
                      // convert to signed zero
                      ResultTemp := UInt16(Sign shr 16);
                    // post-computation exceptions, FTZ check
                    {$message 'P359 - FTZ ignored for halfs?'}
                    If GetSSEFlag(flFlushToZero) and GetSSEExceptionMask(excUnderflow) then
                      begin
                        // FTZ mode active - return signed zero
                        SetSSEExceptionFlag(excUnderflow,True);
                        SetSSEExceptionFlag(excPrecision,True);
                        PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
                      end
                    else
                      begin
                        // FTZ mode inactive - normal processing
                        ExceptionSetOrRaise(excUnderflow);
                        ExceptionSetOrRaise(excPrecision);
                        PUInt16(HalfPtr)^ := ResultTemp;
                        {$message 'P359 - pe set for unmasked ue?'}
                      end;
                  end
                else raise EF16UDenormal.CreateDefMsg;
              end
            // DAZ mode - return signed zero
            else PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
          end
        // mantissa of 0 - return signed zero
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16);

      {
        exponent 1..101 (-126..-26 unbiased) - exponent too small to be
        represented in half even as denormal
      }
   1..
  $65:  begin
          If ((RoundMode = rmUp) and (Sign = 0)) or
             ((RoundMode = rmDown) and (Sign <> 0)) then
            // return signed smallest representable number
            ResultTemp := UInt16(Sign shr 16) or UInt16(1)
          else
            // convert to signed zero
            ResultTemp := UInt16(Sign shr 16);
          {$message 'P359 - FTZ ignored for halfs?'}
          // post-computation exceptions, FTZ check
          If GetSSEFlag(flFlushToZero) and GetSSEExceptionMask(excUnderflow) then
            begin
              // FTZ mode active - return signed zero
              SetSSEExceptionFlag(excUnderflow,True);
              SetSSEExceptionFlag(excPrecision,True);
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
            end
          else
            begin
              // FTZ mode inactive - normal processing
              ExceptionSetOrRaise(excUnderflow);
              ExceptionSetOrRaise(excPrecision);
              PUInt16(HalfPtr)^ := ResultTemp;
            end;
        end;

      {
        exponent 102..112 (-25..-15 unbiased) - exponent still too small to be
        represented in half, but the result can be denormalized (implicit
        exponent of -14, explicit 0)
      }
  $66..
  $70:  begin
        {
          denormalize

          Note that mantissa can overflow into exponent, this is normal and
          expected.
        }
          ResultTemp := UInt16(Sign shr 16) or UInt16(ShiftMantissa(Mantissa or F32_MASK_INTB,$7E - Exponent,BitsLost));
          // post-computation exceptions
          If GetSSEExceptionMask(excUnderflow) then
            begin
              // underflow exception masked
              If BitsLost then
                begin
                  // inexact result
                  If (ResultTemp and F16_MASK_EXP) = 0 then
                    begin
                      // result is denormal
                      If GetSSEFlag(flFlushToZero) then
                        // FTZ mode active - flush to signed zero
                        ResultTemp := UInt16(Sign shr 16);
                      ExceptionSetOrRaise(excUnderflow);
                    end;
                  ExceptionSetOrRaise(excPrecision);
                  PUInt16(HalfPtr)^ := ResultTemp;
                end
              else PUInt16(HalfPtr)^ := ResultTemp;
            end
          else
            begin
              // underflow exception not masked
              If (ResultTemp and F16_MASK_EXP) = 0 then
                raise EF16UUnderflow.CreateDefMsg;
              If BitsLost then
                // inexact result
                ExceptionSetOrRaise(excPrecision);
              PUInt16(HalfPtr)^ := ResultTemp;
            end;
        end;

      {
        exponent 143..254 (+16..+127 unbiased) - too large to be represented
        in half (resulting exponent would be larger than 15)
      }
  $8F..
  $FE:  begin
          If (RoundMode = rmTruncate) or
             ((RoundMode = rmUp) and (Sign <> 0)) or
             ((RoundMode = rmDown) and (Sign = 0)) then
            // return signed largest representable number
            ResultTemp:= UInt16(Sign shr 16) or UInt16($7BFF)
          else
            // convert to signed infinity
            ResultTemp := UInt16(Sign shr 16) or F16_MASK_EXP;
          // post-computation exceptions
          ExceptionSetOrRaise(excOverflow);
          ExceptionSetOrRaise(excPrecision);
          PUInt16(HalfPtr)^ := ResultTemp;
        end;

        // max exponent - infinity or NaN
  $FF:  If Mantissa <> 0 then
          begin
            // not a number (NaN)
            If (Mantissa and F32_MASK_FHB) = 0 then
              begin
                // signaling NaN
                If GetSSEExceptionMask(excInvalidOP) then
                  begin
                    // return quiet signed NaN with truncated mantissa
                    SetSSEExceptionFlag(excInvalidOP,True);
                    PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP or F16_MASK_FHB or
                                         UInt16(Mantissa shr 13);
                  end
                // singal NaN
                else raise EF16UInvalidOP.CreateDefMsg;
              end
            // quiet signed NaN with truncated mantisssa
            else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP or
                                      UInt16(Mantissa shr 13);
          end
        // return signed infinity
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP;

else
  // exponent 113..142 (-14..+15 unbiased) - representable numbers, normalized value
  Mantissa := ShiftMantissa(Mantissa,13,BitsLost);
  // check if mantisa overflowed - if so, increase exponent to compensate
  If Mantissa > F16_MASK_FRAC then
    begin
      Inc(Exponent);
      ManOverflow := True;
    end
  else ManOverflow := False;
  ResultTemp := UInt16(Sign shr 16) or
                UInt16((Exponent - 112) shl 10) or
                UInt16(Mantissa and F16_MASK_FRAC);
  // post-computation exceptions
  If ManOverflow and (Exponent > 142) then
    // number was converted to infinity
    ExceptionSetOrRaise(excOverflow);
  If BitsLost then
    ExceptionSetOrRaise(excPrecision);
  PUInt16(HalfPtr)^ := ResultTemp;
end;
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
  {$PUSH}
  {$WARN 2087 OFF}  //  Suppresses warnings on following $WARN
  {$WARN 7121 OFF}  //  Warning: Check size of memory operand "op: memory-operand-size is X bits, but expected [Y bits]"
{$ENDIF}

procedure Fce_HalfToSingle_Asm(HalfPtr,SinglePtr: Pointer); register; assembler;
asm
    MOVZX   EAX,  word ptr [HalfPtr]
    MOVD    XMM0, EAX

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVSS   dword ptr [SinglePtr], XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Asm(SinglePtr,HalfPtr: Pointer); register; assembler;
asm
    MOVSS   XMM0, dword ptr [SinglePtr]

    // $04 - rounding selected in MXCSR is used
    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVD    EAX,  XMM0
    MOV     word ptr [HalfPtr],  AX
end;

//------------------------------------------------------------------------------

procedure Fce_HalfToSingle4x_Asm(HalfPtr,SinglePtr: Pointer); register; assembler;
asm
    MOVSD   XMM0, qword ptr [HalfPtr]

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVUPS  dqword ptr [SinglePtr],  XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingletoHalf4x_Asm(SinglePtr,HalfPtr: Pointer); register; assembler;
asm
    MOVUPS  XMM0, dqword ptr [SinglePtr]

    // $04 - rounding selected in MXCSR is used
    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVSD   qword ptr [HalfPtr],   XMM0
end;

{$IFDEF ASMSuppressSizeWarnings}
  {$POP}
{$ENDIF}

{$ENDIF}

//==============================================================================

var
  Var_HalfToSingle:   procedure(HalfPtr,SinglePtr: Pointer); register;
  Var_SingleToHalf:   procedure(SinglePtr,HalfPtr: Pointer); register;
  Var_HalfToSingle4x: procedure(HalfPtr,SinglePtr: Pointer); register;
  Var_SingleToHalf4x: procedure(SinglePtr,HalfPtr: Pointer); register;

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

procedure HalfToSingle(HalfPtr,SinglePtr: Pointer);
begin
Var_HalfToSingle(HalfPtr,SinglePtr);
end;

//------------------------------------------------------------------------------

procedure SingleToHalf(SinglePtr,HalfPtr: Pointer);
begin
Var_SingleToHalf(SinglePtr,HalfPtr);
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

procedure HalfToSingle4x(HalfPtr,SinglePtr: Pointer);
begin
Var_HalfToSingle4x(HalfPtr,SinglePtr);
end;

//------------------------------------------------------------------------------

procedure SingleToHalf4x(SinglePtr,HalfPtr: Pointer);
begin
Var_SingleToHalf4x(SinglePtr,HalfPtr);
end;


{-------------------------------------------------------------------------------
================================================================================
                               Number information
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Number information - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Number information - number class
-------------------------------------------------------------------------------}

Function IsZero(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// bits other than sign are zero
Result := _Value and F16_MASK_NSGN = 0;
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// zero exponent, non-zero mantissa
Result := ((_Value and F16_MASK_EXP) = 0) and ((_Value and F16_MASK_FRAC) <> 0);
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
// non-zero less than max exponent, any mantissa
Exponent := (_Value and F16_MASK_EXP) shr 10;
Result := (Exponent > 0) and (Exponent < $1F);
end;

{-------------------------------------------------------------------------------
    Number information - sign-related
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
================================================================================
                              Comparison functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Comparison functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Comparison functions - basic comparison
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

{-------------------------------------------------------------------------------
    Comparison functions - ordered comparison
-------------------------------------------------------------------------------}

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
================================================================================
                              Arithmetic functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Arithmetic functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Arithmetic functions - basic arithmetic
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


{-------------------------------------------------------------------------------
================================================================================
                            Type half decode/encode
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Type half decode/encode - declaration
===============================================================================}

procedure DecodeFloat16(const Value: Half; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
var
  _Value: UInt16 absolute Value;
begin
Sign := (_Value and F16_MASK_SIGN) <> 0;
If BiasedExp then
  Exponent := (_Value and F16_MASK_EXP) shr 10
else
  Exponent := ((_Value and F16_MASK_EXP) shr 10) - FLOAT16_EXPONENTBIAS;
If IntBit then
  begin
    If IsDenormal(Value) or IsZero(Value) then
      Mantissa := _Value and F16_MASK_FRAC
    else
      Mantissa := (_Value and F16_MASK_FRAC) or F16_MASK_INTB;
  end
else Mantissa := _Value and F16_MASK_FRAC;
end;

//------------------------------------------------------------------------------

procedure DecodeHalf(const Value: Half; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat16(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

//==============================================================================

Function EncodeFloat16(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Half;
var
  _Result:  UInt16 absolute Result;
begin
_Result := Mantissa and F16_MASK_FRAC;
If BiasedExp then
  _Result := _Result or ((Exponent shl 10) and F16_MASK_EXP)
else
  _Result := _Result or (((Exponent + FLOAT16_EXPONENTBIAS) shl 10) and F16_MASK_EXP); 
If Sign then
  _Result := _Result or F16_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function EncodeHalf(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Half;
begin
Result := EncodeFloat16(Mantissa,Exponent,Sign,BiasedExp);
end;


{$IFDEF FPC}
{-------------------------------------------------------------------------------
================================================================================
                              Operators overloading
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Operators overloading - implementation
===============================================================================}

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

{-------------------------------------------------------------------------------
================================================================================
                         Unit implementation management
================================================================================
-------------------------------------------------------------------------------}
const
  UIM_FLOAT16UTILS_PASCAL_IMPL: array[TUIM_Float16Utils_Function] of Pointer = (
    @Fce_SetMXCSR_Pas,
    @Fce_HalfToSingle_Pas,@Fce_SingleToHalf_Pas,
    @Fce_HalfToSingle4x_Pas,@Fce_SingleToHalf4x_Pas);

{$IFDEF F16U_ASM_IMPL}
  UIM_FLOAT16UTILS_ASSEMBLY_IMPL: array[TUIM_Float16Utils_Function] of Pointer = (
    @Fce_SetMXCSR_Asm,
    @Fce_HalfToSingle_Asm,@Fce_SingleToHalf_Asm,
    @Fce_HalfToSingle4x_Asm,@Fce_SingleToHalf4x_Asm);
{$ENDIF}

//------------------------------------------------------------------------------

Function UIM_GetFunctionVarAddr(Func: TUIM_Float16Utils_Function): PPointer;
begin
case Func of
  fnMXCSRAccess:    Result := Addr(@Var_SetMXCSR);
  fnHalfToSingle:   Result := Addr(@Var_HalfToSingle);
  fnSingleToHalf:   Result := Addr(@Var_SingleToHalf);
  fnHalfToSingle4x: Result := Addr(@Var_HalfToSingle4x);
  fnSingleToHalf4x: Result := Addr(@Var_SingleToHalf4x);
else
  raise EF16UUnknownFunction.CreateFmt('UIM_GetFunctionVarAddr: Unknown function %d.',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function UIM_CheckASMSupport(Func: TUIM_Float16Utils_Function): Boolean;
begin
Result := False;
{$IFDEF F16U_ASM_IMPL}
with TSimpleCPUID.Create do
try
  case Func of
    fnMXCSRAccess,
    fnHalfToSingle,fnSingleToHalf,
    fnHalfToSingle4x,fnSingleToHalf4x:
      // SSE2 for MOVD instruction
      Result := Info.SupportedExtensions.F16C and Info.SupportedExtensions.SSE2;
  else
    raise EF16UUnknownFunction.CreateFmt('UIM_CheckASMSupport: Unknown function (%d).',[Ord(Func)]);
  end;
finally
  Free;
end;
{$ENDIF}
end;
{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//==============================================================================

Function UIM_Float16Utils_AvailableFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;
begin
case Func of
  fnMXCSRAccess,
  fnHalfToSingle,fnSingleToHalf,
  fnHalfToSingle4x,fnSingleToHalf4x:
    Result := [imNone,imPascal{$IFDEF F16U_ASM_IMPL},imAssembly{$ENDIF}];
else
  raise EF16UUnknownFunction.CreateFmt('UIM_Float16Utils_AvailableFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_Float16Utils_SupportedFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;
begin
case Func of
  fnMXCSRAccess,
  fnHalfToSingle,fnSingleToHalf,
  fnHalfToSingle4x,fnSingleToHalf4x:
    If UIM_CheckASMSupport(Func) then
      Result := [imNone,imPascal,imAssembly]
    else
      Result := [imNone,imPascal];
else
  raise EF16UUnknownFunction.CreateFmt('UIM_Float16Utils_SupportedFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_Float16Utils_GetFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementation;
var
  FuncVarAddr:  PPointer;
begin
Result := imNone;
FuncVarAddr := UIM_GetFunctionVarAddr(Func);
// no need to check FuncVarAddr for validity
If Assigned(FuncVarAddr^) then
  begin
    If FuncVarAddr^ = UIM_FLOAT16UTILS_PASCAL_IMPL[Func] then
      Result := imPascal
  {$IFDEF F16U_ASM_IMPL}
    else If FuncVarAddr^ = UIM_FLOAT16UTILS_ASSEMBLY_IMPL[Func] then
      Result := imAssembly
  {$ENDIF};
  end;
end;

//------------------------------------------------------------------------------

Function UIM_Float16Utils_SetFuncImpl(Func: TUIM_Float16Utils_Function; NewImpl: TUIM_Float16Utils_Implementation): TUIM_Float16Utils_Implementation;
begin
Result := UIM_Float16Utils_GetFuncImpl(Func);
If Func = fnMXCSRAccess then
  begin
    case NewImpl of
      imPascal:   begin
                    Var_GetMXCSR := Fce_GetMXCSR_Pas;
                    Var_SetMXCSR := Fce_SetMXCSR_Pas;
                  end;
    {$IFDEF F16U_ASM_IMPL}
      imAssembly: begin
                    Var_GetMXCSR := Fce_GetMXCSR_Asm;
                    Var_SetMXCSR := Fce_SetMXCSR_Asm;
                  end;
    {$ELSE}
      imAssembly: begin
                    Var_GetMXCSR := Fce_GetMXCSR_Pas;
                    Var_SetMXCSR := Fce_SetMXCSR_Pas;
                  end;
    {$ENDIF}
    else
     {imNone}
      Var_GetMXCSR := nil;
      Var_SetMXCSR := nil;
    end;
  end
else
  begin
    case NewImpl of
      imPascal:   UIM_GetFunctionVarAddr(Func)^ := UIM_FLOAT16UTILS_PASCAL_IMPL[Func];
    {$IFDEF F16U_ASM_IMPL}
      imAssembly: UIM_GetFunctionVarAddr(Func)^ := UIM_FLOAT16UTILS_ASSEMBLY_IMPL[Func];
    {$ELSE}
      imAssembly: UIM_GetFunctionVarAddr(Func)^ := UIM_FLOAT16UTILS_PASCAL_IMPL[Func];
    {$ENDIF}
    else
     {imNone}
      UIM_GetFunctionVarAddr(Func)^ := nil;
    end;
  end;
end;

{-------------------------------------------------------------------------------
================================================================================
                               Unit initialization
================================================================================
-------------------------------------------------------------------------------}

procedure Initialize;
var
  i:  TUIM_Float16Utils_Function;
begin
MXCSR_MASK_Init(not UIM_CheckASMSupport(fnMXCSRAccess));
For i := Low(TUIM_Float16Utils_Function) to High(TUIM_Float16Utils_Function) do
  If UIM_CheckASMSupport(i) then
    UIM_Float16Utils_SetFuncImpl(i,imAssembly)
  else
    UIM_Float16Utils_SetFuncImpl(i,imPascal);
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

end.