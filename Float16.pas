{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Float16

  Main purpose of this library is to provide routines for conversion from and to
  half precision (16 bit) floating point number (Single -> Half, Half -> Single).
  It also provides functions for basic arithmetic and comparison, as well as
  overloaded operators when compiled using FPC.
  F16C extension is used when symbol AllowF16CExtension is defined, PurePascal
  is not defined, and when (and only when) it is supported by the CPU and OS.

  NOTE - type Half is declared in unit AuxTypes, not here.

  ©František Milt 2017-06-10

  Version 1.0

  Dependencies:
    AuxTypes    - github.com/ncs-sniper/Lib.AuxTypes
  * SimpleCPUID - github.com/ncs-sniper/Lib.SimpleCPUID

  SimpleCPUID is required only when AllowF16CExtension symbol is defined and
  PurePascal symbol is not defined.

===============================================================================}
unit Float16;

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}{$MODESWITCH CLASSICPROCVARS+}
  {$INLINE ON}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
    {$DEFINE ASMSuppressSizeWarnings}
  {$ENDIF}
{$ENDIF}

{
  AllowF16CExtension

  When defined, allows the use of F16C extension in ASM. The extension is used
  only when both CPU and OS supports it, otherwise pascal implementation is
  called instead.
  Has no meaning when PurePascal symbol is defined.
}
{$DEFINE AllowF16CExtension}

{
  H2S_Lookup

  When defined, pascal implementation of Half to Single conversion is done using
  large lookup table.
  This is faster than procedural conversion, but inclusion of the table
  increases size of the resulting binary by 256KiB and prevents raising of an
  exception on signaling NaN (it is instead converted to quiet NaN).

  Not defined by default.
}
{.$DEFINE H2S_Lookup}

interface

uses
  AuxTypes {contains declaration of type Half};
  
//==  Public constants  ========================================================
//------------------------------------------------------------------------------

const
  Infinity: Half = ($00,$7C); // positive infinity
  NaN:      Half = ($00,$7E); // quiet NaN
  MaxHalf:  Half = ($FF,$7B); // 65504
  MinHalf:  Half = ($01,$00); // 5.96046e-8

//==  Auxiliary functions  =====================================================
//------------------------------------------------------------------------------

{$IF not Declared(GetMXCSR)}
{$DEFINE Implement_GetMXCSR}
Function GetMXCSR: UInt32; {$IFNDEF PurePascal}register; assembler;{$ENDIF}
{$IFEND}

{$IF not Declared(SetMXCSR)}
{$DEFINE Implement_SetMXCSR}
procedure SetMXCSR(NewValue: UInt32); {$IFNDEF PurePascal}register; assembler;{$ENDIF}
{$IFEND}

//==  Conversion functions  ====================================================
//------------------------------------------------------------------------------

Function MapHalfToWord(Value: Half): UInt16;
Function MapWordToHalf(Value: UInt16): Half;

Function HalfToSingle(Value: Half): Single;
Function SingleToHalf(Value: Single): Half;

procedure HalfToSingle4x(Input, Output: Pointer);
procedure SingleToHalf4x(Input, Output: Pointer);

//==  Number information functions  ============================================
//------------------------------------------------------------------------------

Function IsZero(const Value: Half): Boolean;
Function IsNaN(const Value: Half): Boolean;
Function IsInfinite(const Value: Half): Boolean;

//==  Sign-related functions  ==================================================
//------------------------------------------------------------------------------

type
  TValueSign = -1..1;

Function Sign(const Value: Half): TValueSign;
Function Abs(const Value: Half): Half;
Function Minus(const Value: Half): Half;

//==  Comparison functions  ====================================================
//------------------------------------------------------------------------------

Function IsEqual(const A,B: Half): Boolean;
Function IsLess(const A,B: Half): Boolean;
Function IsGreater(const A,B: Half): Boolean;
Function IsLessOrEqual(const A,B: Half): Boolean;
Function IsGreaterOrEqual(const A,B: Half): Boolean;

//==  Arithmetic functions  ====================================================
//------------------------------------------------------------------------------

Function Add(const A,B: Half): Half;
Function Substract(const A,B: Half): Half;
Function Multiply(const A,B: Half): Half;
Function Divide(const A,B: Half): Half;

//==  Operators overloading  ===================================================
//------------------------------------------------------------------------------

{$IFDEF FPC}

// assignment operators
operator := (Value: Half): Single; inline;
operator := (Value: Single): Half; inline;

// explicit assignment operators
operator explicit (Value: Half): Single; inline;
operator explicit (Value: Single): Half; inline;

// comparison operators
operator = (A,B: Half): Boolean; inline;
operator > (A,B: Half): Boolean; inline;
operator < (A,B: Half): Boolean; inline;
operator >= (A,B: Half): Boolean; inline;
operator <= (A,B: Half): Boolean; inline;
operator <> (A,B: Half): Boolean; inline;

// arithmetic operators
operator + (A,B: Half): Half; inline;
operator - (A,B: Half): Half; inline;
operator * (A,B: Half): Half; inline;
operator / (A,B: Half): Half; inline;

{$ENDIF}

implementation

uses
  SysUtils
{$IF Defined(AllowF16CExtension) and not Defined(PurePascal)}
  , SimpleCPUID
{$IFEND};

//==  Auxiliary functions  =====================================================
//------------------------------------------------------------------------------

{$IF (Defined(Implement_GetMXCSR) or Defined(Implement_SetMXCSR)) and Defined(PurePascal)}
var
  // rounding set to nearest; masked precission, underflow and denormal exceptions
  Pas_MXCSR: UInt32 = $00001900;
{$IFEND}

//------------------------------------------------------------------------------

{$IFDEF Implement_GetMXCSR}
Function GetMXCSR: UInt32; {$IFNDEF PurePascal}register; assembler;
var
  Temp: UInt32;
asm
    STMXCSR   dword ptr [Temp]
    MOV       EAX,  dword ptr [Temp]
end;
{$ELSE}
begin
Result := Pas_MXCSR;
end;
{$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF Implement_SetMXCSR}
procedure SetMXCSR(NewValue: UInt32); {$IFNDEF PurePascal}register; assembler;
var
  Temp: UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV       dword ptr [Temp], ECX
  {$ELSE}
    MOV       dword ptr [Temp], EDI
  {$ENDIF}
{$ELSE}
    MOV       dword ptr [Temp], EAX
{$ENDIF}
    LDMXCSR   dword ptr [Temp]
end;
{$ELSE}
begin
Pas_MXCSR := NewValue;
end;
{$ENDIF}
{$ENDIF}

//==  Conversion functions  ====================================================
//------------------------------------------------------------------------------

const
  MXCSR_EInvalidOP = UInt32($00000080);
  MXCSR_EOverflow  = UInt32($00000400);
  MXCSR_EUnderflow = UInt32($00000800);

//------------------------------------------------------------------------------

procedure Fce_HalfToSingle_Pas(HalfPtr, SinglePtr: Pointer); register;
{$IFDEF H2S_Lookup}
{$DEFINE Included}
  {$INCLUDE '.\Float16_H2S_Lookup.inc'}
{$UNDEF Included}
begin
PUInt32(SinglePtr)^ := H2S_Lookup[PUInt16(HalfPtr)^];
end;
{$ELSE}
var
  Sign:           UInt16;
  Exponent:       Int32;
  Mantissa:       UInt16;
  MantissaShift:  Integer;
  MXCSR:          UInt32;

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
MXCSR := GetMXCSR;
Sign := PUInt16(HalfPtr)^ and $8000;
Exponent := Int32(PUInt16(HalfPtr)^ shr 10) and $1F;
Mantissa := PUInt16(HalfPtr)^ and $3FF;
case Exponent of
        // zero or subnormal
    0:  If Mantissa <> 0 then
          begin
            // subnormals, normalizing
            MantissaShift := HighZeroCount(Mantissa) + 8;
            PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                                   UInt32(UInt32(Exponent - MantissaShift + 126) shl 23) or
                                   (UInt32(UInt32(Mantissa) shl MantissaShift) and UInt32($007FFFFF));
          end
        // return signed zero
        else PUInt32(SinglePtr)^ := UInt32(Sign shl 16);

        // infinity or NaN
  $1F:  If Mantissa <> 0 then
          begin
            If (Mantissa and UInt16($0200)) = 0 then
              begin
                // signaled NaN
                If (MXCSR and MXCSR_EInvalidOP) <> 0 then
                  // quiet signed NaN with mantissa
                  PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or UInt32($7FC00000) or
                                      UInt32(UInt32(Mantissa) shl 13)
                else
                  // signaling NaN
                  raise EInvalidOp.Create('Invalid floating point operation');
              end
            // quiet signed NaN with mantissa
            else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or UInt32($7F800000) or
                                     UInt32(UInt32(Mantissa) shl 13);
          end
        // signed infinity
        else PUInt32(SinglePtr)^ := UInt32(Sign shl 16) or UInt32($7F800000);
else
  // normal number
  PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                         UInt32(UInt32(Exponent + 112) shl 23) or
                         UInt32(UInt32(Mantissa) shl 13);
end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Pas(SinglePtr, HalfPtr: Pointer); register;
var
  Sign:       UInt32;
  Exponent:   Int32;
  Mantissa:   UInt32;
  MXCSR:      UInt32;
  RoundMode:  Integer;

  Function ShiftMantissa(Value: UInt32; Shift: Byte): UInt32;
  var
    ShiftedOut: UInt32;
    Distance:   UInt32;
  begin
    If (Shift > 0) and (Shift <= 32) then
      begin
        If Shift = 32 then Result := 0
          else Result := Value shr Shift;
        ShiftedOut := Value and (UInt32($FFFFFFFF) shr (32 - Shift));
        case RoundMode of
              // nearest
          0:  If ShiftedOut <> 0 then
                begin
                  If Shift >= 32 then Distance := UInt32(-Int32(ShiftedOut))
                    else Distance := UInt32((UInt32(1) shl Shift) - ShiftedOut);
                  If (Distance < ShiftedOut) or ((Distance = ShiftedOut) and ((Result and 1) <> 0)) then
                    Inc(Result);
                end;
              // down
          1:  If (Sign <> 0) and (ShiftedOut <> 0) then
                Inc(Result);
              // up
          2:  If (Sign = 0) and (ShiftedOut <> 0) then
                Inc(Result);
        else
          {truncate}  // nothing to do
        end;
      end
    else Result := Value;
  end;

begin
MXCSR := GetMXCSR;
RoundMode := Integer((MXCSR shr 13) and 3);
Sign := PUInt32(SinglePtr)^ and UInt32($80000000);
Exponent := Int32(PUInt32(SinglePtr)^ shr 23) and $FF;
Mantissa := PUInt32(SinglePtr)^ and UInt32($007FFFFF);
case Exponent of
        // zero or subnormal
    0:  If Mantissa <> 0 then
          begin
            // subnormal
            If (MXCSR and MXCSR_EUnderflow) <> 0 then
              begin
                If ((RoundMode = 1{down}) and (Sign <> 0)) or
                   ((RoundMode = 2{up}) and (Sign = 0)) then
                  // convert to smallest representable number
                  PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16(1)
                else
                  // convert to signed zero
                  PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
              end
            // signal underflow
            else raise EUnderflow.Create('Floating point underflow');
          end
        // return signed zero
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16);

        // exponent is too small to be represented in half even as subnormal
   1..
  $65:  If (MXCSR and MXCSR_EUnderflow) <> 0 then
          begin
            If ((RoundMode = 1{down}) and (Sign <> 0)) or
               ((RoundMode = 2{up}) and (Sign = 0)) then
              // convert to smallest representable number
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16(1)
            else
              // convert to signed zero
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
          end
        // signal underflow
        else raise EUnderflow.Create('Floating point underflow');

        // result is subnormal value (resulting exponent in half is 0)
  $66..
  $71:  If (MXCSR and MXCSR_EUnderflow) <> 0 then
         PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or
           ShiftMantissa(Mantissa or UInt32($00800000),$7E - Exponent)
        else
          // signal underflow
          raise EUnderflow.Create('Floating point underflow');

        // exponent is too large to be represented in half (resulting exponent
        // would be larger than $1E)
  $8F..
  $FE:  If (MXCSR and MXCSR_EOverflow) <> 0 then
          begin
            If (RoundMode = 3{trunc}) or
               ((RoundMode = 1{down}) and (Sign = 0)) or
               ((RoundMode = 2{up}) and (Sign <> 0)) then
              // convert to largest representable number
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16($7BFF)
            else
              // convert to signed infinity
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16($7C00);
          end
        // signal overflow
        else raise EOverflow.Create('Floating point overflow');

        // special cases (INF, NaN, ...)
  $FF:  If Mantissa <> 0 then
          begin
            If (Mantissa and UInt32($00400000)) = 0 then
              begin
                // signalled NaN
                If (MXCSR and MXCSR_EInvalidOP) <> 0 then
                  // quiet signed NaN with truncated mantissa
                  PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16($7E00) or
                                       UInt16(Mantissa shr 13)
                else
                  // signaling NaN
                  raise EInvalidOp.Create('Invalid floating point operation');
              end
            // quiet signed NaN with truncated mantisssa
            else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16($7C00) or
                                      UInt16(Mantissa shr 13);
          end
        // signed infinity
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16($7C00);
else
  // representable numbers, normalized value
  Exponent := Exponent - 112;
  // mantissa shift correction
  Mantissa := ShiftMantissa(Mantissa,13);
  If (Mantissa and UInt32($00000400)) <> 0 then
    Inc(Exponent);
  PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16((Exponent and $1F) shl 10) or
                       UInt16(Mantissa and $000003FF);
end;
end;

//==============================================================================

procedure Fce_HalfToSingle4x_Pas(HalfPtr, SinglePtr: Pointer); register;
begin
Fce_HalfToSingle_Pas(HalfPtr,SinglePtr);
Fce_HalfToSingle_Pas({%H-}Pointer({%H-}PtrUInt(HalfPtr) + 2),{%H-}Pointer({%H-}PtrUInt(SinglePtr) + 4));
Fce_HalfToSingle_Pas({%H-}Pointer({%H-}PtrUInt(HalfPtr) + 4),{%H-}Pointer({%H-}PtrUInt(SinglePtr) + 8));
Fce_HalfToSingle_Pas({%H-}Pointer({%H-}PtrUInt(HalfPtr) + 6),{%H-}Pointer({%H-}PtrUInt(SinglePtr) + 12));
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf4x_Pas(SinglePtr, HalfPtr: Pointer); register;
begin
Fce_SingleToHalf_Pas(SinglePtr,HalfPtr);
Fce_SingleToHalf_Pas({%H-}Pointer({%H-}PtrUInt(SinglePtr) + 4),{%H-}Pointer({%H-}PtrUInt(HalfPtr) + 2));
Fce_SingleToHalf_Pas({%H-}Pointer({%H-}PtrUInt(SinglePtr) + 8),{%H-}Pointer({%H-}PtrUInt(HalfPtr) + 4));
Fce_SingleToHalf_Pas({%H-}Pointer({%H-}PtrUInt(SinglePtr) + 12),{%H-}Pointer({%H-}PtrUInt(HalfPtr) + 6));
end;

//==============================================================================

{$IFNDEF PurePascal}

{$IFDEF ASMSuppressSizeWarnings}
  {$WARN 2087 OFF}  //  Supresses warnings on following $WARN
  {$WARN 7121 OFF}  //  Warning: Check size of memory operand "op: memory-operand-size is X bits, but expected [Y bits]"
{$ENDIF}

procedure Fce_HalfToSingle_Asm(Input, Output: Pointer); register; assembler;
asm
    MOV     AX,   word ptr [Input]
    AND     EAX,  $FFFF
    MOVD    XMM0, EAX

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVSS   dword ptr [Output], XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Asm(Input, Output: Pointer); register; assembler;
asm
    MOVSS   XMM0, dword ptr [Input]

    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVD    EAX,  XMM0
    MOV     word ptr [Output],  AX
end;

//------------------------------------------------------------------------------

procedure Fce_HalfToSingle4x_Asm(Input, Output: Pointer); register; assembler;
asm
    MOVSD   XMM0, qword ptr [Input]

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVUPS  dqword ptr [Output],  XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingletoHalf4x_Asm(Input, Output: Pointer); register; assembler;
asm
    MOVUPS  XMM0, dqword ptr [Input]

    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVSD   qword ptr [Output],   XMM0
end;

{$IFDEF ASMSuppressSizeWarnings}
  {$WARN 7122 ON}
  {$WARN 2087 ON}
{$ENDIF}

{$ENDIF}

//==============================================================================

var
  Var_HalfToSingle:   procedure(Input, Output: Pointer); register;
  Var_SingleToHalf:   procedure(Input, Output: Pointer); register;
  Var_HalfToSingle4x: procedure(Input, Output: Pointer); register;
  Var_SingleToHalf4x: procedure(Input, Output: Pointer); register;


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

procedure HalfToSingle4x(Input, Output: Pointer);
begin
Var_HalfToSingle4x(Input,Output);
end;

//------------------------------------------------------------------------------

procedure SingleToHalf4x(Input, Output: Pointer);
begin
Var_SingleToHalf4x(Input,Output);
end;

//==  Number information functions  ============================================
//------------------------------------------------------------------------------

Function IsZero(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
Result := _Value and UInt16($7FFF) = 0;
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
Result := ((_Value and UInt16($7C00)) = $7C00) and ((_Value and UInt16($03FF)) <> 0);
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
Result := ((_Value and UInt16($7C00)) = $7C00) and ((_Value and UInt16($03FF)) = 0);
end;

//==  Sign-related functions  ==================================================
//------------------------------------------------------------------------------

Function Sign(const Value: Half): TValueSign;
var
  _Value: UInt16 absolute Value;
begin
If (_Value and UInt16($7FFF)) <> 0 then
  begin
    If (_Value and UInt16($8000)) <> 0 then
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
_Result := _Value and UInt16($7FFF);
end;

//------------------------------------------------------------------------------

Function Minus(const Value: Half): Half;
var
  _Value:   UInt16 absolute Value;
  _Result:  UInt16 absolute Result;
begin
_Result := _Value xor UInt16($8000);
end;

//==  Comparison functions  ====================================================
//------------------------------------------------------------------------------

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

//==  Arithmetic functions  ====================================================
//------------------------------------------------------------------------------

Function Add(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) + HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Substract(const A,B: Half): Half;
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

//==  Operators overloading  ===================================================
//------------------------------------------------------------------------------

{$IFDEF FPC}

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

operator + (A,B: Half): Half;
begin
Result := Add(A,B);
end;

//------------------------------------------------------------------------------

operator - (A,B: Half): Half;
begin
Result := Substract(A,B);
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

//==  Unit initialization  =====================================================
//------------------------------------------------------------------------------

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
{$IF Defined(AllowF16CExtension) and not Defined(PurePascal)}
with TSimpleCPUID.Create do
try
  If Info.SupportedExtensions.F16C and Info.SupportedExtensions.SSE2 then
    begin
      Var_HalfToSingle := Fce_HalfToSingle_Asm;
      Var_SingleToHalf := Fce_SingleToHalf_Asm;
      Var_HalfToSingle4x := Fce_HalfToSingle4x_Asm;
      Var_SingleToHalf4x := Fce_SingleToHalf4x_Asm;
    end;
finally
  Free;
end;
{$IFEND}
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

end.
