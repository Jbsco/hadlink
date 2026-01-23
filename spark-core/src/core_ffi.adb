--  hadlink - High-assurance URL shortener
--  Copyright (C) 2026 hadlink contributors
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

--  FFI boundary implementation
pragma SPARK_Mode (Off);

with Core; use Core;

package body Core_FFI is

   --  Ada runtime initialization
   procedure Hadlink_Init is
   begin
      --  Call Ada elaboration (automatically handled by pragma Elaborate_Body)
      null;
   end Hadlink_Init;

   --  Ada runtime finalization
   procedure Hadlink_Final is
   begin
      null;
   end Hadlink_Final;

   --  Forward declaration for private helper
   function Result_Code_To_FFI (RC : Result_Code) return FFI_Result_Code;

   function Result_Code_To_FFI (RC : Result_Code) return FFI_Result_Code is
   begin
      case RC is
         when Success             => return FFI_Success;
         when Invalid_Length      => return FFI_Invalid_Length;
         when Invalid_Scheme      => return FFI_Invalid_Scheme;
         when Invalid_Host        => return FFI_Invalid_Host;
         when Private_Address     => return FFI_Private_Address;
         when Credentials_Present => return FFI_Credentials_Present;
         when Invalid_Characters  => return FFI_Invalid_Characters;
      end case;
   end Result_Code_To_FFI;

   function Canonicalize_FFI
     (Input      : chars_ptr;
      Output     : chars_ptr;
      Output_Len : access size_t)
     return FFI_Result_Code
   is
      Input_Str : constant String := Value (Input);
      Result : Core.Canonicalize_Result;
   begin
      --  Validate input length
      if Input_Str'Length = 0 or else
         Input_Str'Length > Core.Max_URL_Length
      then
         Output_Len.all := 0;
         return FFI_Invalid_Length;
      end if;

      --  Call SPARK canonicalize
      Result := Core.Canonicalize (Input_Str);

      if Result.Status = Success then
         declare
            Output_Str : constant String := To_String (Result.URL);
         begin
            --  Convert to char_array and copy (no checking)
            declare
               C_Array : constant char_array :=
                 To_C (Output_Str, Append_Nul => True);
            begin
               Update (Output, 0, C_Array, Check => False);
            end;
            Output_Len.all := Output_Str'Length;
         end;
      else
         Output_Len.all := 0;
      end if;

      return Result_Code_To_FFI (Result.Status);
   end Canonicalize_FFI;

   function Make_Short_Code_FFI
     (URL    : chars_ptr;
      Secret : chars_ptr;
      Output : chars_ptr)
     return int
   is
      URL_Str    : constant String := Value (URL);
      Secret_Str : constant String := Value (Secret);
      Canon_Result : Core.Canonicalize_Result;
      Key        : Core.Secret_Key;
   begin
      --  Validate secret length
      if Secret_Str'Length /= 32 then
         return 1;
      end if;

      --  Convert secret to fixed-length key
      Key := Core.Secret_Key
        (Secret_Str (Secret_Str'First .. Secret_Str'First + 31));

      --  Reconstruct Valid_URL (assumes URL_Str is already validated)
      Canon_Result := Core.Canonicalize (URL_Str);
      if Canon_Result.Status /= Success then
         return 2;
      end if;

      --  Generate short code
      declare
         Code : constant Short_Code :=
            Make_Short_Code (Canon_Result.URL, Key);
         Code_Str : constant String := To_String (Code);
      begin
         --  Convert to char_array and copy (no checking)
         declare
            C_Array : constant char_array :=
              To_C (Code_Str, Append_Nul => True);
         begin
            Update (Output, 0, C_Array, Check => False);
         end;
      end;

      return 0;
   end Make_Short_Code_FFI;

end Core_FFI;
