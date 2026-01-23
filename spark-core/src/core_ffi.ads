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

--  FFI boundary for calling SPARK core from Haskell
pragma SPARK_Mode (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Core_FFI is

   --  Ada runtime initialization (call before other FFI functions)
   procedure Hadlink_Init
   with
     Export        => True,
     Convention    => C,
     External_Name => "hadlink_init";

   --  Ada runtime finalization (call on shutdown)
   procedure Hadlink_Final
   with
     Export        => True,
     Convention    => C,
     External_Name => "hadlink_final";

   --  Result codes matching Core.Result_Code
   type FFI_Result_Code is new int;

   FFI_Success              : constant FFI_Result_Code := 0;
   FFI_Invalid_Length       : constant FFI_Result_Code := 1;
   FFI_Invalid_Scheme       : constant FFI_Result_Code := 2;
   FFI_Invalid_Host         : constant FFI_Result_Code := 3;
   FFI_Private_Address      : constant FFI_Result_Code := 4;
   FFI_Credentials_Present  : constant FFI_Result_Code := 5;
   FFI_Invalid_Characters   : constant FFI_Result_Code := 6;

   --  Canonicalize URL
   --  Input: null-terminated C string
   --  Output: buffer for canonical URL (must be at least 2048 bytes)
   --  Output_Len: actual length written
   --  Returns: result code
   function Canonicalize_FFI
     (Input      : chars_ptr;
      Output     : chars_ptr;
      Output_Len : access size_t)
     return FFI_Result_Code
   with
     Export        => True,
     Convention    => C,
     External_Name => "hadlink_canonicalize";

   --  Generate short code
   --  URL: canonical URL (null-terminated C string)
   --  Secret: 32-byte secret key
   --  Output: buffer for short code (must be at least 8 bytes)
   --  Returns: 0 on success, non-zero on error
   function Make_Short_Code_FFI
     (URL    : chars_ptr;
      Secret : chars_ptr;
      Output : chars_ptr)
     return int
   with
     Export        => True,
     Convention    => C,
     External_Name => "hadlink_make_short_code";

end Core_FFI;
