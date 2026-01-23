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

--  Core types and interfaces for hadlink
--  SPARK mode for formal verification
pragma SPARK_Mode (On);

package Core with Pure is

   --  Maximum URL length (enforced at boundary)
   Max_URL_Length : constant := 2048;

   --  Fixed short code length
   Short_Code_Length : constant := 8;

   --  Base62 alphabet size
   Base62_Size : constant := 62;

   --  URL length constraints
   subtype URL_Length is Natural range 1 .. Max_URL_Length;

   --  Short code length constraint
   subtype Code_Length is Natural range Short_Code_Length .. Short_Code_Length;

   --  Result codes for operations
   type Result_Code is
     (Success,
      Invalid_Length,
      Invalid_Scheme,
      Invalid_Host,
      Private_Address,
      Credentials_Present,
      Invalid_Characters);

   --  Opaque URL types (construction only via validation)
   type Raw_URL is private;
   type Valid_URL is private;
   type Short_Code is private;

   --  Secret key for HMAC-based short code generation
   subtype Secret_Key is String (1 .. 32);

   --  Result type for Canonicalize
   type Canonicalize_Result is record
      Status : Result_Code;
      URL    : Valid_URL;
   end record;

   --  Canonicalize a raw URL into a valid, normalized form
   --  Pre: Input length is within bounds and indices are reasonable
   --  Post: If Success, output is canonicalized and safe
   function Canonicalize (Input : String) return Canonicalize_Result
   with
     Pre  => Input'Length <= Max_URL_Length and then
             Input'First >= 1 and then
             Input'Last < Integer'Last - 10,
     Post => (if Canonicalize'Result.Status = Success
              then Is_HTTP_Or_HTTPS (Canonicalize'Result.URL)
                and then Not_Private_Address (Canonicalize'Result.URL)
                and then No_Credentials (Canonicalize'Result.URL));

   --  Generate deterministic short code from valid URL
   --  Pre: URL is validated
   --  Post: Code is exactly Short_Code_Length characters
   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   with
     Post => Length (Make_Short_Code'Result) = Short_Code_Length;

   --  Query functions for proof
   function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean;
   function Not_Private_Address (URL : Valid_URL) return Boolean;
   function No_Credentials (URL : Valid_URL) return Boolean;
   function Length (Code : Short_Code) return Natural
   with Post => Length'Result = Short_Code_Length;
   function To_String (Code : Short_Code) return String
   with Post => To_String'Result'Length = Short_Code_Length;
   function To_String (URL : Valid_URL) return String
   with Post => To_String'Result'Length >= 1 and then
                To_String'Result'Length <= Max_URL_Length and then
                To_String'Result'First = 1;

private

   --  Private representations (not exposed to FFI)
   type Raw_URL is record
      Data : String (1 .. Max_URL_Length);
      Len  : URL_Length;
   end record;

   type Valid_URL is record
      Data : String (1 .. Max_URL_Length);
      Len  : URL_Length;
   end record;

   type Short_Code is record
      Data : String (1 .. Short_Code_Length);
   end record;

end Core;
