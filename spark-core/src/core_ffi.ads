--  FFI boundary for calling SPARK core from Haskell
pragma SPARK_Mode (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Core_FFI is

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
