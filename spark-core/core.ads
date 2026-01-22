-- Core types and interfaces for hadlink
-- SPARK mode for formal verification
pragma SPARK_Mode (On);

package Core with Pure is

   -- Maximum URL length (enforced at boundary)
   Max_URL_Length : constant := 2048;
   
   -- Fixed short code length
   Short_Code_Length : constant := 8;
   
   -- Base62 alphabet size
   Base62_Size : constant := 62;

   -- URL length constraints
   subtype URL_Length is Natural range 1 .. Max_URL_Length;
   
   -- Short code length constraint
   subtype Code_Length is Natural range Short_Code_Length .. Short_Code_Length;

   -- Result codes for operations
   type Result_Code is
     (Success,
      Invalid_Length,
      Invalid_Scheme,
      Invalid_Host,
      Private_Address,
      Credentials_Present,
      Invalid_Characters);

   -- Opaque URL types (construction only via validation)
   type Raw_URL is private;
   type Valid_URL is private;
   type Short_Code is private;

   -- Secret key for HMAC-based short code generation
   subtype Secret_Key is String (1 .. 32);

   -- Result type for Canonicalize
   type Canonicalize_Result is record
      Status : Result_Code;
      URL    : Valid_URL;
   end record;

   -- Canonicalize a raw URL into a valid, normalized form
   -- Pre: Input length is within bounds
   -- Post: If Success, output is canonicalized and safe
   function Canonicalize (Input : String) return Canonicalize_Result
   with
     Pre  => Input'Length <= Max_URL_Length,
     Post => (if Canonicalize'Result.Status = Success
              then Is_HTTP_Or_HTTPS (Canonicalize'Result.URL)
                and then Not_Private_Address (Canonicalize'Result.URL)
                and then No_Credentials (Canonicalize'Result.URL));

   -- Generate deterministic short code from valid URL
   -- Pre: URL is validated
   -- Post: Code is exactly Short_Code_Length characters
   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   with
     Post => Length (Make_Short_Code'Result) = Short_Code_Length;

   -- Query functions for proof
   function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean;
   function Not_Private_Address (URL : Valid_URL) return Boolean;
   function No_Credentials (URL : Valid_URL) return Boolean;
   function Length (Code : Short_Code) return Natural;
   function To_String (Code : Short_Code) return String;
   function To_String (URL : Valid_URL) return String;

private

   -- Private representations (not exposed to FFI)
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
