--  hadlink - High-assurance URL shortener
--  Copyright (C) 2026 hadlink contributors
--
--  PROTOTYPE: Gold-level SPARK proof using Unified Functions
--  This eliminates pragma Assume by having query functions call the same
--  predicates used during validation, enabling direct proof by substitution.
--
pragma SPARK_Mode (On);

package Core_Gold with Pure is

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

   --  Opaque URL types
   type Raw_URL is private;
   type Valid_URL is private;
   type Short_Code is private;

   --  Secret key for HMAC-based short code generation
   subtype Secret_Key is String (1 .. 32);

   ---------------------------------------------------------------------------
   --  Shared Validation Predicates
   --
   --  These are used by BOTH Canonicalize (for validation) and the query
   --  functions (for postcondition proofs). This unification is the key
   --  to eliminating pragma Assume.
   ---------------------------------------------------------------------------

   --  Check if string has valid HTTP/HTTPS scheme
   function Has_Valid_Scheme (S : String) return Boolean is
     ((S'Length >= 8 and then S (S'First .. S'First + 7) = "https://")
      or else
      (S'Length >= 7 and then S (S'First .. S'First + 6) = "http://"))
   with Pre => S'Length >= 1 and then
               S'First >= 1 and then
               S'Last < Integer'Last - 10;

   --  Check if string contains credentials (user:pass@)
   function Has_Credentials (S : String) return Boolean
   with Pre => S'Length >= 1 and then
               S'First >= 1 and then
               S'Last < Integer'Last - 10;

   --  Check if host portion is a private IP
   function Has_Private_Host (S : String) return Boolean
   with Pre => S'Length >= 7 and then
               S'First >= 1 and then
               S'Last < Integer'Last - 10;

   ---------------------------------------------------------------------------
   --  Valid_URL Accessors
   ---------------------------------------------------------------------------

   function To_String (URL : Valid_URL) return String
   with Post => To_String'Result'Length >= 1 and then
                To_String'Result'Length <= Max_URL_Length and then
                To_String'Result'First = 1;

   function Length (URL : Valid_URL) return URL_Length;

   ---------------------------------------------------------------------------
   --  Query Functions - Expression Functions Calling Shared Predicates
   --
   --  The key insight: these call the SAME predicates that Canonicalize
   --  uses for validation. Combined with Make_Valid_URL's postcondition
   --  (To_String(Result) = Input), the prover can chain:
   --
   --    Has_Valid_Scheme(Input) = True  [checked by Canonicalize]
   --    To_String(URL) = Input          [guaranteed by Make_Valid_URL]
   --    Therefore: Has_Valid_Scheme(To_String(URL)) = True
   --    Therefore: Is_HTTP_Or_HTTPS(URL) = True
   ---------------------------------------------------------------------------

   function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean is
     (Has_Valid_Scheme (To_String (URL)))
   with Pre => Length (URL) >= 7;

   function Not_Private_Address (URL : Valid_URL) return Boolean is
     (not Has_Private_Host (To_String (URL)))
   with Pre => Length (URL) >= 7;

   function No_Credentials (URL : Valid_URL) return Boolean is
     (not Has_Credentials (To_String (URL)))
   with Pre => Length (URL) >= 7;

   ---------------------------------------------------------------------------
   --  Short_Code Type
   ---------------------------------------------------------------------------

   function Length (Code : Short_Code) return Natural
   with Post => Length'Result = Short_Code_Length;

   function To_String (Code : Short_Code) return String
   with Post => To_String'Result'Length = Short_Code_Length;

   ---------------------------------------------------------------------------
   --  Core Operations
   ---------------------------------------------------------------------------

   --  Result type for Canonicalize
   type Canonicalize_Result is record
      Status : Result_Code;
      URL    : Valid_URL;
   end record;

   --  Constructor with explicit postcondition about content equality
   --  This is the bridge that enables proof chaining
   function Make_Valid_URL (S : String) return Valid_URL
   with
     Pre  => S'Length >= 7 and then
             S'Length <= Max_URL_Length and then
             S'First = 1,
     Post => To_String (Make_Valid_URL'Result) = S and then
             Length (Make_Valid_URL'Result) = S'Length;

   --  Canonicalize with provable postcondition (NO pragma Assume!)
   --
   --  Proof strategy:
   --  1. Canonicalize checks Has_Valid_Scheme(Input) - returns if False
   --  2. Canonicalize checks Has_Credentials(Input) - returns if True
   --  3. Canonicalize checks Has_Private_Host(Input) - returns if True
   --  4. Make_Valid_URL(Input) guarantees To_String(Result.URL) = Input
   --  5. Query functions call predicates on To_String(URL)
   --  6. By substitution: predicate(To_String(URL)) = predicate(Input)
   --  7. Therefore postcondition is satisfied
   --
   function Canonicalize (Input : String) return Canonicalize_Result
   with
     Pre  => Input'Length >= 1 and then
             Input'Length <= Max_URL_Length and then
             Input'First = 1 and then
             Input'Last < Integer'Last - 10,
     Post => (if Canonicalize'Result.Status = Success
              then To_String (Canonicalize'Result.URL) = Input and then
                   Is_HTTP_Or_HTTPS (Canonicalize'Result.URL) and then
                   Not_Private_Address (Canonicalize'Result.URL) and then
                   No_Credentials (Canonicalize'Result.URL));

   --  Generate deterministic short code from valid URL
   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   with
     Pre  => Length (URL) >= 7,
     Post => Length (Make_Short_Code'Result) = Short_Code_Length;

private

   type Raw_URL is record
      Data : String (1 .. Max_URL_Length);
      Len  : URL_Length;
   end record;

   type Valid_URL is record
      Data : String (1 .. Max_URL_Length) := (others => ' ');
      Len  : URL_Length := 1;
   end record;

   type Short_Code is record
      Data : String (1 .. Short_Code_Length) := (others => '0');
   end record;

   function To_String (URL : Valid_URL) return String is
     (URL.Data (1 .. URL.Len));

   function Length (URL : Valid_URL) return URL_Length is
     (URL.Len);

   function Length (Code : Short_Code) return Natural is
     (Short_Code_Length);

   function To_String (Code : Short_Code) return String is
     (Code.Data);

end Core_Gold;
