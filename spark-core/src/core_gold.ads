--  hadlink - High-assurance URL shortener
--  Copyright (C) 2026 hadlink contributors
--
--  PROTOTYPE: Gold-level SPARK proof using Type_Invariant
--  This eliminates pragma Assume by making Valid_URL self-certifying
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

   --  Secret key for HMAC-based short code generation
   subtype Secret_Key is String (1 .. 32);

   ---------------------------------------------------------------------------
   --  Validation Predicates (public for use in Type_Invariant)
   ---------------------------------------------------------------------------

   --  Check if string has valid HTTP/HTTPS scheme
   function Has_Valid_Scheme (S : String) return Boolean
   with Pre => S'Length >= 1 and then S'Last < Integer'Last - 10;

   --  Check if string contains credentials (user:pass@)
   --  Simplified: looks for @ with : before it (after scheme)
   function Has_Credentials (S : String) return Boolean
   with Pre => S'Length >= 1 and then
               S'First >= 1 and then
               S'Last < Integer'Last - 10;

   --  Check if host portion is a private IP
   function Has_Private_Host (S : String) return Boolean
   with Pre => S'Length >= 7 and then S'Last < Integer'Last - 10;

   ---------------------------------------------------------------------------
   --  Valid_URL Type with Invariant
   ---------------------------------------------------------------------------

   type Valid_URL is private with
      Type_Invariant => Valid_URL_Properties (Valid_URL);

   --  The type invariant: all Valid_URLs satisfy these properties
   function Valid_URL_Properties (URL : Valid_URL) return Boolean;

   --  Public accessors
   function To_String (URL : Valid_URL) return String
   with Post => To_String'Result'Length >= 1 and then
                To_String'Result'Length <= Max_URL_Length and then
                To_String'Result'First = 1;

   function Length (URL : Valid_URL) return URL_Length;

   --  Query functions (now trivially true due to Type_Invariant)
   function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean
   with Post => Is_HTTP_Or_HTTPS'Result = True;  -- Guaranteed by invariant

   function Not_Private_Address (URL : Valid_URL) return Boolean
   with Post => Not_Private_Address'Result = True;  -- Guaranteed by invariant

   function No_Credentials (URL : Valid_URL) return Boolean
   with Post => No_Credentials'Result = True;  -- Guaranteed by invariant

   ---------------------------------------------------------------------------
   --  Short_Code Type
   ---------------------------------------------------------------------------

   type Short_Code is private;

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
      URL    : Valid_URL;  -- Only meaningful when Status = Success
   end record;

   --  Canonicalize a raw URL into a valid, normalized form
   --  Note: Postcondition no longer needs pragma Assume!
   --  The Type_Invariant on Valid_URL guarantees the properties.
   function Canonicalize (Input : String) return Canonicalize_Result
   with
     Pre  => Input'Length >= 1 and then
             Input'Length <= Max_URL_Length and then
             Input'First >= 1 and then
             Input'Last < Integer'Last - 10,
     Post => (if Canonicalize'Result.Status = Success
              then Is_HTTP_Or_HTTPS (Canonicalize'Result.URL)
                and then Not_Private_Address (Canonicalize'Result.URL)
                and then No_Credentials (Canonicalize'Result.URL));
   --  ^^^ These are now trivially provable because Valid_URL has Type_Invariant

   --  Generate deterministic short code from valid URL
   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   with
     Post => Length (Make_Short_Code'Result) = Short_Code_Length;

private

   --  Private representation
   type Valid_URL is record
      Data : String (1 .. Max_URL_Length) := (others => ' ');
      Len  : URL_Length := 19;  -- Length of placeholder
   end record;

   type Short_Code is record
      Data : String (1 .. Short_Code_Length) := (others => '0');
   end record;

   --  Implementation of invariant check
   function Valid_URL_Properties (URL : Valid_URL) return Boolean is
     (URL.Len >= 7 and then
      Has_Valid_Scheme (URL.Data (1 .. URL.Len)) and then
      not Has_Credentials (URL.Data (1 .. URL.Len)) and then
      not Has_Private_Host (URL.Data (1 .. URL.Len)));

   function To_String (URL : Valid_URL) return String is
     (URL.Data (1 .. URL.Len));

   function Length (URL : Valid_URL) return URL_Length is
     (URL.Len);

   function Length (Code : Short_Code) return Natural is
     (Short_Code_Length);

   function To_String (Code : Short_Code) return String is
     (Code.Data);

   --  Query functions - now just delegate to predicates
   function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean is
     (Has_Valid_Scheme (URL.Data (1 .. URL.Len)));

   function Not_Private_Address (URL : Valid_URL) return Boolean is
     (not Has_Private_Host (URL.Data (1 .. URL.Len)));

   function No_Credentials (URL : Valid_URL) return Boolean is
     (not Has_Credentials (URL.Data (1 .. URL.Len)));

   --  Placeholder URL that satisfies invariant (used for error cases)
   --  "https://example.com" is valid: correct scheme, public host, no creds
   function Placeholder_URL return Valid_URL is
     ((Data => "https://example.com" & (20 .. Max_URL_Length => ' '),
       Len  => 19));

end Core_Gold;
