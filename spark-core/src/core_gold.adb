--  hadlink - High-assurance URL shortener
--  Copyright (C) 2026 hadlink contributors
--
--  PROTOTYPE: Gold-level SPARK proof using Type_Invariant
--
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with SPARKNaCl;          use SPARKNaCl;
with SPARKNaCl.MAC;
with SPARKNaCl.Hashing.SHA256;

package body Core_Gold is

   --  Base62 alphabet for encoding
   Base62_Alphabet : constant String :=
     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

   ---------------------------------------------------------------------------
   --  Helper: Extract host from URL string
   ---------------------------------------------------------------------------
   procedure Extract_Host
     (Input      : String;
      Host_Start : out Natural;
      Host_End   : out Natural;
      Valid      : out Boolean)
   with
     Pre  => Input'Length >= 7 and then Input'Last < Integer'Last - 10,
     Post => (if Valid then
                Host_Start >= Input'First and then
                Host_End <= Input'Last and then
                Host_Start <= Host_End)
   is
      Scheme_End : Natural;
   begin
      Host_Start := 0;
      Host_End := 0;
      Valid := False;

      --  Determine scheme length
      if Input'Length >= 8 and then
         Input (Input'First .. Input'First + 7) = "https://"
      then
         Scheme_End := Input'First + 7;
      elsif Input (Input'First .. Input'First + 6) = "http://" then
         Scheme_End := Input'First + 6;
      else
         return;
      end if;

      --  Host starts after scheme
      if Scheme_End >= Input'Last then
         return;
      end if;

      Host_Start := Scheme_End + 1;

      --  Find end of host (first '/', '?', '#', or end of string)
      Host_End := Input'Last;
      for I in Host_Start .. Input'Last loop
         if Input (I) = '/' or else Input (I) = '?' or else Input (I) = '#'
         then
            Host_End := I - 1;
            exit;
         end if;
         pragma Loop_Invariant (I >= Host_Start);
         pragma Loop_Invariant (Host_End = Input'Last);
      end loop;

      if Host_Start <= Host_End then
         Valid := True;
      end if;
   end Extract_Host;

   ---------------------------------------------------------------------------
   --  Helper: Check if host string is private IP
   ---------------------------------------------------------------------------
   function Is_Private_IP (Host : String) return Boolean
   with Pre => Host'Length >= 1 and then Host'Last < Integer'Last - 1
   is
   begin
      if Host = "localhost" then
         return True;
      end if;

      if Host'Length >= 3 then
         declare
            Prefix3 : constant String := Host (Host'First .. Host'First + 2);
         begin
            if Prefix3 = "10." or else Prefix3 = "127" then
               return True;
            end if;
         end;
      end if;

      if Host'Length >= 8 then
         declare
            Prefix7 : constant String := Host (Host'First .. Host'First + 6);
         begin
            if Prefix7 = "192.168" or else
               Prefix7 = "172.16." or else Prefix7 = "172.17." or else
               Prefix7 = "172.18." or else Prefix7 = "172.19." or else
               Prefix7 = "172.20." or else Prefix7 = "172.21." or else
               Prefix7 = "172.22." or else Prefix7 = "172.23." or else
               Prefix7 = "172.24." or else Prefix7 = "172.25." or else
               Prefix7 = "172.26." or else Prefix7 = "172.27." or else
               Prefix7 = "172.28." or else Prefix7 = "172.29." or else
               Prefix7 = "172.30." or else Prefix7 = "172.31."
            then
               return True;
            end if;
         end;
      end if;

      return False;
   end Is_Private_IP;

   ---------------------------------------------------------------------------
   --  Public Predicate Implementations
   ---------------------------------------------------------------------------

   function Has_Valid_Scheme (S : String) return Boolean
   is
   begin
      return (S'Length >= 8 and then S (S'First .. S'First + 7) = "https://")
             or else
             (S'Length >= 7 and then S (S'First .. S'First + 6) = "http://");
   end Has_Valid_Scheme;

   function Has_Credentials (S : String) return Boolean
   is
   begin
      for I in S'Range loop
         if S (I) = '@' then
            for J in S'First .. I - 1 loop
               if S (J) = ':' then
                  if J >= S'First + 5 then
                     if J > S'First + 6 or else
                        S (S'First .. S'First + 3) /= "http"
                     then
                        return True;
                     end if;
                  end if;
               end if;
               pragma Loop_Invariant (J >= S'First);
            end loop;
         end if;
         pragma Loop_Invariant (I >= S'First);
      end loop;
      return False;
   end Has_Credentials;

   function Has_Private_Host (S : String) return Boolean
   is
      Host_Start : Natural;
      Host_End   : Natural;
      Host_Valid : Boolean;
   begin
      Extract_Host (S, Host_Start, Host_End, Host_Valid);
      if not Host_Valid or else Host_Start = 0 then
         return True;  -- Invalid = treat as private (fail safe)
      end if;

      if Host_End >= Host_Start and then
         Host_End - Host_Start < Max_URL_Length
      then
         return Is_Private_IP (S (Host_Start .. Host_End));
      else
         return True;  -- Invalid = treat as private (fail safe)
      end if;
   end Has_Private_Host;

   ---------------------------------------------------------------------------
   --  Compute HMAC-SHA256
   ---------------------------------------------------------------------------
   function Compute_Hash
     (Message : String;
      Key     : Secret_Key)
     return String
   with
     Pre  => Message'Length >= 1 and then
             Message'Length <= Max_URL_Length and then
             Message'Last < Integer'Last,
     Post => Compute_Hash'Result'Length = 32 and then
             Compute_Hash'Result'First = 1
   is
      Msg_Len   : constant N32 := N32 (Message'Length);
      Msg_Bytes : Byte_Seq (0 .. Msg_Len - 1) := (others => 0);
      Key_Len   : constant N32 := N32 (Key'Length);
      Key_Bytes : Byte_Seq (0 .. Key_Len - 1) := (others => 0);
      Output    : SPARKNaCl.Hashing.SHA256.Digest;
      Result    : String (1 .. 32);
   begin
      for I in Message'Range loop
         Msg_Bytes (N32 (I - Message'First)) :=
           Byte (Character'Pos (Message (I)));
         pragma Loop_Invariant (I >= Message'First);
      end loop;

      for I in Key'Range loop
         Key_Bytes (N32 (I - Key'First)) :=
           Byte (Character'Pos (Key (I)));
         pragma Loop_Invariant (I >= Key'First);
      end loop;

      SPARKNaCl.MAC.HMAC_SHA_256 (Output, Msg_Bytes, Key_Bytes);

      for I in 1 .. 32 loop
         Result (I) := Character'Val (Integer (Output (N32 (I - 1))));
         pragma Loop_Invariant (I >= 1);
      end loop;

      return Result;
   end Compute_Hash;

   ---------------------------------------------------------------------------
   --  Create Valid_URL (internal helper)
   ---------------------------------------------------------------------------
   function Make_Valid_URL (S : String) return Valid_URL
   with
     Pre => S'Length >= 7 and then
            S'Length <= Max_URL_Length and then
            Has_Valid_Scheme (S) and then
            not Has_Credentials (S) and then
            not Has_Private_Host (S)
   is
      Result : Valid_URL;
   begin
      Result.Data (1 .. S'Length) := S;
      Result.Len := S'Length;
      return Result;
   end Make_Valid_URL;

   ---------------------------------------------------------------------------
   --  Canonicalize
   ---------------------------------------------------------------------------
   function Canonicalize (Input : String) return Canonicalize_Result
   is
      Host_Start : Natural;
      Host_End   : Natural;
      Host_Valid : Boolean;
      Result     : Canonicalize_Result;
   begin
      Result.URL := Placeholder_URL;
      Result.Status := Invalid_Length;

      --  Check length
      if Input'Length = 0 or else Input'Length > Max_URL_Length then
         Result.Status := Invalid_Length;
         return Result;
      end if;

      --  Check scheme
      if not Has_Valid_Scheme (Input) then
         Result.Status := Invalid_Scheme;
         return Result;
      end if;

      --  Check for credentials
      if Has_Credentials (Input) then
         Result.Status := Credentials_Present;
         return Result;
      end if;

      --  Ensure minimum length for Extract_Host
      if Input'Length < 7 then
         Result.Status := Invalid_Scheme;
         return Result;
      end if;

      --  Extract and validate host
      Extract_Host (Input, Host_Start, Host_End, Host_Valid);
      if not Host_Valid or else Host_Start = 0 then
         Result.Status := Invalid_Host;
         return Result;
      end if;

      --  Check for private addresses
      if Has_Private_Host (Input) then
         Result.Status := Private_Address;
         return Result;
      end if;

      --  All checks passed - create Valid_URL
      --  The Type_Invariant is automatically verified here because:
      --  1. Has_Valid_Scheme(Input) = True (checked above)
      --  2. Has_Credentials(Input) = False (checked above)
      --  3. Has_Private_Host(Input) = False (checked above)
      --  Therefore Make_Valid_URL's precondition is satisfied,
      --  and the resulting Valid_URL satisfies its Type_Invariant.
      Result.URL := Make_Valid_URL (Input);
      Result.Status := Success;

      --  NO PRAGMA ASSUME NEEDED!
      --  The postcondition is trivially true because:
      --  - Is_HTTP_Or_HTTPS returns True for any Valid_URL (by invariant)
      --  - Not_Private_Address returns True for any Valid_URL (by invariant)
      --  - No_Credentials returns True for any Valid_URL (by invariant)

      return Result;
   end Canonicalize;

   ---------------------------------------------------------------------------
   --  Make_Short_Code
   ---------------------------------------------------------------------------
   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   is
      URL_Str  : constant String := To_String (URL);
      Code     : Short_Code;
      Hash_Val : Unsigned_64 := 0;
   begin
      declare
         Hash : constant String := Compute_Hash (URL_Str, Secret);
      begin
         for I in 1 .. 8 loop
            Hash_Val := Hash_Val * 256;
            Hash_Val := Hash_Val + Unsigned_64 (Character'Pos (Hash (I)));
            pragma Loop_Invariant (I >= 1);
         end loop;
      end;

      for I in reverse 1 .. Short_Code_Length loop
         Code.Data (I) := Base62_Alphabet
           (Natural (Hash_Val mod Base62_Size) + 1);
         Hash_Val := Hash_Val / Base62_Size;
         pragma Loop_Invariant (I >= 1);
      end loop;

      return Code;
   end Make_Short_Code;

end Core_Gold;
